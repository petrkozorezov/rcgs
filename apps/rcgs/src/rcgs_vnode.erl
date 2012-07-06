-module(rcgs_vnode).
-behaviour(riak_core_vnode).

-include_lib("riak_core/include/riak_core_vnode.hrl").
-include("logger.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_coverage/4,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_exit/3]).

-record(state, {
    partition,
    pid2id,
    id2pid,
    sup_ref
}).



-opaque state()             :: #state{}.


%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).


-spec init(list()) -> {ok, state()}.
init([Partition]) ->
    {ok, SupRef} = rcgs_vnode_sup:start_link(),
    {ok, #state {
        partition = Partition,
        pid2id = ets:new(?MODULE, [bag, private]),
        id2pid = ets:new(?MODULE, [bag, private]),
        sup_ref = SupRef
    }}.


-spec handle_command(Cmd :: term(), Sender :: term(), State :: state()) ->
    {reply, term(), state()} |
    {noreply, state()} |
    {stop, term(), state()}.

handle_command(ping, _Sender, State) ->
    ?LOG_DEBUG("ping ;-)"),
    {reply, {pong, State#state.partition}, State};
handle_command({handle_message, EntityID, Message}, Sender, State) ->
    EntityPid = get_entity(EntityID, State),
    ok = rcgs_vnode_entity:handle_message(EntityPid, Message, Sender),
    {noreply, State};
handle_command(Message, Sender, State) ->
    ?LOG_INFO("unhandled_command, ~p ~p", [Message, Sender]),
    {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
    Fun1 =
        fun({ID, Pid}, Acc0) ->
            EntityState = hangoff_entity(Pid, ID, State),
            Fun(ID, EntityState, Acc0)
        end,
    Acc = ets:foldl(Fun1, Acc0, State#state.id2pid),
    {reply, Acc, State};
handle_handoff_command(Message, Sender, State) ->
    ?LOG_DEBUG("unexpected handoff_command: ~p ~p", [Message, Sender]),
    {noreply, State}.

handoff_starting(TargetNode, State) ->
    ?LOG_DEBUG("handoff_starting: ~p", [TargetNode]),
    {true, State}.

handoff_cancelled(State) ->
    ?LOG_DEBUG("handoff_cancelled"),
    {ok, State}.

handoff_finished(TargetNode, State) ->
    ?LOG_DEBUG("handoff_finished: ~p", [TargetNode]),
    {ok, State}.

handle_coverage(_, _, _, State) ->
    ?LOG_ERROR("unexpected handle_coverage"),
    {ok, State}.

handle_handoff_data(Data, State) ->
    ?LOG_DEBUG("handle_handoff_data: ~p", [Data]),
    {EntityID, EntityState} = binary_to_term(Data),
    _ = start_entity([EntityID, EntityState], State),
    {reply, ok, State}.

encode_handoff_item(ObjectID, ObjectValue) ->
    ?LOG_DEBUG("encode_handoff_item: ~p ~p", [ObjectID, ObjectValue]),
    term_to_binary({ObjectID, ObjectValue}).

is_empty(State=#state{id2pid = ID2Pid}) ->
    ?LOG_DEBUG("is_empty"),
    {ets:info(ID2Pid, size) =:= 0, State}.

delete(State) ->
    ?LOG_DEBUG("delete"),
    {ok, State}.

handle_exit(Pid, Reason, State) ->
    ?LOG_DEBUG("handle_exit: ~p ~p", [Pid, Reason]),
    {noreply, State}.

terminate(Reason, _State) ->
    ?LOG_DEBUG("handle_exit: ~p", [Reason]),
    ok.


%% local
get_entity(EntityID, State=#state{id2pid=ID2Pid}) ->
    case ets:lookup(ID2Pid, EntityID) of
        [] ->
            start_entity([EntityID], State);
        [{EntityID, Pid}] ->
            Pid
    end.

start_entity(Args=[EntityID | _], State=#state{sup_ref=SupRef, pid2id=Pid2ID, id2pid=ID2Pid}) ->
    {ok, Pid} = rcgs_vnode_sup:start_vnode_entity(SupRef, Args),
    ets:insert_new(Pid2ID, {Pid, EntityID}),
    ets:insert_new(ID2Pid, {EntityID, Pid}),
    Pid.

hangoff_entity(Pid, ID, State=#state{pid2id=Pid2ID, id2pid=ID2Pid}) ->
    {ok, EntityState} = rcgs_vnode_entity:handoff(Pid),
    ets:delete(Pid2ID, Pid),
    ets:delete(ID2Pid, ID),
    EntityState.
