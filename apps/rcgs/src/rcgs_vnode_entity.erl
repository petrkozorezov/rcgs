-module(rcgs_vnode_entity).
-behaviour(gen_server).

-include("logger.hrl").


%% подписка: эвенты, фильтры, роли

%%  -define(HIBERNATE, , hibernate).   % hibernate on
    -define(HIBERNATE, , 10000).       % hibernate after 10 sec
%%  -define(HIBERNATE, ).              % hibernate off


-export([
         start_link/1,
         handoff/1,
         handle_message/3
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([behaviour_info/1]).

-record(state, {id, entity_state}).


behaviour_info(callbacks) -> [
    {init, 1},
    {handle_message, 2},
    {terminate, 2}
];
behaviour_info(_) -> undefined.



start_link(ID) ->
    gen_server:start_link(?MODULE, ID, []).

handoff(Pid) ->
    gen_server:call(Pid, {handoff}).

handle_message(Pid, Msg, From) ->
    gen_server:cast(Pid, {handle_message, Msg, From}).


init(Args) ->
    process_flag(trap_exit, true),
    State = case Args of
        [ID] ->
            ?LOG_DEBUG("[~p]: starting...", [ID]),
            {_, Name} = ID,
            #state{ id = ID, entity_state = call(ID, init, [Name]) };
        [ID, EntityState] ->
            ?LOG_DEBUG("[~p]: continue...", [ID]),
            #state{ id = ID, entity_state = EntityState }
    end,
    {ok, State ?HIBERNATE}.

handle_call({handoff}, _From, State=#state{entity_state = EntityState, id = ID}) ->
    ?LOG_DEBUG("[~p]: stopping for hangoff...", [ID]),
    {stop, {shutdown, handoff}, {ok, EntityState}, State};
handle_call(Msg, From, State) ->
    ?LOG_ERROR(": unexpected call received from ~p: ~p", [From, Msg]),
    {noreply, State ?HIBERNATE}.

handle_cast({handle_message, Msg, ReplyTo}, State=#state{entity_state = EntityState, id = ID}) ->
    ?LOG_DEBUG("[~p]: handling message...", [ID]),
    {Action, Reply, NewEntityState} = call(ID, handle_message, [Msg, EntityState]),
    riak_core_vnode:reply(ReplyTo, Reply),
    NewState = State#state{entity_state = NewEntityState},
    case Action of
        stop ->
            {stop, NewState};
        reply ->
            {noreply, NewState ?HIBERNATE}
    end;

handle_cast(Msg, State) ->
    ?LOG_ERROR(": unexpected cast received: ~p", [Msg]),
    {noreply, State ?HIBERNATE}.

handle_info(timeout, State) ->
    % ?LOG_DEBUG("[~s]: hibernating", [UserID]),
    {noreply, State, hibernate};

handle_info(Msg, State) ->
    ?LOG_ERROR(": unexpected info received: ~p", [Msg]),
    {noreply, State ?HIBERNATE}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate({shutdown, handoff}, #state{id = ID, entity_state = EntityState}) ->
    ?LOG_DEBUG("[~p]: handoff terminating...", [ID]),
    ok;
terminate(Reason, #state{id = ID, entity_state = EntityState}) ->
    ?LOG_DEBUG("[~p]: terminating...", [ID]),
    call(ID, terminate, [Reason, EntityState]),
    ok.

call({Type, _}, Method, Args) ->
    Mod = list_to_atom("rcgs_test_entity_" ++ atom_to_list(Type)),
    apply(Mod, Method, Args).
