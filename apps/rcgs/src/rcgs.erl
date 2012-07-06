-module(rcgs).
-include_lib("riak_core/include/riak_core_vnode.hrl").
-include("logger.hrl").


-export([
         start/0,
         stop/0,
         ping/2,
         call/2
        ]).

%% Public API

start() ->
    rcgs_apputils:start_application(?MODULE).

stop() ->
    application:stop(?MODULE).

% @doc Pings N vnodes with Key
ping(Key, N) ->
    PrefList = riak_core_apl:get_apl(chash:key_of(Key), N, ?MODULE),
    % ?LOG_INFO("~p", [PrefList]),
    F = fun(IndexNode) ->
            riak_core_vnode_master:sync_spawn_command(IndexNode, ping, rcgs_vnode_master)
        end,
    [F(Index) || Index <- PrefList].


-type entity_id() :: {atom(), binary()}.

-spec call(entity_id(), term()) -> {ok, term()} | {error, term()}.
call(EntityID, Message) ->
    [IndexNode] = riak_core_apl:get_apl(chash:key_of(EntityID), 1, ?MODULE),
    riak_core_vnode_master:sync_spawn_command(IndexNode, {handle_message, EntityID, Message}, rcgs_vnode_master).
