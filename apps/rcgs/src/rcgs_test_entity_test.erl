-module(rcgs_test_entity_test).
-behaviour(rcgs_vnode_entity).

-include("logger.hrl").

-export([init/1, handle_message/2, terminate/2]).

-record(state, {id, counter}).

init(ID) ->
    ?LOG_INFO(": init"),
    #state{id = ID, counter = 0}.

handle_message(ping, State) ->
    ?LOG_INFO(": ping: ~p", [State]),
    {reply, pong, State#state{counter = State#state.counter + 1}};
handle_message(Msg, State) ->
    ?LOG_INFO(": unexpected message: ~p ~p", [Msg, State]),
    {reply, {error, undefined_message}, State}.

terminate(Reason, State) ->
    ?LOG_INFO(": terminate: ~p ~p", [Reason, State]),
    ok.
