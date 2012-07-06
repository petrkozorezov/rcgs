-module(rcgs_vnode_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    start_vnode_entity/2
]).

%% Supervisor callbacks

-export([
    init/1
]).

%% Implementation

start_link() ->
    supervisor:start_link(?MODULE, []).

start_vnode_entity(SupRef, Args) ->
    supervisor:start_child(SupRef, [Args]).


init([]) ->
    Strategy = {simple_one_for_one, 10, 10},
    VnodeEntitySpec = {rcgs_vnode_entity, {rcgs_vnode_entity, start_link, []}, temporary, 5000, worker, [rcgs_vnode_entity]},
    {ok, {Strategy, [VnodeEntitySpec]}}.
