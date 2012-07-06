-module(rcgs_apputils).

-include_lib("kernel/include/file.hrl").

-export([

    childspec/4,
    permanent/3,
    transient/3,
    temporary/3,
    supervisor/3,

    is_app_enabled/0,
    is_app_enabled/1,

    options/1,

    priv_dir/0,
    priv_dir/1,
    priv_path/1,

    start_dependencies/1,
    start_application/1,
    get_not_running_dependencies/1

]).

-export([start_singleton/1]).

-define(DEFAULT_TIMEOUT, 5000).

%% Spec = {singleton, Spec} | {Name, Module, Entry} | {Name, Module} | Module
%% RegType = none | local | global
%% Options = none | inherit | global

permanent(Spec, RegType, Options) -> childspec(permanent, Spec, RegType, Options).
transient(Spec, RegType, Options) -> childspec(transient, Spec, RegType, Options).
temporary(Spec, RegType, Options) -> childspec(temporary, Spec, RegType, Options).
supervisor(Spec, RegType, Options) -> childspec(supervisor, Spec, RegType, Options).

childspec(Role, Spec, RegType, Options) ->
    {Name, Entry, Deps} = entry(Spec, {RegType, Options}),
    {Name, Entry, restart_mode(Role), kill_mode(Role), role(Role), Deps}.

entry({singleton, Spec}, {RegType, _}) when RegType =:= none ->
    throw({invalid_specification, Spec});

entry({singleton, Spec}, Options) ->
    {Name, MFA, Deps} = entry(Spec, Options),
    {Name, {?MODULE, start_singleton, [MFA]}, Deps};

entry({Name, Module, Entry}, {RegType, Options}) ->
    {Name, {Module, Entry, arguments(RegType, Name, options(Options, Name))}, [Module]};

entry({Name, Module}, Options) ->
    entry({Name, Module, start_link}, Options);

entry(Module, Options) ->
    entry({Module, Module, start_link}, Options).

is_app_enabled()  ->
    Globals = application:get_all_env(),
    rcgs_core_props:get([enabled], Globals, false).

is_app_enabled(App)  ->
    Globals = application:get_all_env(App),
    rcgs_core_props:get([enabled], Globals, false).

arguments(none, _, Args) -> [Args];
arguments(Type, Name, Args) -> [{Type, Name}, Args].

role(supervisor) -> supervisor;
role(_) -> worker.

restart_mode(supervisor) -> permanent;
restart_mode(Mode) -> Mode.

kill_mode(supervisor) -> infinity;
kill_mode(temporary) -> brutal_kill;
kill_mode(_) -> ?DEFAULT_TIMEOUT.

options({inherit, Name}) ->
    rcgs_core_props:get([Name], application:get_all_env(), []);

options(none) ->
    [];

options(global) ->
    application:get_all_env().

options(inherit, Name) ->
    options({inherit, Name});

options(none, _) ->
    options(none);

options(global, _) ->
    options(global);

options(Other, _) ->
    Other.

%% Private directories expansion

priv_dir() ->
    {ok, AppName} = application:get_application(),
    priv_dir(AppName).

priv_dir(AppName) ->
    case code:priv_dir(AppName) of
        Value when is_list(Value) ->
            Value ++ "/";
        _Error ->
            case file:read_file_info([priv]) of
                {ok, #file_info{type=directory}} ->
                    "priv/";
                _ ->
                    erlang:throw({error, failed_to_find_priv_dir})
            end
     end.

priv_path({AppName, [$/ | Relative]}) ->
    priv_dir(AppName) ++ Relative;

priv_path({AppName, Relative}) ->
    priv_dir(AppName) ++ Relative;

priv_path(Relative) ->
    {ok, AppName} = application:get_application(),
    priv_path({AppName, Relative}).

start_application(AppName) ->
    case application:start(AppName) of
        ok -> 
            ok;
        {error, {already_started, AppName}} -> 
            ok;
        {error, {not_started, _}} -> 
            start_dependencies(AppName),
            application:start(AppName);
        Error ->
            Error
    end.

start_dependencies(AppName) ->
    Deps = get_not_running_dependencies(AppName),
    [ ok = application:start(Dep) || Dep <- Deps ].

get_not_running_dependencies(AppName) ->
    Live = [ A || {A, _, _} <- application:which_applications() ],
    lists:reverse(gather_deps(AppName, []) -- [AppName | Live]).

gather_deps(AppName, Acc) ->
    case lists:member(AppName, Acc) of
        true -> 
            Acc;
        _    ->
            application:load(AppName),
            case application:get_key(AppName, applications) of
                {ok, DepsList} -> [AppName | lists:foldl(fun gather_deps/2, Acc, DepsList)];
                _              -> [AppName | Acc]
            end
    end.

%% Supervisor custom bootstrappers

start_singleton({Module, Function, Args}) ->
    case Result = (catch erlang:apply(Module, Function, Args)) of
        {ok, P} when is_pid(P) ->
            Result;
        {error, {already_started, P}} when is_pid(P) ->
            true = link(P),
            {ok, P};
        _ ->
            Result
    end.

