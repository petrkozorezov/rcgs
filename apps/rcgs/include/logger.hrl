-ifndef(_logger_included).
-define(_logger_included, yeah).

-define(LOG(Level,  Pattern, Args),
    lager:log(Level, self(), "[~p]" ++ unicode:characters_to_list(list_to_binary(Pattern)), [?MODULE | Args])). %% как ещё сделать??

-define(LOG_DEBUG(Pattern), ?LOG(debug, Pattern, [])).
-define(LOG_DEBUG(Pattern, Args), ?LOG(debug, Pattern, Args)).

-define(LOG_INFO (Pattern), ?LOG(info , Pattern, [])).
-define(LOG_INFO (Pattern, Args), ?LOG(info , Pattern, Args)).

-define(LOG_WARN (Pattern), ?LOG(error , Pattern, [])).
-define(LOG_WARN (Pattern, Args), ?LOG(error , Pattern, Args)).

-define(LOG_ERROR(Pattern), ?LOG(error, Pattern, [])).
-define(LOG_ERROR(Pattern, Args), ?LOG(error, Pattern, Args)).

-define(LOG_FATAL(Pattern), ?LOG(fatal, Pattern, [])).
-define(LOG_FATAL(Pattern, Args), ?LOG(fatal, Pattern, Args)).


-define(TODO(Msg), ?TODO(Msg, [])).
-define(TODO(Msg, Args), ?LOG_ERROR(" [TODO]: " ++ (fun() -> TODO = Msg end)(), Args)).

-define(FIXME(Msg), ?FIXME(Msg, [])).
-define(FIXME(Msg, Args), ?LOG_ERROR(" [FIXME]: " ++ (fun() -> FIXME = Msg end)(), Args)).

-endif.
