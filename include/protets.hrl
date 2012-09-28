-define(ERROR(Msg), ?ERROR(Msg, [])).
-define(ERROR(Msg, Params),
    error_logger:error_msg("[~p] " ++ Msg, [?MODULE | Params])).

-define(WARNING(Msg), ?WARNING(Msg, [])).
-define(WARNING(Msg, Params),
    error_logger:warning_msg("[~p] " ++ Msg, [?MODULE | Params])).

-define(INFO(Msg), ?INFO(Msg, [])).
-define(INFO(Msg, Params),
    error_logger:info_msg("[~p] " ++ Msg, [?MODULE | Params])).

-define(WORKERS_TABLE, '$protets_ets$').