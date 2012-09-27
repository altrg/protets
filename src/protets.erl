-module(protets).

-export([get/1, get/2, drop/1]).

get(Name) ->
    get(Name, []).
get(Name, Opts) when is_list(Opts) ->
    gen_server:call(protets_mon, {get, Name, Opts}).
drop(?WORKERS_TABLE) ->
    {error, denied};
drop(Name) ->
    gen_server:call(protets_mon, {drop, Name}).