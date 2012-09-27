-module(protets_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    protets_supersup:start_link().

stop(_State) ->
    ok.
