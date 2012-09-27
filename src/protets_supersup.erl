-module(protets_supersup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        {
            protets_sup, {protets_sup, start_link, []},
            permanent, 5000, supervisor, [protets_sup]
        },
        {
            protets_mon, {protets_mon, start_link, []},
            permanent, 5000, worker, [protets_mon]
        }
    ],
    Policy = {{one_for_one, 10, 10}, Children},
    {ok, Policy}.
