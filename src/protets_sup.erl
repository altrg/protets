-module(protets_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [{
        protets_worker, {protets_worker, start_link, []},
        temporary, 5000, worker, [protets_worker]
    }],
    Policy = {{simple_one_for_one, 1, 1}, Children},
    {ok, Policy}.