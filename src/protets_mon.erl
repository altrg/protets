-module(protets_mon).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-include("protets.hrl").

-record(
    worker,
    {
        name                :: any(),
        pid                 :: pid(),
        ref                 :: any()
    }
).

-record(
    state,
    {}
).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
    ok = init_workers_table([named_table, ordered_set, {keypos, 2}]),
    {ok, #state{}}.

%% handles get call, returns {ok, TableId}.
handle_call({get, Name, Opts}, _From, State) ->
    {ok, TRef} = get_tref(Name, Opts),
    {reply, {ok, TRef}, State};
%% handles drop call, returns ok.
handle_call({drop, Name}, _From, State) ->
    case ets:lookup(?WORKERS_TABLE, Name) of
        [Worker] ->
            ok = stop_worker(Worker#worker.pid),
            ok;
        [] ->
            ok
    end,
    {reply, ok, State};
%% stub
handle_call(_Msg, _From, State) ->
    {reply, ignore, State}.

%% handles ETS-TRANSFER from protets_worker, creates new worker
%% and transfers table back
handle_info({'ETS-TRANSFER', Tid, From, Name}, State) ->
    receive
        %% worker terminates normally, because of 'drop table' call
        {'DOWN', _Ref, process, From, normal} ->
            true = ets:delete(?WORKERS_TABLE, Name),
            true = ets:delete(Tid);
        %% worker terminates abnormally, starting resque routines
        {'DOWN', _Ref, process, From, _Why} ->
            Pid = start_worker(Name, undefined),
            Worker = #worker{
                name = Name,
                pid = Pid,
                ref = erlang:monitor(process, Pid)
            },
            true = ets:insert(?WORKERS_TABLE, Worker),
            true = ets:give_away(Tid, Pid, Name)
    after
        5000 ->
            %% something really wrong here,
            %% ETS-TRANSFER without worker termination
            exit({partial, 'ETS-TRANSFER'})
    end,
    {noreply, State};
%% stub
handle_info(_Msg, State) ->
    {noreply, State}.

%% stub
handle_cast(_Msg, State) ->
    {noreply, State}.

%% stub
terminate(_Reason, _State) ->
    ok.

%% stub
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% initialises service table, which will hold workers data
init_workers_table(Opts) ->
    case ets:info(?WORKERS_TABLE, owner) of
        %% this is first protets_mon start
        undefined ->
            Pid = start_worker(?WORKERS_TABLE, Opts),
            Worker = #worker{
                name = ?WORKERS_TABLE,
                pid = Pid,
                ref = erlang:monitor(process, Pid)
            },
            true = ets:insert(?WORKERS_TABLE, Worker);
        %% protets_mon was terminated earlier,
        %% we need to renew tables heir data
        _Pid ->
            ok = ets:foldl(fun renew_heir/2, undefined, ?WORKERS_TABLE)
    end,
    ok.

%% creates new worker if needed
get_tref(Name, Opts) ->
    case ets:lookup(?WORKERS_TABLE, Name) of
        [Worker] ->
            get_tref(Worker#worker.pid);
        [] ->
            Pid = start_worker(Name, Opts),
            Worker = #worker{
                name = Name,
                pid = Pid,
                ref = erlang:monitor(process, Pid)
            },
            true = ets:insert(?WORKERS_TABLE, Worker),
            get_tref(Pid)
    end.

%% returns TableId of running worker
get_tref(Pid) ->
    {ok, TRef} = gen_server:call(Pid, get_tref),
    {ok, TRef}.

%% starts new worker
start_worker(Name, Opts) ->
    start_worker(Name, Opts, self()).
start_worker(Name, Opts, Heir) ->
    {ok, Pid} = supervisor:start_child(protets_sup, [Name, Opts, Heir]),
    Pid.

%% stops worker
stop_worker(Pid) ->
    gen_server:cast(Pid, terminate_normal),
    ok.

%% renew-heir function
renew_heir(Worker, _Acc) ->
    ok = gen_server:call(Worker#worker.pid, {renew_heir, self()}),
    ok.