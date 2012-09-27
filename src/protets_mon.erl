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

init([]) ->
    ok = init_workers_table([named_table, ordered_set, {keypos, 2}]),
    {ok, #state{}}.

handle_call({get, Name, Opts}, _From, State) ->
    {ok, TRef} = get_tref(Name, Opts),
    {reply, {ok, TRef}, State};
handle_call({drop, Name}, _From, State) ->
    case ets:lookup(?WORKERS_TABLE, Name) of
        [Worker] ->
            ok = stop_worker(Worker#worker.pid),
            ok;
        [] ->
            ok
    end,
    {reply, ok, State};
handle_call(Msg, _From, State) ->
    {reply, ignore, State}.

handle_info({'ETS-TRANSFER', Tid, From, Name}, State) ->
    receive
        {'DOWN', _Ref, process, From, normal} ->
            true = ets:delete(?WORKERS_TABLE, Name),
            true = ets:delete(Tid);
        {'DOWN', _Ref, process, From, Why} ->
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
            exit({partial, 'ETS-TRANSFER'})
    end,
    {noreply, State};
handle_info(Msg, State) ->
    {noreply, State}.

handle_cast(Msg, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

init_workers_table(Opts) ->
    case ets:info(?WORKERS_TABLE, owner) of
        undefined ->
            Pid = start_worker(?WORKERS_TABLE, Opts),
            Worker = #worker{
                name = ?WORKERS_TABLE,
                pid = Pid,
                ref = erlang:monitor(process, Pid)
            },
            true = ets:insert(?WORKERS_TABLE, Worker);
        Pid ->
            ok = ets:foldl(fun renew_heir/2, undefined, ?WORKERS_TABLE)
    end,
    ok.

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

get_tref(Pid) ->
    {ok, TRef} = gen_server:call(Pid, get_tref),
    {ok, TRef}.

start_worker(Name, Opts) ->
    start_worker(Name, Opts, self()).

start_worker(Name, Opts, Heir) ->
    {ok, Pid} = supervisor:start_child(protets_sup, [Name, Opts, Heir]),
    Pid.

stop_worker(Pid) ->
    gen_server:cast(Pid, terminate_normal),
    ok.

renew_heir(Worker, Acc) ->
    ok = gen_server:call(Worker#worker.pid, {renew_heir, self()}),
    ok.