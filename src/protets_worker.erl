-module(protets_worker).

-behaviour(gen_server).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-define(OPT_ACCESS, public).
-define(OPT_HEIR(Pid, HeirData), {heir, Pid, HeirData}).

-record(
    state,
    {
        name            :: any(),
        tref            :: any()
    }
).

start_link(Name, Opts, Heir) ->
    gen_server:start_link(?MODULE, [Name, Opts, Heir], []).

init([Name, undefined, Heir]) ->
    {ok, #state{name = Name}};
init([Name, Opts, Heir]) ->
    try ets:new(Name, build_opts(Name, Opts, Heir)) of
        Id ->
            {ok, #state{tref = Id, name = Name}}
    catch
        error:Why ->
            {stop, Why}
    end.

handle_call(get_tref, _From, State) ->
    {reply, {ok, State#state.tref}, State};
handle_call({renew_heir, Pid}, From, State) ->
    true = ets:setopts(State#state.tref, [?OPT_HEIR(Pid, State#state.name)]),
    {reply, ok, State};
handle_call(Msg, _From, State) ->
    {reply, ignore, State}.

handle_info({'ETS-TRANSFER', Tid, _From, Name}, #state{name = Name} = State) ->
    {noreply, State#state{tref = Tid}};
handle_info({'ETS-TRANSFER', Tid, _From, Name}, State) ->
    true = ets:delete(Tid),
    {noreply, State};
handle_info(Msg, State) ->
    {noreply, State}.

handle_cast(terminate_normal, State) ->
    exit(normal);
handle_cast(Msg, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

build_opts(Name, Opts, Heir) ->
    [?OPT_ACCESS, ?OPT_HEIR(Heir, Name)] ++ lists:filter(fun allowed_opt/1, Opts).

allowed_opt(set)                    -> true;
allowed_opt(ordered_set)            -> true;
allowed_opt(bag)                    -> true;
allowed_opt(duplicate_bag)          -> true;
allowed_opt({keypos, _})            -> true;
allowed_opt({write_concurrency, _}) -> true;
allowed_opt({read_concurrency, _})  -> true;
allowed_opt(compressed)             -> true;
allowed_opt(named_table)            -> true;
allowed_opt(_)                      -> false.
