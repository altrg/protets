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

%% gen_server callbacks
%% starting worker with already existing table
init([Name, undefined, _Heir]) ->
    {ok, #state{name = Name}};
%% new worker, we need to create table here
init([Name, Opts, Heir]) ->
    try ets:new(Name, build_opts(Name, Opts, Heir)) of
        Id ->
            {ok, #state{tref = Id, name = Name}}
    catch
        error:Why ->
            {stop, Why}
    end.

%% handles get_tref signal, return TableId
handle_call(get_tref, _From, State) ->
    {reply, {ok, State#state.tref}, State};
%% handles renew_heir signal, renews ets heir options
handle_call({renew_heir, Pid}, _From, State) ->
    true = ets:setopts(State#state.tref, [?OPT_HEIR(Pid, State#state.name)]),
    {reply, ok, State};
% stub
handle_call(_Msg, _From, State) ->
    {reply, ignore, State}.

%% handles ETS-TRANSFER back
handle_info({'ETS-TRANSFER', Tid, _From, Name}, #state{name = Name} = State) ->
    {noreply, State#state{tref = Tid}};
%% something strange happened, we don't need this table
handle_info({'ETS-TRANSFER', Tid, _From, _Name}, State) ->
    true = ets:delete(Tid),
    {noreply, State};
%% stub
handle_info(_Msg, State) ->
    {noreply, State}.

%% handles terminate signal
handle_cast(terminate_normal, _State) ->
    exit(normal);
%% stub
handle_cast(_Msg, State) ->
    {noreply, State}.

%% stub
terminate(_Reason, _State) ->
    ok.

%% stub
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%% this function filters ets options,
%% denying creation of non-public tables
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
