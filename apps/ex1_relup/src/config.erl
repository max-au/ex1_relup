%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @copyright (C) 2019, Maxim Fedorov
%%% @doc
%%%     Configuration keeper process. Owns ETS table used as cache,
%%%     enabling processes to read the table without config porcess
%%%     becoming a bottleneck.
%%% @end
%%%-------------------------------------------------------------------
-module(config).
-author("maximfca@gmail.com").

-behaviour(gen_server).

%% API
-export([get/1,
    get/2,
    persist/1]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(CONFIG, "/tmp/cdp").

-define(TABLE, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API

%%--------------------------------------------------------------------
%% @doc
%% Looks up a configuration key that is guaranteed to be available.
%% Fails with 'try_clause' when key is missing from the cache, and
%% falls back to config gen_server when cache table is not available.
-spec get(Key :: atom()) -> term().
get(Key) ->
    try ets:lookup(?TABLE, Key) of
        [{Key, Value}] ->
            Value
    catch
        error:badarg ->
            gen_server:call(?SERVER, {get, Key})
    end.

%%--------------------------------------------------------------------
%% @doc
%% Looks up a configuration key that is may be missing.
%% Returns Default for missing keys, or when cache table is missing.
-spec get(Key :: atom(), Default :: term()) -> term().
get(Key, Default) ->
    try ets:lookup(?TABLE, Key) of
        [] ->
            Default;
        [{Key, Value}] ->
            Value
    catch
        error:badarg ->
            Default
    end.

%%--------------------------------------------------------------------
%% @doc
%% Persists configuration change, writing to file and
%%  caches new value.
-spec persist({Key :: atom(), Value :: term()}) -> ok.
persist(Term) ->
    gen_server:call(?MODULE, {persist, Term}).

%%--------------------------------------------------------------------
%% @doc
%% Starts configuration server and links to the calling process.
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes configuration cache, loads initial configuration from
%%  file on disk. Expects file to be readable, fails otherwise.
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    % configuration delivery pipeline guarantees this file is valid
    {ok, Terms} = file:consult(?CONFIG),
    % avoid race conditions: create a temporary table for configuration
    %   and rename it to ?TABLE when bootstrap is done
    Loader = ets:new('$temporary', [named_table, {read_concurrency, true}]),
    _ = [true = ets:insert(Loader, Term) || Term <- Terms],
    ?TABLE = ets:rename('$temporary', ?TABLE),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Implements configuration persistence.
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({persist, Term}, _From, State) ->
    % expect that file operation fails
    case file:write_file(?CONFIG, io_lib:format("~tp.~n", [Term]), [append]) of
        ok ->
            % do NOT expect ETS to fail
            true = ets:insert(?TABLE, Term),
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% Configuration server *expects* garbage input for calls, and drops
%%  this input.
handle_call(_Request, _From, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Invalid casts are expected, and are silently dropped
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Invalid messages are expected, and are silently dropped
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% ETS table cleanup helps to avoid race conditions when other
%%  processes may still try to use cache that is no longer valid.
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ets:delete(?TABLE).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
