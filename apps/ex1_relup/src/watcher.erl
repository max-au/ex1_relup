%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @copyright (C) 2019, Maxim Fedorov
%%% @doc
%%% Process watching configuration changes and feeding them into
%%%     consumption pipeline. External process used to monitor
%%%     changes is expected to be unreliable, and failure is deemed
%%%     transient if external process is unavailable for 10
%%%     seconds.
%%% @end
%%%-------------------------------------------------------------------
-module(watcher).
-author("maximfca@gmail.com").

-behaviour(gen_server).

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

%% Monitor state, including subscriptions
-record(state, {
    port :: port(),
    deadline :: integer(),
    subscriptions :: [atom()]
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server and links it to the caller.
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
%% Initialises the server. There is no guarantee that feed is available
%%  during initialisation stage!
%% Supervision tree guarantees that subscription_manager is
%%   started before watcher, which means watcher is guaranteed
%%   availability of subscription list
%% In this example, subscription list if used only to illustrate
%%   a way to recover watcher state in case of its crash.
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    % do not try to open delivery channel here - there is no *guarantee*
    %   of availability!
    % Bootstrap the process with sending first 'reopen' message.
    self() ! reopen,
    {ok, #state{subscriptions = subscription_manager:get_subscriptions()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Illustrative example: do *not* expect any calls! That is,
%%  deliberately crash.
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, _State) ->
    erlang:error(undef).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Illustrative example: do *not* expect any calls!
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, _State) ->
    erlang:error(undef).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles reconnection to external information source.
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(reopen, State) ->
    try
        open_port({spawn_executable, os:find_executable("tail")},
            [{args, ["-fn0", "/tmp/cdp.new"]}, {line, 80}, exit_status])
        of
        Port when is_port(Port) ->
            {noreply, State#state{port = Port, deadline = undefined}}
    catch
        error:_Reason ->
            {noreply, reopen(State)}
    end;
% let's introduce a bug to show how re-subscription works, and
%   deliberately treat lines longer than 80 characters as
%   something unexpected. Skip clause with {noeol, ...}.
handle_info({Port, {data, {eol, Line}}}, #state{port = Port} = State) ->
    % here we expect garbage input, and handle it appropriately
    try
        [Sub, Mod, Key | Value] = string:lexemes(Line, ":"),
        try_change(list_to_existing_atom(Sub), list_to_existing_atom(Mod),
            list_to_existing_atom(Key), lists:append(Value),
            State#state.subscriptions),
        {noreply, State}
    catch
        error:_Reason ->
            reject_change(Line)
    end,
    {noreply, State};
%% Handle external process dying.
handle_info({Port, {exit_status, _}}, #state{port = Port} = State) ->
    reopen(State),
    {noreply, State}.
%% do not handle any other messages, and restart the watcher if
%%  anything unexpected happens. After all, this is the whole point
%%  of making watcher robust.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Exercise 3: add better shutdown sequence, and kill external process
%%  when terminating the watcher.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

reopen(#state{deadline = undefined} = State) ->
    % set deadline to 10 seconds from now. Yes, hardcode it, because it's
    %   guarantee, and if it needs a change, it's ok to upgrade code.
    Now = erlang:system_time(millisecond),
    reopen(State#state{deadline = Now + 10 * 1000}, Now);

reopen(State) ->
    Now = erlang:system_time(millisecond),
    reopen(State, Now).

reopen(#state{deadline = Deadline} = State, Now) when Now < Deadline ->
    erlang:send_after(1000, self(), reopen),
    State.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the change against subscriptions made.
%% Only for sake of the exercise, in production world this is done
%%  by the configuration delivery pipeline. Here it server the only
%%  purpose, illustrate how state recovery works.
try_change(Sub, Mod, Key, Val, Subscriptions) ->
    true = lists:member(Sub, Subscriptions),
    attempt_change(Mod, Key, Val).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Validate the change and persist valid value.
attempt_change(Mod, Key, Val) ->
    ValidValue = Mod:Key(Val),
    config:persist({Key, ValidValue}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Reject an invalid change
reject_change(Line) ->
    file:write_file("/tmp/cdp.rej", Line ++ "\n", [append]).
