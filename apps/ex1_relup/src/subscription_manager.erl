%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @copyright (C) 2019, Maxim Fedorov
%%% @doc
%%% Subscription manager is designed to decouple configuration
%%%     subscriptions from the monitoring process, that is expected
%%%     to be unstable.
%%% Feature creep (inevitable!): subscription manager keeps track of
%%%     subscribers to automatically remove a subscription when no
%%%     more subscribers are left for a particular subscription.
%%% @end
%%%-------------------------------------------------------------------
-module(subscription_manager).
-author("maximfca@gmail.com").

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([subscribe/1,
    get_subscriptions/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

%% Subscriptions: reference counter & process map.
-record(state, {
    %% how many processes have subscribed to this subscription
    sub_refc :: #{atom() => pos_integer()},
    %% specific pid subscriptions.
    %% do not expect many subscriptions, and use list for it
    pid_map :: #{pid() => [atom()]}
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Subscribers calling process for 'Key'.
-spec subscribe(atom()) -> ok.
subscribe(Key) ->
    gen_server:call(?SERVER, {subscribe, Key}).

%%--------------------------------------------------------------------
%% @doc
%% Returns a list of all active subscriptions.
-spec get_subscriptions() -> [atom()].
get_subscriptions() ->
    gen_server:call(?SERVER, get_subscriptions).

%%--------------------------------------------------------------------
%% @doc
%% Starts the subscription manager and links it to caller.
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
%% Initialises an empty subscription list.
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #state{sub_refc = maps:new(), pid_map = maps:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles subscription calls.
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(get_subscriptions, _From, #state{sub_refc = Subs} = State) ->
    {reply, maps:keys(Subs), State};
handle_call({subscribe, Key}, {Pid, _Ref}, #state{sub_refc = Subs, pid_map = Pids} = State) ->
    % increment reference counter only if this process subscribes for the
    % first time
    case is_subscriber(Key, Pid, Pids) of
        true ->
            {reply, ok, State};
        {false, Keys} ->
            monitor(process, Pid),
            {reply, ok, State#state{
                sub_refc = maps:put(Key, maps:get(Key, Subs, 0) + 1, Subs),
                pid_map = maps:put(Pid, [Key | Keys], Pids)}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ignore expected garbage casts.
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({'DOWN', _, process, Pid, _}, #state{sub_refc = Subs, pid_map = Pids} = State) ->
    % remove subscriber, and, potentially, subscription
    % this function does *not* expect garbage input, as it means
    % state of the subscription manager is not consistent, and
    % it must be restarted.
    Keys = maps:get(Pid, Pids),
    NewSubs = lists:foldl(
        fun (Key, Acc) ->
            case maps:get(Key, Acc) of
                1 ->
                    maps:remove(Key, Acc);
                N when N > 1 ->
                    maps:put(Key, N - 1, Acc)
            end
        end, Subs, Keys),
    {noreply, State#state{sub_refc = NewSubs, pid_map = maps:remove(Pid, Pids)}};
%% Ignore expected garbage messages.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% At the moment, termination is handlded by OTP. However it may be a
%%  viable exercise to add some notification capabilities for monitoring
%%  purposes.
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

is_subscriber(Key, Pid, Pids) ->
    case maps:get(Pid, Pids, []) of
        [] ->
            {false, []};
        Keys ->
            case lists:member(Key, Keys) of
                true ->
                    true;
                false ->
                    {false, Keys}
            end
    end.