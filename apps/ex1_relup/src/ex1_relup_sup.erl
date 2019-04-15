%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @copyright (C) 2019, Maxim Fedorov
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ex1_relup_sup).
-author("maximfca@gmail.com").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%% @doc ex1_relup server depends on ETS table owned by config. If config
%%  is restarted, ex1_relup must also be restarted. However, config
%%  does not depend on ex1_relup, and does not need to be restarted
%%  if ex1_relup dies. rest_for_one strategy implements this behaviour.
%%  ex1_relup also wants a guarantee that when it subscribes to
%%  configuration changes, subscription will not be lost. Which means
%%  ex1_rel also depends on subscription manager.
%%====================================================================

init([]) ->
    {ok, {
        #{strategy => rest_for_one, intensity => 5, period => 60},
        [
            #{id => config_sup,
                start => {supervisor, start_link, [?MODULE, config_sup]}, type => supervisor},
            #{id => ex1_relup,
                start => {ex1_relup, start_link, []}, modules => [ex1_relup]},
            #{id => watcher_sup,
                start => {supervisor, start_link, [?MODULE, watcher_sup]}, type => supervisor}
        ]}};

init(config_sup) ->
    {ok, {
        #{strategy => rest_for_one, intensity => 1, period => 60},
        [
            #{id => config,
                start => {config, start_link, []}, modules => [config]},
            #{id => subscription_manager,
                start => {subscription_manager, start_link, []}, modules => [subscription_manager]}
        ]}};

init(watcher_sup) ->
    {ok, {
        #{strategy => one_for_one, intensity => 5, period => 10 * 60}, [
            #{id => watcher,
                start => {watcher, start_link, []}, modules => [watcher]}
        ]}}.
