%%%-------------------------------------------------------------------
%%% @author Maxim Fedorov <maximfca@gmail.com>
%%% @copyright (C) 2019, Maxim Fedorov
%%% @doc
%%% Application behaviour, starts top-level supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(ex1_relup_app).
-author("maximfca@gmail.com").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API

start(_StartType, _StartArgs) ->
    ex1_relup_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.
