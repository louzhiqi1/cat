%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2012.06.14
%%% @doc 游戏启动模块
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(game).
-behaviour(application).

-export([start/0, stop_server/0]).
-export([start/2, stop/1]).
-export([set_loglevel/1]).

-include("wg_log.hrl").
-include("common.hrl").

%% @doc 启动游戏
start() ->
    try
        ensure_apps(),
        set_loglevel(debug),
        ok = application:start(game)
    catch
        Type:Error ->
            lager:error("application start error:~p:~p", [Type, Error]),
            init:stop(?STATUS_ERROR)
    end.


%% @doc 停止游戏服务器
%% 同步停止,立刻停止
stop_server() ->
    application:stop(?MAIN_APP),
    init:stop(),
    ?STATUS_SUCCESS.

%% @doc 启动游戏
start(_Type , StartArgs) ->
    game_config:init(),
	{ok, Sup} = game_sup:start_link(StartArgs),
    log_init:start(Sup),
    {ok, Sup}.

%% @doc 停止游戏
stop(_State) ->
    try
        log_init:stop()
    catch
        _T:_R ->
            lager:error("application stop error ~p:~p", [_T, _R])
    end,
	ok.


%%----------------------
%% Internal API
%%----------------------
%% 确保app已启动
ensure_apps() ->
    ok = lager:start(),
    ok = application:start(sasl),
    ok = application:start(ranch),
    ok = application:start(crypto),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(http_server).

%% 设置日志等级
set_loglevel(Level) ->
	lager:set_loglevel(console, Level).

