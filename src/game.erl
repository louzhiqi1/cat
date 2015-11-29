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

-export([start/0, stop_server/1]).
-export([start/2, stop/1]).

-include("wg_log.hrl").
-include("common.hrl").

%% @doc 启动游戏
start() ->
    try
		set_loglevel(),
        ensure_apps(),
        ok = application:start(game)
    catch
        Type:Error ->
            ?ERROR2(?_U("启动游戏服务器出错:~p:~p"), [Type, Error]),
            init:stop(?STATUS_ERROR)
    end.


%% @doc 停止游戏服务器
%% 同步停止,立刻停止
stop_server(true) ->
    do_stop_server(),
    init:stop(),
    ?STATUS_SUCCESS.

%% 停止游戏
do_stop_server() ->
	application:stop(?MAIN_APP),
	init:stop().

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
            ?ERROR2(?_U("停止app保存数据出错 ~p:~p"), [_T, _R])
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
    ok = application:start(cowboy).

%% 设置日志等级
set_loglevel() ->
	L = game:module_info(compile),
	Opts = ?KV_GET(options, L),	
	Macros = [M || {K, M} <- Opts, K =:= 'd'],	
	?IF(lists:member('TEST', Macros),
		wg_loglevel:set(5), 
		wg_loglevel:set(3)).

