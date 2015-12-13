%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2012.06.11
%%% @doc 日志相关初始化
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(log_init).
-include("common.hrl").
-include("log.hrl").

-export([start/1, stop/0]).

%% @doc 初始化日志相关
start(Sup) ->
    ok = start_logic_log_server(Sup),
    ok.

%% @doc 停止
stop() ->
    ok.

%%------------------------------
%% internal API
%%------------------------------

%% 启动logic log server
start_logic_log_server(Sup) ->
    LogDir = game_path:log_dir(),
    [begin
        Child = {LogName, {log_server, start_link, [LogName, LogDir]},
                    permanent, 2000, worker, [log_server]},
        ok = util:start_child(Sup, Child)
    end || LogName <- ?LOG_TYPE_LIST],
    ok.
