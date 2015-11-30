-ifndef(GAME_HRL).
-define(GAME_HRL, true).

-define(MAIN_APP, game).

%% 代码路径
-define(CODE_PATH, "ebin/game").

-include("wg.hrl").
-include("wg_log.hrl").
-include("ecode.hrl").
-include("db.hrl").
-include("log.hrl").
%%--------------
%% 关于配置文件
%%--------------
-define(CONFIG(K), game_config:get(K)).
-define(CONFIG(K,D), game_config:get(K, D)).
%%-------------
%% 关于日志等级
%%-------------
-ifdef(TEST).
-define(LOG_LEVEL, 5). 
-else.
-define(LOG_LEVEL, 3). 
-endif.
%%------------------------------
%% 应用对应的id,退出时作为状态码
%%------------------------------
-define(STATUS_SUCCESS, 0).     % 成功
-define(STATUS_ERROR,  1).      % 错误
-define(STATUS_NORUN,   2).     % 未运行
-define(STATUS_BADRPC,  3).     % rpc调用错误
-define(STATUS_USAGE,  4).      % 用法错误
-define(STATUS_STARTING, 5).    % 正在启动
-define(STATUS_RUNNING, 6).     % 正在运行中
-define(STATUS_STOPING, 7).     % 正在停止

%%--------------------------
%% gen_server默认timeout
-define(GEN_SERVER_TIMEOUT, 5000).
-define(NONE, none).
-define(DAY_SECONDS, 86400).
%% 永远
-define(INFINITY, 16#ffffffff).

%% 数据同步event
-define(DATA_SYNC_EVENT, 'mod_data_sync_event').
%% 网络发送
-define(GAME_SEND_PUSH, (game_send:new(send_push))).
-define(GAME_SEND_MERGE, (game_send:new(send_push))).

%% 抛出错误码
-define(C2SERR(R), throw({error, R})).

%% 封装处理handle_cast
-define(HANDLE_CAST_WRAP(Req, State),
    try 
        do_cast(Req, State)
    catch
        Class:Error ->
            lager:error("handle cast:~p error\n~p:~p", [Req, Class, Error]),
            {noreply, State}
    end).
%% 封装处理handle_call
-define(HANDLE_CALL_WRAP(Req, State), ?HANDLE_CALL_WRAP(Req, From, State)).
-define(HANDLE_CALL_WRAP(Req, From, State),
    try 
        do_call(Req, From, State)
    catch
        Class:Error ->
            lager:error("handle call:~p error\n~p:~p", [Req, Class, Error]),
            {noreply, State}
    end).
%% 封装处理handle_info
-define(HANDLE_INFO_WRAP(Req, State),
    try 
        do_info(Req, State)
    catch
        Class:Error ->
            lager:error("handle info:~p error\n~p:~p", [Req, Class, Error]),
            {noreply, State}
    end).

-define(PID_TYPE_ROLE, 2).      % 角色
-define(PID_TYPE_UNKNOWN, 10).  % 未知的pid类型

-define(PROB_FULL, 10000).	% 满概率
-define(TIMEOUT_EVENT, timeout_event).

%% 基础速度
-define(ROLE_PY_SPEED_MAX, 1200).   % y方向最大速度(39像素/帧)

%% CD对网络延迟的容忍(100ms)
-define(CD_NET_TOLERATE, 100).
-define(NET_TOLERATE, 100).

-endif.