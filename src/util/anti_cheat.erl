%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2013.02.25
%%% @doc 防外挂模块
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(anti_cheat).
-include("common.hrl").
-include("counter.hrl").
-include("global_data.hrl").
-include("role.hrl").
-include("anti_cheat.hrl").

-export([check_heart_quick/1, check_heart_timeout/1, 
        clear_heart_last_value/0]).
-export([try_start_timer/1, handle_timeout/2]).
-export([inc_cheat/1, dec_cheat/1, clear_cheat/0,
         get_cheat/0, get_lock_times/0]).

%% 防作弊检测的起始等级
-define(ANTI_CHEAT_ROLE_LVL_MIN, 40).

%% 防作弊等级
-define(ANTI_CHAET_LVL_NOT, 0).     % 不检测
-define(ANTI_CHEAT_LVL_HEART, 1).   % 只检测心跳
-define(ANTI_CHEAT_LVL_MAX, 2).     % 最高检测

%%-------------
%% 关于心跳
%%-------------
-ifdef(TEST).
%% 心跳最大超时次数
-define(HEART_TIMEOUT_COUNT_MAX, 16#ffffffff).
-else.
-define(HEART_TIMEOUT_COUNT_MAX, 3).
-endif.

%% 上次收到心跳包时间
-define(HEART_LAST_TIME, 'heart_last_time').
%% 检测加速次数
-define(HEART_QUICK_COUNT, 'heart_quick_time').

%% 上次心跳包数
-define(HEART_LAST_PACKET_COUNT, 'heart_last_pack_count').
%% 心跳超时次数
-define(HEART_TIMEOUT_COUNT, 'heart_last_timeout_count').


%% @doc 进行心跳加速检测
check_heart_quick(?ROLE_LOGIN_POLICY) ->
    ok;
check_heart_quick(#role{login_state = ?ROLE_LOGIN_WAITING}) ->
    ok;
check_heart_quick(_Role) ->
    LastTime = get_heart_last_value(?HEART_LAST_TIME),
    QuickCount = get_heart_last_value(?HEART_QUICK_COUNT),
    Now = util:now_ms_no_cache(),
    case LastTime of
        0 ->
            QuickCount2 = QuickCount;
        _ ->
            Diff = Now - LastTime,
            %?WARN("**hyeart diff:~p", [Diff]),
            {ok, Score, QuickCount2} = do_calc_heart_quick_score(Diff, QuickCount),
            ?IF(Score =/= 0, inc_cheat(Score), ok)
    end,
    set_heart_last_value(?HEART_LAST_TIME, Now),
    set_heart_last_value(?HEART_QUICK_COUNT, QuickCount2).

%% 进行超时检测
check_heart_timeout(?ROLE_LOGIN_POLICY) ->
    ok;
check_heart_timeout(#role{sock = Sock, accname = _AccName}) ->
    HeartCount = get_heart_last_value(?HEART_LAST_PACKET_COUNT),
    HeartT = get_heart_last_value(?HEART_TIMEOUT_COUNT),
    Ret = inet:getstat(Sock, [recv_cnt]),
    ?IF(Ret =:= {error, einval},
        role_server:notify_self_tcp_error(Sock, einval),
        ok),
    {ok, [{recv_cnt, RecvCnt}]} = Ret,
    %?WARN("*** packet ~p -> ~p", [HeartCount, RecvCnt]),
    case HeartCount =/= RecvCnt of
        true ->  
            HeartT2 = 0,
            set_heart_last_value(?HEART_LAST_PACKET_COUNT, RecvCnt),
            set_heart_last_value(?HEART_TIMEOUT_COUNT, 0);
        false -> % not receive new packets
            HeartT2 = HeartT + 1,
            set_heart_last_value(?HEART_TIMEOUT_COUNT, HeartT2)
    end,
    case HeartT2 =:= ?HEART_TIMEOUT_COUNT_MAX of
        true -> 
            % 达到最多超时次数
            %?WARN(?_U("心跳检测,玩家(平台帐号:~p)结束"), [_AccName]),
            role_server:async_stop(self(), ?LOG_LOGOUT_TYPE_HEART_TIMEOUT);
        false ->
            ok
    end.


%% @doc 尝试启动检测timer
try_start_timer(_Role) ->
    proc_timer:start_timer(?MODULE, 1000, check_anti_cheat, true),
    ok.

%% @doc 执行检测逻辑
handle_timeout(check_anti_cheat, Role) ->
    do_check(Role).

%% @doc 增加作弊值,超过300分会认为使用了外挂
inc_cheat(V) when is_integer(V), V > 0, V =< 100 ->
    lager:error("增加作弊分数:~p", [V]),
    ?COUNTER_DB:inc(?COUNTER_ANTI_CHEAT_SCORE, V).

%% @doc 减少作弊值
dec_cheat(V) ->
    ?COUNTER_DB:dec(?COUNTER_ANTI_CHEAT_SCORE, V).

%% @doc 清空作弊值
clear_cheat() ->
    ?COUNTER_DB:set(?COUNTER_ANTI_CHEAT_SCORE, 0).

%% @doc 获取作弊分数
get_cheat() ->
    ?COUNTER_DB:get(?COUNTER_ANTI_CHEAT_SCORE).

%% @doc 获取作弊次数
get_lock_times() ->
    ?COUNTER_DB:get(?COUNTER_ANTI_CHEAT_LOCK_TIMES).

%%----------------
%% internal API
%%----------------
%%------------
%% 心跳检测
%%------------

%% 获取心跳相关数据
get_heart_last_value(Type) ->
    case erlang:get(Type) of
        undefined ->
            0;
        N ->
            N
    end.

%% 设置到心跳数据
set_heart_last_value(Type, V) ->
    erlang:put(Type, V),
    ok.

%% @doc 清理心跳相关数据
clear_heart_last_value() ->
    erlang:erase(?HEART_LAST_TIME),
    erlang:erase(?HEART_QUICK_COUNT),
    ok.

%% 计算心跳加速得分
%% 连续两次加速才踢掉
do_calc_heart_quick_score(Diff, QuickCount) ->
    {Score, QuickCount2} =
    if
        Diff > 4500 ->
            {0, QuickCount};
        Diff > 4300 ->
            {?ANTI_CHEAT_SCORE_2, QuickCount + 1};
        Diff > 4100 ->
            {?ANTI_CHEAT_SCORE_3, QuickCount + 1};
        Diff > 3800 ->
            {?ANTI_CHEAT_SCORE_4, QuickCount + 1};
        true ->
            {?ANTI_CHEAT_SCORE_5, QuickCount + 1}
    end,
    if
        QuickCount2 >= 1 ->
            {ok, Score, QuickCount2};
        true ->
            {ok, 0, QuickCount2}
    end.


%% 计算锁定时间
do_calc_lock_time(Times) ->
    LockTimeDef = ?CONFIG(cheat_lock_time, 60),
    erlang:min(LockTimeDef * Times, 36000).

%%----------------
%% 检测逻辑
%%----------------
%% 执行检测逻辑
do_check(#role{id = _Id}) ->
    Score = get_cheat(),
    case Score >= ?ANTI_CHEAT_SCORE_MAX of
        true ->
            New = ?COUNTER_DB:inc(?COUNTER_ANTI_CHEAT_LOCK_TIMES),
            LockTime = do_calc_lock_time(New),
            clear_cheat(),
            role_server:async_stop(self(), ?LOG_LOGOUT_TYPE_ANTI_CHEAT, LockTime),
            ok;
        false ->
            ok
    end.
