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
-export([update_pos/2, update_pos/3, clear_pos_prev/0,
         set_pos_check_stop/1, is_pos_check_stop/1]).
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
    case is_check_heart() of
        false ->
            ok;
        true ->
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
            set_heart_last_value(?HEART_QUICK_COUNT, QuickCount2)
    end.

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

%%---------------
%% 移动检测相关
%%---------------

%% @doc 更新位置
update_pos(Role, PPos) ->
    update_pos(Role, PPos, util:now_ms_no_cache()).

%% 1,玩家等级较低不进行检测
%% 2,如果关闭了则不检测
%% 2,是否关闭了位置检测(瞬移，冲锋，加速时会关闭)?
update_pos(#role{lvl = Lvl}, _PPos, _Now) when Lvl < ?ANTI_CHEAT_ROLE_LVL_MIN ->
    % 1
    ok;
update_pos(Role, PPos, Now) ->
    % 2
    case is_check_move() of
        false ->
            ok;
        true ->
            % 3
            case is_pos_check_stop(Now) of
                true ->
                    clear_pos_prev();
                false ->
                    do_update_pos(Role, PPos, Now)
            end
    end.

%% @doc 设置是否开启关闭位置检测(T为关闭时间ms)
%% 设置关闭时间时，确保不会把时间缩短
-define(POS_CHECK_SWITCH_KEY, anti_cheat_pos_check_switch).
set_pos_check_stop(T) when is_integer(T) ->
    Now = util:now_ms_no_cache(),
    T2 = Now + T,
    case T2 > get_pos_check_stop()  of
        true ->
            %?WARN("****set check stop t:~p now:~p", [T2, Now]),
            erlang:put(?POS_CHECK_SWITCH_KEY, T2);
        false ->
            ok
    end.

%% 获取检测关闭时间
get_pos_check_stop() ->
    case erlang:get(?POS_CHECK_SWITCH_KEY) of
        undefined ->
            0;
        V ->
            V
    end.

%% @doc 位置检测是否关闭
is_pos_check_stop(Now) ->
    case get_pos_check_stop() of
        0 ->
            false;
        V ->
            V > Now
    end.

%% @doc 尝试启动检测timer
try_start_timer(#role{lvl = Lvl}) when Lvl < ?ANTI_CHEAT_ROLE_LVL_MIN ->
    ok;
try_start_timer(_Role) ->
    proc_timer:start_timer(?MODULE, 1000, check_anti_cheat, true),
    ok.

%% @doc 执行检测逻辑
handle_timeout(check_anti_cheat, Role) ->
    CheckLvl = get_check_lvl(),
    ?IF(CheckLvl =/= 0, do_check(Role), ok).

%% @doc 增加作弊值,超过300分会认为使用了外挂
inc_cheat(V) when is_integer(V), V > 0, V =< 100 ->
    %?ERROR2(?_U("增加作弊分数:~p"), [V]),
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

%% 是否检测心跳
is_check_heart() ->
    is_check_heart(get_check_lvl()).
is_check_heart(CheckLvl) ->
    CheckLvl >= ?ANTI_CHEAT_LVL_HEART.

%% 是否检测移动
is_check_move() ->
    is_check_move(get_check_lvl()).
is_check_move(CheckLvl) ->
    CheckLvl >= ?ANTI_CHEAT_LVL_MAX.

%% 获取检测配置
get_check_lvl() ->
    ?CONFIG(anti_cheat, ?ANTI_CHEAT_LVL_HEART).

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

%%-------------
%% 移动检测
%%-------------

%% 更新位置逻辑
%% x方向251像素/s
%% y方向1287像素/s
%% 客户端每400毫秒提交一次状态,走100像素
-define(MOVE_STEP_PX_MAX, 110).     % x方向单步最大距离(100像素)
-define(MOVE_STEP_PY_MAX, 1287).    % y方向单步最大距离
do_update_pos(Role, {PX, PY}, Now) ->
    SpeedX = role_internal:speed(Role),
    do_check_move(?COUNTER_ANTI_CHEAT_XRANGE, 
        PX, Now, ?MOVE_STEP_PX_MAX, SpeedX),
    do_check_move(?COUNTER_ANTI_CHEAT_YRANGE, 
        PY, Now, ?MOVE_STEP_PY_MAX, ?ROLE_PY_SPEED_MAX).

%% 移动检测
%% 1,如果旧值为空，则更新
%% 2,如果没有变化则忽略此次更新(跳跃)
%% 3,进行检测
%% 4,更新上次记录
do_check_move(Type, P, Now, RangeMax, SpeedMax) ->
    case get_pos_prev(Type) of
        ?NONE ->
            % 1
            set_pos_prev(Type, {P, Now});
        {P, _} ->
            % 2
            ok;
        {PPrev, TimePrev} ->
            % 3
            do_check_move_internal(Type, P, Now, PPrev, TimePrev, RangeMax, SpeedMax),
            set_pos_prev(Type, {P, Now})
    end.


%% 获取先前的位置
-define(MOVE_POS_PREV(Type), {anti_cheat_move_pos_prev, Type}).
get_pos_prev(Type) ->
    case erlang:get(?MOVE_POS_PREV(Type)) of
        undefined ->
            ?NONE;
        V ->
            V
    end.

%% 设置先前的位置
set_pos_prev(Type, {_, _} = V) ->
    erlang:put(?MOVE_POS_PREV(Type), V),
    ok.

%% @doc 清空先前的位置
clear_pos_prev() ->
    erlang:erase(?MOVE_POS_PREV(?COUNTER_ANTI_CHEAT_XRANGE)),
    erlang:erase(?MOVE_POS_PREV(?COUNTER_ANTI_CHEAT_YRANGE)),
    ok.

%% 计算锁定时间
do_calc_lock_time(Times) ->
    LockTimeDef = ?CONFIG(cheat_lock_time, 60),
    erlang:min(LockTimeDef * Times, 36000).

%% 检测移动
%% 1,计算时间间隔
%% 2,计算距离和速度
%% 3,检测是否合法
%% 4,更新数据
do_check_move_internal(Type, P, Now, PPrev, TimePrev, RangeMax, SpeedMax) ->
    % 1
    TimeDiff = erlang:max(1, Now - TimePrev),
    %?WARN("timediff:~p", [TimeDiff]),
    % 2
    PRange = erlang:abs(PPrev - P),
    PSpeed = PRange * 1000 div TimeDiff,
    %?WARN("~p range:~p/~p speed:~p/~p", [?IF(Type =:= 1001, "x", "y"), PRange, RangeMax, PSpeed, SpeedMax]),

    % 3
    InvalidTimes =
    case PRange > RangeMax andalso PSpeed > SpeedMax of
        true ->
            inc_invalid_cont_times();
        false ->
            clear_invalid_cont_times(),
            0
    end,
    ?IF(InvalidTimes >= 2, inc_cheat(?ANTI_CHEAT_SCORE_4), ok),

    % 4
    ?COUNTER_RAM:inc(Type, PRange),
    ok.

%% 设置移动不合法连续次数
-define(MOVE_INVALID_CONT_TIMES, anti_cheat_invalid_cont_times).
set_invalid_cont_times(N) ->
    erlang:put(?MOVE_INVALID_CONT_TIMES, N),
    ok.

%% 获取移动不合法连续次数
get_invalid_cont_times() ->
    case erlang:get(?MOVE_INVALID_CONT_TIMES) of
        undefined ->
            0;
        N ->
            N
    end.

%% 增加移动不合法连续次数
inc_invalid_cont_times() ->
    N = get_invalid_cont_times() + 1,
    set_invalid_cont_times(N),
    N.

%% 清理移动不合法连续系数
clear_invalid_cont_times() ->
    erlang:erase(?MOVE_INVALID_CONT_TIMES),
    ok.

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
