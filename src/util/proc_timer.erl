%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2012.07.04
%%% @doc 进程内timer模块，基于进程字典实现
%%% 当time循环触发时，根据Ref List获取
%%% 所有的timer(进程字典，key为Ref)是否需要触发，如果触发
%%% 则调用Mod:handle_timeout/2处理
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(proc_timer).
-include("common.hrl").

-export([start_timer/3, start_timer/4, read_timer/1, cancel_timer/1, timer_list/0]).
%% 内部使用
-export([on_timeout_loop/1]).

%% timer ref 列表
-define(TIMER_LIST_KEY, 'proc_timer_list').
%% timer 对应的key
-define(TIMER_REF_KEY(Ref), {proc_timer_ref, Ref}).

%% 数据
-record(timer_data, {
        mod,        % 回调模块
        time,       % timer时间
        msg,        % 对应的msg
        loop,       % 是否循环

        invoke_time % 触发时间
        }).


%% @doc 开始timer
start_timer(Mod, Time, Msg) ->
    start_timer(Mod, Time, Msg, false).

%% @doc 开始timer
start_timer(Mod, Time, Msg, Loop) ->
    do_add_timer(Mod, Time, Msg, Loop).

%% @doc 读取timer剩余时间
read_timer(Ref) ->
    do_get_timer_remain(Ref).

%% @doc 取消timer
cancel_timer(Ref) when is_reference(Ref) ->
    do_delete_timer(Ref);
cancel_timer(_) ->
    ok.

%% @doc 执行timer逻辑
on_timeout_loop(State) ->
    Now = util:now_ms(),
    L = do_get_timer_list(),
    State2 =
    lists:foldl(
    fun(Ref, StateAcc) ->
        #timer_data{invoke_time = InvokeTime} = TimerData = erlang:get(?TIMER_REF_KEY(Ref)),
        case InvokeTime =< Now of
            true ->
                % 执行
                do_invoke_timeout(Ref, TimerData, Now, StateAcc);
            false ->
                StateAcc
        end
    end, State, L),
    {ok, State2}.

%% @doc 获取timer列表
timer_list() ->
    [erlang:get(?TIMER_REF_KEY(Ref)) || Ref <- do_get_timer_list()].

%%------------------------
%% internal API
%%------------------------

%% 添加timer
do_add_timer(Mod, Time, Msg, Loop) ->
    Ref = erlang:make_ref(),
    Now = util:now_ms(),
    TimerData = 
    #timer_data{
            mod = Mod,
            time = Time, 
            msg = Msg,
            loop = Loop,
            invoke_time = Now + Time
    },
    do_set_timer_list([Ref | do_get_timer_list()]),
    erlang:put(?TIMER_REF_KEY(Ref), TimerData),
    Ref.

%% 获取timer剩余时间
do_get_timer_remain(RefKey) ->
    case erlang:get(?TIMER_REF_KEY(RefKey)) of
        undefined ->
            ?NONE;
        #timer_data{invoke_time = Time} ->
            Remain = Time - util:now_ms(),
            ?IF(Remain >= 0, Remain, false)
    end.
    
%% 删除timer
do_delete_timer(Ref) ->
    do_set_timer_list(lists:delete(Ref, do_get_timer_list())),
    erlang:erase(?TIMER_REF_KEY(Ref)),
    ok.

%% 获取timer list
do_get_timer_list() ->
    case erlang:get(?TIMER_LIST_KEY) of
        undefined ->
            [];
        L ->
            L
    end.

%% 设置timer list
do_set_timer_list(L) ->
    erlang:put(?TIMER_LIST_KEY, L).

%% 执行timeout
do_invoke_timeout(Ref, #timer_data{mod = Mod, msg = Msg, time = Time, loop = Loop} = TimerData, 
                  Now, State) ->
    try
        case Mod:handle_timeout(Msg, State) of
            ok ->
                State2 = State;
            {ok, State2} ->
                State2;
            State2 ->
                State2
        end,
        case Loop of
            false ->
                do_delete_timer(Ref);
            true ->
                TimerData2 = TimerData#timer_data{invoke_time = Now + Time},
                erlang:put(?TIMER_REF_KEY(Ref), TimerData2)
        end,
        State2
    catch
        _T:_R ->
            lager:error(":~p处理timeout:~p 错误~p:~p", [Mod, Msg, _T, _R]),
            do_delete_timer(Ref),
            State
    end.
