%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2011.10.12
%%% @doc 临时性的cd处理模块(非持久化),基于进程字典
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(temp_cd).
-include("common.hrl").

-export([valid/2, valid/3, set/1, set/2, get_diff/2]).

%% @doc 检测cd是否合法,时间为ms
valid(Type, Time) when Time > 0 ->
    %?DEBUG(?_U("检测cd:~p 时间间隔:~p"), [Type, Time]),
    valid(Type, Time, util:now_ms()).

%% @doc 检测cd是否合法,时间为ms(有网络容忍)
valid(Type, Time, Now) when Time > 0, Now > 1340000000000 ->
    T = Now - get_last_time(Type),
    %?DEBUG(?_U("检测cd:~p 时间间隔:~p now:~p t:~p"), [Type, Time, Now, T]),
    T >= Time - ?CD_NET_TOLERATE;
%% @doc 检测cd是否合法,时间为sec
valid(Type, Time, Now) when Time > 0 ->
    T = Now - get_last_time(Type),
    %?DEBUG(?_U("检测cd:~p 时间间隔:~p now:~p t:~p"), [Type, Time, Now, T]),
    T >= Time.

%% @doc 设置发生时间
set(Type) ->
    set(Type, util:now_ms()).

%% @doc 设置发生时间
set(Type, Now) ->
    put({temp_cd, Type}, Now),
    ok.

%% @doc 获取与上次时间之间的差距
get_diff(Type, Now) ->
    Now - get_last_time(Type).

%%----------------
%% internal API
%%----------------

%% 获取上次cd时间
get_last_time(Type) ->
    case erlang:get({temp_cd, Type}) of
        undefined ->
            0;
        T when is_integer(T) ->
            T
    end.

%%------------
%% EUNIT Test
%%------------
-ifdef(EUNIT).

basic_test_() ->
    {inorder, 
    [
        ?_assertEqual(false, valid("test_cd_1", 100000, 2000)),
        ?_assertEqual(true, valid("test_cd_1", 1000, 2000)),

        ?_assertEqual(ok, set("test_cd_1", 1000)),
        ?_assertEqual(false, valid("test_cd_1", 1000, 1050)),
        ?_assertEqual(true, valid("test_cd_1", 1000, 2000))

    ]}.

-endif.
