%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2011.05.17
%%% @doc 玩家的各种计数器,基于进程辞典,持久化通过db_counter完成
%%%  增加模块化参数SaveType: 
%%%     db表示数据库;
%%%     ram表示运行时数据(重启会丢失)
%%%     daily表示每日清理的数据(重启会丢失)
%%%  *注意*：key必须为数字，value为erlang term
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(role_counter, [SaveType]).
-include("common.hrl").
-include("counter.hrl").
-include("pmod.hrl").

-export([i/0]).
-export([get/1, get/2, set/2, 
        set_if_not_exist/1, set_if_not_exist/2,
        inc/1, inc/2, dec/1, dec/2,
        list/0, delete/1, clear/0]).
-export([load/1, save/1]).

%% 用来存储玩家相关计数的key
-define(ROLE_COUNTER_KEY(Id), {ckey, SaveType, Id}).

%% i信息
i() ->
    list().

%% @doc 获取某个counter,如果不存在返回0
get(Id) ->
    (?MODULE:new(SaveType)):get(Id, 0).

%% @doc 获取某个counter,如果不存在返回Def
get(Id, Def) ->
    case erlang:get(?ROLE_COUNTER_KEY(Id)) of
        undefined ->
            Def;
        Val ->
            Val
    end.

%% @doc 设置某个counter
set(Id, Data) ->
    erlang:put(?ROLE_COUNTER_KEY(Id), Data),
    ok.

%% @doc 设置某个counter,仅当这个counter不存在时
%% 如记录某件事情是否做过,如果已经做过则不会再更新
set_if_not_exist(Id) ->
    set_if_not_exist(Id, 1).
set_if_not_exist(Id, N) ->
    case erlang:get(?ROLE_COUNTER_KEY(Id)) of
        undefined ->
            set(Id, N);
        _ ->
            ok
    end.
    
%% @doc 某个计数器+1
inc(Id) ->
    inc(Id, 1).

%% @doc 某个计数器增加
inc(Id, N) ->
    New = (?MODULE:new(SaveType)):get(Id) + N,
    %?DEBUG(?_U("计数器:~p +1 ~p"), [Id, New]),
    set(Id, New),
    New.

%% @doc 某个计数器减1
dec(Id) ->
    dec(Id, 1).

%% @doc 某个计数器减少
dec(Id, N) ->
    New = (?MODULE:new(SaveType)):get(Id) - N,
    %?DEBUG(?_U("计数器:~p -1 ~p"), [Id, New]),
    New2 = ?IF(New < 0, 0, New),
    set(Id, New2),
    New2.

%% @doc 获取玩家的计数器列表
list() ->
    [{K, V} || {{ckey, Type, K}, V} <- erlang:get(), Type =:= SaveType].

%% @doc 清除某个计数
delete(Key) ->
    erlang:erase(?ROLE_COUNTER_KEY(Key)),
    ok.

%% @doc 清理玩家的所有计数
clear() ->
    ?DEBUG(?_U("**清除计数器"), []),
    [begin
        erlang:erase(?ROLE_COUNTER_KEY(K)) 
    end || {K, _V} <- list()],
    ok.

%% @doc 加载玩家计数器
load(Data) ->
    [set(K, V) || {K, V} <- Data].
%% @doc 存储玩家计数器(玩家离线时)
save(_RId) ->
    list().

%%----------------
%% internal API
%%----------------

%%------------
%% EUNIT Test
%%------------
-ifdef(EUNIT).

basic_test_() ->
    {inorder, 
    [
    ]}.

-endif.
