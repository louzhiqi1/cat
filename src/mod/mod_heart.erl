%%%--------------------------------------------
%%% @date  2012.11.22
%%% @doc   心跳
%%% @end
%%%
%%%--------------------------------------------

-module(mod_heart).
-include("common.hrl").
-include("role.hrl").
-include("ecode.hrl").
-include("protocol.hrl").

-behaviour(gen_mod).
-export([start_heart_check_timer/0]).
-export([i/1, p/1, init/0, register_event/0, init_role/1, terminate_role/1, clear_daily/1,
         handle_c2s/3, handle_timeout/2, handle_s2s_call/2, handle_s2s_cast/2]).

%% @doc 启动heart timeout timer
start_heart_check_timer() ->
    proc_timer:start_timer(?MODULE, 20000, check_heart_timeout, true).

%% @doc 运行信息
i(_Role) ->
    ok.

%% @doc 运行信息字符
p(_Info) ->
    "".

%% 初始化
init() ->
    ok.

%% @doc 关心的事件
register_event() ->
    [].

%% @doc 初始玩家数据
init_role(Role) ->
    {ok, Role}.

%% @doc 玩家模块结束
terminate_role(_Role) ->
    ok.

%% 清除daily
clear_daily(_Role) ->
	ok.

%% @doc 处理timeout
handle_timeout(check_heart_timeout, Role) ->
    anti_cheat:check_heart_timeout(Role),
    {ok, Role};
handle_timeout(_Event, Role) ->
    {ok, Role}.

%% @doc 处理c2s请求
handle_c2s(?P_HEART_ROLE, _, Role) ->
    ?GAME_SEND_MERGE:self(Role, ?P_HEART, ?P_HEART_ROLE, util:now_sec()),
    anti_cheat:check_heart_quick(Role),
    {ok, Role};

%% 其他未知协议
handle_c2s(_Fi, _, Role) ->  
    {ok, Role}.


%% @doc 处理服务器内服的cast
handle_s2s_cast(_Req, Role) ->
    {ok, Role}.

%% @doc 处理服务器内部的call
handle_s2s_call(_Req, Role) ->
    {ok, Role}.


