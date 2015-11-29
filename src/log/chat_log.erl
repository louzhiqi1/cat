%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2011.08.09
%%% @doc 玩家聊天日志记录
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(chat_log).
-include("common.hrl").
-include("role.hrl").

-export([i/0]).
-export([world/2]).

-define(TAB, $\t).
-define(CHAT_WORLD, <<"世">>).
-define(CHAT_MAP, <<"图">>).
-define(CHAT_GUILD, <<"帮">>).
-define(CHAT_P2P, <<"私">>).
-define(CHAT_TEAM, <<"组">>).
-define(CHAT_HORN, <<"传">>).

%% @doc 状态信息
i() ->
    [].
    %log_server:i(?CHAT_LOG_SERVER).

%% @doc 世界聊天
world(#role{id = RoleId} = Role, Content) ->
    do_chat_monitor(Role, Content),
    do_write([?CHAT_WORLD, ?N2S(RoleId), Content, ""]).

%%--------------
%% Internal API
%%--------------

%% 写入log
do_write([_A, _B, _C, _D]) ->
    ok.
    %log_server:write_iodata(?CHAT_LOG_SERVER, 
    %    [?N2S(util:now_sec()), ?TAB, A, ?TAB, B, ?TAB, C, ?TAB, D, "\n"]).

%% 聊天日志监控
do_chat_monitor(_Role, _Content) ->
    ok.
