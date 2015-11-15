%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2012.06.29
%%% @doc 网络发送, SendType发送类型: send_push立刻发送，send_merge合并发送
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(game_send, [SendType]).
-include("common.hrl").
-include("role.hrl").
-include("protocol.hrl").
-include("pmod.hrl").

-export([self/4, role/4, roles/4]).

%% @doc 发送数据到玩家自己(同一进程)
self(Role, Mi, Fi, Send) ->
    {packed, Bin, Size} = pack:pack_msg(Mi, Fi, Send),
    case SendType of
        send_push ->
            role_server:do_send_data(Bin, Role);
        send_merge ->
            role_server:do_send_msg_merge(Bin, Size, Role)
    end.

%% @doc 发送数据到玩家
role(Id, Mi, Fi, Send) ->
	%?DEBUG(?_U("发送给玩家~p数据:~p"), [Id, {Mi, Fi, Send}]),
	Respon = pack:pack_msg(Mi, Fi, Send),
	role_server:SendType(Id, Respon).

%% @doc 向多人发送数据
roles(Ids, Mi, Fi, Send) ->
	Respon = pack:pack_msg(Mi, Fi, Send),
	[role_server:SendType(Id, Respon) || Id <- Ids],
    ok.

