%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2010.12.27
%%% @doc  处理客户端的gm命令
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(gm_command).
-include("common.hrl").
-include("role.hrl").
-export([process/2]).

%% @doc 处理gm命令
process(#role{} = Role, Content) ->
    % 检测gm权限
    ?IF(role_status:is_gm(Role), ok, throw(false)),
    do_command(Content, Role).

%%----------------
%% internal API
%%----------------

%% 主动崩溃
do_command(<<"#crash", _Rest/bytes>>, _Role) ->
    {crash, "crash_by_gm_command"};
%% 查询自己id
do_command(<<"#who", _Rest/bytes>>, 
           #role{id = Id} = Role) ->
    ?DEBUG(?_U("玩家:~p执行gm命令:who"), [Id]),
    Msg = ?S2B(lists:concat([Id])),
    mod_chat:send_msg_to_self(Role, Msg),
    {ok, Role};
do_command(_, Role) ->
    {ok, Role}.

%%------------
%% EUNIT Test
%%------------
-ifdef(EUNIT).

str_to_n_test_() ->
    [
        ?_assertEqual(0, str_to_n("0")),
        ?_assertEqual(0, str_to_n(" 0 ")),
        ?_assertEqual(-1, str_to_n(" -1 ")),
        ?_assertEqual(1, str_to_n(" +1 ")),
        ?_assertEqual(1, str_to_n(" 1 ")),
        ?_assertEqual(1, str_to_n("1")),
        ?_assertEqual(10000, str_to_n("10000"))
    ].

-endif.
