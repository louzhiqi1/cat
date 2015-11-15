%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2010.11.02
%%% @doc the game ctl module
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(game_ctl).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("common.hrl").

-export([start/0, init/0, process/1]).
-export([start_trace/0]).

start() ->
    case init:get_plain_arguments() of
    [SNode | Args]->
        SNode1 = case string:tokens(SNode, "@") of
        [_Node, _Server] ->
            SNode;
        _ ->
            case net_kernel:longnames() of
             true ->
                 SNode ++ "@" ++ inet_db:gethostname() ++
                      "." ++ inet_db:res_option(domain);
             false ->
                 SNode ++ "@" ++ inet_db:gethostname();
             _ ->
                 SNode
             end
        end,
        Node = list_to_atom(SNode1),
        Status = 
        case rpc:call(Node, ?MODULE, process, [Args], 60000) of
            {badrpc, nodedown} ->
                ?STATUS_NORUN;
            {badrpc, _Reason} ->
                ?PRINT("RPC failed on the node ~p: ~p~n", [Node, _Reason]),
                ?STATUS_BADRPC;
            S ->
                %?PRINT("RPC return :~p~n", [S]),
                S
        end,
        halt(Status);
    _ ->
        halt(?STATUS_USAGE)
    end.

%% @doc 启动trace client
start_trace() ->
    case init:get_plain_arguments() of
    [SNode, Port | _Rest] = _Args->
        %?PRINT("plain arguments is(~p):~n~p~n", [length(_Args), _Args]),
        SNode1 = case string:tokens(SNode, "@") of
        [_Node, _Server] ->
            SNode;
        _ ->
            case net_kernel:longnames() of
             true ->
                 SNode ++ "@" ++ inet_db:gethostname() ++
                      "." ++ inet_db:res_option(domain);
             false ->
                 SNode ++ "@" ++ inet_db:gethostname();
             _ ->
                 SNode
             end
        end,
        Node = list_to_atom(SNode1),
        case net_kernel:connect(Node) of
            true ->
                dbg:trace_client(ip, ?S2N(Port)),
                ?STATUS_SUCCESS;
            _ ->
                ?PRINT("connect node ~p failed ~n", [Node]),
                ?STATUS_ERROR
        end;
    _ ->
        halt(?STATUS_USAGE)
    end.


init() ->
    ok.

process(["status"]) ->
    do_status([?MAIN_APP]);
process(["stop", Sync | _]) ->
    erlang:group_leader(erlang:whereis(user), self()),
    try
        game:stop_server(?S2EA(Sync))
    catch
        Class:Reason ->
            ?PRINT("stop game server error ~w:~w", [Class, Reason]),
            ?ERROR2("********* stop ~p:~p", [Class, Reason]),
            ?STATUS_ERROR
    end;

process(["restart"]) ->
    init:restart(),
    ?STATUS_SUCCESS;
% 重新加载数据
process(["reload", Type | T]) ->
    case Type of
        "config" ->
            ok = game_config:reload(),
            ?STATUS_SUCCESS;
        "code" ->
            Mods = [?S2A(M) || M <- T],
            do_reload_code(Mods)
    end;
process([_|_] = _Arg) ->
    io:format("***************** arg:~p", [_Arg]),
    ?STATUS_USAGE.

%%-------------------
%% internal API
%%-------------------
%% 重新加载代码
do_reload_code(L) ->
    Ret = 
    case L of
        [] ->
            wg_reloader:reload(60);
        [_|_] ->
            wg_reloader:reload_modules(L)
    end,
    case Ret of
        [] ->
            ?STATUS_SUCCESS;
        [_|_] ->
            ?PRINT("modules:~p reload failed\n", [L]),
            ?STATUS_ERROR
    end.

%% 获取对应app的状态
do_status(Apps) ->
    %{InternalStatus, _ProvidedStatus} = init:get_status(),
    %?PRINT("Node ~p is ~p. Status: ~p~n",
    %          [node(), InternalStatus, _ProvidedStatus]),
    case util:is_app_running(Apps) of
        true ->
            %?PRINT("node is running~n", []),
            ?STATUS_SUCCESS;
        false ->
            %?PRINT("node:~p~n", [node()]),
            %?PRINT("node is not running:~p~n", [application:which_applications()]),
            ?STATUS_NORUN
    end.

