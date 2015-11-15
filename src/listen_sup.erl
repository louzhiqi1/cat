%%%----------------------------------------------------------------------
%%%
%%% @date  2012.06.12
%%% @doc 监听sup
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(listen_sup).
-behaviour(supervisor).
-include( "common.hrl" ) .

-export([start_link/0, start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

%% @doc 启动
start_link() -> 
    start_link([]).

start_link(_Args) ->
    ?DEBUG(" start listen_sup" , []),
    {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    do_socket_listen(),
    {ok, Sup}.

init([]) ->
    AChild = {serv_listen , {serv_listen ,start_link,[]},
                permanent , 20000 , worker, [serv_listen]}, 
    {ok,{{simple_one_for_one , 10 , 10 }, [AChild]}}.


%%--------------------
%% Internal API
%%--------------------

%% 启动端口监听
-define(TCP_OPTS,
    [binary, 
    {active, false}, 
    {reuseaddr, true}, 
    {delay_send, true}, 
    {nodelay, true}, 
    {send_timeout, 8000}
    ]).

do_socket_listen() ->
    Port = ?CONFIG(gate_port, 9001),
    case gen_tcp:listen(Port, ?TCP_OPTS) of
        {ok, Listener} ->
            Max = ?CONFIG(gate_conn_max, 4000),
            {ok, _} = supervisor:start_child(listen_sup, [[Listener, Max]]);
        Error ->
            ?ERROR("error open port : ~p ~n" , [Error]), 
            exit(Error)
    end,
    ok.
