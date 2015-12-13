%%%----------------------------------------------------------------------
%%%
%%% @date  2012.07.19
%%% @doc 玩家监督树
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(role_sup).
-include("common.hrl").

-export([start/0]).

-define(SERVER, ?MODULE).

-define(TCP_OPTS,
        [
                binary, 
                {active, false}, 
                {reuseaddr, true}, 
                {delay_send, true}, 
                {nodelay, true}, 
                {send_timeout, 8000},
                {max_connections, ?CONFIG(gate_conn_max)},
                {packet_size, 2}
        ]).


start() ->
        case ranch:start_listener(role_sup, 10, ranch_tcp, 
                [{port, ?CONFIG(gate_port)} | ?TCP_OPTS], role_server, []) of
                {error, Reason} ->
                        lager:error("start role_sup error, Reason = ~p", [Reason]),
                        halt();
                {ok, Pid} ->
                        erlang:register(role_sup, Pid)
        end.





