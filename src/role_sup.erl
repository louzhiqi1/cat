%%%----------------------------------------------------------------------
%%%
%%% @date  2012.07.19
%%% @doc 玩家监督树
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(role_sup).
-include("common.hrl").

-behaviour(supervisor).

-export([start_link/0, start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->	
    start_link([]) .

start_link(_Args) ->
	?DEBUG("client sup start" , []) ,
	supervisor:start_link({local , ?MODULE}, ?MODULE, []) .

init([]) ->
    Child = {	role_server ,
				{ role_server ,start_link,[]},
	      		temporary,
				200000,
				worker,
				[ role_server ]
			 } , 
    {ok, {{simple_one_for_one , 10 , 10 }, [Child]}}.
