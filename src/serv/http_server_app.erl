-module(http_server_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
        Dispatch = cowboy_router:compile([
            {'_', [{"/", web_handler, []}]}
        ]),

        {ok, _} = cowboy:start_http(http_server, 100, [{port, 8080}], 
                        [
                         {env, [{dispatch, Dispatch}]}
                        ]
                ),

        web_server_sup:start_link().

stop(_Reason) ->
        lager:debug("http_server_app stoped ~p", [_Reason]).