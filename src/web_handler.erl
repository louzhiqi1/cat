-module(web_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("wg_log.hrl").

-record(state, {
}).

init(_, Req, Opts) ->
	Method = cowboy_req:method(Req),
	#{echo := Echo} = cowboy_req:match_qs([web_handler], Req),
	io:format("web_handler = ~p", [Req]),
	Req2 = echo(Method, Echo, Req),
	{ok, Req2, Opts}.

echo(<<"GET">>, undefined, Req) ->
	cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req);
echo(<<"GET">>, Echo, Req) ->
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], Echo, Req),
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
	{ok, Req2} = cowboy_req:reply(200, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
