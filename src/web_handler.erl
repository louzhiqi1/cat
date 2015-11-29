-module(web_handler).

-export([init/2,
		 handle/2]).

-record(state, {
}).

init(Req, Opts) ->
	io:format("web_handler = ~p", [Req]),
	Method = cowboy_req:method(Req),
	#{web_handler := Echo} = cowboy_req:match_qs([web_handler], Req),
	io:format("web_handler = ~p", [Echo]),
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
