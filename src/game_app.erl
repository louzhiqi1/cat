-module(game_app).
-behaviour(application).

-export([start/2,
		 stop/1]).

start(StartType, StartArgs) ->
	io:format("StartType = ~p, StartArgs = ~p",  [StartType, StartArgs]),
	ok.

stop(Reason) ->
	io:format("stop Reason = ~p", [Reason]),
	ok.
