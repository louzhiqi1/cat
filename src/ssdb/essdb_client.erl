-module(essdb_client).
-behaviour(gen_server).
-export([
         start/3,
         start_link/3,
         stop/1,
         shutdown/2,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("db.hrl").

-record(state, {host :: string() | undefined,
                port :: integer() | undefined,
                socket :: port() | undefined,
                parser_state :: #pstate{} | undefined,
                queue :: queue:queue() | undefined}).

-define(SOCKET_OPTS, [binary, {active, once}, {packet, raw}, {reuseaddr, true}]).
-define(RECONNECT_SLEEP, 100).

-spec start(Host::list(), Port::integer(), No::integer()) -> {ok, Pid::pid()} | {error, Reason::term()}.

start(Host, Port, No) ->
    gen_server:start({local, list_to_atom(atom_to_list(?MODULE) ++ "_" ++ integer_to_list(No))}, ?MODULE, [Host, Port], []).

-spec start_link(Host::list(), Port::integer(), No::integer()) -> {ok, Pid::pid()} | {error, Reason::term()}.
start_link(Host, Port, No) ->
    gen_server:start_link({local, list_to_atom(atom_to_list(?MODULE) ++ "_" ++ integer_to_list(No))}, ?MODULE, [Host, Port], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

shutdown(Pid, Timeout) ->
    catch gen_server:call(Pid, shutdown, Timeout),

    ok.

init([Host, Port]) ->
    State = #state{host = Host,
                   port = Port,
                   parser_state = essdb_parser:init(),
                   queue = queue:new()},

    case connect(State) of
        {ok, NewState} ->
            {ok, NewState};
        {error, Reason} ->
            {stop, {connection_error, Reason}}
    end.

handle_call({request, Cmd, Req}, From, State) ->
    io:format("Cmd = ~p, Req = ~p", [Cmd, Req]),
    do_request(Cmd, Req, From, State);

handle_call(shutdown, _From, State) ->
    terminate(shutdown, State),

    exit(self(), kill),

    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, _Socket, Bs}, State) ->
    inet:setopts(State#state.socket, [{active, once}]),
    io:format("Bs = ~p", [Bs]),
    {noreply, handle_response(Bs, State)};

handle_info({tcp_closed, _Socket}, State) ->
    Self = self(),
    spawn(fun() -> reconnect_loop(Self, State) end),

    {noreply, State#state{socket = undefined, queue = queue:new()}};

handle_info({connection_ready, Socket}, #state{socket = undefined} = State) ->
    {noreply, State#state{socket = Socket}};

handle_info(_Info, State) ->
    {stop, {unhandled_message, _Info}, State}.

terminate(shutdown, _) ->
    ok;

terminate(_Reason, State) ->
    case State#state.socket of
        undefined -> ok;
        Socket    -> gen_tcp:close(Socket)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec do_request(Cmd::list(), Req::iolist(), From::pid(), #state{}) ->
                        {noreply, #state{}} | {reply, Reply::any(), #state{}}.

do_request(_Cmd, _Req, _From, #state{socket = undefined} = State) ->
    {reply, {error, no_connection}, State};

do_request(Cmd, Req, From, State) ->
    ok = inet:setopts(State#state.socket, [{active, once}]),
    case gen_tcp:send(State#state.socket, Req) of
        ok ->
            NewQueue = queue:in({From, Cmd}, State#state.queue),
            {noreply, State#state{queue = NewQueue}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

-spec handle_response(Data::binary(), State::#state{}) -> NewState::#state{}.
handle_response(Data, #state{parser_state = ParserState,
                             queue = Queue} = State) ->
    case essdb_parser:parse(ParserState, Data) of
        {ReturnCode, Value, NewParserState} ->
            NewQueue = reply({ReturnCode, Value}, Queue),
            State#state{parser_state = NewParserState,
                        queue = NewQueue};
        {ReturnCode, Value, Rest, NewParserState} ->
            NewQueue = reply({ReturnCode, Value}, Queue),
            handle_response(Rest, State#state{parser_state = NewParserState,
                                              queue = NewQueue});
        {continue, NewParserState} ->
            State#state{parser_state = NewParserState}
    end.

to_tlist(_, [], Tlist, _) ->
    Tlist;

to_tlist(Fun, [Key | Rest], Tlist, {}) ->
    to_tlist(Fun, Rest, Tlist, {Key});

to_tlist(Fun, [Value | Rest], Tlist, {Key}) ->
    to_tlist(Fun, Rest, lists:append(Tlist, [{Key, Fun(Value)}]), {}).

format_value(Cmd, {ok, [Value]}) when Cmd == set;
                                      Cmd == setx;
                                      Cmd == multi_set;
                                      Cmd == del;
                                      Cmd == multi_del;
                                      Cmd == incr;
                                      Cmd == decr;
                                      Cmd == multi_hset;
                                      Cmd == multi_hdel;
                                      Cmd == hsize;
                                      Cmd == hset;
                                      Cmd == hdel;
                                      Cmd == hclear;
                                      Cmd == hincr;
                                      Cmd == hdecr;
                                      Cmd == multi_zset;
                                      Cmd == multi_zdel;
                                      Cmd == zset;
                                      Cmd == zsize;
                                      Cmd == zdel;
                                      Cmd == zrank;
                                      Cmd == zrrank;
                                      Cmd == zclear;
                                      Cmd == zincr;
                                      Cmd == zdecr;
                                      Cmd == zcount;
                                      Cmd == zsum;
                                      Cmd == zremrangebyscore;
                                      Cmd == zremrangebyrank ->
    {ok, list_to_integer(binary_to_list(Value), 10)};

format_value(Cmd, {ok, [Value]}) when Cmd == zavg ->
    {ok, list_to_float(binary_to_list(Value))};

format_value(Cmd, {ok, [Value]}) when Cmd == get;
                                      Cmd == hget;
                                      Cmd == zget ->
    {ok, Value};

format_value(Cmd, {ok, [Value]}) when Cmd == exists;
                                      Cmd == hexists;
                                      Cmd == zexists ->
    case Value of
        <<"1">> ->
            {ok, true};
        <<"0">> ->
            {ok, false}
    end;

format_value(Cmd, {ok, Value}) when Cmd == multi_exists;
                                    Cmd == multi_hexists;
                                    Cmd == multi_zexists ->
    {ok, to_tlist(fun(Value2) ->
                    case Value2 of
                        <<"1">> ->
                            true;
                        <<"0">> ->
                            false
                    end
                  end,
                  Value, [], {})};

format_value(Cmd, {ok, Value}) when Cmd == scan;
                                    Cmd == rscan;
                                    Cmd == hgetall;
                                    Cmd == hscan;
                                    Cmd == hrscan;
                                    Cmd == multi_hget;
                                    Cmd == multi_zget;
                                    Cmd == zrange;
                                    Cmd == zrrange;
                                    Cmd == zscan;
                                    Cmd == zrscan ->
    {ok, to_tlist(fun(Value2) -> Value2 end, Value, [], {})};

format_value(Cmd, {ok, Value}) when Cmd == multi_hsize;
                                    Cmd == multi_zsize ->
    {ok, to_tlist(fun(Value2) ->
                    list_to_integer(binary_to_list(Value2), 10)
                  end,
                  Value, [], {})};

format_value(_, Value) ->
    Value.

reply(Value, Queue) ->
    case queue:out(Queue) of
        {{value, {From, Cmd}}, NewQueue} ->
            gen_server:reply(From, format_value(Cmd, Value)),
            NewQueue;
        {empty, Queue} ->
            error_logger:info_msg("Nothing in queue, but got value from parser~n"),
            throw(empty_queue)
    end.

connect(State) ->
    case gen_tcp:connect(State#state.host, State#state.port, ?SOCKET_OPTS) of
        {ok, Socket} ->
            {ok, State#state{socket = Socket}};
        {error, Reason} ->
            {error, {connection_error, Reason}}
    end.

% do_sync_command(Socket, Command) ->
%     inet:setopts(Socket, [{active, false}]),
%     case gen_tcp:send(Socket, Command) of
%         ok ->
%             case gen_tcp:recv(Socket, 0) of
%                 {ok, <<"+OK\r\n">>} ->
%                     inet:setopts(Socket, [{active, once}]),
%                     ok;
%                 Other ->
%                     {error, {unexpected_data, Other}}
%             end;
%         {error, Reason} ->
%             {error, Reason}
%     end.

reconnect_loop(Client, State) ->
    case catch(connect(State)) of
        {ok, #state{socket = Socket}} ->
            gen_tcp:controlling_process(Socket, Client),
            Client ! {connection_ready, Socket};
        {error, _Reason} ->
            timer:sleep(?RECONNECT_SLEEP),
            reconnect_loop(Client, State);
        {'EXIT', _} ->
            timer:sleep(?RECONNECT_SLEEP),
            reconnect_loop(Client, State)
    end.