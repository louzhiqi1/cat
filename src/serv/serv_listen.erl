%%%-------------------------------------------------------------------
%%% 
%%% @doc 监听端口
%%% @date 2011-8-22
%%%
%%% -------------------------------------------------------------------
-module(serv_listen).
-behaviour(gen_server).
-include("common.hrl").

-export([start_link/1, stop/0, i/0]).
-export([max_conn/0, current_conn/0, clients/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
        sock,
        ref,
        max,            % 最大连接数
        cur = 0         % 当前连接数
    }).

-define(SERVER, ?MODULE).

%% @doc 启动
start_link([LSock, Max]) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {LSock, Max}, []).

%% @doc 停止
stop() ->
    gen_server:cast(?SERVER, stop).

%% @doc i信息
i() ->
    gen_server:call(?SERVER, i).

%% @doc 获取最大允许连接数
max_conn() ->
    gen_server:call(?SERVER, max_conn).

%% @doc 获取当前连接
current_conn() ->
    gen_server:call(?SERVER, current_conn).

%% @doc 获取client连接列表
clients() ->
    gen_server:call(?SERVER, clients).

%% @doc 初始化
init({LSock, Max}) ->
    erlang:process_flag(trap_exit, true),
    erlang:process_flag(priority, high),
    gen_server:cast(self(), accept),
    {ok, #state{sock = LSock, max = Max}}.

handle_call(i, _From, State = #state{cur = Cur, max = Max}) ->
    Reply = [{cur, Cur}, {max, Max}],
    {reply, Reply, State};
handle_call(max_conn, _From, State = #state{max = Max}) ->
    {reply, Max, State};
handle_call(current_conn, _From, State = #state{cur = Cur}) ->
    {reply, Cur, State};
handle_call(clients, _From, State) ->
    {monitors, L} = erlang:process_info(self(), monitors),
    Clients = [Pid || {process, Pid} <- L],
    {reply, Clients, State};
handle_call(_Request, _From, State) ->
    ?WARN(?_U("未知的call请求:~p"), [_Request]),
    {noreply, State}.

handle_cast(accept, State) ->
    accept(State);
handle_cast(Req, State) ->
    ?WARN(?_U("未知的cast请求:~p"), [Req]),
    {noreply, State}.

handle_info({inet_async, _LSock, _Ref, {ok, Sock}},
                        State = #state{cur = Max, max = Max}) -> % reach the max limit
    ?INFO("not accepting new connection, reach max ~p", [Max]),
    gen_tcp:close(Sock),
    accept(State);
handle_info({inet_async, LSock, Ref, {ok, Sock}}, 
            State = #state{sock = LSock, ref = Ref, cur = Cur}) ->
    %?DEBUG("************ opts is :~p", [prim_inet:getopts(Sock,  [send_timeout])]),
    true = inet_db:register_socket(Sock, inet_tcp),
    case supervisor:start_child(role_sup, []) of
        {ok, Child} ->
            _MRef = erlang:monitor(process, Child),
            true = erlang:unlink(Child),
            State2 =
            case gen_tcp:controlling_process(Sock, Child) of
                ok ->
                    role_server:active(Child, Sock),
                    State#state{cur = Cur + 1};
                {error, Reason} ->
                    ?ERROR("controlling process error:~p", [Reason]),
                    gen_tcp:close(Sock),
                    State
            end,
            accept(State2);
        Error ->
            gen_tcp:close(Sock),
            ?ERROR("start client:~p", [Error]),
            accept(State)
    end;
handle_info({inet_async, LSock, Ref, {error, closed}}, State=#state{sock=LSock, ref=Ref}) ->
    ?DEBUG("listen sock closed ~p" , [ State ] ) ,
    {stop, normal, State};
handle_info({'DOWN', _MRef, process, _Pid, _Info}, State = #state{cur = Cur}) ->
    %?DEBUG("client process ~p exit reason:~p", [Pid, Info]),
    {noreply, State#state{cur = Cur - 1}};

handle_info( _Info , State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?ERROR("server lister terminate for ~p~n" , [_Reason]),
    ok.

code_change( _OldVsn , State, _Extra) ->
    {ok, State}.

%%---------------------------------------------------------------------
%% Internal API
%%--------------------------------------------------------------------

%% 异步接受连接
accept(State = #state{sock = LSock}) ->
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} ->
            {noreply, State#state{ref=Ref}};
        Error     ->    
            {stop, {cannot_accept, Error}, State}
    end.
