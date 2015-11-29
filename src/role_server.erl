%%%----------------------------------------------------------------------
%%%
%%% @date  2012.06.12
%%% @doc 玩家主进程，负责网络处理和逻辑处理。
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(role_server).
-behaviour(ranch_protocol).
-behaviour(gen_server).

%% API.
-export([start_link/4]).

-include("common.hrl").
-include("role.hrl").
-include("protocol.hrl").
-include("counter.hrl").
-include("log.hrl").

-export([active/2, stop/1, 
        async_stop/1, async_stop/2,
        kickout/1, login/1, get_state/1]).
-export([i/1, i/2, p/1, pid/1, is_online/1,
         get/1, get_name/1, get_by_name/1, get_id_by_name/1]).
-export([send_push/2, send_merge/2, socket/1, peer/1]).

%% 调试使用
-export([apply/2]).


-export([s2s_call/3, s2s_call/4, s2s_call_all/2, s2s_cast/3, s2s_cast_all/2,
         notify_timeout_event/1
        ]).
-export([set_track/2, clear_counter/1]).
-export([init/1, init/4, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% 内部使用
-export([do_send_data/2, do_send_msg_merge/3, notify_self_tcp_error/2]).

%% 消息合并最大长度
-define(PACKET_MERGE_SIZE_MAX, 4096).  
%% 消息合并缓存时间(200ms)
-define(PACKET_MERGE_TIME, 200).
%% 连接丢失事件
-define(CONNECT_LOST_EVENT, 'connect_lost_event').
%% 数据包头长度
-define(HEADER_LENGTH, 2).               

-type opts() :: []. 
-export_type([opts/0]).
 
-record(state, {
    socket :: inet:socket(),
    transport :: module()
}).
 
start_link(Ref, Socket, Transport, Opts) ->
      proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init(_) ->
    {ok, undefined}.
 
init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),

    process_flag(trap_exit , true),
    random:seed(erlang:timestamp()),
    util:set_pid_type(?PID_TYPE_ROLE),
    % 开始启动超时检测
    mod_heart:start_heart_check_timer(),

    gen_server:enter_loop(?MODULE, [], ?ROLE_LOGIN_POLICY).

%% @doc 激活socket
active(Pid, Sock) ->
    cast(Pid, {active, Sock}).

%% @doc 停止玩家进程
stop(Id) ->
    ?IF(is_online(Id), call(Id, stop), ok).

%% @doc 异步停止进程
async_stop(Id) ->
    async_stop(Id, normal).
async_stop(Id, Reason) ->
    async_stop(Id, Reason).

%% @doc 踢玩家下线(同步)
kickout(Id) when is_integer(Id) ->
    ?IF(is_online(Id), call(Id, kickout), ok).

%% @doc 获取玩家信息
i(Id) ->
    i(Id, all).
i(Id, Type) ->
    call(Id, {i, Type}).

%% @doc 打印信息
p(Id) ->
    L = i(Id),
    do_p(L).

%% @doc 获取玩家的pid
pid(Rid) when is_integer(Rid) ->
    Pid = erlang:whereis(util:role_name(Rid)),
    ?IF(Pid =:= undefined, ?NONE, Pid).

%% @doc 判断玩家是否在线
is_online(Id) ->
    pid(Id) =/= ?NONE.

%% @doc 获取某个玩家数据
get(Id) when is_integer(Id) ->
    case serv_role_mgr:get(Id) of
        ?NONE ->
            ?NONE;
        R ->
            R
    end;

%% 获取多个玩家,如果玩家不存在则跳过!
%%  获取的顺序为反序
get(Ids) when is_list(Ids) ->
    lists:foldl(
    fun(Id, Acc) ->
        case catch ?MODULE:get(Id) of
            ?NONE ->
                Acc;
            R ->
                [R | Acc]
        end
    end, [], Ids).


%% @doc 查询玩家姓名
get_name(Id) when is_integer(Id) ->
    serv_role_mgr:get_name_by_id(Id).

%% @doc 依据名字查询玩家
get_by_name(Name) when is_binary(Name) ->
    case serv_role_mgr:get_id_by_name(Name) of
    ?NONE ->
        ?NONE;
    Id ->
        serv_role_mgr:get(Id)
    end.

%% @doc 查询玩家的id
get_id_by_name(Name) when is_binary(Name) ->
    serv_role_mgr:get_id_by_name(Name).

%% @doc 立刻发送消息
send_push(Id, Data) ->
    cast(Id, {send_push, Data}).

%% @doc 发送消息
send_merge(Id, Data) ->
    cast(Id, {send_merge, Data}).

%% @doc 获取对应的socket(调试使用)
socket(Id) ->
    call(Id, get_socket).

%% @doc 获取对应的ip
peer(Id) ->
    Sock = call(Id, get_socket),
    util:peer_str(Sock).

%% @doc 玩家登录(在玩家进程执行)
login(Role) ->
    do_login(Role, 0).
    
%% 取状态信息#role
get_state( Rid ) ->
    call(Rid , get_state) .

%% @doc 在role中执行一个函数
apply(Id, Fun) when is_function(Fun) ->
    call(Id, {apply, Fun}).


%% @doc 服务器内部call给某个玩家
s2s_call(Id, Mod, Req) ->
    call(Id, {s2s_call, Mod, Req}).
s2s_call(Id, Mod, Req, Timeout) ->
    call(Id, {s2s_call, Mod, Req}, Timeout).

%% @doc 服务器内部cast所有玩家
s2s_call_all(Mod, Req)->
    [s2s_call(Pid, Mod, Req) || Pid <- serv_role_mgr:pid_list()].

%% @doc 服务器内部cast给某个玩家
s2s_cast(Id, Mod, Req) ->
    cast(Id, {s2s_cast, Mod, Req}).

%% @doc 服务器内部cast所有玩家
s2s_cast_all(Mod, Req)->
    [s2s_cast(Pid, Mod, Req) || Pid <- serv_role_mgr:pid_list()].

%% @doc 通知timeout事件
notify_timeout_event(Id) ->
    cast(Id, ?TIMEOUT_EVENT).

%% @doc 设置追踪
set_track(Id, Track) ->
    case is_online(Id) of
        true ->
            call(Id, {set_track, Track});
        false ->
            false
    end.

%% @doc 清理计数器
clear_counter(Id) ->
    cast(Id, clear_counter).

handle_call(Req, From, State) ->
    ?HANDLE_CALL_WRAP(Req, State).

handle_cast(Req, State) ->
    ?HANDLE_CAST_WRAP(Req, State).

handle_info(Info, State ) ->
    ?HANDLE_INFO_WRAP(Info, State).

%% 退出时清理角色资源
terminate(normal, ?ROLE_LOGIN_POLICY) ->
    ok;
terminate(normal, #role{login_state = ?ROLE_LOGIN_WAITING}) ->
    ok;
terminate(_Reason, #role{login_state = ?ROLE_LOGIN_WAITING}) ->
    ?ERROR("role terminate when login_state is ?ROLE_LOGIN_WAITING:~p", [_Reason]),
    ok;
terminate(_Reason, #role{sock = Sock, id = Id} = Role) ->
    TerminateR = util:get_terminate_reason(),
    ?IF(_Reason =:= normal, ok, ?ERROR("role ~p terminate:~w and :~w" , [Id, _Reason, TerminateR])),
    try 
        serv_role_mgr:delete(Id),
        ?IF(erlang:is_port(Sock), close_socket(Sock), ok),
        Role2 = gen_mod:terminate_role(Role),
        close_track_fd(),
        ?IF(Id =/= 0, do_log_logout(Role2, TerminateR), ok)
    catch 
        Error:Reason ->
            ?ERROR2("logout error ~p:~p" , [Error , Reason]) 
    end.

code_change( _OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal API
%%---------------------------------------------------------------------

%% 发送cast请求
cast(#role{pid = Pid}, Req) ->
    gen_server:cast(Pid, Req);
cast(Id, Req) when is_integer(Id) ->
    gen_server:cast(util:role_name(Id), Req);
cast(Id, Req) when is_pid(Id) ->
    gen_server:cast(Id, Req).

%% 发送call请求
call(Id, Req) ->
    call(Id, Req, ?GEN_SERVER_TIMEOUT).
call(#role{pid = Pid}, Req, Timeout) ->
    call(Pid, Req, Timeout);
call(Id, Req, Timeout) when is_integer(Id) ->
    gen_server:call(util:role_name(Id), Req, Timeout);
call(Id, Req, Timeout) when is_pid(Id) ->
    gen_server:call(Id, Req, Timeout).

%%-----------------
%% 处理handle_call
%%-----------------

%% 获取状态
do_call({i, Type}, _From, State)->
    Reply = do_i(State, Type),
    {reply, Reply, State};
do_call(get_state, _From , State ) ->
    {reply, {ok , State}, State} ;
%% 设置追踪
do_call({set_track, Track}, _From , State ) ->
    set_track(Track),
    {reply, ok, State};
%% 停止
do_call(stop, _From , State ) ->
    {stop, normal, ok, State} ;
%% 踢出玩家
do_call(kickout, _From , State) ->
    send_msg_login_again(State),
    {stop, normal, ok, State};
%% 1,是否失去连接?
%%  1a,是,继续
%%  1b,否,通知断开连接,关闭原socket
%% 2,更新socket
%% 3,清理心跳数据防止误检测加速
do_call({check_duplicate_login, Sock}, _From, 
    #role{connect_lost = ConnectLost, sock = SockOld} = State) ->
    % 1
    case ConnectLost of
        true ->
            % 1a
            ok;
        false ->
            % 1b
            send_msg_login_again(State),
            close_socket(SockOld)
    end,
    % 2
    State2 = State#role{sock = Sock, connect_lost = false},
    anti_cheat:clear_heart_last_value(),
    {reply, ok, State2};

%% 处理服务内部的call消息
do_call({s2s_call, Mod, Req}, _From, State) ->
    Module =
    if
        is_integer(Mod) ->
            ?HANDLER_NAME(Mod);
        is_atom(Mod) ->
            Mod
    end,
    case handle_c2s_reply(catch Module:handle_s2s_call(Req, State), {s2s_call, Req}, State) of
        {ok, State2} ->
            do_update_role(State2),
            {reply, ok, State2};
        {ok, Reply, State2} ->
            do_update_role(State2),
            {reply, Reply, State2};
        {stop, State2} ->
            {stop, normal, ok, State2}
    end;
do_call({apply, Fun}, _From, State)->
    case catch do_apply(Fun, State) of
        ok ->
            {reply, ok, State};
        #role{} = Role2 ->
            {reply, ok, Role2};
        {ok, #role{} = State2} ->
            {reply, ok, State2};
        {error, Code} ->
            ?WARN(?_U("执行apply函数返回错误码:~p"), [Code]),
            {reply, {error, Code}, State};
        {'EXIT', _Reason} ->
            ?ERROR(?_U("执行apply函数出错:~p"), [_Reason]),
            {reply, {error, _Reason}, State};
        _Other ->
            ?DEBUG(?_U("执行apply函数返回:~p"), [_Other]),
            {reply, _Other, State}
    end;
do_call( _Request, _From, State ) ->
    ?WARN("unknown request:~p", [_Request]),
    {noreply, State}.

%%------------------
%% 处理handle_cast
%%------------------
-ifdef(TEST).
-define(WAIT_ACTIVE_TIMEOUT, 500000).
-else.
-define(WAIT_ACTIVE_TIMEOUT, 5000).
-endif.

do_cast({active, Sock}, ?ROLE_LOGIN_POLICY) ->
    case gen_tcp:recv(Sock, 5, 30000) of
        %% 心跳包
        {ok , <<0,3,1,0,201>>} ->
            State = #role{
                id = 0,
                accname = <<>> ,
                pid = self(),
                sock = Sock,
                length = 0,
                login_state = ?ROLE_LOGIN_WAITING
            },
            {ok, State2} = async_recv(State, ?HEADER_LENGTH),
            {ok, State3} = on_set_socket(State2),
            {noreply, State3};
        _Other ->
            ?DEBUG("wait active error :~p~n" , [_Other]),
            util:set_terminate_reason(waiting_active),
            {stop, normal, #role{login_state = ?ROLE_LOGIN_WAITING}}
    end ;

do_cast(active_for_relogin, State) ->
    {ok, State2} = async_recv(State, ?HEADER_LENGTH),
    {ok, State3} = on_set_socket(State2),
    % 设置一个标记，说明是断线保护内重连,mod_map使用
    role_internal:set_connect_lost_relogin_flag(true),
    mod_role:send_msg_login_role_info(State3),
    {noreply, State3};
do_cast({send_push, {packed, Bin, _Size}} , State ) ->
    do_send_data(Bin, State) ,
    {noreply , State};
do_cast({send_merge, {packed, Bin, Size}}, State ) ->
    do_send_msg_merge(Bin, Size, State) ,
    {noreply , State} ;

%% 处理服务内部的cast消息
do_cast({s2s_cast, Mod, Req}, State) ->
    Module =
    if
        is_integer(Mod) ->
            ?HANDLER_NAME(Mod);
        is_atom(Mod) ->
            Mod
    end,
    case handle_c2s_reply(catch Module:handle_s2s_cast(Req, State), {s2s_cast, Req}, State) of
        {ok, State2} ->
            do_update_role(State2),
            {noreply , State2};
        _Other ->    
            ?WARN(?_U("处理模块~p s2s_cast:~p出错: ~p"), [Module, Req, _Other]),
            {noreply , State}
    end;
%% timeout事件,由wg_timer_engine产生
do_cast(?TIMEOUT_EVENT, State) ->
    {ok, State2} = proc_timer:on_timeout_loop(State),
    {ok, State3} = on_self_loop(State2),
    {noreply, State3};
%% 清理counter
do_cast(clear_counter, State) ->
    ?DEBUG(?_U("玩家:~p清理counter"), [State#role.id]),
    % 不能清空db counter!
    ok = ?COUNTER_DAILY:clear(),
 	% 每个模块清理daily
 	gen_mod:clear_daily(State),
    {noreply, State};
%% 停止进程
do_cast({async_stop, Reason}, State) ->
    %?WARN("*** async stop:~p locktime:~p", [Reason, LockTime]),
    util:set_terminate_reason(Reason),

    {stop, normal, State};

do_cast(Msg, State) ->
    ?ERROR("unkonw messsage : ~p~n" , [ Msg ] ) ,
    {noreply, State}.

%%------------------
%% 处理handle_info
%%------------------

%% 收到数据包头
do_info({inet_async, Sock, Ref, {ok , Data}} , 
    State = #role{sock = Sock, ref = Ref, length = 0}) ->
    <<Len:16>> = Data ,
    {ok, State2} = async_recv(State, Len),
    { noreply , State2#role{length = Len}} ;
%% 收到实际数据
do_info({inet_async, Sock, Ref, {ok , Data}} , 
    State = #role{sock = Sock, ref = Ref}) ->
    <<Mi:8, Fi:8, Bin/binary>> = Data ,
    case catch handle_c2s(Mi, Fi, Bin, State) of
        {stop, State2} ->
            do_update_role(State2),
            {stop, normal, State2};
        {ok, State2} ->
            {ok, State3}= async_recv(State2, ?HEADER_LENGTH),
            do_update_role(State3),
            {noreply, State3#role{length = 0}}
    end;
%% 超时处理
do_info({inet_async, Sock, Ref, {error, timeout}} , 
    State = #role{sock = Sock, ref = Ref, timeout = Timeout}) ->
    if  
        Timeout >= 5 ->
            util:set_terminate_reason(?LOG_LOGOUT_TYPE_TCP_TIMEOUT),
            {stop, normal, State};     
        true ->
            {noreply , State#role{timeout = Timeout + 1}}
    end;
%% sock数据错误
do_info({inet_async, Sock, Ref, {error, Reason}}, 
    State = #role{sock = Sock, ref = Ref}) ->
    State2 = do_when_socket_closed(Reason, State),
    {noreply, State2};
do_info({inet_async, _, _, _} = _Req, State = #role{}) ->
	% ?WARN(?_U("收到未知的inet_async请求:~p"), [_Req]),
    {noreply, State};
do_info({'EXIT', Sock, _Reason}, State = #role{sock = Sock}) ->
    State2 = do_when_socket_closed(closed, State),
    {noreply, State2};
do_info({tcp_error, Sock, Reason}, #role{sock = Sock} = State) ->
    ?WARN("role ~p tcp error ~p", [State#role.id, Reason]),
    util:set_terminate_reason(?LOG_LOGOUT_TYPE_TCP_ERROR),
    {stop, normal, State};
%% 处理inet_reply
do_info({inet_reply, _, ok}, State) ->
    {noreply, State};
do_info({inet_reply, Sock, {error, _Reason}}, #role{sock = Sock} = State) ->
    State2 = do_when_socket_closed(_Reason, State),
    {noreply, State2};
do_info({inet_reply, _, {error, _Reason}}, State) ->
    {noreply, State};

do_info(?CONNECT_LOST_EVENT, State = #role{connect_lost = true}) ->
    % 丢失连接，保护时间到
    %?WARN(?_U("****丢失连接，保护时间到...")),
    util:set_terminate_reason(?CONNECT_LOST_EVENT),
    {stop, normal, State};
do_info(?CONNECT_LOST_EVENT, State) ->
    %?WARN(?_U("****丢失连接，保护时间到,状态不对阿..")),
    {noreply, State};


do_info(Info , State ) ->
    ?ERROR("unknow info request: ~p", [Info]) ,
    {noreply, State}.

do_handle_c2s_on_dead(_Mi, _Fi, _Req, State) ->
    {ok, State}.

%% 当切图时处理客户端请求
%% 忽略所有的客户端请求
%% do_handle_c2s_on_switching(?P_HEART, ?P_HEART_ROLE, Req, State) ->
%%     mod_heart:handle_c2s(?P_HEART_ROLE, Req, State);
do_handle_c2s_on_switching(_Mi, _Fi, _Req, State) ->
    %?WARN(?_U("玩家处于切图状态收到非法消息 ~p:~p ~w"), [_Mi, _Fi, _Req]),
    {ok, State}.

%% 处理请求
handle_c2s(Mi, Fi, Bin, State) ->
    Module = ?HANDLER_NAME(Mi) ,
    {Request, _} = pack:unpack(Bin),
    ReqArg = {c2s, Mi, Fi, Request},
    if
        Mi =:= ?P_LOGIN
            andalso (Fi =:= ?P_LOGIN_LOGIN orelse Fi =:= ?P_LOGIN_CREATE_PAGE) ->
            % 登录请求特殊处理
            ok;
        Mi =:= ?P_ROLE andalso Fi =:= ?P_ROLE_CREATE ->
            % 创角请求特殊处理
            ok;
		Mi =:= ?P_HEART andalso Fi =:= ?P_HEART_ROLE ->
			% 心跳
			ok;
        true ->
            ?IF(role_status:in_dead(State),
                begin
                    RetOnDead = (catch do_handle_c2s_on_dead(Mi, Fi, Request, State)),
                    throw(handle_c2s_reply(RetOnDead, ReqArg, State))
                end,
                ok),
            ?IF(role_status:in_switching(State),
                begin
                    RetOnSwitch = (catch do_handle_c2s_on_switching(Mi, Fi, Request, State)),
                    throw(handle_c2s_reply(RetOnSwitch, ReqArg, State))
                end,
                ok)
    end,
    do_track_in(State#role.id, Mi, Fi, Request),
    handle_c2s_reply(catch Module:handle_c2s(Fi, Request, State), ReqArg, State).

%% 处理handle_request, handle_s2s_call, handle_s2s_cast返回值
handle_c2s_reply(ok, _, State) ->
    {ok, State};
handle_c2s_reply({ok, State}, _, _) ->
    {ok, State};
handle_c2s_reply({ok, Reply, State}, _, _) ->
    {ok, Reply, State};
handle_c2s_reply({crash, Reason}, _, Role) ->
    ?ERROR(?_U("玩家:~pcrash:~p"), [Role#role.id, Reason]),
    {stop, Role};
handle_c2s_reply({respon, Respon, State}, {c2s, Mi, Fi, _Req}, _) ->
    ?IF(Respon =:= ?E_UNKONW_DATA,
        ?DEBUG(?_U("***玩家~p的请求~p-~p未知:~p!"), [State#role.id, Mi, Fi, _Req]),
        ok),
    Data = pack:pack_msg(Mi, Fi, Respon ),
    do_send_data(Data, State),
    {ok, State};
handle_c2s_reply({'EXIT', relogin}, _, State) ->
    ?DEBUG(?_U("玩家已经登录...使用原进程，本进程退出")),
    util:set_terminate_reason(relogin),
    {stop, State};
handle_c2s_reply({'EXIT', _Reason}, {c2s, Mi, Fi, _Req}, State) ->
    ?ERROR2(?_U("玩家~p处理请求~p-~p:~p错误:~p"), [State#role.id , Mi, Fi, _Req, _Reason]),
    {ok, State};
handle_c2s_reply({error, _Reason}, {c2s, Mi, Fi, _Req}, State) ->
    ?ERROR2(?_U("玩家~p处理请求~p-~p:~p错误:~p"), [State#role.id , Mi, Fi, _Req, _Reason]),
    {ok, State};
handle_c2s_reply(Response, {c2s, Mi, Fi, _Req}, State) ->
    Bin = pack:pack_msg(Mi, Fi, Response),
    do_send_data(Bin, State),
    {ok, State};
handle_c2s_reply(_Reason, Data, State) ->
    ?ERROR2(?_U("玩家~p处理请求~p错误:~p"), [State#role.id , Data, _Reason]),
    {ok, State}.

%% 异步接受信息
async_recv(#role{sock = Sock} = State, Length) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Length, -1) of
        {error, Reason} -> 
            %?ERROR( "recv error for ~p~n" , [ { Sock, Length, Timeout , Reason }]), 
            State2 = do_when_socket_closed(Reason, State),
            {ok, State2};
        {ok, Ref} -> 
            {ok, State#role{ref = Ref}}
    end.

%% 发送数据
do_send_data(_Data, #role{connect_lost = true}) ->
    ok;
do_send_data(Data, #role{sock = Sock, id = Id, connect_lost = false}) ->
    Bin2 = 
    case Data of
        {merge, Bin} ->
            [do_track_out(Id, E) || E <- Bin],
            Bin;
        {packed, Bin, _Size} ->
            do_track_out(Id, Bin),
            Bin;
        {Bin, _} ->
            do_track_out(Id, Bin),
            Bin;
        Bin ->
            do_track_out(Id, Bin),
            Bin
    end,
    case catch erlang:port_command(Sock, Bin2, [force, nosuspend]) of
        true ->
            ok; 
        false ->
            % busy
            ?WARN("role ~p send data busy", [Id]),
            ok;
        _Reason ->
            ?IF(is_start_for_debug(), ok,
                notify_self_tcp_error(Sock, _Reason)),
            ok
    end.

%% 发送消息给自身，通知tcp_error
notify_self_tcp_error(Sock, Reason) ->
    self() ! {tcp_error, Sock, Reason}.

%% 当socket出错
do_when_socket_closed(_Reason, #role{connect_lost = true} = State) ->
    %?WARN(?_U("玩家socket已经出错"), []),
    State;
do_when_socket_closed(_Reason, #role{login_state = ?ROLE_LOGIN_FINISH} = State) ->
    ?DEBUG(?_U("玩家~p socket error:~p 启动保护timer!"), [State#role.id, _Reason]),
    % 启动保护时间 ,若玩家是在死亡状态断线的，不进行保护。
    ?IF(role_status:in_dead(State),
        (self() ! ?CONNECT_LOST_EVENT), 
        erlang:send_after(10000, self(), ?CONNECT_LOST_EVENT)),
    State#role{connect_lost = true};
do_when_socket_closed(_Reason, State) ->
    self() ! ?CONNECT_LOST_EVENT,
    State#role{connect_lost = true}.

%% 处理login
%% 判断玩家是否在线?
%% a,否，注册名称
%% b,是，则通知原玩家更新socket,此进程退出
do_login(#role{id = Rid}, 3) ->
    ?ERROR(?_U("玩家~p尝试登录失败"), [Rid]),
    exit(relogin_limit);
do_login(#role{id = Rid, sock = Sock} = Role, N) ->
    case pid(Rid) of
        ?NONE ->
            % a
            case catch erlang:register(util:role_name(Rid), self()) of
                {'EXIT', {badarg, _}} ->
                    ?ERROR("role ~p register name error!", [Rid]),
                    exit(register_name);
                true ->
                    Role1 = Role#role{login_state = ?ROLE_LOGIN_FINISH},
                    Role3 = gen_mod:init_role(Role1),
                    Role31 = Role3#role{last_login_time = util:now_sec(), last_logout_time = 0},
                    Role4 = game_formula:calc_role(Role31),
                    TrackType = serv_gm_ctrl:get_track(Rid),
                    ?IF(TrackType =/= ?ROLE_TRACK_NONE, set_track(TrackType), ok),
                    game_log:login(Role4),
                    Role4
            end;
        Pid ->
            % b
            try
                ok = call(Pid, {check_duplicate_login, Sock}, 30000),
                ok = gen_tcp:controlling_process(Sock, Pid),
                cast(Pid, active_for_relogin),
                exit(relogin)
            catch
                exit:relogin ->
                    exit(relogin);
                _T:_R ->
                    ?WARN("role pid:~p check duplicate ~p:~p", [Pid, _T, _R]),
                    do_login(Role, N + 1)
            end
    end.

%% 当更新socket时
on_set_socket(#role{connect_lost = true} = State) ->
    {ok, State};
on_set_socket(#role{sock = Sock, connect_lost = false} = State) ->
    try
        {ok, {Ip , Port}} = inet:peername(Sock),
        {ok, State#role{
            last_login_ip = util:ip_ntoa(Ip),
            client_port = Port
        }}
    catch
        _:_Reason ->
            notify_self_tcp_error(Sock, _Reason),
            {ok, State}
    end.

%% role_server自身的loop
on_self_loop(State) ->
    Now = util:now_ms(),
    % 数据清理检测
    case Now - get_last_time(flush) >= ?PACKET_MERGE_TIME of
        true ->
            do_flush_merge_msg(State),
            set_last_time(flush, Now);
        false ->
            ok
    end,
    % 流量检测
    case Now - get_last_time(traffic_check) >= util:rand(6000, 60000) of
        true ->
            ok = traffic_check(State),
            set_last_time(traffic_check, Now);
        false ->
            ok
    end,
    % 定时更新数据
    case Now - get_last_time(update_role) >= 1000 of
        true ->
            do_update_role(State, true),
            set_last_time(update_role, Now);
        false ->
            ok
    end,
    {ok, State}.

%% 获取上一次时间
get_last_time(Type) ->
    case erlang:get({last_loop_time, Type}) of
        undefined ->
            0;
        N ->
            N
    end.

%% 设置上一次时间
set_last_time(T, N) ->
    erlang:put({last_loop_time, T}, N),
    ok.

%%-------------
%% 关于消息
%%-------------

%% 发送重复登录消息
send_msg_login_again(State) ->
    Data = pack:pack_msg(?P_LOGIN , ?P_SERVER_REJECT , ?SERVER_REJECT_LOGIN_AGAIN),
    do_send_data(Data, State).

%%------------------
%% 关于玩家数据追踪
%%------------------

-define(TRACK_FILE_NAME(Id), "/track_" ++ ?N2S(Id) ++ ".log").
-define(TRACK_FILE_FD, "track_file_fd").
-define(TRACK, client_track).

%% 获取track文件fd
get_track_fd(Id) ->
    case erlang:get(?TRACK_FILE_FD) of
        undefined ->
            Filename = lists:append(game_path:run_log_dir(), ?TRACK_FILE_NAME(Id)),
            {ok, Fd} = file:open(Filename, [append, write ]),
            erlang:put(?TRACK_FILE_FD, Fd),
            {ok, Fd};
        Fd ->
            {ok, Fd}
    end.

%% 关闭track文件fd
close_track_fd() ->
    case erlang:get(?TRACK_FILE_FD) of
        undefined ->
            ok;
        Fd ->
            file:close(Fd)
    end.

%% 记录输出的数据记录
-ifdef(TEST).
do_track_out(undefined, _) ->
    ok;
do_track_out(Id, Data) ->
    Bin = iolist_to_binary(Data),
    Len = byte_size(Bin),
    <<_ResponLen:16, Mi:8, Fi:8, ResponBin/binary>> = Bin,
    {Respon, _} = pack:unpack(ResponBin),
    case {Mi, Fi} of
        {?P_MAP, ?P_MAP_MON_MOVE} ->
            ok;
        {10, 10} ->
            ok;
        {50,1} ->
            ?VERBOSE(?_U("*OUT* 向玩家:~p发送应答~p-~p 数据(长度:~p):~w"), [Id, Mi, Fi, Len, Respon]);
        _ ->
%%             ?VERBOSE(?_U("*OUT* 向玩家:~p发送应答~p-~p 数据(长度:~p):~w"), [Id, Mi, Fi, Len, Respon]),
            ok
    end,
    case is_be_track_out() of
        true ->
            do_track(Id, <<"OUT">>, Mi, Fi, Respon);
        _   ->
            ok
    end.
-else.
do_track_out(undefined, _Data) ->
    ok;
do_track_out(Rid, Data) ->
    case is_be_track_out() of
        true ->
            Bin = iolist_to_binary(Data),
            << _ResponLen:16 , Mi:8, Fi:8, ResponBin/binary >> = Bin,
            Module = ?HANDLER_NAME(Mi),
            do_track(Rid, <<"OUT">>, Module, Fi, pack:unpack(ResponBin));
        _   ->
            ok
    end.
-endif.

%% 记录网络接受的数据包
do_track_in(Rid, M, Fi, Record) ->
    case is_be_track_in() of
        true ->
            do_track(Rid, <<"IN">>, M, Fi, Record);
        _ ->
            ok
    end.
do_track(Rid, Type, Module, Fi, Record ) ->
    do_write_track_log(Rid, Type, Module, Fi, Record).

% 设置追踪标记
set_track(Flag) ->
    erlang:put(?TRACK, Flag),
    ok.

%% 是否追踪发送给服务器的数据包
is_be_track_in() ->
    case erlang:get(?TRACK) of
        undefined ->
            false;
        V ->
            (V band ?ROLE_TRACK_IN) =/= 0
    end.

%% 是否追踪发送给客户端的数据包
is_be_track_out() ->
    case erlang:get(?TRACK) of
        undefined ->
            false;
        V ->
            (V band ?ROLE_TRACK_OUT) =/= 0
    end.

%% 记录数据到文件
do_write_track_log(Rid, Type, Module, Fi, Record) ->
    {ok, Fd} = get_track_fd(Rid),
    Str = 
    io_lib:format(<<"~s ~s ~w ~w-~w: ~w\n">>, 
        [util:localtime_string(), Type, Rid, Module, Fi, Record]),
    ok = file:write(Fd, Str),
    ok.

%%------------------
%% 玩家数据同步
%%------------------
%% role同步cd
-define(ROLE_SYNC_TEMP_CD, '!role_sync_temp_cd').
%% 否则1秒钟更新一次
do_update_role(Role) ->
    do_update_role(Role, false).
do_update_role(#role{login_state = ?ROLE_LOGIN_FINISH} = Role, Must) ->
    Now = util:now_ms(),    
    case Must orelse temp_cd:valid(?ROLE_SYNC_TEMP_CD, 1000, Now) of
        true ->
            serv_role_mgr:update_role(Role),
            temp_cd:set(?ROLE_SYNC_TEMP_CD, Now);
        false ->
            ok
    end;
do_update_role(#role{}, _Must) ->
    ok.

%%-----------------
%% 合并发送网络数据
%%-----------------

%% 合并发送给玩家的数据(优化)
%% 如果距离上次发送小于一定时间则缓存
%% port_command消耗比较大(80us),减少网络发送次数,有好处.
do_send_msg_merge(Packet, Size, State) ->
    add_merge_msg(Packet, Size, State).

%% 发送缓存的消息
do_flush_merge_msg(State) ->
    case take_prepare_send_msg() of
        [] ->
            ok;
        Data ->
            %?DEBUG(?_U("被发送的数据(~p)...:~p"), [iolist_size(Data), Data]),
            do_send_data({merge, Data}, State)
    end.

%% 累积待发送数据
-define(MERGE_MSG, 'merge_msg').
%% 累积待发送数据长度
-define(MERGE_MSG_SIZE, 'merge_msg_size').

%% 获取待发送消息长度 
get_merge_msg_size() ->
    case erlang:get(?MERGE_MSG_SIZE) of
        undefined ->
            0;
        Size ->
            Size
    end.

%% 设置待发送消息长度
set_merge_msg_size(Size) ->
    erlang:put(?MERGE_MSG_SIZE, Size),
    ok.

%% 添加累积发送数据
add_merge_msg(Msg, MsgSize, State) ->
    PrevSize = get_merge_msg_size(),
    if
        MsgSize + PrevSize >= ?PACKET_MERGE_SIZE_MAX ->
            % 需要立刻发送数据
            do_flush_merge_msg(State),
            proc_list:add(?MERGE_MSG, Msg),
            set_merge_msg_size(MsgSize);
        true ->
            proc_list:add(?MERGE_MSG, Msg),
            set_merge_msg_size(PrevSize + MsgSize)
    end.

%% 获取并清理累积发送数据
take_prepare_send_msg() ->
    case proc_list:erase_list(?MERGE_MSG) of
        undefined ->
            [];
        Data ->
            lists:reverse(Data)
    end.

%% 关闭socket
%% from prim_inet.erl
close_socket(S) ->
    % avoid getting {'EXIT', S, Reason}
    unlink(S),  
    catch erlang:port_close(S),
    receive {tcp_closed, S} -> ok after 0 -> ok end.

%%---------------
%% 网络流量检测
%%---------------

%% 设置上一次流量检测数据
set_prev_traffic_data(Packet, Byte, Time) ->
    put(traffic_data, {Packet, Byte, Time}),
    ok.

%% 获取上一次流量检测数据
get_prev_traffic_data() ->
    case erlang:get(traffic_data) of
        undefined ->
            ?NONE;
        Data ->
            Data
    end.


%% 设置连续流量超标次数
set_traffic_over_count(N) ->
    erlang:put( traffic_over_count, N ),
    ok.

%% 获取连续流量超标次数
get_traffic_over_count() ->
    case erlang:get(traffic_over_count) of
        undefined ->
            0;
        N ->
            N
    end.

%% 网络包检测相关宏
-define(PACKET_PER_SEC_MAX, 30).    % 每秒收包最大数
-define(BYTE_PER_SEC_MAX,   1000).  % 每秒收包最大字节数
-define(TRAFFIC_OVER_MAX, 2).       % 流畅检测最大超标次数

%% 进行流量检测
traffic_check(Role) ->
    case catch do_traffic_check(Role) of
        ok ->
            ok;
        _Other ->
            ?WARN(?_U("流量检测出错:~p"), [_Other]),
            ok
    end.
do_traffic_check(?ROLE_LOGIN_POLICY) ->
    ok;
do_traffic_check(#role{sock = Sock, id = _RoleId}) ->
    case inet:getstat(Sock, [recv_cnt, recv_oct]) of
        {error, _Error} ->
            %?DEBUG(?_U("进行流量检测时,获取socket信息出错:~p"), [Error]),
            RecvPacket = 0, % 无用,只为去除编译警告
            RecvByte = 0,   % 无用,只为去除编译警告
            throw(ok);
        {ok, [{recv_cnt, RecvPacket}, {recv_oct, RecvByte}]} ->
            ok
    end,

    % 2
    case get_prev_traffic_data() of
        ?NONE ->
            % 2a
            ok;
        {PrevPacket, PrevByte, PrevTime} ->
            % 2b
            Packet = RecvPacket - PrevPacket,
            Byte = RecvByte - PrevByte,
            Time = util:now_sec() - PrevTime,
            PacketPS = round(Packet / Time),
            BytePS = round(Byte / Time),
            Count = get_traffic_over_count(),
            Overflow =
            if
                PacketPS > ?PACKET_PER_SEC_MAX ->
                    Code = ?E_TRAFFIC_PACKET_LIMIT,
                    true;
                BytePS > ?BYTE_PER_SEC_MAX ->
                    Code = ?E_TRAFFIC_BYTE_LIMIT,
                    true;
                true ->
                    Code = ?E_OK,
                    false
            end,
            Count2 = ?IF(Overflow, Count + 1, Count),
            case Count2 =:= ?TRAFFIC_OVER_MAX of
                true ->
                    ?WARN(?_U("玩家:~p每秒发包流量:~p包/s ~p字节/s"), [_RoleId, PacketPS, BytePS]),
                    notify_self_tcp_error(Sock, Code);
                false ->
                    ?IF(Overflow,
                        set_traffic_over_count(Count2),
                        set_traffic_over_count(0))
            end
    end,
    % 3
    set_prev_traffic_data(RecvPacket, RecvByte, util:now_sec()),
    ok.

%% 记录退出日志
do_log_logout(Role, TerminateR) ->
    Type =
    case TerminateR of
        ?CONNECT_LOST_EVENT ->
            ?LOG_LOGOUT_TYPE_NORMAL;
        waiting_active ->
            ?LOG_LOGOUT_TYPE_WATING_ACTIVE;
        {tcp_error, _} ->
            ?LOG_LOGOUT_TYPE_TCP_ERROR;
        relogin ->
            ?LOG_LOGOUT_TYPE_RELOGIN;
        _ when is_integer(TerminateR) ->
            TerminateR;
        _Other ->
            ?LOG_LOGOUT_TYPE_UNKNOWN
    end,
    game_log:logout(Role, Type).

%%---------------
%% 调试相关
%%---------------
%% 执行某个操作(测试使用)
do_apply(Fun, _Role) when is_function(Fun, 0) ->
    Fun();
do_apply(Fun, Role) when is_function(Fun, 1) ->
   Fun(Role).

%% 设置调试启动标记
set_start_for_debug() ->
    erlang:put(start_for_debug, true),
    ok.

%% 是否为调试而启动
is_start_for_debug() ->
    erlang:get(start_for_debug) =:= true.

%% 获取玩家信息(调试使用)
do_i(Role, all) ->
    %?DEBUG("******** role:~p", [Role]),
    gen_mod:i(Role);
do_i(_Role, counter) ->
    [{db, ?COUNTER_DB:i()},
     {ram, ?COUNTER_RAM:i()},
     {daily, ?COUNTER_DAILY:i()}];
do_i(Role, Mod) when is_atom(Mod) ->
    gen_mod:i(Role, Mod).

do_p(Info) ->
    Str = lists:flatten(gen_mod:p(Info)),
    io:format(
    ?_U("玩家信息~n~80..=s~n"
    "~ts"),
    ["=", Str]),
    ok.
