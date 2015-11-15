%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2012.07.04
%%% @doc timer驱动器，一个timer驱动多个进程
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(serv_timer_engine).
-include("common.hrl").
-behaviour(gen_server).

-export([start_link/0, i/0, get_state/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% 名字
-define(SERVER, ?MODULE).

%%---------
%% 对外接口
%%---------

%% @doc 启动
start_link()->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc 获取运行信息
i() ->
    [].

%% @doc 获取内部状态
get_state() ->
    call(get_state).

%%----------------------
%% gen_server callbacks
%%----------------------
init(_Type) ->
    erlang:process_flag(trap_exit, true),
    erlang:process_flag(priority, high),
    ok = do_start_timer(),
    {ok, []}.

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(?TIMEOUT_EVENT, State) ->
    % 玩家timer
    [role_server:notify_timeout_event(Pid) || Pid <- serv_role_mgr:pid_list()],
    ok = do_start_timer(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?DEBUG(?_U("进程停止:~p"), [_Reason]),
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%%--------------
%% internal API
%%--------------

%% call调用
call(Req) ->
    gen_server:call(?SERVER, Req, ?GEN_SERVER_TIMEOUT).

%% 启动timer
do_start_timer() ->
    erlang:send_after(500, self(), ?TIMEOUT_EVENT),
    ok.
