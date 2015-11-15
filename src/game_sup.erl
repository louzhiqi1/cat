%%%----------------------------------------------------------------------
%%%
%%% @date  2012.07.19
%%% @doc 游戏监督树
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(game_sup).
-include("common.hrl").
-behaviour(supervisor).

-export([
		 start_link/1 ,	
		 init/1 ,
		 stop/0
		]).

-define(SERVER, ?MODULE).

start_link( _Args ) ->
	{ok, Sup} = supervisor:start_link({local , ?MODULE} , ?MODULE, []),

    ok = serv_role_mgr:init(),

    ok = start_reloader(Sup),

    ok = gen_mod:init(),

    % ok = util:start_child(Sup, {wg_time_cache, {wg_time_cache, start_link, [fun() -> util:open_time() end]},
    %                             permanent, brutal_kill, worker, [wg_time_cache]}),

    ok = start_wg_stats(Sup), 

    CronFile = game_path:config_file("game.crontab"),

    OpenDaysFun = fun() -> util:open_days() end,

    ok = util:start_child(Sup, {wg_crontab_server, {wg_crontab_server, start_link, [CronFile, OpenDaysFun]},
                                permanent, brutal_kill, worker, [wg_crontab_server]}),

    ok = util:start_child(Sup, {serv_timer_engine, {serv_timer_engine, start_link, []},
                                permanent, brutal_kill, worker, [serv_timer_engine]}),

    ok = util:start_child(Sup, {essdb_sup, {db_sup, start_link, []},
                                permanent, brutal_kill, supervisor, [essdb_sup]}),

    
    
    {ok, Sup}.

stop() ->
	ok .

%% sup 初始化
init([]) ->
	{ok, {{one_for_one, 10, 10}, []}}.


%%--------------------
%% Internal API
%%--------------------
%% 启动reloader
-ifdef(TEST).
-define(RELOAD_INTERVAL, 5).    % 5秒
-else.
-define(RELOAD_INTERVAL, 0). 
-endif.

start_reloader(Sup) ->
    ?INFO(?_U("启动reloader服务"), []),
    Child =
    {reloader, {wg_reloader, start_link, [?RELOAD_INTERVAL]},
        permanent, brutal_kill, worker, [wg_reloader]},
    ok = util:start_child(Sup, Child),
    ok.

% %% 启动运维平台对应的httpd服务
% start_admin_httpd(Sup) ->
%     Ip = ?CONFIG(admin_ip, "127.0.0.1"),
%     Port = ?CONFIG(admin_port, 8888),
%     Opts = [{name, game_api_httpd}, {ip, Ip}, {port, Port}],
%     HandlerDirs = ["ebin/game"],
%     WWW = "", 
%     Child = {wg_httpd, {wg_httpd, start_link, [Opts, HandlerDirs, WWW]},
%                 permanent, brutal_kill, worker, [wg_httpd]},
%     util:start_child(Sup, Child).

%% 启动统计信息
%% 检测消息队列，内存长度，如果超过一定值会kill相关进程
%% 并创建文件*.dump文件
start_wg_stats(Sup) ->
    FunOnDump =
    fun() ->
        [] 
    end,
    ErrorLoggerDir = game_path:run_log_dir(),
    PidNameList = [<<"mapi_">>, <<"mapm_">>, <<"rid">>],
    PlanArgs = init:get_plain_arguments(),
    CheckErrorLogger = lists:member("start", PlanArgs),
    ?INFO(?_U("启动wg_stats")),
    util:start_child(Sup, {wg_stats, {wg_stats, start_link, [ErrorLoggerDir, PidNameList, FunOnDump, CheckErrorLogger]},
                                permanent, brutal_kill, worker, [wg_stats]}).
