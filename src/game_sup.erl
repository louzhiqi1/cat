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

-define(TCP_OPTS,
    [binary, 
    {active, false}, 
    {reuseaddr, true}, 
    {delay_send, true}, 
    {nodelay, true}, 
    {send_timeout, 8000}
    ]).

start_link( _Args ) ->
	{ok, Sup} = supervisor:start_link({local , ?MODULE} , ?MODULE, []),

    ok = serv_role_mgr:init(),

    ok = start_reloader(Sup),

    ok = gen_mod:init(),

    ok = start_wg_stats(Sup), 

    {ok, _Pid} = start_httpd(),

    CronFile = game_path:config_file("game.crontab"),

    ok = util:start_child(Sup, {wg_cron_server, {wg_cron_server, start_link, [CronFile]},
                                permanent, brutal_kill, worker, [wg_cron_server]}),

    ok = util:start_child(Sup, {serv_timer_engine, {serv_timer_engine, start_link, []},
                                permanent, brutal_kill, worker, [serv_timer_engine]}),

    ok = util:start_child(Sup, {essdb_sup, {db_sup, start_link, []},
                                permanent, brutal_kill, supervisor, [essdb_sup]}),

    ok = util:start_child(Sup, {role_sup, {ranch, start_listener, [role_sup, 10, ranch_tcp, [{port,9001} |?TCP_OPTS], role_server, []]},
                                permanent, brutal_kill, supervisor, [role_sup]}),

    Dispatch = cowboy_router:compile([
        {'_', [{"/", web_handler, []}]}
    ]),

    ok = util:start_child(Sup, {http_server, {cowboy, start_http, [http_server, 10, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]]}, 
                                permanent, brutal_kill, supervisor, [http_server]}),

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
    lager:error("reloader start"),
    Child =
    {reloader, {wg_reloader, start_link, [?RELOAD_INTERVAL]},
        permanent, brutal_kill, worker, [wg_reloader]},
    ok = util:start_child(Sup, Child),
    ok.


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


start_httpd() ->
    web_server_sup:start_link().