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
    
        CronFile = game_path:config_file("game.crontab"),

        role_sup:start(),
    
        ok = util:start_child(Sup, {wg_cron_server, {wg_cron_server, start_link, [CronFile]},
                                    permanent, brutal_kill, worker, [wg_cron_server]}),
    
        ok = util:start_child(Sup, {serv_timer_engine, {serv_timer_engine, start_link, []},
                                    permanent, brutal_kill, worker, [serv_timer_engine]}),
    
        ok = util:start_child(Sup, {essdb_sup, {db_sup, start_link, []},
                                    permanent, brutal_kill, supervisor, [essdb_sup]}),
    
        % ok = util:start_child(Sup, {role_sup, {ranch, start_listener, [role_sup, 10, ranch_tcp, [{port, ?CONFIG(gate_port)} |?TCP_OPTS], role_server, []]},
        %                             permanent, brutal_kill, supervisor, [role_sup]}),

        
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
        Child =
        {reloader, {wg_reloader, start_link, [?RELOAD_INTERVAL]},
            permanent, brutal_kill, worker, [wg_reloader]},
        ok = util:start_child(Sup, Child),
        ok.