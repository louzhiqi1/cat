%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date 2012-04-09
%%% @doc 游戏需要的配置文件
%%%
%%%----------------------------------------------------------------------
-module(game_config).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("common.hrl").

-export([init/0, reload/0]).
-export([get/1, get/2]).

%% 对应的配置文件mod
-define(GAME_CONF_MOD, game_conf_mod).

%% @doc 初始化加载game.conf数据
init() ->
    do_load().

%% @doc 重新加载
reload() ->
    do_load().

%% @doc 获取key对应的值
get(K) ->
    wg_config_dynamic:get(?GAME_CONF_MOD, K).

%% @doc 获取key如果没有这使用默认值
get(K, Def) ->
    wg_config_dynamic:get(?GAME_CONF_MOD, K, Def).

%%----------------------
%% internal API
%%----------------------

%% 加载
do_load() ->
    do_load_game_conf(),
    ok. 

%% 加载文件名称
do_load_game_conf() ->
    %?DEBUG("***env:~p", [os:getenv()]),
    RootDir = os:getenv("ROOT_DIR"),
    LogDir = os:getenv("GAME_LOG_DIR"),
    RunLogDir = os:getenv("RUN_LOG_DIR"),
    ConfigDir = os:getenv("GAME_CONFIG_DIR"),
    EbinDir = filename:join([RootDir, "ebin"]),
    DataDir = filename:join([RootDir, "data"]),

    GameConf = filename:join([ConfigDir, "game.conf"]),
    CommonConf = filename:join([ConfigDir, "common.conf"]),

    filelib:ensure_dir(LogDir ++ "/"),
    filelib:ensure_dir(RunLogDir ++ "/"),
    {ok, L} = file:consult(GameConf),
    {ok, LCommon} = file:consult(CommonConf),
    L2 = L ++ [{root_dir, RootDir}, 
          {ebin_dir, EbinDir},
          {log_dir, LogDir}, 
          {config_dir, ConfigDir},
          {data_dir, DataDir},
          {run_log_dir, RunLogDir} | LCommon],
    lager:info("load config:~p", [L2]),
    wg_config_dynamic:compile(?GAME_CONF_MOD, L2, 1, EbinDir).
