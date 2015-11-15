%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date 2011-01-11
%%% @doc 关于游戏中的文件路径
%%%
%%%----------------------------------------------------------------------
-module(game_path).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("common.hrl").

-export([root_dir/0, ebin_dir/0]).
-export([data_dir/0, data_file/1]).
-export([config_dir/0, config_file/1]).
-export([log_dir/0, log_file/1]).
-export([run_log_dir/0, run_log_file/1]).

%% @doc 游戏的根路径
root_dir() ->
    ?CONFIG(root_dir).

%% @doc 游戏的ebin路径
ebin_dir() ->
    ?CONFIG(ebin_dir).

%% @doc 获取数据文件路径
data_dir() ->
    ?CONFIG(data_dir).

%% @doc 获取数据文件路径
data_file(FileName) when is_list(FileName) ->
    case FileName of
        [] ->
            data_dir();
        [H|_] when is_list(H) ->
            filename:join([data_dir() | FileName]);
        [_|_] ->
            filename:join([data_dir(), FileName])
    end.

%% @doc 获取配置文件路径
config_dir() ->
    ?CONFIG(config_dir).

%% @doc 获取配置文件全路径
config_file(FileName) when is_list(FileName) ->
    case FileName of
        [] ->
            config_dir();
        [H|_] when is_list(H) ->
            filename:join([config_dir() | FileName]);
        [_|_] ->
            filename:join([config_dir(), FileName])
    end.

%% @doc 获取log路径
log_dir() ->
    ?CONFIG(log_dir).

%% @doc 获取日志文件路径
log_file(FileName) when is_list(FileName) ->
    case FileName of
        [] ->
            log_dir();
        [H|_] when is_list(H) ->
            filename:join([log_dir() | FileName]);
        [_|_] ->
            filename:join([log_dir(), FileName])
    end.


%% @doc 获取run log路径
run_log_dir() ->
    ?CONFIG(run_log_dir).

%% @doc 获取日志文件路径
run_log_file(FileName) when is_list(FileName) ->
    case FileName of
        [] ->
            run_log_dir();
        [H|_] when is_list(H) ->
            filename:join([run_log_dir() | FileName]);
        [_|_] ->
            filename:join([run_log_dir(), FileName])
    end.

%%------------------
%% internal API
%%------------------

%%------------------
%% Eunit TEST
%%------------------
-ifdef(EUNIT).

    
-endif.
