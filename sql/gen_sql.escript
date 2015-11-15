#!/usr/bin/env escript
%%----------------------------------------------------------------------
%% -*- erlang -*- 
%%! -smp disable 
%%
%% @author litaocheng
%% @date 2012-11-13
%% @doc 分析sql根据时间生成对应的erlang模块
%%
%%----------------------------------------------------------------------
-mode(compile).
-define(DB_GAME, (db:new(mysql_game_pool))).
-define(S2N(S), erlang:list_to_integer(S)).
-define(IF(C, T, F), case (C) of true -> (T); false -> (F) end).

%% 主函数
%% 1,分析Sql文件
%% 2,生成update/_db_sql_*.erl文件
main([WorkDir, SqlFile, DbVersion]) ->
    % 1
    set_update_dir(WorkDir),
    ok = parse_sql_file(SqlFile, DbVersion),
    ok;
main(_Other) ->
    do_error("用法错误! ~s 工作目录 sql文件 数据库版本", [escript:script_name()]).

%% 分析sql文件
%% 结果结果存储在进程字典中
parse_sql_file(SqlFile, DbVersion) ->
    case file:read_file(SqlFile) of
        {ok, Bin} ->
            Lines = binary:split(Bin, [<<"\n">>, <<"\r">>, <<"\r\n">>], [global, trim]),
            Lines2 = do_filter_space_lines(Lines),
            do_parse_lines(Lines2, ?S2N(DbVersion));
        _Other ->
            do_error("读取sql:~p出错:~p", [SqlFile, _Other])
    end.

%% 解析行
do_parse_lines([], _DbVersion) ->
    ok = do_save_db_erl(get_sql_date(), get_sql_content()),
    ok;
do_parse_lines([<<>> | T], DbVersion) ->
    do_parse_lines(T, DbVersion);
do_parse_lines([<<"--", _/bytes>> = Line | T], DbVersion) ->
    % 判断是否为日期,用来获取文件名
    case re:split(Line, "--\s*([0-9]{8})",[trim,{return,list}]) of
        [_] ->
            % 跳过
            do_parse_lines(T, DbVersion);
        [_, Date] ->
            % 为日期
            DateN = ?S2N(Date),
            DateCur = get_sql_date(),
            case DateN > DbVersion of
                true ->
                    case DateN =:= DateCur of
                        true ->
                            do_parse_lines(T, DbVersion);
                        false ->
                            % 需要保存内容
                            ok = do_save_db_erl(get_sql_date(), get_sql_content()),
                            set_sql_date(DateN),
                            clear_sql_content(),
                            do_parse_lines(T, DbVersion)
                    end;
                false ->
                    do_parse_lines(T, DbVersion)
            end
    end;
do_parse_lines([<<"/*", _/bytes>> | T], DbVersion) ->
    do_parse_lines(T, DbVersion);
do_parse_lines([<<"*", _/bytes>> | T], DbVersion) ->
    do_parse_lines(T, DbVersion);
do_parse_lines([<<"*/", _/bytes>> | T], DbVersion) ->
    do_parse_lines(T, DbVersion);
do_parse_lines([Line | T], DbVersion) ->
    add_sql_content(escape(Line)),
    do_parse_lines(T, DbVersion).
    
%% 添加内容
-define(SQL_CONTENT, sql_content).
add_sql_content(T) ->
    %io:format("*****add content:~s\n", [T]),
    L2 =
    case erlang:get(?SQL_CONTENT) of
        undefined ->
            [T];
        L ->
            [T | L]
    end,
    erlang:put(?SQL_CONTENT, L2),
    ok.

%% 清空内容
clear_sql_content() ->
    erlang:erase(?SQL_CONTENT),
    ok.

%% 获取内容
get_sql_content() ->
    case erlang:get(?SQL_CONTENT) of
        undefined ->
            [];
        L ->
            lists:reverse(L)
    end.

%% 设置sql对应时间
-define(SQL_DATE, sql_date).
set_sql_date(Date) ->
    erlang:put(?SQL_DATE, Date),
    ok.

%% 获取sql对应时间
get_sql_date() ->
    case erlang:get(?SQL_DATE) of
        undefined ->
            0;
        V ->
            V
    end.

%% 生成_db_*.erl文件
do_save_db_erl(0, _Content) ->
    ok;
do_save_db_erl(Date, Content) ->
    % 拆分成多个sql语句
    ContentBin = iolist_to_binary(Content),
    SqlLines = binary:split(ContentBin, [<<";">>], [global]),
    SqlLines2 = [[$", Line, $"] || Line <- SqlLines, Line =/= <<" ">>, Line =/= <<>>],
    %io:format("****date:~p\nlines:~s\n", [Date, SqlLines2]),
    Str1 = 
    io_lib:format(
"-module('_db_sql_~b').
-export([do/0]).

do() ->
    {sql_list, [
", [Date]),
    Str2 = str_join(SqlLines2, ",\n"),
    Str3 = "]}.\n",
    FileName = lists:concat(["_db_sql_", Date, ".erl"]),
    FilePath = filename:join([get_update_dir(), FileName]),
    ok = file:write_file(FilePath, [Str1, Str2, Str3]).

%% 设置update目录
-define(UPDATE_DIR, update_dir).
set_update_dir(Dir) ->
    erlang:put(?UPDATE_DIR, Dir),
    ok.

%% 获取update目录
get_update_dir() ->
    erlang:get(?UPDATE_DIR).

%% 添加分割
str_join([], _S) ->
    [];
str_join([H|T], S) ->
    [H] ++ [[S, E] || E <- T].

%% 对"进行转义
escape(String) when is_list(String) ->
    lists:reverse(escape(String, [])); %% 34 is $"
escape(Bin) when is_binary(Bin) ->
    list_to_binary(escape(binary_to_list(Bin))).

escape([], Acc) ->
    Acc;
escape([34 | Rest], Acc) ->      %% 34 is $"
    escape(Rest, [34, $\\ | Acc]);   %% 34 is $"
escape([C | Rest], Acc) ->
    escape(Rest, [C | Acc]).

%% 错误
do_error(F, D) ->
    do_error(F, D, 1).
do_error(F, D, Code) ->
    io:format("错误:" ++ F ++ "\n", D),
    exit(Code).

% 过滤空行
do_filter_space_lines(Lines) ->
    lists:foldr(
    fun(L, Acc) ->
        S = binary_to_list(L),
        S2 = string:strip(S),
        ?IF(S2 =:= "", Acc,
            [list_to_binary(S2) | Acc])
    end, [], Lines).
