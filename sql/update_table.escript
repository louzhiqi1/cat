#!/usr/bin/env escript
%%----------------------------------------------------------------------
%% -*- erlang -*- 
%%! -smp disable 
%%
%% @author litaocheng
%% @date 2012-11-13
%% @doc 用来更新数据表
%%
%%----------------------------------------------------------------------
-mode(compile).
-define(DB_GAME, (db:new(mysql_game_pool))).
%% debug
-define(DEBUG(F, D), io:format(F ++ "\n", D)).
-define(S2N(S), erlang:list_to_integer(S)).

%% 主函数
%% 1,扫描目录获取要执行的脚本
%% 2,建立数据库连接
%% 3,获取所有数据,顺序执行更新操作
%% 4,更新db_version
main([RootDir, WorkDir, DbUser, DbPass, DbName, DbVersionStart]) ->
    set_root_dir(RootDir),
    code:add_paths(ebin_dirs()),
    code:add_path(WorkDir),
    wg_loglevel:set(5),
    % 1
    UpdateMods = find_update_mods(WorkDir, DbVersionStart),
    ?DEBUG("更新模块~p", [UpdateMods]),
    % 2
    ok = setup_db_conn(DbUser, DbPass, DbName),
    % 3
    lists:foreach(
    fun(Mod) ->
        case catch Mod:do() of
            {sql_list, List} ->
                do_sql(Mod, List);
            List when is_list(List) ->
                [do_trans_with_table(Mod, Sql, Fun) || {Sql, Fun} <- List];
            {Sql, Fun} when is_list(Sql), is_function(Fun, 1) ->
                % 数据转换
                do_trans_with_table(Mod, Sql, Fun);
            _Other ->
                do_error("~p:do()返回值错误!~p", [Mod, _Other])
        end
    end, UpdateMods),
    % 4
    do_update_version(UpdateMods),
    ok;
main(_) ->
    do_error("用法错误! ~s 根目录 数据库用户 数据库密码 数据库 版本", [escript:script_name()]).

%% 与数据库连接
setup_db_conn(User, Pass, Name)->
    Server = "127.0.0.1",
    Port = 3306,
    case (mysql:new(mysql_game_pool)):start_link(mysql_game_pool, Server, Port, User, Pass, Name, 
        fun(_,_,_,_) -> ok end, utf8) of
        {ok, _} ->
            io:format("与数据库连接成功~n"),
            ok; 
        {error, {already_started, _}} ->
            ok; 
        {error, _Reason} ->
            io:format("连接数据库失败:~p!", [_Reason]),
            throw({error,1})
    end.

%% 扫描获得更新模块
%% _db_tab_YYYYMMDD.erl
%% _db_sql_YYYYMMDD.erl
find_update_mods(WorkDir, DbVersion) ->
    CompileOpts = [{i, include_dir()}, {outdir, WorkDir}, warnings_as_errors],
    %?DEBUG("*****dir:~p", [WorkDir]),
    L = filelib:wildcard(filename:join([WorkDir, "_db_*.erl"])),
    case make:files(L, CompileOpts) of
        up_to_date ->
            ok;
        _Other ->
            do_error("编译模块出错:~p", [_Other])
    end,
    DbVersionN = list_to_integer(DbVersion),
    Mods =
    lists:foldl(
    fun(File, Acc) ->
        Mod = filename:basename(File, ".erl"),
        Date = string:sub_string(Mod, 9),
        case list_to_integer(Date) > DbVersionN of
            true ->
                [list_to_atom(Mod) | Acc];
            false ->
                Acc
        end
    end, [], L),

    % 根据DbVersion排序
    lists:sort(
    fun(ModA, ModB) ->
        ModStrA = atom_to_list(ModA),
        ModStrB = atom_to_list(ModB),
        DateA = ?S2N(string:sub_string(ModStrA, 9)),
        TypeA = string:sub_string(ModStrA, 6, 9),
        DateB = ?S2N(string:sub_string(ModStrB, 9)),
        %TypeB = string:sub_string(ModStrB, 6, 9),
        if
            DateA =:= DateB ->
                TypeA =:= "sql";
            DateA < DateB ->
                true;
            DateA > DateB ->
                false
        end
    end, Mods).

%% 执行sql语句
do_sql(Mod, SqlList) ->
    do_sql(Mod, SqlList, true).
do_sql(Mod, SqlList, Print) ->
    [begin
        case Print of
            true ->
                io:format("~s 执行语句: ~s\n", [Mod, iolist_to_binary(Sql)]);
            false ->
                ok
        end,
        %io:format("****************sql:~p", [Sql]),
        case ?DB_GAME:fetch(Sql) of
            {updated, _} ->
                ok;
            {error, _Other} ->
                do_error("执行~p模块sql 失败:~p", [Mod, _Other])
       end
    end || Sql <- SqlList],
    ok.

%% 执行表的转化工作
do_trans_with_table(Mod, Sql, Fun) ->
    io:format("~p开始更新表\n", [Mod]),
    case ?DB_GAME:fetch(Sql) of
        {selected, _Fields, Rows} ->
            N = length(Rows),
            io:format("需要更新~p条记录\n", [N]),
            lists:foldl(
            fun(R, I) ->
                Sql2 = 
                try Fun(R)
                catch
                    _T:_R ->
                        do_error("更新数据出错 ~p:~p", [_T, _R])
                end,
                case Sql2 of
                    skip ->
                        I;
                    _ ->
                        do_sql(Mod, [Sql2], false),
                        case (I rem 100) =:= 0 of
                            true ->
                                io:format("~p\n", [I]);
                            false ->
                                ok
                        end,
                        I + 1
                end
            end, 1, Rows),
            io:format("~p更新完成\n", [Mod]),
            ok;
        {error, _Other} ->
            do_error("更新表，读取数据:~p 失败:~p", [Sql, _Other])
   end.

%% 更新db_version
do_update_version([]) ->
    ok;
do_update_version(Mods) ->
    % 获取最后一个版本
    LastMod = lists:last(Mods),
    DbVersion = ?S2N(string:sub_string(atom_to_list(LastMod), 9)),
    case ?DB_GAME:insert("db_version", [DbVersion]) of
        {updated, _} ->
            ok;
        _Other ->
            do_error("更新db_version出错:~p", [_Other])
    end.


%% 设置root路径
-define(ROOT_DIR, root_dir).
set_root_dir(RootDir) ->
    erlang:put(?ROOT_DIR, RootDir),
    ok.
    
%% 获取root路径
root_dir() ->
    erlang:get(?ROOT_DIR).

%% ebin目录
ebin_dirs() ->
    [
    filename:join([root_dir(), "ebin", "game"]),
    filename:join([root_dir(), "ebin", "base"])
    ].

%% include路径
include_dir() ->
    filename:join([root_dir(), "include"]).

%% 错误
do_error(F, D) ->
    do_error(F, D, 1).
do_error(F, D, Code) ->
    io:format("错误:" ++ F ++ "\n", D),
    exit(Code).
