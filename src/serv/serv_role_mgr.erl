%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2012.06.12
%%% @doc 游戏中所有在线玩家的id, name, pid映射关系
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(serv_role_mgr).
-author("litaocheng@gmail.com").
-vsn('0.1').

-include("common.hrl").
-include("role.hrl").

-export([i/0]).
-export([init/0, list/0, id_list/0, pid_list/0, count/0, role_list/0]).
-export([add/1, update_role/1, delete/1, update_name/2]).
-export([get/1, get_name_by_id/1, get_id_by_name/1, get_pid_by_id/1]).
-export([send_all/2, send_all_exclude/3]).
-export([save_id_list/0]).

-record(role_online, {
        id,         % 玩家id
        name,       % 玩家名字
        pid         % 进程
        }).

%% 数据库名称
-define(TABLE_ID, role_online_id).
-define(TABLE_NAME, role_online_name2id).
%% 保存在线玩家的列表,id列表，pid列表
-define(TABLE_LIST, role_online_list).
-define(TABLE_ROLE, role_online).

%% @doc 获取运行信息
i() ->
    [{role_count, count()},
    {role_list, id_list()},
    {list, ets:tab2list(?TABLE_LIST)}
    ].
    
%% @doc 初始化数据库
init() ->
    ok = create_table(),
    true = ets:insert(?TABLE_LIST, {id, []}),
    true = ets:insert(?TABLE_LIST, {pid, []}),
    ok.

%% @doc 获取在线列表
list() ->
     ets:tab2list(?TABLE_ID).

%% @doc 获取在线玩家role列表
role_list() ->
    ets:tab2list(?TABLE_ROLE).

%% @doc 获取在线玩家id列表
id_list() ->
    ets:lookup_element(?TABLE_LIST, id, 2).

%% @doc 获取在线玩家pid列表
pid_list() ->
    ets:lookup_element(?TABLE_LIST, pid, 2).

%% @doc 总数
count() ->
    ets:info(?TABLE_ID, size).

%% @doc 玩家上线,添加到数据库
add(#role{id = Id, name = Name, pid = Pid} = Role) 
        when is_integer(Id), is_binary(Name), is_pid(Pid) ->
    %?DEBUG("add user:~p", [Id]),
    true = ets:insert(?TABLE_ID, #role_online{id = Id, name = Name, pid = Pid}),
    true = ets:insert(?TABLE_NAME, {Name, Id}),
    true = ets:insert(?TABLE_LIST, {id, [Id | id_list()]}),
    true = ets:insert(?TABLE_LIST, {pid, [Pid | pid_list()]}),
    true = ets:insert(?TABLE_ROLE, Role),
    ok.

%% @doc 更新数据
update_role(#role{} = Role) ->
    true = ets:insert(?TABLE_ROLE, Role),
    ok.

%% @doc 更新玩家名字
update_name(#role{name = OldName, id = Id}, NewName) ->
	true = ets:delete(?TABLE_NAME, OldName),
    true = ets:insert(?TABLE_NAME, {NewName, Id}),
    ok.

%% @doc 玩家离线则删除
delete(RId) when is_integer(RId) ->
    case ets:lookup(?TABLE_ID, RId) of
        [#role_online{name =  PlayerName, pid = Pid}] ->
            true = ets:delete(?TABLE_ID, RId),
            true = ets:delete(?TABLE_NAME, PlayerName),
            true = ets:insert(?TABLE_LIST, {id, lists:delete(RId, id_list())}),
            true = ets:insert(?TABLE_LIST, {pid, lists:delete(Pid, pid_list())}),
            true = ets:delete(?TABLE_ROLE, RId),
            ok;
        [] ->
            ok
    end.

%% @doc 获取role信息
get(Id) ->
    case ets:lookup(?TABLE_ROLE, Id) of
        [Role] ->
            Role;
        [] ->
            ?NONE
    end.

%% @doc 根据id获取名字
get_name_by_id(Id) ->
    case catch ets:lookup_element(?TABLE_ID, Id, #role_online.name) of
        {'EXIT', {badarg, _}} ->
            none;
        Name ->
            Name
    end.

%% @doc 根据名字获取id
get_id_by_name(Name) when is_binary(Name) ->
    case ets:lookup(?TABLE_NAME, Name) of
        [{_, Id}] ->
            Id;
        [] ->
            ?NONE
    end.

%% @doc 根据id获取pid
get_pid_by_id(Id) ->
    case catch ets:lookup_element(?TABLE_ID, Id, #role_online.pid) of
        {'EXIT', {badarg, _}} ->
            none;
        Pid ->
            Pid
    end.

%% @doc 将数据发送给所有玩家
send_all(SendType, Data) ->
    [role_server:SendType(Pid, Data) || Pid <- pid_list()],
    ok.

%% @doc 将数据发送给所有玩家, 除去Exclude
send_all_exclude(PidList, SendType, Data) ->
	AllPid = exclude_pid_list(PidList, pid_list()),
	lager:debug("过滤后的Pid列表~p", [AllPid]),
    [role_server:SendType(Pid, Data) || Pid <- AllPid],
    ok.

%% 过滤Exclude
exclude_pid_list([], AllPid) ->
	AllPid;
exclude_pid_list([Pid | T], AllPid) ->
	AllPid2 = lists:delete(Pid, AllPid),
	exclude_pid_list(T, AllPid2).

%% 保存在线玩家id列表
save_id_list() ->
    %?DEBUG("current ids:~p~n", [id_list()]),
    IdList = id_list(),
    Str = io_lib:format("~w", [IdList]),
    Filename = game_path:log_file("last_online_ids"),
    file:write_file(Filename, Str).

%%-------------
%% internal API
%%-------------

%% 创建表
create_table() ->
    ?TABLE_ID = ets:new(?TABLE_ID, [named_table, set, public,
                    {keypos, #role_online.id}, {read_concurrency, true}]),
    ?TABLE_NAME = ets:new(?TABLE_NAME, [named_table, set, public,
                    {keypos, 1}, {read_concurrency, true}]),
    ?TABLE_LIST = ets:new(?TABLE_LIST, [named_table, set, public,
                    {keypos, 1}, {read_concurrency, true}]),
    ?TABLE_ROLE = ets:new(?TABLE_ROLE, [named_table, set, public,
                    {keypos, #role.id}, {read_concurrency, true}]),
    ok.

%%-------------
%% EUNIT test
%%-------------
-ifdef(EUNIT).

r_1() ->
    #role{id = 1, name = <<"name1">>, pid = self()}.

m_1() ->
    #role_online{id = 1, name = <<"name1">>, pid = self()}.

r_2() ->
    #role{id = 2, name = <<"name2">>, pid = self()}.

m_2() ->
    #role_online{id = 2, name = <<"name2">>, pid = self()}.

full_test_() ->
    {spawn, {setup,
    fun() -> init() end,
    fun(_) -> ok end,
    [
        ?_assertEqual([], list()),
        ?_assertEqual(0, count()),

        % 添加
        ?_assertEqual(ok, add(r_1())),
        ?_assertEqual(ok, add(r_2())),
        ?_assertEqual([m_1(), m_2()], lists:sort(list())),
        ?_assertEqual(2, count()),

        % 删除
        ?_assertEqual(ok, delete(1)),
        ?_assertEqual([m_2()], lists:sort(list())),
        ?_assertEqual(1, count()),
        ?_assertEqual(ok, delete(2)),
        ?_assertEqual([], list()),
        ?_assertEqual(0, count())
    ]}}.

-endif.
