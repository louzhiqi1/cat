%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2012.07.02
%%% @doc   gen_mod behaviour
%%%     所有的mod都运行于玩家进程，用来完成游戏各种子系统
%%% 流程如下:
%%%     Module:init_role
%%%     Module:handle_c2s
%%%     Module:handle_timeout
%%%     Module:handle_s2s_call
%%%     Module:handle_s2s_cast
%%%     Module:terminate_role
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(gen_mod).
-include("common.hrl").
-include("role.hrl").

%% 外部接口
-export([init/0, i/0, i/1, i/2, p/1, 
        init_role/1, terminate_role/1, clear_daily/1]).
-export([module_list/0, on_event/2, on_event/3]).

%% 关于数据同步
-export([start_data_sync/3, handle_timeout/2, get_last_data_sync_time/1]).

-ifdef(TEST).
-compile([export_all]).
-endif.

%%------------------
%% behaviour 信息
%%------------------
%% 获取相关信息
-callback i(Role :: #role{}) -> term().
%% 相关信息字符串描述
-callback p(term()) -> term().
%% 本模块的初始化
%% 如果SyncTime为0则表示下线保存数据,单位为分钟
%% 如果Index为0则表示最高次序。如果想要模块优先启动，则要设置一个较小的数
-callback init() -> ok 
    | {ok, Index :: integer()}
    | {ok, SyncTime :: integer(), SyncFun :: tuple()}
    | {ok, Index :: integer(), SyncTime :: integer(), SyncFun:: tuple()}.
%% 模块关心的事件:
%%  on_upgrade
%%      - 当玩家升级时调用, LvlAdd为升的级数
%%      - Mod:on_upgrade(Role, LvlAdd)
-callback register_event() -> [Event :: any()].
%% 玩家初始化mod
-callback init_role(Role :: #role{}) -> #role{} | {ok, #role{}}.
%% 玩家停止mod
-callback terminate_role(Role :: #role{}) -> term().
%% 每日数据清理
-callback clear_daily(Role :: #role{}) -> any().
%% 处理c2s请求
-callback handle_c2s(MsgId :: integer(), Req :: term(), Role :: #role{}) ->
    ok |
    {ok, #role{}} |
    {respon, term(), #role{}}.
%% 处理timer
-callback handle_timeout(Event :: term(), Role :: #role{}) ->
    {ok, #role{}}.
%% 处理s2s call请求
-callback handle_s2s_call(Req :: term(), Role :: #role{}) ->
    ok |
    {ok, #role{}} | 
    {ok, term(), #role{}} |
    {respon, term(), #role{}}.
%% 处理s2s cast请求
-callback handle_s2s_cast(Req :: term(), Role :: #role{}) ->
    ok |
    {ok, #role{}} |
    {respon, term(), #role{}}.

%% @doc 初始化gen_mod
init() ->
    IndexDef = 99999,
    L = 
    [begin
        Mod = list_to_atom(filename:basename(File, ".beam")),
        %?DEBUG("***********role mod:~p start", [Mod]),
        try
            case Mod:init() of
                ok ->
                    {Mod, IndexDef, 0, none};
                {ok, Index} when is_integer(Index) ->
                    {Mod, Index, 0, none};
                {ok, SyncTime, SyncFun} when is_tuple(SyncFun), 
                                is_integer(SyncTime), SyncTime >= 0 ->
                    {Mod, IndexDef, SyncTime * 60000, SyncFun};
                {ok, Index, SyncTime, SyncFun} ->
                    {Mod, Index, SyncTime * 60000, SyncFun}
            end
        catch
            T:R ->
                lager:error("module:~p init failed ~p:~p", [Mod, T, R]),
                exit(normal)
        end
    end || File <- filelib:wildcard(?CODE_PATH ++ "/mod_*.beam")],
    % 按照Index排序
    L2 =
    lists:sort(
    fun({_, IndexA, _, _}, {_, IndexB, _, _}) ->
        IndexA =< IndexB
    end, L),
    % 编译模块列表
    ok = wg_config_dynamic:compile(gen_mod_module_list, L2, 1, ""),
    do_compile_event(L2),
    ok.

%% @doc 玩家执行各个mod初始化
init_role(Role) ->
    lists:foldl(
    fun({Mod, _, SyncTime, SyncFun}, Acc) ->
        try 
            Acc3 = 
            case Mod:init_role(Acc) of
                {ok, Acc2} ->
                    Acc2;
                #role{} = Acc2 ->
                    Acc2
            end,
            ?IF(SyncTime =/= 0, 
                start_data_sync(Mod, SyncTime, SyncFun), ok),
            Acc3
        catch
            T:R ->
                lager:error("player:~p module:~p init_role failed ~p:~p", 
                    [Role#role.id, Mod, T, R]),
                erlang:raise(T, R, erlang:get_stacktrace())
        end 
    end, Role, module_list()).

%% @doc 玩家执行各个模块结束
terminate_role(Role) ->
    % 结束与初始化顺序相反
    lists:foldl(
    fun({Mod, _, _, SyncFun}, Acc) ->
        try
            %?DEBUG("******* mod~p terminate", [Mod]),
            Acc3 = 
            case Mod:terminate_role(Acc) of
                ok ->
                    Acc;
                {ok, #role{} = Acc2} ->
                    Acc2;
                #role{} = Acc2 ->
                    Acc2
            end,
            case SyncFun of
                ?NONE ->
                    ok;
                _ when is_list(SyncFun) ->
                    [do_call_sync_fun(Fun, Acc3) || Fun <- SyncFun],
                    ok;
               _ ->
                    do_call_sync_fun(SyncFun, Acc3)
            end,
            Acc3
        catch
            T:R ->
                lager:error("player:~pmodule:~pterminate error ~p:~p",
                    [Role#role.id, Mod, T, R]),
                erlang:raise(T, R, erlang:get_stacktrace())
        end
    end, Role, lists:reverse(module_list())).

%% @doc 玩家执行各个模块每日清理
clear_daily(Role) ->
    % 与初始化顺序相反
    lists:foreach(
    fun({Mod, _, _, _SyncFun}) ->
        try
            Mod:clear_daily(Role)
        catch
            T:R ->
                lager:error("player:~pmodule~pclear_daily error ~p:~p",
                    [Role#role.id, Mod, T, R]),
                erlang:raise(T, R, erlang:get_stacktrace())
        end
    end, lists:reverse(module_list())).

%% @doc gen_mon本身信息
i() ->
    module_list().

%% @doc info
i(Role) ->
    [{Mod, i(Role, Mod)} || {Mod, _, _, _} <- module_list()].

%% @doc info
i(Role, Mod) ->
    case catch Mod:i(Role) of
        {'EXIT', _Other} ->
            lager:error("module:~p i info error:~p", [Mod, _Other]),
            ?NONE;
        Val ->
            %?DEBUG(?_U("模块:~p i信息:~p"), [Mod, Val]),
            Val
    end.

%% @doc 信息字符串
p(Info) ->
    [begin
        Val = ?PLIST_VAL(Mod, Info),
        ?ASSERT(Val =/= undefined),
        Ret = Mod:p(Val),
        case is_list(Ret) of
            true ->
                Ret;
            false ->
                lager:error("~w:p/1 return error(not string)!", [Mod]),
                ""
        end
    end || {Mod, _, _, _} <- module_list()].

%% @doc 获取module list
module_list() ->
    gen_mod_module_list:list().

%% @doc 当事件触发时
%% 返回:
%%  any() | throw({error, Code}).
on_event(Role, Event) ->
    on_event(Role, Event, ?NONE).
on_event(Role, Event, Data) ->
    ArgCur = calc_event_arg(Role, Event),
    EventList = 
    case catch gen_mod_event_list:get(Event) of
        {'EXIT', _} ->
            [];
        L ->
            L
    end,

    lists:foldl(
    fun
        ({Mod, all}, Acc) ->
            do_call_event(Mod, Event, Acc, Data);
        ({Mod, Arg}, Acc) when Arg =:= ArgCur ->
            do_call_event(Mod, Event, Acc, Data);
        (_, Acc) ->
            Acc
    end, Role, EventList).

%%------------------------
%% internal API
%%------------------------

%% 编译模块事件
do_compile_event(ModList) ->
    L =
    [begin
        try
            case Mod:register_event() of
                [] ->
                    {Mod, []};
                all ->
                    {Mod, event_list()};
                [_|_] = EventList ->
                    EventList2 =
                    lists:foldl(
                        fun(E, Acc) ->
                            {Event, EventData} = 
                                case tuple_size(E) == 2 of
                                    true ->
                                        E;
                                    false ->
                                        {E, all}
                                end,

                            case lists:member(Event, event_type_list()) of
                                true ->
                                    [{Event, EventData} | Acc];
                                false ->
                                    lager:error("module:~pregister event error:~p", [Mod, Event]),
                                    exit(normal)
                            end
                    end, [], EventList),
                    {Mod, EventList2}
            end
        catch
            T:R ->
                lager:error("module:~p register event error ~p:~p", [Mod, T, R]),
                exit(normal)
        end
    end || {Mod, _, _, _} <- ModList],
    % 生成{Event, [{Mod, EventData}]}列表
    L2 =
    lists:append([[{Event, {Mod, EventData}} || {Event, EventData} <- ModEvent] || {Mod, ModEvent} <- L]),
    Dict =
    lists:foldl(
    fun({Event, {Mod, EventData}}, Acc) ->
        dict:append(Event, {Mod, EventData}, Acc)
    end, dict:new(), L2),
    L3 = dict:to_list(Dict),
    ok = wg_config_dynamic:compile(gen_mod_event_list, L3, 1, ""),
    ok.

%% 事件列表
event_type_list() ->
    [T || {T, _} <- event_list()].

%% 事件列表
%% [{事件, 触发参数}]
event_list() ->
    [
	{on_upgrade, all}].

%% 调用事件函数
do_call_event(Mod, Event, Role, Data) ->
    try
        Ret =
        case Data of
            ?NONE ->
                Mod:Event(Role);
            _ ->
                Mod:Event(Role, Data)
        end,
        case Ret of
            ok ->
                Role;
            {ok, #role{} = Role2} ->
                Role2;
            #role{} = Role2 ->
                Role2;
            _Other ->
                lager:error("module:~pcall event ~p unknown return value:~p", [Mod, Event, _Other]),
                Role
        end
    catch
        _T:_R ->
            lager:error("module:~pcall event:~perror ~p:~p", [Mod, Event, _T, _R]),
            Role
    end.

%% 升级事件没参数
calc_event_arg(#role{}, on_upgrade) ->
	?NONE.
	

%%------------------
%% 关于数据定期同步
%%------------------

%% @doc 启动数据同步
%% SyncFun:可以为一个函数,或者函数列表
start_data_sync(Type, Time, SyncFun) ->
    %?DEBUG(?_U("********启动数据同步timer:~p time:~p"), [Type, Time]),
    ?ASSERT(is_list(SyncFun) or is_tuple(SyncFun)),
    Ref = make_ref(),
    proc_timer:start_timer(?MODULE, Time, {?DATA_SYNC_EVENT, Ref}),
    erlang:put({'$data_sync_fun', Type}, SyncFun),
    erlang:put(Ref, {Type, Time}),
    ok.

%% @doc 处理timeout
%% 1,执行同步函数
%% 2,设置更新时间
%% 3,重启sync timer
handle_timeout({?DATA_SYNC_EVENT, Ref}, State) ->
    {Type, Time} = erlang:get(Ref),
    %?DEBUG(?_U("********收到数据:~p同步,time:~p"), [Type, Time]),
    % 1
    case get_data_sync_fun(Type) of
        ?NONE ->
            ok;
        List when is_list(List) ->
            [do_call_sync_fun(Fun, State) || Fun <- List],
            ok;
        Fun ->
            do_call_sync_fun(Fun, State)
    end,
    % 2
    set_last_data_sync_time(Type),
    % 3
    proc_timer:start_timer(?MODULE, Time, {?DATA_SYNC_EVENT, Ref}),
    {ok, State}.

%% @doc 获取上一次同步时间
get_last_data_sync_time(Type) ->
    case erlang:get({'$last_data_sync_time', Type}) of
        undefined ->
            0;
        Time ->
            Time
    end.

%% 设置上一次同步时间
set_last_data_sync_time(Type) ->
    erlang:put({'$last_data_sync_time', Type}, util:now_sec()),
    ok.

%% 获取同步函数
get_data_sync_fun(Type) ->
    case erlang:get({'$data_sync_fun', Type}) of
        undefined ->
            ?NONE;
        [] ->
            ?NONE;
        SyncFun ->
            SyncFun
    end.

%% 调用同步函数
do_call_sync_fun({M, F, 0}, _State) ->
    M:F();
do_call_sync_fun({M, F, 1}, State) ->
    M:F(State).

%%-----------
%% EUNIT TEST
%%-----------
-ifdef(EUNIT).


-endif.
