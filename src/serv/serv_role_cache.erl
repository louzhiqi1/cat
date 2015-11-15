%%% -------------------------------------------------------------------
%%% Author  : qingouqi
%%% Description : 缓存(LRU)
%%% Created : 2012-12-20
%%% 说明： 本模块用于缓冲一些常用的离线玩家数据, 方便查询 , 数据格式是 #role_basic_info{}
%%% 1, 每天凌晨3点清除过期的玩家数据
%%% 2, 查询时, 缓冲区中没该玩家数据, 那么从数据库中查询, 并缓存该玩家数据
%%% 3, 玩家数据被查询时, 修改缓冲表中的expire字段
%%% 4, 在线玩家信息可直接从 serv_role_mgr 中取得到.
%%% 5, 修改缓冲区大小的方法:
%%%    (1) 在game.conf文件中加入  {max_role_cache_size, Size}. 其中Size为自己定义的数
%%%    (2) 在线运行时 serv_role_cache:set_max_size(Size). 其中Size为自己定义的数
%%% 6, 数据校验：
%%%    (1) 每天凌晨3点清除过期玩家数据的同时, 会检查缓冲区数据是否有错
%%%    (2) 可以手动检查数据 serv_role_cache:check().
%%% -------------------------------------------------------------------
-module(serv_role_cache).
-vsn('0.1').
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("common.hrl").
-include("role.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/0,start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([clear_expire_data/0, clear_all/0, check/0]).
-export([notify_update_role_cache/1]).
-export([get_basic_info_by_id/1]).
-export([set_max_size/1]).
-export([i/0, i/1]).

-define(NAME, ?MODULE).     %% 名字
-define(ROLE_CACHE_TABLE, role_cache_table). % 玩家常用数据
-define(INDEX_TABLE, index_table).           % 索引表

-define(DEF_CACHE_SIZE, 1000).   %缓冲区默认大小, 单位:记录

-ifdef(TEST).
-define(EXPIRE_TIME, 1000).
-else.
-define(EXPIRE_TIME, 86400).   %过期时间为1天
-endif.

%% 索引, 快速查找到最长时间没被使用的玩家
-record(index,{
               key = {0, 0},    % 索引关键词, {expire, id}, expire 是缓冲过期时间, id是玩家id 
               id               % 玩家id
              }).

-record(state, 
        {
         max_size = ?DEF_CACHE_SIZE     % 缓冲区默认大小
        }).


%% @doc 启动服务
start() ->
    gen_server:start({local, ?NAME}, ?MODULE, [], []).

%% @doc 启动服务
start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

%%-----------------
%% Export APIs
%%-----------------

i() ->
    'try: i(size) | i(all)'.
%% 查看缓冲区
i(size) ->
    call(i_size);
i(all) ->
    call(i_all);
i(_) ->
    badarg.
%% @doc 获取玩家缓冲信息(基本信息)
%% 返回 #role_basic_info{}
get_basic_info_by_id(RId) ->
    do_get(RId).

%% @doc 检查缓冲区数据是否正确, 如果有错则清空, 否则什么也不做
%% 供人工干预
check() ->
    cast(check).

%% @doc 清理过期数据(暂定为1天)
clear_expire_data() ->
    cast(clear_expire_data).

%% 清空缓冲区
clear_all() ->
    cast(clear_all).

%% @doc 更新玩家缓冲区信息(当玩家下线时)
notify_update_role_cache(#role{} = Role) ->
    cast({update_role_cache, Role});

%% @doc 更新玩家缓冲区数据(查询不命中时)
notify_update_role_cache(#role_basic_info{} = BasicInfo) ->
    cast({update_role_cache, BasicInfo}).

%% @doc 更新玩家缓冲区数据(查询命中时)
notify_update_expire(BasicInfo) ->
    cast({update_expire, BasicInfo}).

set_max_size(Size) when is_integer(Size) ->
    call({set_max_size, Size});
set_max_size(Size) ->
    {badarg, Size}.

init([]) ->
    ?DEBUG(?_U("** 初始化玩家基本数据缓冲模块 **")),
    erlang:process_flag(trap_exit, true),
    random:seed(erlang:timestamp()),
    init_cache(),
    State = get_max_size(#state{}),
    ?DEBUG("role_cache max_size is ~p", [State#state.max_size]),
    {ok, State}.

handle_call({set_max_size, Size}, _From, State) ->
    case catch do_set_max_size(State, Size) of
        {ok ,State2} ->
            Reply = {ok, State2};
        {error, Code} ->
            State2 = State,
            Reply = {error, Code, State2}
    end,
    {reply, Reply, State2};

handle_call(i_size, _From, #state{max_size = MaxSize} = State) ->
    IndexSize = ets:info(?INDEX_TABLE, size),
    CacheSize = ets:info(?ROLE_CACHE_TABLE, size),
    Reply = [{index_size, IndexSize},
             {cache_size, CacheSize},
             {max_size, MaxSize}],
    {reply, Reply, State};

handle_call(i_all, _From, #state{max_size = MaxSize} = State) ->
    IndexSize = ets:info(?INDEX_TABLE, size),
    CacheSize = ets:info(?ROLE_CACHE_TABLE, size),
    Cache = ets:tab2list(?ROLE_CACHE_TABLE),
    Index = ets:tab2list(?INDEX_TABLE),
    Reply = [{index_size, IndexSize},
             {cache_size, CacheSize},
             {max_size, MaxSize},
             {index, Index},
             {cache, Cache}],
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(clear_expire_data, State) ->
    do_clear_expire_data(),
    case do_check(State) of
        ok ->
            ok;
        {error, Type} ->
            ok = do_clear_all(),
            ?WARN(?_U("缓冲区数据错误, 类型为 ~p, 缓冲区已经被重置."), [Type]),
            ok
    end,
    {noreply, State};

handle_cast(clear_all, State) ->
    ok = do_clear_all(),
    {noreply, State};

handle_cast({update_expire, BasicInfo}, State) ->
    case catch do_update_expire(State, BasicInfo) of
        {error, Code} ->
            ?WARN("更新错误码~p", [Code]),
            ok;
        _ ->
            ok
    end,
    {noreply, State};

handle_cast({update_role_cache, #role_basic_info{} = BasicInfo}, State) ->
    case catch do_update_role_cache(State, BasicInfo) of
        {error, Code} ->
            ?WARN("更新错误~p", [Code]),
            ok;
        _ ->
            ok
    end,
    {noreply, State};

handle_cast({update_role_cache, #role{} = Role}, State) ->
    case catch do_update_role_cache(State, Role) of
        {error, Code} ->
            ?WARN("更新错误 ~p", [Code]),
            ok;
        _ ->
            ok
    end,
    {noreply, State};

handle_cast(check, State) ->
    case do_check(State) of
        ok ->
            ok;
        {error, Type} ->
            ok = do_clear_all(),
            ?WARN(?_U("缓冲区数据错误, 类型为 ~p, 缓冲区已经被重置."), [Type]),
            ok
    end,
    {noreply, State};

handle_cast(_Req, State) ->
    ?DEBUG(?_U("未知的cast请求 ~p"),[_Req]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%---------------
%% internal API
%%---------------
init_cache() ->
    ?ROLE_CACHE_TABLE = ets:new(?ROLE_CACHE_TABLE, [named_table, set, public,
                                                    {keypos, #role.id}, {read_concurrency, true}]),
    ?INDEX_TABLE = ets:new(?INDEX_TABLE, [named_table, ordered_set, public,
                                          {keypos,#index.key},{read_concurrency, true}]).
%% 设置缓冲区大小
do_set_max_size(#state{max_size = OldSize} = State, Size) when is_integer(Size), Size >= 0 ->
    ?IF(Size < OldSize, do_resize(OldSize, Size), ok),
    {ok, State#state{max_size = Size}};
do_set_max_size(_State, _Size) ->
    ?C2SERR(?E_BADARG).

%% 从配置文件中读取缓冲区大小
get_max_size(State) ->
    MaxSize = ?CONFIG(max_role_cache_size, ?DEF_CACHE_SIZE),
    State#state{max_size = MaxSize}.

%% 获取玩家缓冲信息
%% 1 在缓冲表取
%% 2 缓冲表不存在, 那么从从数据库取, 同时保存到缓冲表
do_get(RId) ->
    case role_server:get(RId) of
        #role{} = R ->
            #role_basic_info{
                id = R#role.id,
                name = R#role.name,
                lvl = R#role.lvl,
                sex = R#role.sex,
                job = R#role.job,
                vip = R#role.vip,
                repu = R#role.repu,
                flower = R#role.flower,
                title = R#role.title,
                last_login_time = R#role.last_login_time,
                last_logout_time = R#role.last_logout_time
            };
        ?NONE ->
            do_get_from_cache(RId)
    end.
do_get_from_cache(RId) ->
    case ets:lookup(?ROLE_CACHE_TABLE, RId) of
        [#role_basic_info{} = BasicInfo] ->
            notify_update_expire(BasicInfo),       % 更新expire 字段
%%             inc_hit(),
            BasicInfo;
        [] ->
            BasicInfo = db_role:get_basic_info_by_id(RId),
            Expire = util:now_sec() + ?EXPIRE_TIME,
            BasicInfo2 = BasicInfo#role_basic_info{expire = Expire},
            notify_update_role_cache(BasicInfo2),
            BasicInfo2
    end.

%% 清理过期的数据
do_clear_expire_data() ->
    Now = util:now_sec(),
    MS1 = [{#role_basic_info{id = '_',name = '_',lvl = '_',sex = '_',
                             job = '_',vip = '_',repu = '_',flower = '_',title = '_',
                             last_login_time = '_',last_logout_time = '_',expire = '$1'},
            [{'>',Now,'$1'}],
            [true]}],
    MS2 = [{{index,{'$1', '_'},'_'},[{'>',Now,'$1'}],[true]}],
    ets:select_delete(?ROLE_CACHE_TABLE, MS1),
    ets:select_delete(?INDEX_TABLE, MS2).

%% 清空缓冲区
do_clear_all() ->
    ets:delete_all_objects(?ROLE_CACHE_TABLE),
    ets:delete_all_objects(?INDEX_TABLE),
    ok.

%% 玩家下线时更新玩家离线数据
do_update_role_cache(State, #role{id = RId} = Role) ->
    BasicInfo = to_role_basic_info(Role),
    case ets:lookup(?ROLE_CACHE_TABLE, RId) of
        [] ->
            ok;
        [#role_basic_info{id = RId, expire = Expire}] ->
            ets:delete(?INDEX_TABLE, {Expire, RId}),
            ets:delete(?ROLE_CACHE_TABLE, RId)
    end,
    do_update_role_cache(State, BasicInfo);
do_update_role_cache(#state{max_size = MaxSize} = _State, 
                     #role_basic_info{id = Id, expire = Expire} = BasicInfo) ->
    NowSize = ets:info(?ROLE_CACHE_TABLE, size),
    case NowSize < MaxSize of
        true ->
            Index = #index{key = {Expire, Id}, id = Id},
            ets:insert(?INDEX_TABLE, Index),
            ets:insert(?ROLE_CACHE_TABLE, BasicInfo);
        false ->
            % 删除最久未被使用的
            FirstKey = ets:first(?INDEX_TABLE),
            ?IF(FirstKey =/= '$end_of_table', ok, ?C2SERR(?E_BADARG)),
            [#index{key = {_, IdKey}}] = ets:lookup(?INDEX_TABLE, FirstKey),
            ets:delete(?INDEX_TABLE, FirstKey),
            ets:delete(?ROLE_CACHE_TABLE, IdKey),
            % 插入新项
            Index = #index{key = {Expire, Id}, id = Id},
            ets:insert(?INDEX_TABLE, Index),
            ets:insert(?ROLE_CACHE_TABLE, BasicInfo)
    end.

%% 更新玩家的过期时间
do_update_expire(_State, #role_basic_info{id = Id, expire = Expire} = BasicInfo) ->
    %1 删除原来的索引
    OldIndex = #index{key = {Expire, Id}, id = Id},
    ets:delete_object(?INDEX_TABLE, OldIndex),
    %2 增加新索引及更新数据
    Expire2 = util:now_sec() + ?EXPIRE_TIME,
    RoleCache2 = BasicInfo#role_basic_info{expire = Expire2},
    Index = #index{key = {Expire2, Id}, id = Id},
    ets:insert(?INDEX_TABLE, Index),
    ets:insert(?ROLE_CACHE_TABLE, RoleCache2),
    ok.

%% 检查缓冲区数据是否有错
do_check(#state{max_size = MaxSize}) ->
    %1 取两个表的大小
    CacheSize = ets:info(?ROLE_CACHE_TABLE, size),
    IndexSize = ets:info(?INDEX_TABLE, size),
    case CacheSize =:= IndexSize of
        true ->
            %2 两个表大小相等
            case CacheSize =< MaxSize of
                true ->
                    %3 缓冲数据表小于最大值
                    case do_ckeck_one_by_one() of
                        ok ->
                            ok;
                        error ->
                            {error, bad_data}
                    end;
                false ->
                    %4 缓冲数据表大于最大值
                    {error, bad_cache_size}
            end;
        %5 两个表大小不一致
        false ->
            {error, badmatch_size}
    end.

%% 逐一匹对索引和表数据表
do_ckeck_one_by_one() ->
    IndexList = ets:tab2list(?INDEX_TABLE),
    do_check_one_by_one(IndexList).
do_check_one_by_one([]) ->
    ok;
do_check_one_by_one([Index | T]) ->
    #index{key = {Expire, Id}, id = Id} = Index,
    case ets:lookup(?ROLE_CACHE_TABLE, Id) of
        [#role_basic_info{expire = Expire}] ->
            do_check_one_by_one(T);
        _ ->
            error
    end.

%% 调整缓冲区大小, 将多余的数据清除(从大调到小)
do_resize(OldSize, Size) ->
    Len = OldSize - Size,
    IndexList = ets:tab2list(?INDEX_TABLE),
    ToDelList = lists:sublist(IndexList, Len),
    lists:foreach(
      fun(#index{key = Key, id = RId}) ->
              ets:delete(?INDEX_TABLE, Key),
              ets:delete(?ROLE_CACHE_TABLE, RId),
              ok
      end,
      ToDelList).
    
%% cast调用
cast(Req) ->
    gen_server:cast(?NAME, Req).

call(Req) ->
    gen_server:call(?NAME, Req).

%%--------
%% 数据转换
%%--------
%% 将#role{} 结构转成 #role_basic_info{}
to_role_basic_info(#role{} = R) ->
    #role_basic_info{
                     id = R#role.id,
                     name = R#role.name,
                     lvl = R#role.lvl,
                     sex = R#role.sex,
                     job = R#role.job,
                     vip = R#role.vip,
                     repu = R#role.repu,
                     flower = R#role.flower,
                     title = R#role.title,
                     last_login_time = R#role.last_login_time,
                     last_logout_time = R#role.last_logout_time,
                     expire = util:now_sec() + ?EXPIRE_TIME
                    }.

%%--------
%% 统计
%%--------
-ifdef(TEST).
-compile([export_all]).
inc_count() ->
    N = get_count(),
    erlang:put('lookup_count', N + 1).

get_count() ->
    case erlang:get('lookup_count') of
        undefined ->
            0;
        V ->
            V
    end.

inc_hit() ->
    N = get_hit(),
    erlang:put('hit_count', N + 1).

get_hit() ->
    case erlang:get('hit_count') of
        undefined ->
            0;
        V ->
            V
    end.

%% 获取统计数据
get_re() ->
    All = get_count(),
    Hit = get_hit(),
    HitRatio = Hit/All * 100,
    [{lookup_count, All},
     {lookup_hit, Hit},
     {hit_ratio, HitRatio,'%'}].

test(Count) ->
    clear_re(),
    Ids = db_role:get_ids_where([{lvl, 35, 100}]),
    Len = length(Ids),
    ?WARN(?_U("玩家个数 ~p "), [Len]),
    test(Ids, Len, Count).

test(_, _, 0) ->
    get_re();
test(Ids, Len, Count) ->
    N = util:rand(1, Len),
    Id = lists:nth(N, Ids),
    case catch ?MODULE:do_get(Id) of
        {error, Code} ->
            ?WARN(?_U("查询玩家 ~p 错误码 ~p "), [Id, Code]),
            ok;
        #role_basic_info{} ->
%%             inc_count(),
            ok;
        _Other ->
            ?WARN(?_U("查询玩家 ~p 错误码 ~p "), [Id, _Other]),
            ok
    end,
    test(Ids, Len, Count - 1).

%% 清除统计数据
clear_re() ->
    erlang:erase('lookup_count'),
    erlang:erase('hit_count'),
    ok.

-endif.
