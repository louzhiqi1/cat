%%%----------------------------------------------------------------------
%%%
%%% @date  2012.07.10
%%% @doc 后台控制，包括禁ip，禁登录，禁言
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(serv_gm_ctrl).
-include("common.hrl").
-include("role.hrl").
-include("gm_ctrl.hrl").

-export([add_ip_ban/2, add_role_ban/2, add_chat_ban/2,
         delete_ip_ban/1, delete_role_ban/1, delete_chat_ban/1
        ]).
-export([is_ip_ban/1, is_chat_ban/1, is_role_ban/1, 
        set_track/1, set_track/2, delete_track/1, get_track/1, 
        get_chat_ban/1
        ]).
-export([is_insider/1, add_insider/1, delete_insider/1, insider_list/0]).

-export([i/0, list/0, list_ban_acc/0, list_ban_chat/0, list_ban_ip/0]).
-export([init/0, add/2, add/4, get/2, clear/0, delete/2, is_timein_exists/2]).

-define(TABLE, gm_ctrl).

%% 控制类型定义
-define(IP_BAN, 1).             % 禁IP
-define(ROLE_BAN, 2).           % 禁角色
-define(CHAT_BAN, 3).           % 禁聊天
-define(ROLE_INSIDER, 4).       % 内服号
-define(CM_STATUS, 10).         % 防沉迷状态
-define(ROLE_TRACKED, 20).      % 玩家跟踪

%%---------
%% 对外接口
%%---------

%% 添加一条禁ip记录
add_ip_ban(Time, Target) when is_list(Target) ->
    StartTime = util:now_sec(),
    ok = add(?IP_BAN, StartTime, StartTime + Time, Target),
    ok.

%% 添加一条禁角色记录
add_role_ban(Time, Target) ->
    StartTime = util:now_sec(),
    ok = add(?ROLE_BAN, StartTime, StartTime + Time, Target),
    ok.

%% 添加一条禁言记录
add_chat_ban(Time, Target) ->
    StartTime = util:now_sec(),
    ok = add(?CHAT_BAN, StartTime, StartTime + Time, Target),
    ok.

%% 删除一条禁ip记录
delete_ip_ban(Target) when is_list(Target) ->
    delete(?IP_BAN, Target).

%% 删除一条禁登录记录
delete_role_ban(Target) ->
    delete(?ROLE_BAN, Target).

%% 删除一条禁言记录
delete_chat_ban(Target) ->
    delete(?CHAT_BAN, Target).

%% @doc 初始化
init() ->
    ?INFO(?_U("初始化禁封列表"),[]),
    ?TABLE = ets:new(?TABLE, [set, public, named_table, 
                              {keypos, #gm_ctrl.id}, {read_concurrency, true}]),
    ok = do_load_data(),
    ok.

%% 被禁聊天记录是否已经存在
is_chat_ban(Id) ->
    is_timein_exists(?CHAT_BAN, Id).

%% 被禁ip记录是否已经存在
is_ip_ban(Target) ->
    is_timein_exists(?IP_BAN, Target).

%% 被禁登录记录是否已经存在
is_role_ban(Id) ->
    is_timein_exists(?ROLE_BAN, Id).

%%---------------
%% 关于内服号
%%---------------

%% @doc 是否内服号
is_insider(Id) ->
    ets:member(?TABLE, {?ROLE_INSIDER, Id}).

%% @doc 添加内服号
%% 尝试设置成VIP3
add_insider(Id) ->
    add(?ROLE_INSIDER, Id).

%% @doc 删除内服号
delete_insider(Id) ->
    delete(?ROLE_INSIDER, Id).

%% @doc 获取内服号列表
insider_list() ->
    [Id || #gm_ctrl{target = Id} <- list(?ROLE_INSIDER)].

%% @doc 追踪玩家
set_track(Id) ->
    set_track(Id, ?ROLE_TRACK_BOTH).
set_track(Id, Type) ->
    ?MODULE:add(?ROLE_TRACKED, Id),
    role_server:set_track(Id, Type),
    ok.

%% @doc 取消追踪
delete_track(Id) ->
    ?MODULE:delete(?ROLE_TRACKED, Id),
    role_server:set_track(Id, ?ROLE_TRACK_NONE),
    ok.

%% 是否追踪玩家
get_track(Id) ->
    case ?MODULE:get(?ROLE_TRACKED, Id) of
        ?NONE ->
            ?ROLE_TRACK_NONE;
        _ ->
            ?ROLE_TRACK_IN
    end.

%% @doc 获取禁言
get_chat_ban(Target) ->
    ?MODULE:get(?CHAT_BAN, Target).

%% 获取被禁记录
get(CtrlType, Target) ->
    ?DEBUG(?_U("获取被禁记录，CtrlType:~p, Target:~p"),[CtrlType, Target]),
    case ets:lookup(?TABLE, {CtrlType, Target}) of
        [] ->
            ?NONE;
        [GmCtrl] ->
            GmCtrl
    end.

%% 被禁记录是否已经存在且在有效时间内
is_timein_exists(CtrlType, Target) ->
    ?DEBUG(?_U("被禁记录是否已经存在，CtrlType:~p, Target:~p"),[CtrlType, Target]),
    case ets:lookup(?TABLE, {CtrlType, Target}) of
        [] ->
            %?DEBUG(?_U("被禁记录不存在，CtrlType:~p, Target:~p"),[CtrlType, Target]),
            false;
        [GmCtrl] ->
            case GmCtrl#gm_ctrl.end_time > util:now_sec() of
                true ->
                    %?DEBUG(?_U("被禁记录已经存在，Type:~p target:~p end_time:~p"),
                    %    [CtrlType, Target, GmCtrl#gm_ctrl.end_time]),
                    true;
               false ->
                   delete(CtrlType, Target),
                   %?DEBUG(?_U("被禁记录是否不存在，CtrlType:~p, Target:~p"),[CtrlType, Target]),
                   false
            end
    end.

%% @doc 清除
clear() ->
    true = ets:delete_all_objects(?TABLE),
    ok.

%% @doc 当前状态
i() ->
    [{count, length(list())}].

%% @doc 所有的数据id
list() ->
    [Id || #gm_ctrl{id = Id} <- ets:tab2list(?TABLE)].

%% @doc 获取封号列表
list_ban_acc() ->
    list(?ROLE_BAN).

%% @doc 获取禁言列表
list_ban_chat() ->
    list(?CHAT_BAN).

%% @doc 获取封ip列表
list_ban_ip() ->
    list(?IP_BAN).

%% @doc 获取指定列表
list(Type) ->
    ets:match_object(?TABLE, #gm_ctrl{ctrl_type = Type, _ = '_'}).

%% 添加一条禁止
add(Type, Target) ->
    add(Type, 0, ?INFINITY, Target).
add(CtrlType, StartTime, EndTime, Target) ->
    GmCtrl = #gm_ctrl{
        db_id = serv_id:gm_ctrl(),
        id = {CtrlType, Target},
        ctrl_type = CtrlType,
        begin_time = StartTime,
        end_time = EndTime,
        target = Target
    },
    ets:insert(?TABLE, GmCtrl),
    db_gm_ctrl:add(GmCtrl),
    ok.

%% 删除一条禁止数据
delete(CtrlType, Target) ->
    Key = {CtrlType, Target},
    case ets:lookup(?TABLE, Key) of
        [] ->
            ok;
        [GmCtrl] ->
            true = ets:delete(?TABLE, Key),
            db_gm_ctrl:delete(GmCtrl)
    end.

%%--------------
%% internal API
%%--------------

%% 加载数据
do_load_data() ->
    List = db_gm_ctrl:list(),
    do_insert(List).

%% 加载数据
do_insert(List) ->
    ?INFO(?_U("加载被禁记录条数:~b"), [length(List)]),
    [begin
        GmCtrl = trans_gm_ctrl(Row),
        true = ets:insert(?TABLE, GmCtrl)
    end || Row <- List],
    ok.

%% 转化gm_ctrl结构
trans_gm_ctrl(#gm_ctrl{ctrl_type = CtrlType, target = Target0} = GmCtrl) ->
    Target =
    case CtrlType of
        ?IP_BAN ->
            ?B2S(Target0);
        ?ROLE_BAN ->
            ?B2N(Target0);
        ?CHAT_BAN ->
            ?B2N(Target0);
        ?ROLE_TRACKED ->
            ?B2N(Target0)
    end,
    GmCtrl#gm_ctrl{
        id = {CtrlType, Target},
        target = Target
    }.
