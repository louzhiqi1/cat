%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2011.03.11
%%% @doc 玩家及系统日志记录
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(game_log).
-include("common.hrl").
-include("role.hrl").
-include("log.hrl").

-compile([export_all]).

%% 文件写入日志
-define(FILE_LOG(LogType, Args), do_write_log(LogType, Args)).

%% 添加引号
-define(QUOTE(V), [$", V, $"]).

%% @doc 注册角色
register(RoleId, Ip) ->
    ?FILE_LOG(?LOG_REGISTER, [RoleId, Ip | now_sec_to_dtw()]).

%%--------------
%% internal API
%%--------------

%% 写入日志,csv格式
do_write_log(Type, L) ->
    Str = do_join(L, ","),
    log_server:write(Type, [Str, "\n"]).

%% 添加分隔符
do_join([], Sep) when is_list(Sep) ->
    []; 
do_join([H|T], Sep) ->
    [util:any_to_iodata(H)] ++ lists:append([[Sep, util:any_to_iodata(X)] || X <- T]).

%% 根据now_sec生成对应的日期
now_sec_to_dtw() ->
    now_sec_to_dtw(util:now_sec()).
now_sec_to_dtw(Now) ->
    {{Y, Mon, D} = Date, {H, _Min, _Sec}} = util:now_sec_to_local_time(Now),
    % 0--6，周日--周六
    Dow = calendar:day_of_the_week(Date), 
    NowSecDay = Now - (H * 3600 + _Min * 60 + _Sec),
    [Now, NowSecDay, Y, Mon, D, H, ?IF(Dow =:= 7, 0, Dow)].

%% now转化成date
now_sec_to_date(Now) ->
    {_Date, {H, _Min, _Sec}} = util:now_sec_to_local_time(Now),
    Now - (H * 3600 + _Min * 60 + _Sec).
