%%%----------------------------------------------------------------------
%%%
%%% @author ningning
%%% @date  2012.07.01
%%% @doc 角色系统
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(mod_role).
-include("common.hrl" ) .
-include("role.hrl").
-include("protocol.hrl").
-include("ecode.hrl").
-include("counter.hrl").
-behaviour(gen_mod).

%% 对外接口
-export([admin_payment/3]).


%% gen_mod callbacks
-export([i/1, p/1, init/0, register_event/0, init_role/1, terminate_role/1, clear_daily/1,
         handle_c2s/3, handle_timeout/2, handle_s2s_call/2, handle_s2s_cast/2]).

-export([on_enter_map/2]).

-export([admin_ban_acc/2, admin_enable_acc/1, admin_enable_ip/1, admin_ban_ip/2]).

-export([on_payment/4]).

%% @doc 帐号封禁
%% Time 封禁时长,单位为秒
admin_ban_acc(Id, Time) when is_integer(Id) ->
    % 先踢出玩家
    role_server:kickout(Id),
    % 添加一条禁角色记录
    ok = serv_gm_ctrl:add_role_ban(Time, Id).

%% @doc 帐号解禁
admin_enable_acc(Id) ->
    % 删除一条禁登录记录
    serv_gm_ctrl:delete_role_ban(Id).

%% @doc ip封禁
%% Time 封禁时长,单位为秒
admin_ban_ip(Ip, Time) ->
    % 添加一条禁角色记录
    ok = serv_gm_ctrl:add_ip_ban(Time, Ip).

%% @doc ip解禁
admin_enable_ip(Ip) ->
    % 删除一条禁登录记录
    serv_gm_ctrl:delete_ip_ban(Ip).

%% @doc 后台充值
%% 1,玩家是否在线?
%%  1a,在线,直接调用进行充值
%%  1b,离线,从数据库获取玩家原金币,增加金币,计算vip等级
%% 2,以上操作未发生异常,则充值成功,否则失败
%% 3,记录日志
%% 4,发放礼包
admin_payment(Id, OrderId, PayGold) when is_integer(PayGold), PayGold > 0 ->
    try
        % 1
        Ret =
        case role_server:get(Id) of
            #role{} ->
                % 1a
                ok = role_server:s2s_call(Id, ?MODULE, {admin_payment, PayGold}, 30000);
            ?NONE ->
                % 1b
                admin_payment(Id, OrderId, PayGold)
        end,
        % 1
        game_log:pay(Id, OrderId, PayGold),

        Ret
    catch
        _Type:_Reason ->
            % 2
            {error, ?E_PAYMENT}
    end.


%% @doc 本模块信息
i(Role) ->
    [Role
    ].

%% @doc 打印本模块信息
p(_) ->
    "".

%% @doc 关心的事件
register_event() ->
    [{on_enter_map, all}].

%% @doc 当玩家进入地图
on_enter_map(_, true) ->
    anti_cheat:clear_pos_prev();
on_enter_map(_Role, _) ->
    anti_cheat:clear_pos_prev(),
    ok.

%% @doc 模块初始化
init() ->
    % 每隔10分钟，执行db_role:udpate(Role)
    {ok, 0, 11, {db_role, save, 1}}.

%% @doc 玩家初始化
init_role(Role) ->
    {ok, Role}.

%% @doc 玩家结束
terminate_role(#role{id = Id} = Role) ->
    ok = ?COUNTER_RAM:save(Id),
    ok = ?COUNTER_DAILY:save(Id),
    serv_role_cache:notify_update_role_cache(Role).

%% 清除daily
clear_daily(_Role) ->
	
	ok.

%% 创建角色
handle_c2s(?P_ROLE_CREATE, [Name, Ip], 
        #role{id = 0, accname = AccName} = Role) ->
    case catch do_create(AccName, Ip, Name) of
        {ok, Id} ->
            game_log:register(Id, Ip),
            % client在收到创建成功后，会请求数据
            % TODO 修改成创建角色后，数据初始化完成，不用从新从数据库load数据
            role_server:s2s_cast(self(), mod_login, {login_after_create}),
            {ok, Role#role{id = Id, reg_time = util:now_sec()}};
        {error, Code} ->
            lager:error("create role error:~p", [Code]),
            Msg = [Code],
            {respon, Msg, Role}
    end;
%% 已经创建了角色
handle_c2s(?P_ROLE_CREATE  ,  [ _Name, _Vocation]  , _Role ) ->
    {?E_ROLE_RECREATE} ;

handle_c2s(_Fi, _Data, Role) ->  
    {ok, Role}.

handle_timeout(_Event, Role) ->
    {ok, Role}.

%%-----------------------------
%% @doc 处理服务器内部的call
%%-----------------------------

%% 充值(后台使用)
handle_s2s_call({admin_payment, Gold}, Role) ->    
        Role2 = role_internal:inc_gold(Role, Gold),
        game_log:gold(Role2, Gold, ?LOG_EARN_GOLD_TYPE_PAYMENT),
        {ok, ok, Role2};
handle_s2s_call(_Req, Role) ->
        {ok, Role}.

handle_s2s_cast(_Req, Role) ->
        {ok, Role}.


%%------------------
%% 关于充值
%%------------------
%% 当充值时,独立函数的目的是捕获异常
%% 不要让充值报错
on_payment(Role, PayGold, OldVip, OldTotalPayment) ->
    try do_on_payment(Role, PayGold, OldVip, OldTotalPayment)
    catch
        _T:_R ->
            lager:error("player ~p onpayment error ~p:~p", [Role#role.id, _T, _R]),
            ok
    end.
do_on_payment(Role, _PayGold, _OldVip, OldTotalPayment) ->
    % 关于首充
	?IF(OldTotalPayment =:= 0, mod_gift:send_msg_payment_first_info(Role, 0), ok),
	ok.
       

%% 创建玩家
%% 1,名字合法?否,返回错误
%% 2,职业合法?否,返回错误
%% 3,性别合法?否,返回错误
%% 4,创建玩家
do_create(AccName, Ip, Name) ->
    % 1
    ?IF(util:is_text_valid(Name, 2, 7), ok, ?C2SERR(?E_ROLE_NAME_LONG)),
    ?IF(util:is_text_valid(AccName, 1, 24), ok, ?C2SERR(?E_ROLE_ACCNAME_LONG)),

    % 4
    Id = serv_id:role(),

    db_role:create(Id, AccName, Ip, Name).         