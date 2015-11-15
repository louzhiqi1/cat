%%%----------------------------------------------------------------------
%%%
%%% @doc 协议定义,添加新协议请别忘记更新协议映射
%%% @end
%%%
%%%----------------------------------------------------------------------
-ifndef(PROTECT_HRL).
-define(PROTECT_HRL, ok).

%% 关于协议映射
-define(HANDLER_NAME(Mi),
        case Mi of
        	3 -> mod_role;
            200 -> mod_heart;       % 心跳

            _ -> mod_unknown        % 未知模块
        end).

%%----------------------------
%% 心跳
%%----------------------------
-define(P_HEART, 200) .   
-define(P_HEART_ROLE, 0).            % 心跳包

%%-----------------------------
%% 登陆
%%-----------------------------
-define(P_LOGIN, 1).

-define(P_LOGIN_HEART, 0).          % 心跳
-define(P_LOGIN_LOGIN, 1).          % 登陆
-define(P_SERVER_REJECT, 6).        % 服务器主动断开连接
-define(P_SERVER_STOP_CAST, 7).     % 通知玩家服务器将要断开

-define(P_LOGIN_CHECK_CM, 8).       % 验证身份信息
-define(P_LOGIN_CREATE_PAGE, 9).    % 进入创建角色页面

-define(P_LOGIN_PUSH_CM_STATE, 11). % 防沉迷状态
-define(P_LOGIN_CM_ALARM, 12).      % 防沉迷提示

%%---------------------------------------------
%% 角色系统
%%---------------------------------------------
-define(P_ROLE,             2 ) .    

-define(P_ROLE_CREATE, 0).      % 角色创建
-define(P_ROLE_INFO, 1).        % 角色信息(角色面板)
-define(P_ROLE_OTHER_INFO, 2).  % 查询其他角色信息(角色面板)
-define(P_ROLE_ATTR_VAR, 3).    % 角色战斗属性更新
-define(P_ROLE_EXP_UPDATE, 4).  % 经验更新
-define(P_ROLE_UPGRADE, 5).     % 升级广播
-define(P_ROLE_CD_LIST, 6).     % 玩家自己cd列表
-define(P_ROLE_FORTUNE, 7).     % 角色财富信息
-define(P_ROLE_STAR_POINT, 8).     % 角色星灵信息
-define(P_ROLE_CHANGE_NAME, 9).     % 角色改名

-define(P_ROLE_REVIVE, 10).     % 复活
-define(P_ROLE_CHANGE_PK, 11).  % 修改pk模式
-define(P_ROLE_CHARM, 12).      % 魅力值
-define(P_ROLE_PROMOTE_INFO, 13).     % 晋升信息
-define(P_ROLE_VIGOUR_BUY, 20). % 购买体力 
-define(P_ROLE_VIGOUR_INFO, 21).% 体力信息
-define(P_ROLE_ONLINE_TIME, 22).% 玩家在线时长

-define(P_ROLE_GM_ZDY_LIST, 30).% gm和指导员信息
-define(P_ROLE_CLEAR_DAILY, 31).% 0点清零daily
-define(P_ROLE_WORLD_AVG_LVL, 40).  % 世界平均等级

-define(P_ROLE_FIGHTPOINT_SELF, 50).    % 自己战斗力信息
-define(P_ROLE_FIGHTPOINT_OTHER, 51).    % 他人战斗力信息

-endif.