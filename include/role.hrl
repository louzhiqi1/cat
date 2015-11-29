-ifndef(ROLE_HRL).
-define(ROLE_HRL, true).

%% 是否受防沉迷影响
-define(CM_TYPE_CHILD, 0).       % 以注册未满18岁
-define(CM_TYPE_ADULT, 1).       % 已注册满18岁
-define(CM_TYPE_UNREG, 2).       % 未注册（应提醒注册）

%% Track类型
-define(ROLE_TRACK_NONE, 0).    % 不追踪
-define(ROLE_TRACK_IN, 1).      % 发给server的数据包
-define(ROLE_TRACK_OUT, 2).     % 发给client的数据包
-define(ROLE_TRACK_BOTH, 3).    % 全部追踪

%% 复活类型
-define(REVIVE_TYPE_FULL, 1).   % 满血原地复活
-define(REVIVE_TYPE_SAFE, 2).   % 安全区复活

%% 玩家PK模式
-define(PK_MODE_MIN, 1).        % 最小值
-define(PK_MODE_PEACE, 1).      % 和平
-define(PK_MODE_ALL, 2).        % 全体
-define(PK_MODE_EVIL, 3).       % 善恶
-define(PK_MODE_GUILD, 4).      % 公会
-define(PK_MODE_TROOP, 5).      % 队伍
-define(PK_MODE_MAX, 5).        % 最大值
-define(PK_MODE_ROLE_NAME_W, 1).    % 白名
-define(PK_MODE_ROLE_NAME_G, 2).    % 灰名
-define(PK_MODE_ROLE_NAME_Y, 3).    % 黄名
-define(PK_MODE_ROLE_NAME_R, 4).    % 红名

%%
%% 关于体力
%%
%% 体力恢复间隔(30分钟)
-ifdef(TEST).
-define(VIGOUR_RECOVER_INTERVAL, 1800).
-else.
-define(VIGOUR_RECOVER_INTERVAL, 1800).
-endif.
%% 体力每次恢复值
-define(VIGOUR_RECOVER_POINT, 5).
%% 体力上限
-define(VIGOUR_MAX, 200).
%% 每次购买获得的体力数
-define(VIGOUR_EVERY_BUY, 40).
%% 体力每日购买次数
-define(VIGOUR_BUY_TIMES, 2).
%% 每天12点18点赠送的体力值
-define(VIGOUR_ADD_EVERY_FORWARD, 50).

%% 性别，0不限制
-define(SEX_MALE, 1).       % 男
-define(SEX_FEMALE, 2).     % 女

%% 服务器主动断开连接原因
-define(SERVER_REJECT_LOGIN_AGAIN,				0 ) .		%% 账号在别处登录
-define(SERVER_REJECT_USE_HOOK,				    1 ) .		%% 使用外挂
-define(SERVER_REJECT_CM,					    2 ) .		%% 防沉迷
-define(SERVER_REJECT_CLOSURE,					3 ) .		%% 账号被封
-define(SERVER_REJECT_HEARTBEAT,				4 ) .		%% 心跳超时
-define(SERVER_REJECT_KICKOUT,					5 ) .		%% 踢玩家下线
-define(SERVER_REJECT_LINE_CLOSE,				6 ) .		%% 线路关闭

%% 身份信息错误原因
-define(AUTHENTICATION_NAME,			1 ) .		%% 名字错误
-define(AUTHENTICATION_ID,				2 ) .		%% 身份证错误

% 角色状态
-define(ROLE_STATE_STEP,                1)  .   %步行
-define(ROLE_STATE_RUN,                 2)  .   %骑乘
-define(ROLE_STATE_TRAIN,               3)  .   %修炼
-define(ROLE_STATE_HANG,                4)  .   %挂机


%% 体力MAX值
-define(ROLE_POWER_MAX,             200) .      
%% 体力购买次数
-define(ROLE_BUY_COUNT,             40) .   
%% 获得体力时间间隔/秒
-define(ROLE_GETPW_TIME,                1800 )  .

%% 玩家连接接登录状态
-define(ROLE_LOGIN_POLICY, 1).  % 等待安全沙箱请求
-define(ROLE_LOGIN_WAITING, 2). % 等待登录
-define(ROLE_LOGIN_FINISH, 3).  % 登录成功

%% 角色信息
-record(role, {
        id = 0,                 % 角色ID
        accname,                % 帐户名
        name = <<>>,            % 角色名
        sock = 0,               % 连接socket
        pid = undefined,        % 
        ref = 0,                % socket信息ref
        length = 0,             % socket数据长度
        client_port = 0,        % 客户端端口
        timeout = 0,            % socket超时次数
        login_state,            % 连接登录状态,参考LOGIN_STATE_XXX
        connect_lost = false,   % 连接是否中断
        reg_time = 0,
        last_login_time,
        last_login_ip,
        last_logout_time,
        gold = 0,
        total_payment = 0       % 总充值
    }).

%% 玩家缩略信息
-record(role_basic_info, {
        id, 
        name,
        last_login_time,
        last_logout_time
    }). 

%% 玩家基础属性值
-record(sys_base_attr, {
        id = 0,             % {lvl, job}
        str = 0,            % 力
        agi = 0,            % 敏
        phy = 0,            % 体
        wit = 0,            % 智
        hp = 0,             % 血
        mp = 0,             % 蓝
        att_p_min = 0,      % 最小物理攻击
        att_p_max = 0,      % 最大物理攻击
        att_m_min = 0,      % 最小魔法攻击
        att_m_max = 0       % 最大魔法攻击
    }).

%%-----------
%% 角色状态
%%-----------
%% 玩家状态信息
-record(role_status, {
        dead = false,       % 是否死亡
        battle = false,     % 是否处于战斗状态
        collection = false, % 是否处于采集状态
        switching = false,  % 是否切图中
		convoy = false,		% 是否护送中
		hiding = false,     % 是否隐身 
		use_magic = false,	% 是否禁魔
		
        %%  
        %% 权限
        %%  
        gm = false,         % 是否可使用gm指令
        chat = true,        % 是否可聊天
        move = true,        % 是否可移动
        use_skill = true,   % 是否可使用技能
        use_goods = true,   % 是否可使用物品

        %%  
        %% buff相关
        %%  
        unbeatable = false, % 是否无敌状态
        damaged_reflex = 0, % 受到伤害反射(0表示没有加成)
        mon_exp_add = 0,    % 打怪经验加成(0表示没有加成，100表示100%)
        att_add = 0,        % 攻击加成(最小最大都增加)
        speed_add = 0,      % 速度加成
        debuff_re = 0,      % debuff抗性
		def_dec = 0,		% 防御减免
		phy_dec = 0, 		% 破防
		seq_damage = 0, 	% 连斩加成
        dizzy_re = 0,       % 麻痹抗性
        rigor_re = 0,       % 僵直抗性
        exp_multi = {0, 0},      % 多重经验倍数
        star_multi = 0,      % 多重星灵倍数
        wal_multi = 0       % 世界平均等级buff加成倍数(0 表示无加成, 10000 表示 100%)
    }).

%%-------------
%% 快捷栏
%%-------------
%% 快捷栏最大值(从0开始)
-define(SHORTCUT_MAX, 9). 

%% 快捷类型
-define(SHORTCUT_TYPE_SKILL, 1). 
-define(SHORTCUT_TYPE_GOODS, 2). 

%%-----------
%% 关于cd
%%-----------
%% cd类型
-define(CD_TYPE_SKILL, 1).      % 技能
-define(CD_TYPE_GOODS, 2).      % 物品
-define(CD_TYPE_EXERCISE, 3).   % 修体
-define(CD_TYPE_STREN, 4).   	% 修体

%% 默认公共cd时间(ms)
-define(CD_PUBLIC_TIME, 1000).

%% 玩家cd信息(需要持久化)
-record(cd, {
        id,         % id为{type, subtype}
        type,       % cd类型
        subtype,    % 对应的id(当type为技能时对应技能id，
                    % 当为物品时为物品cd类型id，其它情况未指定)
        expire      % 过期时间(ms)
    }).

-endif. % ROLE_HRL
