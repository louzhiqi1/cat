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
-define(SERVER_REJECT_USE_HOOK,				1 ) .		%% 使用外挂
-define(SERVER_REJECT_CM,					2 ) .		%% 防沉迷
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
        name,                   % 角色名
        sex,                    % 性别 男:1  女: 0
        job,                    % 职业 1 剑士,2  枪手,3   医师
        lvl,                    % 角色等级
        exp,                    % 当前经验
        exp_sum,                % 总经验值
        mapkey,                 % 地图key
        x,                      % 地图坐标 
        y,                      % 地图坐标
        quad,                   % 在地图中的quad id

        silver,                 % 银币
        gold,                   % 金币
        coupon,                 % 礼券
        gold_give,              % 赠送的金币，计算vip时(gold_give + total_payment)
        vip,                    % vip等级
        total_payment,          % 总充值
        vigour_point,           % 体力值
        achieve_point,          % 成就点
        soul_point,             % 海魂值
        pk_mode,                % pk模式
        title = [],             % 当前使用的称号Id[]
        guild_id = 0,           % 工会id
        guild_name = <<>>,      % 工会名称
		love_id,				% 情侣id
		love_name,				% 情侣名字
		love_time,				% 结婚时间
		is_party,				% 是否参加婚宴
        skill,                  % 玩家skill信息
		talent,					% 玩家天赋技能信息
        buff,                   % 玩家buff信息
        bag_count,              % 玩家背包格子数
        store_count,            % 玩家仓库格子数
		sea_count,				% 海魂格子数
		soul_count,				% 魂力格子数
		hunter_count,			% 猎场格子数
        cd_list,                % 玩家cd列表
        setting,                % 玩家设置信息
        fight_point_max,        % 最大战斗力
        arena = [],             % 竞技场结果#arena_result
		star,					% 星级
		star_lvl,				% 占星大境界
		star_point,				% 星灵
        repu,                   % 声望
		garden,					% 果园
		land_pos,				% 提升土地位置
		seed_count,				% 种子数量
		seed_color,				% 种子颜色
		seq_kill,				% 连斩最大次数
		friend_count,			% 好友数
		turnjob,				% 转职等级

        pet_id = 0,             % 宠物id
        pet_model_id = 0,       % 宠物模板id
        pet_name = <<>>,        % 宠物名字
        pet_quality_type = 0,   % 宠物品质
        pet_lvl = 0,            % 宠物等级
              
        str_grow,               % 力量培养
        agi_grow,               % 敏捷培养
        phy_grow,               % 体质培养
        wit_grow,               % 智力培养

        reg_time,               % 注册时间
        last_login_time,        % 上次登录时间(sec)
        last_logout_time,       % 上次登出时间(sec)
        last_login_ip,          % 最后登录ip
        total_online_time,      % 累计在线时长(sec)
        
        status,                 % 玩家状态#role_status
        group = 0,              % 玩家临时分组(战场使用)
        other,                  % 其他数据

        %%
        %% 客户端相关数据
        %%
        sock = 0,               % 连接socket
        ref = 0,                % socket信息ref
        length = 0,             % socket数据长度
        client_port = 0,        % 客户端端口
        timeout = 0,            % socket超时次数
        pid = 0,                % 进程ID
        login_state,            % 连接登录状态,参考LOGIN_STATE_XXX
        connect_lost = false,   % 连接是否中断

        %%
        %% 其他数据
        %%
        cm = ?CM_TYPE_UNREG,    % 0 已经登记信息但没有满18岁（未通过防沉迷）
                                % 1 已经登记信息并且已经满18岁（已通过防沉迷）
                                % 2 没有登录信息（未通过防沉迷，应该提醒用户填写）
        pk_rolename = 1,        % 玩家名字颜色(1:白名,2:灰名,3:黄名,4:红名玩家)
        pk_point = 0,           % pk点数(杀戮值)
        fight_point,            % 当前战斗力
        lock_time,              % 玩家添加锁定的时间点(维护玩家数据使用)

        %%
        %% 玩家形象相关的数据
        %%
        weapon = 0,         % 武器类型id
        weapon_stren = 0,   % 武器强化等级
        clothes = 0,        % 衣服类型id
        clothes_stren = 0,  % 衣服强化等级
        wing = 0,           % 翅膀类型id
        wing_stren = 0,     % 翅膀强化等级 
        fashion = 0,        % 时装类型id
        suit_id = 0,  		% 橙装id
        fashion_wing_set = 0,   % 时装、翅膀显示设置(二进制形式:00表示都不显示,01表示只显示翅膀,10表示只显示时装,11表示两个都显示)
        zazen,              % 打坐#role_zazen
        
        %% 队伍相关
        team_id = 0,        % 队伍id
        team_leader = 0,    % 队长id
        
        %% 鲜花
        flower = 0,         % 接收鲜花数量
        charm = 0,          % 魅力值
        goal_list = [],     % 目标系统的信息
        
        %% 占卜密室
        backroom_exp = 0,         % 水晶当前密室经验
        backroom_exp_sum = 0,     % 水晶密室总经验值
        backroom_lvl = 1,         % 水晶密室等级
        talent_point = 0,       % 天赋点
        tough = 0,             % 历练值
		%% 战队
		clan_id = 0,         % 战队id
		clan_leader =0       % 战队队长

			  
    }).

%% 玩家缩略信息
-record(role_basic_info, {
        id, 
        name,
        lvl,
        sex,
        job,
        vip,
        repu,
        flower,
        title,
        last_login_time,
        last_logout_time,
        expire          % 玩家的信息一旦被查询, 就会更新该值(只在缓冲区模块用到)
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
-define(ROLE_STATE_LIVE, 1).        % 正常
-define(ROLE_STATE_DEAD, 2).        % 死亡
-define(ROLE_STATE_REST, 3).        % 打坐
-define(ROLE_STATE_COLLECTION, 4).  % 采集
-define(ROLE_STATE_HIDING, 5).  % 隐身

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

%% 打坐
-define(ZAZEN_NONE, 0).                 %未打坐
-define(ZAZEN_SINGLE, 1).               %单修
-define(ZAZEN_COUPLE, 2).               %同性双修
-define(ZAZEN_COUPLE_OPPOSITE_SEX, 3).  %异性双修
-define(ZAZEN_COUPLE_AUTO_ASSENT_N, 0). %不同意
-define(ZAZEN_COUPLE_AUTO_ASSENT_Y, 1). %自动同意
-record(role_zazen, {
        partner_id = 0,         % 双修对方id,单休是为0
        status = 0,             % 玩家打坐状态(0不在打坐；1单人打坐；2 同性双修；3 异性双修)
        auto_assent_couple = 1  % 自动同意双修(0不同意；1同意)
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

%% 公共cd是一种subtype
-define(CD_TYPE_PUBLIC, 0). % 公共

%% 物品cd类型id
-define(CD_GOODS_RECOVER_WINK_HP, 1). % 血气瞬加恢复类
-define(CD_GOODS_RECOVER_LAST_HP, 2). % 血气持续恢复类
-define(CD_GOODS_RECOVER_WINK_MP, 3). % 魂气瞬加恢复类
-define(CD_GOODS_RECOVER_LAST_MP, 4). % 魂气持续恢复类

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


%%-------------------------------------------
%% 用户设置
%% 类型0-5对应的数据为一个字符串(客户端解析)
%%-------------------------------------------
%% 设置类型列表
-define(SETTING_TYPE_LIST, [0, 1, 2, 3, 4, 5]).
%% 设置的最大长度(1k)
-define(SETTING_MAX_SIZE, 1024).

%% 时装，翅膀设置标识
-define(SETTING_KEY_FASHION_WING, <<"fashion_and_wing">>).

%% pk模式切换回和平模式的CD,30分钟
-define(PK_MODE_CHANGE_CD, 1800).  
-define(PK_MODE_CHANGE_CD_KEY(Rid), {Rid, pk_mode_change_cd_key}).


-endif. % ROLE_HRL
