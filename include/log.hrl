%%%----------------------------------------------------------------------
%%%
%%% @author zhengh
%%% @date  2012.10.09
%%% @doc 日志定义
%%% @end
%%%
%%%----------------------------------------------------------------------
-ifndef(LOG_HRL).
-define(LOG_HRL, true).

%% 注册角色表
-define(LOG_REGISTER, 'log_register').

%% 日志列表
-define(LOG_TYPE_LIST, 
        [
        ]).

%% 退出类型
-define(LOG_LOGOUT_TYPE_NORMAL, 0).                 % 正常退出
-define(LOG_LOGOUT_TYPE_UNKNOWN,1).                 % 未知类型
-define(LOG_LOGOUT_TYPE_HEART_TIMEOUT, 2).          % 心跳超时
-define(LOG_LOGOUT_TYPE_HEART_QUICK, 3).            % 心跳过快
-define(LOG_LOGOUT_TYPE_OVERFLOW, 4).               % 流量过大
-define(LOG_LOGOUT_TYPE_WATING_ACTIVE, 5).          % 等待激活时退出
-define(LOG_LOGOUT_TYPE_INET_ASYNC, 6).             % async出错
-define(LOG_LOGOUT_TYPE_SOCKET_EXIT, 7).            % socket exit
-define(LOG_LOGOUT_TYPE_RELOGIN, 9).                % 重登录原进程退出
-define(LOG_LOGOUT_TYPE_TCP_ERROR, 10).             % tcp 出错
-define(LOG_LOGOUT_TYPE_CONNECT_LOST, 11).          % 丢失连接
-define(LOG_LOGOUT_TYPE_ANTI_CHEAT, 12).            % 反作弊
-define(LOG_LOGOUT_TYPE_TCP_TIMEOUT, 13).           % tcp timeout


%%------------
%% 物品相关
%%------------
%% 物品获得类型
-define(LOG_EARN_GOODS_TYPE_UNKNOWN, 10000).            % 未知类型
-define(LOG_EARN_GOODS_TYPE_DUNGEON_REWARD, 10001).     % 副本奖励
-define(LOG_EARN_GOODS_TYPE_USE_GIFTBAG, 10002).        % 使用物品(礼包)
-define(LOG_EARN_GOODS_TYPE_BUY, 10003).                % 购买所得
-define(LOG_EARN_GOODS_TYPE_MAIL_ATTACH, 10004).        % 邮件附件
-define(LOG_EARN_GOODS_TYPE_SYNTHETIZE, 10005).         % 合成
-define(LOG_EARN_GOODS_TYPE_REMOVE, 10006).             % 摘除
-define(LOG_EARN_GOODS_TYPE_TASK_REWARD, 10007).        % 任务奖励
-define(LOG_EARN_GOODS_TYPE_TASK_ITEM, 10008).          % 任务道具
-define(LOG_EARN_GOODS_TYPE_GIFTBAG, 10009).            % 礼包奖励
-define(LOG_EARN_GOODS_TYPE_PICK_DROP, 10010).          % 捡取掉落物
-define(LOG_EARN_GOODS_TYPE_TIDY, 10011).               % 背包整理
-define(LOG_EARN_GOODS_TYPE_SPLIT, 10012).              % 背包拆分
-define(LOG_EARN_GOODS_TYPE_GM_COMMAND, 10015).         % GM命令
-define(LOG_EARN_GOODS_TYPE_EXCHANGE, 10016).         	% 兑换
-define(LOG_EARN_GOODS_TYPE_GIFT, 10017).         		% 礼包
-define(LOG_EARN_GOODS_TYPE_MESTRY_TRADER, 10018).      % 神秘商店
-define(LOG_EARN_GOODS_TYPE_VITALITY, 10019).           % 活跃度
-define(LOG_EARN_GOODS_TYPE_NEW_YEAR_GIFT, 10020).      % 元旦活动在线礼包
-define(LOG_EARN_GOODS_TYPE_MAZE, 10021).               % 迷宫
-define(LOG_EARN_GOODS_TYPE_COLLECTION, 10022).         % 采集所得
% -define(LOG_EARN_GOODS_TYPE_ENTRUST, 10023).         % 委托
-define(LOG_EARN_GOODS_TYPE_TIMER_SUIT, 10024).         % 限时橙装
-define(LOG_EARN_GOODS_TYPE_GUILD_WELFARE, 10025).      % 公会活动福利奖励
-define(LOG_EARN_GOODS_TYPE_MARRY_YH_GET, 10026).      % 结婚使用烟花所得
-define(LOG_EARN_GOODS_TYPE_OPEN_ACTIVE_GET, 10027).      % 充值开服活动
-define(LOG_EARN_GOODS_TYPE_GODDESS_GIFT, 10028).      % 女神恩赐
-define(LOG_EARN_GOODS_TYPE_WAND_PRIZE, 10029).      % 幸运魔杖

%% 物品消耗类型
-define(LOG_COST_GOODS_TYPE_UNKNOWN, 20000).            % 未知类型
-define(LOG_COST_GOODS_TYPE_USE, 20001).                % 使用
-define(LOG_COST_GOODS_TYPE_DESTROY, 20002).            % 销毁丢弃
-define(LOG_COST_GOODS_TYPE_SELL, 20003).               % 出售
-define(LOG_COST_GOODS_TYPE_SYNTHETIZE, 20004).         % 合成
-define(LOG_COST_GOODS_TYPE_TIDY, 20005).               % 背包整理
-define(LOG_COST_GOODS_TYPE_TASK_ITEM, 20006).          % 任务道具
-define(LOG_COST_GOODS_TYPE_CREATE_GUILD, 20007).       % 创建公会
-define(LOG_COST_GOODS_TYPE_GEM, 20008).                % 宝石合成
-define(LOG_COST_GOODS_TYPE_PET_ATTR, 20009).           % 宠物成长
-define(LOG_COST_GOODS_TYPE_LOOP_TASK, 20011).          % 提升系数
-define(LOG_COST_GOODS_TYPE_RECRUIT_TASK, 20012).       % 悬赏任务
-define(LOG_COST_GOODS_TYPE_STAR, 20013).        		% 占星
-define(LOG_COST_GOODS_TYPE_SOUL_CONPOSE, 20014).       % 封印合成
-define(LOG_COST_GOODS_TYPE_SOUL_SHOP, 20015).       	% 出售封印
-define(LOG_COST_GOODS_TYPE_SOUL_PUTON, 20016).       	% 装备封印
-define(LOG_COST_GOODS_TYPE_SOUL_DOWN, 20017).       	% 卸下封印
-define(LOG_COST_GOODS_TYPE_EXCHANGE, 20018).       	% 兑换
-define(LOG_COST_GOODS_TYPE_MESTRY_TRADER_REFRESH, 20019). % 神秘商店刷新
-define(LOG_COST_GOODS_TYPE_GARDEN_OPEN_LAND, 20020).      % 果园开垦土地

%%------------
%% 金币相关
%%------------
%% 金币获得类型
-define(LOG_EARN_GOLD_TYPE_UNKNOWN, 10000).            % 后台加元宝
-define(LOG_EARN_GOLD_TYPE_DUNGEON_REWARD, 10001).     % 副本奖励
-define(LOG_EARN_GOLD_TYPE_USE_GOODS, 10002).          % 使用物品
-define(LOG_EARN_GOLD_TYPE_PAYMENT, 10003).            % 充值
-define(LOG_EARN_GOLD_TYPE_MAIL_ATTACH, 10004).        % 邮件附件
-define(LOG_EARN_GOLD_TYPE_TASK_REWARD, 10006).        % 任务奖励
-define(LOG_EARN_GOLD_TYPE_GM_COMMAND, 10010).         % GM命令加
-define(LOG_COST_GOLD_TYPE_WEEK_ONLINE_REWARD_12, 10080).	% 周在线奖励
-define(LOG_COST_GOLD_TYPE_WEEK_ONLINE_REWARD_48, 10081).	% 周在线奖励
-define(LOG_COST_GOLD_TYPE_WEEK_ONLINE_REWARD_88, 10082).	% 周在线奖励
-define(LOG_EARN_GOLD_TYPE_GIFTBAG, 10007).            % 礼包奖励
-define(LOG_EARN_GOLD_TYPE_WAND_PRIZE, 10008).            % 幸运魔杖获奖
-define(LOG_EARN_GOLD_TYPE_INVEST, 10009).            % 投资计划奖励

%% 金币消耗类型
-define(LOG_COST_GOLD_TYPE_UNKNOWN, 20000).             % 后台去元宝
-define(LOG_COST_GOLD_TYPE_BUY_SILVER, 20001).          % 购买银币
-define(LOG_COST_GOLD_TYPE_BUY_VIGOUR, 20002).          % 购买体力
-define(LOG_COST_GOLD_TYPE_RESET_FLOOR_LAYER, 20003).   % 重置无限关卡
-define(LOG_COST_GOLD_TYPE_BAG_EXTEND, 20004).          % 扩展背包
-define(LOG_COST_GOLD_TYPE_STORAGE_EXTEND, 20005).      % 扩展仓库
-define(LOG_COST_GOLD_TYPE_BUY, 20006).                 % 购买物品
-define(LOG_COST_GOLD_TYPE_GROWATTR, 20007).            % 属性培养
-define(LOG_COST_GOLD_TYPE_SEA_EXTEND, 20008).          % 扩展海魂格子
-define(LOG_COST_GOLD_TYPE_CALL_HUNTER, 20009).         % 召唤海魂师
-define(LOG_COST_GOLD_TYPE_SYNTHETIZE, 20010).          % 装备合成筛选
-define(LOG_COST_GOLD_TYPE_CHAT_HORN, 20011).           % 刷喇叭
-define(LOG_COST_GOLD_TYPE_REVIVE_FULL, 20012).         % 满血复活
-define(LOG_COST_GOLD_TYPE_STAR_TIME, 20013).           % 静室续费
-define(LOG_COST_GOLD_TYPE_ENCOURAGE_STRONG, 20014).    % 世界boss（背水一战）
-define(LOG_COST_GOLD_TYPE_ENCOURAGE_GOLD, 20015).      % 世界boss（金币鼓舞）
-define(LOG_COST_GOLD_TYPE_GM_COMMAND, 20020).          % GM命令减
-define(LOG_COST_GOLD_TYPE_HERO_ADD_CHALLENGE, 20030).  % 英雄榜增加挑战次数
-define(LOG_COST_GOLD_TYPE_HERO_CLEAN_CD, 20031).       % 英雄榜清除挑战CD
-define(LOG_COST_GOLD_TYPE_PET_GROW, 20041).            % 宠物成长
-define(LOG_COST_GOLD_TYPE_PET_POTENT, 20042).          % 宠物潜能
-define(LOG_COST_GOLD_TYPE_TASK_FINISH_NOW, 20050).     % 任务立即完成
-define(LOG_COST_GOLD_TYPE_RECRUIT_REFRESH, 20051).     % 金币刷新悬赏榜
-define(LOG_COST_GOLD_TYPE_LOOP_BASE, 20052).     % 提升循环系数
-define(LOG_COST_GOLD_TYPE_OFFLINE_REWARD_DOUBLE, 20060).      % 离线奖励(双)
-define(LOG_COST_GOLD_TYPE_OFFLINE_REWARD_TREBLE, 20061).% 离线奖励(三)
-define(LOG_COST_GOLD_TYPE_OFFLINE_REWARD_TASK, 20062).% 任务离线奖励
-define(LOG_COST_GOLD_TYPE_OFFLINE_REWARD_MAP, 20063).% 副本离线奖励
-define(LOG_COST_GOLD_TYPE_PRAY_GODDESS_FLOWER, 20070). % 祈祷1
-define(LOG_COST_GOLD_TYPE_PRAY_GODDESS_GEM, 20071).    % 祈祷2
-define(LOG_COST_GOLD_TYPE_PRESENT_FLOWER_1, 20083).     % 送花类型1
-define(LOG_COST_GOLD_TYPE_PRESENT_FLOWER_2, 20084).     % 送花类型2
-define(LOG_COST_GOLD_TYPE_PRESENT_FLOWER_3, 20085).     % 送花类型3
-define(LOG_COST_GOLD_TYPE_GARDEN_OPEN_LAND, 20090).	% 果园开垦土地
-define(LOG_COST_GOLD_TYPE_GARDEN_UP_LAND_COLOR, 20091).	% 果园提升土地品质
-define(LOG_COST_GOLD_TYPE_GARDEN_SHOP_LAND, 20092).	% 果园购买种子
-define(LOG_COST_GOLD_TYPE_GARDEN_CLEAR_COOL, 20093).	% 果园清除CD
-define(LOG_COST_GOLD_TYPE_GARDEN_LAND_FULL, 20094).	% 果园神话种子
-define(LOG_COST_GOLD_TYPE_GARDEN_LAND_REFRESH, 20095).	% 果园刷种子品质
-define(LOG_COST_GOLD_GOODS_RENEW, 20100).	% 时装续费
-define(LOG_COST_GOLD_CONVOY_VIP_REFRESH, 20110).	% 护送VIP满级
-define(LOG_COST_GOLD_CONVOY_NORMAL_REFRESH, 20111).	% 护送刷品质
-define(LOG_COST_GOLD_REFRESH_MESTRY, 20112).       % 刷新神秘商店
-define(LOG_COST_GOLD_TYPE_OPEN_STAR, 20113).       % 开启静室
-define(LOG_COST_GOLD_TYPE_GUILD_DOGZ_PRAY, 20115).     % 公会神兽祝福
-define(LOG_COST_GOLD_TYPE_DUEL_ENCOURAGE_GOLD, 20116).      % 勇者对决（金币鼓舞）
-define(LOG_COST_GOLD_TYPE_GUILD_BOSS_ENCOURAGE_STRONG, 20117).    % 公会boss（背水一战）
-define(LOG_COST_GOLD_TYPE_GUILD_BOSS_ENCOURAGE_GOLD, 20118).      % 公会boss（金币鼓舞）
-define(LOG_COST_GOLD_TYPE_EQUIP_POLISH, 20119).      % 装备洗练
-define(LOG_COST_GOLD_TYPE_FIREWORKS_CD, 20120).                   % 参加婚礼放彩带
-define(LOG_COST_GOLD_TYPE_EQUIP_POLISH_ACTIVATE, 20121).      % 装备洗练激活
-define(LOG_COST_GOLD_TYPE_MARRY_PRAY, 20122).      % 婚礼祝福
-define(LOG_COST_GOLD_TYPE_MARRY_PARTY, 20123).      % 婚宴
-define(LOG_COST_GOLD_TYPE_FORCE_DIVORCE, 20124).        % 强制离婚
-define(LOG_COST_GOLD_TYPE_PLUG_TRICK, 20125).        % 使坏
-define(LOG_COST_GOLD_TYPE_BATCH_FINISH_TASK, 20126).        % 委托
-define(LOG_COST_GOLD_TYPE_MAZE_TIPS, 20127).          % 迷宫提示
-define(LOG_COST_GOLD_TYPE_GARDEN_BATCH_PLANT, 20128).  % 批量种植
-define(LOG_COST_GOLD_TYPE_BOSS_AGENCY_GOLD, 20129).    % 世界BOSS活动委托
-define(LOG_COST_GOLD_TYPE_BUY_MESTRY, 20130).    % 神秘商店金币购买物品
-define(LOG_COST_GOLD_TYPE_VIP_MALL_GIVE, 20131).   %商城赠送物品
-define(LOG_COST_GOLD_GUILD_IMPEACH, 20132).    % 公会弹劾
-define(LOG_COST_GOLD_TYPE_DIVINE_CHANGE, 20133).   % 占卜密室改命
-define(LOG_COST_GOLD_WAND_DRAW, 20134).    % 幸运魔杖耗费
-define(LOG_COST_GOLD_TYPE_CREATE_CLAN, 20135).    % 战队组建


%%------------
%% 银币相关
%%------------
%% 银币获得类型
-define(LOG_EARN_SILVER_TYPE_UNKNOWN, 50000).           % 未知类型
-define(LOG_EARN_SILVER_TYPE_DUNGEON_REWARD, 50001).    % 副本奖励
-define(LOG_EARN_SILVER_TYPE_USE_GOODS, 50002).         % 使用物品
-define(LOG_EARN_SILVER_TYPE_SELL_GOODS, 50003).        % 出售物品
-define(LOG_EARN_SILVER_TYPE_MAIL_ATTACH, 50004).       % 邮件附件
-define(LOG_EARN_SILVER_TYPE_SELL_SOUL, 50005).         % 出售海魂
-define(LOG_EARN_SILVER_TYPE_TASK_REWARD, 50006).       % 任务奖励
-define(LOG_EARN_SILVER_TYPE_VIP_BUY, 50007).           % vip购买所得
-define(LOG_EARN_SILVER_TYPE_GIFTBAG, 50008).           % 礼包奖励
-define(LOG_EARN_SILVER_TYPE_PICK_DROP, 50009).         % 捡取掉落物
-define(LOG_EARN_SILVER_TYPE_WORLD_BOSS, 50010).        % 世界BOSS单次伤害
-define(LOG_EARN_SILVER_TYPE_WORLD_BOSS_LHIT, 50011).   % 世界boss最后一击奖励
-define(LOG_EARN_SILVER_TYPE_WORLD_BOSS_TOP3, 50012).   % 世界BOSS前三名奖励
-define(LOG_EARN_SILVER_TYPE_GM_COMMAND, 50015).        % GM命令
-define(LOG_EARN_SILVER_TYPE_HERO_REWARD, 50020).       % 英雄榜发放奖励
-define(LOG_EARN_SILVER_GARDEN_REWARD, 50021).       % 果园种植
-define(LOG_EARN_SILVER_CONVOY_REWARD, 50030).       % 护送奖励
-define(LOG_EARN_SILVER_TYPE_GUILD_BOSS, 50031).        % 公会BOSS单次伤害
-define(LOG_EARN_SILVER_TYPE_GUILD_BOSS_LHIT, 50032).   % 公会boss最后一击奖励
-define(LOG_EARN_SILVER_TYPE_GUILD_BOSS_TOP3, 50033).   % 公会BOSS前三名奖励
-define(LOG_EARN_SILVER_TYPE_MAZE_REWARD, 50034).    % 迷宫奖励
-define(LOG_EARN_SILVER_TYPE_ENTRUST_REWARD, 50035).    % 委托奖励
-define(LOG_EARN_SILVER_TYPE_BOSS_AGENCY, 50036).       % 世界BOSS活动委托
-define(LOG_EARN_SILVER_TYPE_WAND_PRIZE, 50037).       % 幸运魔杖获奖

%% 银币消耗类型
-define(LOG_COST_SILVER_TYPE_UNKNOWN, 60000).            % 未知类型
-define(LOG_COST_SILVER_TYPE_BUY, 60001).                % 购买物品
-define(LOG_COST_SILVER_TYPE_GROWATTR, 60002).           % 属性培养
-define(LOG_COST_SILVER_TYPE_CREATE_GUILD, 60003).       % 创建公会  
-define(LOG_COST_SILVER_TYPE_HUNT_SOUL, 60004).          % 猎魂
-define(LOG_COST_SILVER_TYPE_LEARN_SKILL, 60005).        % 技能学习
-define(LOG_COST_SILVER_TYPE_EQUIP_STREN, 60006).        % 装备强化
-define(LOG_COST_SILVER_TYPE_SYNTHETIZE, 60007).         % 宝石合成
-define(LOG_COST_SILVER_TYPE_EQUIP_POLISH, 60008).        % 装备洗练
-define(LOG_COST_SILVER_TYPE_GM_COMMAND, 60010).         % GM命令
-define(LOG_COST_SILVER_TYPE_BUY_MESTRY, 60011).         % 购买物品
-define(LOG_COST_SILVER_TYPE_PRAY_GODDESS_FRUIT, 60012). % 祈祷
-define(LOG_COST_SILVER_TYPE_EXCHANGE, 60013).           % 兑换
-define(LOG_COST_SILVER_GUILD_WAR, 60014).               % 工会战报名
-define(LOG_COST_SILVER_TYPE_GUILD_DOGZ_PRAY, 60015).    % 公会神兽祝福
-define(LOG_COST_SILVER_TYPE_INVIT_MARRY, 60016).        % 邀请结婚
-define(LOG_COST_SILVER_TYPE_DIVORCE_MARRY, 60017).        % 邀请离婚
-define(LOG_COST_SILVER_TYPE_FIREWORKS_YH, 60019).       % 婚礼放烟花
-define(LOG_COST_SILVER_TYPE_MARRY_PRAY, 60020).      % 婚礼祝福
-define(LOG_COST_SILVER_TYPE_LEARN_TALENT, 60021).      % 天赋学习

%%------------
%% 礼券相关
%%------------
%% 礼券获得类型
-define(LOG_EARN_COUPON_TYPE_UNKNOWN, 70000).           % 未知类型
-define(LOG_EARN_COUPON_TYPE_GM_COMMAND, 70001).        % GM命令
-define(LOG_EARN_COUPON_TYPE_USE_GOODS, 70002).         % 使用物品

%% 礼券消耗类型
-define(LOG_COST_COUPON_TYPE_UNKNOWN, 80000).            % 未知类型
-define(LOG_COST_COUPON_TYPE_GM_COMMAND, 80001).         % GM命令
-define(LOG_COST_COUPON_TYPE_BUY, 80002).                % 购买物品

%%-----------
%% 魅力相关
%%-----------
-define(LOG_CHARM_TYPE_RECV_FLOWER, 90000).     % 收花
-define(LOG_CHARM_TYPE_SEND_FLOWER, 90001).     % 送花
-define(LOG_CHARM_TYPE_GM, 90002).              % GM命令

%%-----------------
%% 玩法相关
%%-----------------
-define(LOG_FUN_TYPE_DUNGEON, 10000).       % 副本
-define(LOG_FUN_TYPE_WORLD_BOSS, 10001).    % 世界boss(怒海之神)
-define(LOG_FUN_TYPE_LD, 10002).            % 冒险乱斗
-define(LOG_FUN_TYPE_ARENA, 10003).         % 竞技场
-define(LOG_FUN_TYPE_STAR, 10004).          % 星宫遗迹
-define(LOG_FUN_TYPE_TASK_LOOP, 10010).     % 循环任务
-define(LOG_FUN_TYPE_TASK_RECRUIT, 10011).  % 悬赏任务
-define(LOG_FUN_TYPE_HEROS, 10020).         % 英雄榜挑战
-define(LOG_FUN_TYPE_CONVOY, 10021).        % 护送
-define(LOG_FUN_TYPE_QUIZ, 10022).          % 冒险问答报名
-define(LOG_FUN_TYPE_PLANT_SEED, 10023).    % 种植(每天种植的记录)
-define(LOG_FUN_TYPE_ACCEPT_WEEK_TASK, 10024).    % 周环接取次数
-define(LOG_FUN_TYPE_FINISH_WEEK_TASK, 10025).    % 周环完成次数
-define(LOG_FUN_TYPE_DUEL, 10026).          % 勇者对决
-define(LOG_FUN_TYPE_MAZE, 10027).          % 迷宫
-define(LOG_FUN_TYPE_PLUG, 10028).          % 博饼
-define(LOG_FUN_TYPE_WAND, 10029).          % 幸运魔杖
%%-------------
%% 商城相关
%%-------------
-define(LOG_MALL_TYPE_NEW_HOT_GOODS, 0).        % 新品热卖
-define(LOG_MALL_TYPE_NORMAL_GOODS, 1).         % 日常用品
-define(LOG_MALL_TYPE_GEM_MATERIAL_GOODS, 2).   % 宝石材料
-define(LOG_MALL_TYPE_PET_GOODS, 3).            % 宠物相关
-define(LOG_MALL_TYPE_DRESS_GOODS, 4).          % 形象装扮
-define(LOG_MALL_TYPE_SILVER_AREA_GOODS, 5).    % 银币专区
-define(LOG_MALL_TYPE_COUPON_AREA_GOODS, 6).    % 礼券专区
-define(LOG_MALL_TYPE_FASTBUG_GOODS, 10).       % 快速购买
-define(LOG_MALL_TYPE_HOT_GOODS, 11).           % 热卖
-define(LOG_MALL_TYPE_LIMIT_GOODS, 12).         % 限购

%%------------
%% 切图类型
%%------------
-define(LOG_MAP_SWITCH_REQUEST, 1).             % 主动请求
-define(LOG_MAP_SWITCH_REVIVE, 2).              % 复活切图
-define(LOG_MAP_SWITCH_FLY_SHOES, 3).           % 使用小飞鞋
-define(LOG_MAP_SWITCH_FLY_SHOES_FREE, 4).      % 20级以下免费小飞鞋
-define(LOG_MAP_SWITCH_DUNGEON_OUT, 5).         % 从副本传出
-define(LOG_MAP_SWITCH_CONVOY_BACK, 6).         % 继续护送
-define(LOG_MAP_SWITCH_PVP_LD, 7).              % 进入乱斗
-define(LOG_MAP_SWITCH_GUILD_WAR, 8).           % 进入公会战
-define(LOG_MAP_SWITCH_GM, 9).                  % GM命令切图
-define(LOG_MAP_SWITCH_COUPLE_RECALL, 10).      % 夫妻召唤
-define(LOG_MAP_SWITCH_MAZE, 11).               % 迷宫切换

-endif.
