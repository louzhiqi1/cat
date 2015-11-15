%%%----------------------------------------------------------------------
%%%
%%% @author: litaocheng
%%% @date: 2012-06-29
%%% @doc 玩家计数器头文件定义
%%%
%%%----------------------------------------------------------------------
-ifndef(COUNTER_HRL).
-define(COUNTER_HRL, true).

%% 玩家个人counter 类型
-define(COUNTER_TYPE_DB, db).
-define(COUNTER_TYPE_RAM, ram).
-define(COUNTER_TYPE_DAILY, daily).

%% 存储到mysql中
-define(COUNTER_DB, (role_counter:new(?COUNTER_TYPE_DB))).
%% 存储到global data ram中
-define(COUNTER_RAM, (role_counter:new(?COUNTER_TYPE_RAM))).
%% 存储在global data daily中
-define(COUNTER_DAILY, (role_counter:new(?COUNTER_TYPE_DAILY))).

%% global daily中每个玩家一个counter list
-define(ROLE_COUNTER_DAILY_KEY(Id), {role_counter, Id}).
%% global data ram中为每个玩家存储一个counter list
-define(ROLE_COUNTER_RAM_KEY(Id), {role_counter, Id}).

%% 玩家清除副本计数的次数(现在每天只能一次)
-define(DUNGEON_COUNT_DEC_KEY(Id), {'$dungeon_count_dec_day', Id}).


%%-------------------
%% 防外挂相关(1000)
%%-------------------
-define(COUNTER_ANTI_CHEAT_SCORE, 1000).    % 防外挂得分
-define(COUNTER_ANTI_CHEAT_XRANGE, 1001).   % 防外挂记录的x总距离
-define(COUNTER_ANTI_CHEAT_YRANGE, 1002).   % 防外挂记录的y总距离
-define(COUNTER_ANTI_CHEAT_XTIME, 1003).    % 防外挂记录的x总时间
-define(COUNTER_ANTI_CHEAT_LOCK_TIMES, 1004).%被锁定次数

-endif.