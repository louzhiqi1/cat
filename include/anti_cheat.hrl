%%%----------------------------------------------------------------------
%%%
%%% @author: litaocheng
%%% @date: 2013-02-26
%%% @doc 防作弊相关头文件
%%%
%%%----------------------------------------------------------------------
-ifndef(ANTI_CHEAT_HRL).
-define(ANTI_CHEAT_HRL, true).

%% 作弊分数等级
-define(ANTI_CHEAT_SCORE_1, 1).     % 最低级
-define(ANTI_CHEAT_SCORE_2, 5).     % 第2级
-define(ANTI_CHEAT_SCORE_3, 10).    % 第3级
-define(ANTI_CHEAT_SCORE_4, 50).    % 第4级
-define(ANTI_CHEAT_SCORE_5, 100).   % 最高级

%% 作弊最大分数
-define(ANTI_CHEAT_SCORE_MAX, 300). 

-endif.
