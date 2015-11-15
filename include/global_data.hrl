%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng@gmail.com
%%% @doc 定义全局数据保存对应的key，使用数字
%%%
%%%----------------------------------------------------------------------
-ifndef(GLOBAL_DATA_HRL).
-define(GLOBAL_DATA_HRL, true).

%% 定义global_data模块
-define(GLOBAL_DATA_RAM, (global_data:new(global_data_ram))).
-define(GLOBAL_DATA_DB, (global_data:new(global_data_db))).
-define(GLOBAL_DATA_DAILY, (global_data:new(global_data_daily))).

-endif.