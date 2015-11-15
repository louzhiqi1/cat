%%%----------------------------------------------------------------------------
%%%
%%% @doc serv_gm_ctrl对应头文件
%%% @author litaocheng
%%% @date 2012.10.23
%%%----------------------------------------------------------------------------
-ifndef(GM_CTRL_HRL).
-define(GM_CTRL_HRL, true).

%% 控制选项
-record(gm_ctrl,{
        db_id,          % 数据库id
        id,             % {ctrl_type, target}
        ctrl_type,      % 控制类型
        begin_time,     % 开始时间,
        end_time,       % 结束时间,
        target          % 禁IP存IP;禁角色存ID;防沉迷是否开启;内服号存玩家id
    }).

-endif.
