-ifndef(__ECODE_HRL__).
-define(__ECODE_HRL__, true).

-define(E_OK, 0).
-define(E_PAYMENT, 0).

-define(E_ROLE_ACCNAME_LONG, 200). %帐号名太长
-define(E_ROLE_NAME_SHORT, 201). %角色名太短
-define(E_ROLE_NAME_LONG, 202). %角色名太长
-define(E_ROLE_NAME_EXISTS, 203). %角色名被使用
-define(E_ROLE_RECREATE, 204). %重复创建角色
-define(E_ROLE_OFFLINE, 205). %玩家不在线
-define(E_ROLE_SEX_INVALID, 219). %性别不合法
-define(E_ROLE_NEXIST, 211). % 玩家不存在
-define(E_TRAFFIC_PACKET_LIMIT, 29). %玩家发包数目过多
-define(E_UNKONW_DATA, 2). %错误的请求
-define(E_TRAFFIC_BYTE_LIMIT, 30). %玩家发包流量过大
-define(E_BADARG, 12).

-endif.