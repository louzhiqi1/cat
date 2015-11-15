%%%----------------------------------------------------------------------
%%%
%%% @doc 协议编码
%%% @end
%%%
%%%----------------------------------------------------------------------
-ifndef(PACK_HRL).
-define(PACK_HRL, ok).

-define(PACK_TYPE_SINT,                 001).     % 整数
-define(PACK_TYPE_WINT,                 002).     % 整数
-define(PACK_TYPE_LINT,                 003).     % 整数

-define(PACK_TYPE_SLIST,                051).     % 短list
-define(PACK_TYPE_WLIST,                052).     % 中等列表(2个字节长度)
-define(PACK_TYPE_LLIST,                053).     % 长列表

-define(PACK_TYPE_SSTRING,              061).     % 字符串
-define(PACK_TYPE_WSTRING,              062).     % 字符串

-define(PACK_TYPE_RBIN,                 100).     % 原始的二进制
-define(PACK_TYPE_SBIN,                 101).     % 二进制
-define(PACK_TYPE_WBIN,                 102).     % 二进制
-define(PACK_TYPE_LBIN,                 103).     % 二进制

-define(PACK_TYPE_FLOAT,                151).     % 浮点数

-define(PACK_TYPE_TRUE,                 201).     % true 
-define(PACK_TYPE_FALSE,                202).     % false

-endif.
