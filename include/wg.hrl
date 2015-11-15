%%%----------------------------------------------------------------------
%%%
%%% @copyright wg
%%%
%%% @author songze.me@gmail.com
%%% @doc wg header file used by user
%%%
%%%----------------------------------------------------------------------
-ifndef(WG_HRL).
-define(WG_HRL, true).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("wg_log.hrl").

%% Print in standard output
-define(PRINT(Format, Args),
    io:format(Format, Args)).

%% syntax similar with ?: in c
-ifndef(IF).
-define(IF(C, T, F), (case (C) of true -> (T); false -> (F) end)).
-endif.

%% 根据key获取val
-define(KV_GET(K, L), proplists:get_value(K, L)).
-define(KV_GET(K, L, Def), proplists:get_value(K, L, Def)).

%% some convert macros
-define(B2S(B), (binary_to_list(B))).
-define(S2B(S), (list_to_binary(S))).
-define(N2S(N), integer_to_list(N)).
-define(S2N(S), list_to_integer(S)).
-define(N2B(N), ?S2B(integer_to_list(N))).
-define(B2N(B), list_to_integer(?B2S(B))).

-define(A2S(A), atom_to_list(A)).
-define(S2A(S), list_to_atom(S)).
-define(S2EA(S), list_to_existing_atom(S)).

%% 断言
%%-ifdef(TEST).
-define(ASSERT(Con), (
        case Con of
            true ->
                ok;
            false ->
                error({assert, ??Con})
        end)).
%%-else.
%%-define(ASSERT(Con), ok).
%%-endif.

%%
%% 当进行dialyzer时用来屏蔽ets concurrency_read属性
%%
%%-ifdef(DIALYZER).
%-define(ETS_CONCURRENCY, {write_concurrency, true}).
-define(ETS_CONCURRENCY, {read_concurrency, true}).
%-else.
%-define(ETS_CONCURRENCY, {read_concurrency, true}, {write_concurrency, true}).
%-endif.

%% wg_config_dynamic的模块名
-define(MOD_DYNAMIC(Mod), (list_to_atom(atom_to_list(Mod) ++ "_dynamic"))).

%% 退出
-define(EXIT(C), timer:sleep(300), init:stop(C)).

%% proplists中获取某个key对应value
-define(PLIST_VAL(Key, List), proplists:get_value(Key, List)).
-define(PLIST_VAL(Key, List, Default), proplists:get_value(Key, List, Default)).

%% FLASH安全沙箱请求
-define(FLASH_POLICY_REQ, <<"<policy-file-request/>", 0>>).
%% FLASH安全沙箱应答
-define(FLASH_POLICY_ACK, 
    <<"<cross-domain-policy><allow-access-from domain='*' to-ports='*' /></cross-domain-policy>", 0>>).

-endif. % WG_HRL
