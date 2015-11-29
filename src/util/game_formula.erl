%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date  2012.07.12
%%% @doc 游戏相关数值的计算公式
%%%
%%%----------------------------------------------------------------------
-module(game_formula).
-include("common.hrl").
-include("role.hrl").

-export([calc_damage/3,
		 calc_role/1
        ]).

calc_damage(_, _, _) ->
    ok.

calc_role(Role) ->
	Role.


%%---------------------
%% Eunit TEST
%%---------------------
-ifdef(EUNIT).

-endif.
