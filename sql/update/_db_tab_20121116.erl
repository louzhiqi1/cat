-module('_db_tab_20121116').
-export([do/0]).

%%
%% 新key -define(COUNTER_DB_ATHLETICS_CREDITS, 2600).          % 竞技积分
%% 旧key -define(COUNTER_DB_CREDITS(Id), {credits,Id}).          % 玩家积分
do() ->
    {"select `id`, `counter` from role where `counter` <> ''",
    fun([Id, B]) ->
        Counter = erlang:binary_to_term(B),
        Counter2 =
        lists:foldl(
        fun
            ({{credits, _}, Val}, Acc) ->
                [{2600, Val} | Acc];
            ({Key, Val}, Acc) ->
                [{Key, Val} | Acc]
        end, [], Counter),
        B2 = erlang:term_to_binary(Counter2),
        ["update role set `counter` = ", db_util:encode(B2), " where id = ", db_util:encode(Id)]
    end
    }.
