-module(db_sup).
-behaviour(supervisor).

-export([init/1,
         chash_get/1,
         start_link/0]).

-include("common.hrl").
-include("db.hrl").

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
	DbHost = ?CONFIG(db_host), 
	DbPort = ?CONFIG(db_port),

	chash_new(),

	Procs = [{{essdb_client, self(), No}, {essdb, start_link, [DbHost, DbPort, No]},
			permanent, brutal_kill, worker, []} || No <- lists:seq(1, ?DB_POOL_NUM)],

	{ok, {{one_for_one, 10, 10}, Procs}}.

chash_new() ->
    ets:new(essdb, [public, set, named_table, {keypos, #ets_pair.name}]),

    PoolTab = ets:new(essdb_pool, [ordered_set]),

    ets:insert(essdb, #ets_pair{name = essdb_pool, tab = PoolTab}),

    lists:foreach(fun(Node) ->
                    StrNode = integer_to_list(Node),

                    lists:foldl(fun(_, Index) ->
                                    VNode = erlang:phash2(StrNode ++ "-" ++ integer_to_list(Index), ?HASH_MAX),

                                    ets:insert(PoolTab, {VNode, Node}),

                                    Index + 1
                                end,
                                1, lists:duplicate(?DB_POOL_NUM, 0))
                  end,
                  lists:seq(1, ?DB_POOL_NUM)).

chash_get(Term) ->
    [_ = #ets_pair{tab = PoolTab}] = ets:lookup(essdb, essdb_pool),

    HashTerm = erlang:phash2(Term, ?HASH_MAX),

    case ets:next(PoolTab, HashTerm) of
        '$end_of_table' ->
            case ets:first(PoolTab) of
                '$end_of_table' ->
                    undef;
                VNode ->
                    [{_, Node}] = ets:lookup(PoolTab, VNode),

                    Node
            end;
        VNode ->
            [{_, Node}] = ets:lookup(PoolTab, VNode),

            Node
    end.