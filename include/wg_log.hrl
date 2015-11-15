%%%----------------------------------------------------------------------
%%%
%%% @copyright wg
%%% @author litaocheng@gmail.com
%%% @doc the log header
%%%
%%%----------------------------------------------------------------------
-ifndef(WG_LOG_HRL).
-define(WG_LOG_HRL, ok).

%% live方式启动在屏幕输出
-ifdef(LIVE).
-define(_U(Text), (unicode:characters_to_list(list_to_binary(Text)))).
-else.
-define(_U(Text), Text). % 输出到文件
-endif.

    -ifdef(EUNIT).

        -define(EUNITFMT(T, F, D), (?debugFmt((T) ++ (F), (D)))).

        -define(VERBOSE(F), ?EUNITFMT("[V]", F, [])).
        -define(VERBOSE(F, D), ?EUNITFMT("[V]", F, D)).

        -define(DEBUG(F), ?EUNITFMT("[D]", F, [])).
        -define(DEBUG(F, D), ?EUNITFMT("[D]", F, D)).

        -define(INFO(F), ?EUNITFMT("[I]", F, [])).
        -define(INFO(F, D), ?EUNITFMT("[I]", F, D)).

        -define(WARN(F), ?EUNITFMT("[W]", F, [])).
        -define(WARN(F, D), ?EUNITFMT("[W]", F, D)).

        -define(ERROR(F), ?EUNITFMT("[E]", F, [])).
        -define(ERROR2(F), ?EUNITFMT("[E]", F ++ "\nstacetrace:~p", [erlang:get_stacktrace()])).
        -define(ERROR(F, D), ?EUNITFMT("[E]", F, D)).
        -define(ERROR2(F, D), ?EUNITFMT("[E]", F ++ "\nstacetrace:~p", D ++ [erlang:get_stacktrace()])).

    -else.
        -define(VERBOSE(F), 
            wg_logger:verbose_msg(?MODULE, ?LINE, F, [])).
        -define(VERBOSE(F, D),
            wg_logger:verbose_msg(?MODULE, ?LINE, F, D)).

        -define(DEBUG(F), 
            wg_logger:debug_msg(?MODULE, ?LINE, F, [])).
        -define(DEBUG(F, D),
            wg_logger:debug_msg(?MODULE, ?LINE, F, D)).

        -define(INFO(F), 
            wg_logger:info_msg(?MODULE, ?LINE, F, [])).
        -define(INFO(F, D),
            wg_logger:info_msg(?MODULE, ?LINE, F, D)).

        -define(WARN(F),
            wg_logger:warning_msg(?MODULE, ?LINE, F, [])).
        -define(WARN(F, D), 
            wg_logger:warning_msg(?MODULE, ?LINE, F, D)).

        -define(ERROR(F),
            wg_logger:error_msg(?MODULE, ?LINE, F, [])).
        -define(ERROR2(F),
            wg_logger:error_msg(?MODULE, ?LINE, F++"\nstacetrace:~p", [erlang:get_stacktrace()])).
        -define(ERROR(F, D),
            wg_logger:error_msg(?MODULE, ?LINE, F, D)).
        -define(ERROR2(F, D),
            wg_logger:error_msg(?MODULE, ?LINE, F++"\nstacetrace:~p", D ++ [erlang:get_stacktrace()])).

    -endif. %EUNIT

-endif. % WG_LOG_HRL
