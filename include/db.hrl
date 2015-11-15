-ifndef(__DB_HRL__).
-define(__DB_HRL__, true).

-define(TIMEOUT, 5000).
-define(DB_POOL_NUM, 50).
-define(HASH_MAX, 4294967295).
-define(NL, "\n").

-define(HLIST_TIMEOUT_1, 1000).

-record(ets_pair, {
			name,
			tab
	}).

%% Internal parser state. Is returned from parse/2 and must be
%% included on the next calls to parse/2.
-record(pstate, {
          state = undefined :: parser_state() | undefined,
          continuation_data :: continuation_data() | undefined
}).



%% Continuation data is whatever data returned by any of the parse
%% functions. This is used to continue where we left off the next time
%% the user calls parse/2.
-type continuation_data() :: any().
-type parser_state() :: status_continue | bulk_continue | multibulk_continue.

-endif.