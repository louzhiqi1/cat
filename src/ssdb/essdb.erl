-module(essdb).
-export([
         start/3,
         start_link/3,
         query/2,
         query/3]).

-include("db.hrl").

start(Host, Port, No) when is_list(Host), is_integer(Port) ->
    start_client(Host, Port, No).

start_link(Host, Port, No) when is_list(Host); is_integer(Port) ->
    start_client_link(Host, Port, No).

start_client(Host, Port, No) ->
    essdb_client:start(Host, Port, No).

start_client_link(Host, Port, No) ->
    essdb_client:start_link(Host, Port, No).

query(Cmd, [Name, ID | Rest]) when Cmd == hset;
                                   Cmd == hget;
                                   Cmd == hdel;
                                   Cmd == hclear;
                                   Cmd == hincr;
                                   Cmd == hdecr;
                                   Cmd == hsize;
                                   Cmd == hexists;
                                   Cmd == hgetall;
                                   Cmd == hscan;
                                   Cmd == hrscan;
                                   Cmd == hkeys;
                                   Cmd == hvals;
                                   Cmd == multi_hexists;
                                   Cmd == multi_hdel;
                                   Cmd == multi_hget;
                                   Cmd == multi_hset ->
        case name(Name, ID) of
                {ok, Name2} ->
                        q([Cmd, Name2 | Rest]);
                {error, Reason} ->
                        {error, Reason}
        end;

query(Cmd, Paras) when Cmd == multi_hsize ->
        case format_mhsize_paras(Paras, []) of
                {ok, Paras2} ->
                        q([Cmd | Paras2]);
                {error, Reason} ->
                        {error, Reason}
        end;

query(Cmd, Condition) when Cmd == hlist ->
        query(Cmd, Condition, ?HLIST_TIMEOUT_1);

query(Cmd, Paras) ->
        q([Cmd | Paras]).

query(Cmd, [Name, <<"">>, <<"">>, Limit], Timeout) when Cmd == hlist,
                                               is_atom(Name) ->
        Name2 = atom_to_list(Name),

        Start = Name2 ++ "|000000000",
        End   = Name2 ++ "|999999999",

        format_hlist_reply(q([Cmd,
                            list_to_binary(Start),
                            list_to_binary(End),
                            Limit], Timeout));

query(Cmd, [Name, Start, End, Limit], Timeout) when Cmd == hlist,
                                           is_atom(Name),
                                           is_binary(Start),
                                           is_binary(End) ->
        Name2 = atom_to_list(Name),

        Start2 = Name2 ++ "|" ++ binary_to_list(Start),
        End2   = Name2 ++ "|" ++ binary_to_list(End),

        format_hlist_reply(q([Cmd,
                                    list_to_binary(Start2),
                                    list_to_binary(End2),
                                    Limit], Timeout));

query(Cmd, _, _) when Cmd == hlist ->
        {error, para_fmt_invalid}.

name(Name, ID) when is_atom(Name), is_integer(ID) ->
        {ok, list_to_binary(atom_to_list(Name) ++ "|" ++ integer_to_list(ID))};

name(_, _) ->
        {error, name_invalid}.

format_mhsize_paras([], []) ->
        {error, para_num_invalid};

format_mhsize_paras([], Ret) ->
        {ok, lists:reverse(Ret)};

format_mhsize_paras(MultiParas = [Name, ID | Rest], Ret) ->
        case length(MultiParas) rem 2 of
                0 ->
                        case name(Name, ID) of
                                {ok, Name2} ->
                                        format_mhsize_paras(Rest, [Name2 | Ret]);
                                {error, Reason} ->
                                        {error, Reason}
                        end;
                _ ->
                        {error, para_num_invalid}
        end;

format_mhsize_paras(_, _) ->
        {error, para_num_invalid}.

format_hlist_reply({ok, Ret}) ->
        Ret2 =
        lists:foldl(fun(Name, Ret3) ->
                        [_, ID] = string:tokens(binary_to_list(Name), "|"),

                        lists:append(Ret3, [list_to_integer(ID)])
                    end,
                    [], Ret),

        {ok, Ret2};

format_hlist_reply(Reply) ->
        Reply.

-spec q(Client::pid(), Command::iolist()) -> {ok, Value::binary()} |
                                             {error, Reason::binary()}.

q(undefined, _) ->
    {error, noproc};

q(Client, Command) when is_pid(Client) ->
    call(Client, Command, ?TIMEOUT);

q(Command, Timeout) ->
    case get(essdb_client) of
        undefined ->
            EssdbClient = list_to_atom("essdb_client_" ++ integer_to_list(db_sup:chash_get(1))),

            put(essdb_client, EssdbClient);
        EssdbClient ->
            pass
    end,

    q(whereis(EssdbClient), Command, Timeout).

q(undefined, _, _) ->
    {error, noproc};

q(Client, Command, Timeout) ->
    call(Client, Command, Timeout).

q(Command) ->
    q(Command, ?TIMEOUT).

call(Client, Command, Timeout) ->
    [Cmd | _] = Command,

    Request = {request, Cmd, create_multibulk(Command)},
    gen_server:call(Client, Request, Timeout).

-spec create_multibulk(Args::iolist()) -> Command::iolist().

create_multibulk(Args) ->
    [lists:map(fun to_bulk/1, lists:map(fun to_binary/1, Args)), [?NL]].

to_bulk(B) when is_binary(B) ->
    [integer_to_list(iolist_size(B)), <<?NL>>, B, <<?NL>>].

to_binary(X) when is_binary(X) -> X;
to_binary(X) -> list_to_binary(lists:flatten(io_lib:format("~p", [X]))).
