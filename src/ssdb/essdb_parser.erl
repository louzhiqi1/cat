-module(essdb_parser).
-export([init/0,
         parse/2]).

-include("db.hrl").

init() ->
    #pstate{}.

parse2(<<10:8, Rest/binary>>, Ret) ->
        {ok, Ret, Rest};
parse2(<<255:8, W:8, Len:W/binary-unit:8, Rest/binary>>, Ret)->
        Len2 = list_to_integer(binary_to_list(Len), 10),
        case Rest of
                <<Value:Len2/binary-unit:8, 10:8>> ->
                        {continue, Ret ++ [Value], <<>>};
                <<Value:Len2/binary-unit:8, 10:8, Rest2/binary>> ->
                        parse2(Rest2, Ret ++ [Value]);
                _ ->
                        {continue, Ret, <<Len/binary, 10:8, Rest/binary>>}
        end;
parse2(<<Len:1/binary-unit:8, 10:8, Rest/binary>>, Ret) ->
        parse2(<<255:8, 1:8, Len/binary, Rest/binary>>, Ret);
parse2(<<Len:2/binary-unit:8, 10:8, Rest/binary>>, Ret) ->
        parse2(<<255:8, 2:8, Len/binary, Rest/binary>>, Ret);
parse2(<<Len:3/binary-unit:8, 10:8, Rest/binary>>, Ret) ->
        parse2(<<255:8, 3:8, Len/binary, Rest/binary>>, Ret);
parse2(<<Len:4/binary-unit:8, 10:8, Rest/binary>>, Ret) ->
        parse2(<<255:8, 4:8, Len/binary, Rest/binary>>, Ret);
parse2(<<Len:5/binary-unit:8, 10:8, Rest/binary>>, Ret) ->
        parse2(<<255:8, 5:8, Len/binary, Rest/binary>>, Ret);
parse2(<<Len:6/binary-unit:8, 10:8, Rest/binary>>, Ret) ->
        parse2(<<255:8, 6:8, Len/binary, Rest/binary>>, Ret);
parse2(<<Len:7/binary-unit:8, 10:8, Rest/binary>>, Ret) ->
        parse2(<<255:8, 7:8, Len/binary, Rest/binary>>, Ret);
parse2(<<Len:8/binary-unit:8, 10:8, Rest/binary>>, Ret) ->
        parse2(<<255:8, 8:8, Len/binary, Rest/binary>>, Ret);
parse2(<<Len:9/binary-unit:8, 10:8, Rest/binary>>, Ret) ->
        parse2(<<255:8, 9:8, Len/binary, Rest/binary>>, Ret);
parse2(<<Len:10/binary-unit:8, 10:8, Rest/binary>>, Ret) ->
        parse2(<<255:8, 10:8, Len/binary, Rest/binary>>, Ret);
parse2(Data, Ret) ->
        {continue, Ret, Data}.

-spec parse(State::#pstate{}, Data::binary()) ->
                   {ok, Value::binary(), NewState::#pstate{}} |
                   {ok, Value::binary(), Rest::binary(), NewState::#pstate{}} |
                   {error, ErrString::binary(), NewState::#pstate{}} |
                   {error, ErrString::binary(), Rest::binary(), NewState::#pstate{}} |
                   {continue, NewState::#pstate{}}.

parse(#pstate{continuation_data = ContinuationData, parsed_values = ParsedValues} = State, NewData) ->
    case parse2(<<ContinuationData/binary, NewData/binary>>, ParsedValues) of
        {ok, [<<"ok">> | Value], <<>>} ->
            {ok, Value, init()};
        {ok, [<<"ok">> | Value], ContinuationData2} ->
            {ok, Value, ContinuationData2, init()};
        {continue, ParsedValues2, ContinuationData2} ->
            {continue, State#pstate{continuation_data = ContinuationData2, parsed_values = ParsedValues2}};
        {ok, [], <<>>} ->
            {error, unknown, init()};
        {ok, [], ContinuationData2} ->
            {error, unknown, ContinuationData2, init()};
        {ok, [Error], <<>>} ->
            {error, list_to_atom(binary_to_list(Error)), init()};
        {ok, [Error], ContinuationData2} ->
            {error, list_to_atom(binary_to_list(Error)), ContinuationData2, init()};
        {ok, [Error | [Detail|_]], <<>>} ->
            {error, {list_to_atom(binary_to_list(Error)), binary_to_list(Detail)}, init()};
        {ok, [Error | [Detail|_]], ContinuationData2} ->
            {error, {list_to_atom(binary_to_list(Error)), binary_to_list(Detail)}, ContinuationData2, init()};
        _ ->
            {error, unknown_response}
    end.