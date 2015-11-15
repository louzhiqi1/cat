%%% -------------------------------------------------------------------
%%% Author  : liuzhongzheng2012@gmail.com
%%% Description : 协议加密解密
%%%
%%% Created : 2014-06-30
%%% -------------------------------------------------------------------

-module(net_encrypt).
-export([encode/2,
         decode/2]).

encode(Data, Key) when byte_size(Data) >= 4 ->
        Data1 = bit_encode(Data, Key),
        Key1 = change_key(Data1, Key),

        {Data1, Key1}.

decode(Data, Key) when byte_size(Data) >= 4 ->
        Key1 = change_key(Data, Key),
        Data1 = bit_decode(Data, Key),

        {Data1, Key1}.

bit_encode(<<D:8, DRes/binary>>, <<K:8, _/binary>>=Key) ->
        D1 = D bxor K,
        KeyTuple = list_to_tuple(binary_to_list(Key)),

        bit_encode(DRes, [D1], 1, KeyTuple).

bit_encode(<<D:8, DRes/binary>>, [Dpre|_]=Acc, N, KeyTuple) ->
        K = element(N+1, KeyTuple),
        
        D1 = D bxor Dpre bxor K,
        N1 = (N+1) band 7 ,

        bit_encode(DRes, [D1|Acc], N1, KeyTuple);

bit_encode(<<>>, Acc, _, KeyTuple) ->
        Data = lists:reverse(Acc),

        bit_encode_1(Data, KeyTuple).

bit_encode_1([D0, D1, D2, D3|DRes], {_, _, K2, K3, K4, K5, _, _}) ->
        E3 = D3 bxor K2,
        E2 = D2 bxor E3 bxor K3,
        E1 = D1 bxor E2 bxor K4,
        E0 = D0 bxor E1 bxor K5,

        list_to_binary([E0, E1, E2, E3|DRes]).

bit_decode(<<D0:8, D1:8, D2:8, D3:8, DRes/binary>>, <<_:16, K2:8, K3:8, K4:8, K5:8, _/binary>>=Key) ->
        E0 = D0 bxor D1 bxor K5,
        E1 = D1 bxor D2 bxor K4,
        E2 = D2 bxor D3 bxor K3,
        E3 = D3 bxor K2,

        Data = binary_to_list(<<E0:8, E1:8, E2:8, E3:8, DRes/binary>>),
        Data1 = lists:reverse(Data),
        Count = length(Data1) - 1,
        KeyTuple = list_to_tuple(binary_to_list(Key)),

        bit_decode_1(Data1, [], Count, KeyTuple).

bit_decode_1([Di, Dii|Dres], Acc, N, KeyTuple) ->
        K = element((N band 7) + 1, KeyTuple),
        Ei = Di bxor Dii bxor K,

        bit_decode_1([Dii|Dres], [Ei|Acc], N-1, KeyTuple);

bit_decode_1([D0], Acc, 0, KeyTuple) ->
        K = element(1, KeyTuple),
        D01 = D0 bxor K,

        list_to_binary([D01|Acc]).

change_key(<<D:32/little, _/binary>>, <<K0:32/little, K1:32/little>>) ->
        E0 = K0 bxor D,
        E1 = K1 + 16#C3,

        <<E0:32/little, E1:32/little>>.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).

init() ->
        erlang:load_nif(?LIB_PATH ++ "net_encrypt", 0).

rand_char() ->
        rand:rand_int(0, 255).

rand_key() ->
        list_to_binary([rand_char() || _ <- lists:seq(1, 8)]).

rand_data() ->
        list_to_binary([rand_char() || _ <- lists:seq(1, rand_char()+4)]).

re_test() ->
        re_test(10000).

re_test(N) when N =< 0 ->
        ok;

re_test(N) ->
        Data = rand_data(),
        Key = rand_key(),
        A = encode(Data, Key),
        Data1 = binary:copy(Data),
        Key1 = binary:copy(Key),
        B = net_encrypt:encode(Data1, Key1),
        A = B,

        Data2 = binary:copy(Data),
        Key2 = binary:copy(Key),
        C = decode(Data, Key),
        D = net_encrypt:decode(binary:copy(Data2), binary:copy(Key2)),
        C = D,

        {DataE, _} = encode(Data, Key),
        {Data, _} = decode(DataE, Key),

        re_test(N-1).

-endif.