%%%----------------------------------------------------------------------
%%%
%%% @author litaocheng
%%% @date 2012-07-12
%%% @doc 协议编解码
%%%
%%%----------------------------------------------------------------------
-module(pack).
-include("common.hrl").
-include("pack.hrl").

-export([pack_msg/3]).
-export([pack/1, unpack/1]).

%% @doc 协议打包，返回binary和size
pack_msg(_Mi, _Fi, {packed, _Bin, _Size} = Packed) when is_integer(_Size) ->
    Packed;
pack_msg(Mi, Fi, Msg) ->
	ResponBin = pack:pack(Msg) ,
	ResponLen = iolist_size(ResponBin) ,
    Size = ResponLen + 2,
    %?WARN("**** pack mi:~p fi:~p msg:~p bin:~p", [Mi, Fi, Msg, ResponBin]),
	{packed, [<<Size:16 , Mi:8, Fi:8>>, ResponBin], Size}.

pack(null) ->
    <<>>;
%% bool值
pack(true) ->
	<<?PACK_TYPE_TRUE:8>>;
pack(false) ->
	<<?PACK_TYPE_FALSE:8>>;
%% 列表
pack([]) ->
    <<?PACK_TYPE_SLIST:8, 0:8>>;
pack(List) when is_list(List) ->
	Len = length(List),
	ListData = [pack(Data) || Data <- List],
	if	Len < 16#ff ->	
			[<<?PACK_TYPE_SLIST:8, Len:8>>, ListData];
		Len < 16#ffff ->
			[<<?PACK_TYPE_WLIST:8, Len:16>>, ListData];
		Len < 16#ffffffff ->
            [<<?PACK_TYPE_LLIST:8, Len:32>>, ListData]
	end;
%% 整数
pack(Data) when is_integer( Data ) ->
    if
		Data < 16#ff ->
			<<?PACK_TYPE_SINT:8, Data:8/unsigned>> ;
		Data <16#ffff ->
			<<?PACK_TYPE_WINT:8, Data:16/unsigned>> ;
		Data <16#ffffffff ->
			<<?PACK_TYPE_LINT:8, Data:32/unsigned>> ;
		true ->	%% 超过范围做浮点数处理
			<<?PACK_TYPE_FLOAT:8, Data:64/float>>
	end ;
%% 浮点数
pack(Data) when is_float( Data ) ->
	<< ?PACK_TYPE_FLOAT:8 , Data:64/float>>;
%% 二进制
pack({raw, Bin}) ->
    [<<?PACK_TYPE_RBIN:8>>, Bin];
pack(Bin) when is_binary(Bin) ->
	Len = byte_size(Bin) ,
	if	Len < 16#ff ->
			<<?PACK_TYPE_SBIN:8, Len:8,  Bin/binary>>;
		Len < 16#ffff ->
			<<?PACK_TYPE_WBIN:8, Len:16, Bin/binary>>;
		Len < 16#ffffffff ->
			<<?PACK_TYPE_LBIN:8, Len:32, Bin/binary>>
	end;
pack(Data) when is_tuple(Data) ->
    pack(tuple_to_list(Data));
%% 错误
pack(Data) ->
	lager:error("error data type ~p", [Data]),
        exit(nonsupport_type).
	
%% @doc 解码
unpack(<<>>) ->
        {<<>>, <<>>};
unpack(<<?PACK_TYPE_TRUE:8, Bin/binary >> ) ->
	{true, Bin};
unpack(<<?PACK_TYPE_FALSE:8, Bin/binary >> ) ->
	{false, Bin};
%% 列表或元组
unpack(<<?PACK_TYPE_SLIST:8, Len:8 , Bin/binary>>) ->
	unpack_list(Len, Bin);
unpack(<<?PACK_TYPE_WLIST:8, Len:16, Bin/binary>>) ->
	unpack_list(Len, Bin);
unpack(<<?PACK_TYPE_LLIST:8, Len:32, Bin/binary>>) ->
	unpack_list(Len, Bin);

%% 整数
unpack(<<?PACK_TYPE_LINT:8, Int:32/unsigned, Rest/binary>>) ->
	{Int, Rest};
unpack(<<?PACK_TYPE_WINT:8, Int:16/unsigned, Rest/binary>>) ->
	{Int, Rest};
unpack(<<?PACK_TYPE_SINT:8, Int:8/unsigned, Rest/binary>>) ->
	{Int, Rest};

%% 浮点数
unpack(<<?PACK_TYPE_FLOAT:8, Float:64/float, Rest/binary>>) ->
	{Float, Rest} ;

%% 原子
unpack(<<?PACK_TYPE_SSTRING:8, Len:8, Str:Len/binary, Rest/binary>>) ->	
	{binary_to_list(Str), Rest};
unpack(<<?PACK_TYPE_WSTRING:8, Len:16, Str:Len/binary, Rest/binary>>) ->	
	{binary_to_list(Str), Rest};
	
%% 二进制
unpack(<<?PACK_TYPE_RBIN:8, Bin/binary>>) ->
    {Bin, <<>>};
unpack(<<?PACK_TYPE_SBIN:8, Len:8, Str:Len/binary, Rest/binary>>) ->
	{Str, Rest};
unpack(<<?PACK_TYPE_WBIN:8, Len:16, Str:Len/binary, Rest/binary>>) ->
	{Str, Rest};
unpack(<<?PACK_TYPE_LBIN:8, Len:32, Str:Len/binary, Rest/binary >>) ->
	{Str, Rest};

%% 错误类型
unpack(<<Type:8 , _/binary>> = Bin) ->
	lager:error("error bin data type:~p bin:~p", [Type, Bin]),
	{error , bad_type, Type} .

%% 解码列表
unpack_list(Len, Bin) ->
	unpack_list(Len, Bin, []).
unpack_list(0, Bin, Acc) ->	
    {lists:reverse(Acc), Bin};
unpack_list(Len, Bin, Acc) ->
	{Data, Rest} = unpack(Bin) ,
	unpack_list(Len - 1, Rest, [Data | Acc]).

%%------------------
%% EUnit Test
%%------------------
-ifdef(EUNIT).

basic_test_() ->
    [
        ?_assertEqual({true, <<>>}, unpack(pack(true))),
        ?_assertEqual({false, <<>>}, unpack(pack(false))),
        ?_assertEqual({0, <<>>}, unpack(pack(0))),
        ?_assertEqual({255, <<>>}, unpack(pack(255))),
        ?_assertEqual({256, <<>>}, unpack(pack(256))),
        ?_assertEqual({1234567, <<>>}, unpack(pack(1234567))),

        ?_assertEqual({<<"123">>, <<>>}, unpack(pack(<<"123">>))),
        ?_assertEqual({<<"NPC A">>, <<>>}, unpack(pack(<<"NPC A">>))),
        
        ?_assertEqual({[1, 3, <<"5">>], <<>>}, unpack(iolist_to_binary(pack([1, 3, <<"5">>]))))
    ].
-endif.
