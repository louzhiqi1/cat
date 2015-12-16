%% Author: rison
%% Created: 2012-1-4
%% Description: TODO: Add description to hash_set
%% Email: binshel@msn.cn
-module(hash_set).

%% 初始hash表长度
-define( INIT_SLOT_NUM,				32 ) .

%% 取key的hash值
-define( HASH_VALUE( Key ) , (1 + erlang:phash2( Key , ?INIT_SLOT_NUM))) .

-record(hash_set, {key_pos = 1,	%% key的位置（默认第一个位置）
		    segs = erlang:make_tuple(?INIT_SLOT_NUM , [])
		   } 
	).




-export([	new/0 ,
			new/1 ,
			insert/2 ,
			lookup/2 ,
			delete/2 ,
			
			to_list/1 ,
			map/2 ,
			fold/3 ,
			foreach/2
		
		
		]).

%% 新建hash表
new() ->
	#hash_set{} .
new( Key_pos ) ->
	#hash_set{	key_pos	= Key_pos } .

%% 插入值
insert( Set , Tuple )  ->
	Key		= element( Set#hash_set.key_pos , Tuple ) ,
	Hash	= ?HASH_VALUE( Key) ,
	Old_value	= element( Hash , Set#hash_set.segs ) ,
	case lists:keytake( Key , Set#hash_set.key_pos , Old_value ) of
		{value, _Tuple , Old_value_2 } ->
			New_value	= [ Tuple | Old_value_2 ] ;
		false ->
			New_value	= [ Tuple | Old_value ]
	end ,
	New_segs	= setelement( Hash , Set#hash_set.segs , New_value  ) ,
	Set#hash_set{	segs	= New_segs } .

%% 查找
lookup( Set , Key ) ->
	Hash	= ?HASH_VALUE( Key ) ,
	Slot	= element( Hash , Set#hash_set.segs ) ,
	lists:keyfind( Key , Set#hash_set.key_pos , Slot ) .
	
%% 删除
delete( Set , Key ) ->
	Hash	= ?HASH_VALUE( Key ) ,
	Slot	= element( Hash , Set#hash_set.segs ) ,
	Slot_new	= lists:keydelete( Key , Set#hash_set.key_pos , Slot ) ,
	New_segs	= setelement( Hash , Set#hash_set.segs , Slot_new ) ,
	Set#hash_set{	segs	= New_segs } .

%% 转换为列表
to_list( Set ) ->
	lists:append( tuple_to_list( Set#hash_set.segs ) ) .
	
	
%% 映射
map( Fun , Set ) ->
	map( Fun , tuple_to_list( Set#hash_set.segs ) , [ ] ) .

map( Fun , [ Slot ] , Rs ) ->
	map_2( Fun , Slot , Rs ) ;
map( Fun , [ Slot | Rest ] , Rs ) ->
	map( Fun , Rest , map_2( Fun , Slot , Rs ) ) .

map_2( _Fun , [  ] , Rs ) ->	Rs ;
map_2( Fun , [ Tuple | Rest ] , Rs ) ->
	map_2( Fun , Rest , [ Fun( Tuple ) | Rs ] ) .

%% 迭代执行
fold( Fun , Acc , Set ) ->
	fold_2( Fun , Acc , tuple_to_list( Set#hash_set.segs ) ) .

fold_2( Fun , Acc , [ Slot | Rest ] ) ->
	fold_2( Fun , fold_3( Fun , Acc , Slot ) , Rest ) ;
fold_2( _Fun , Acc , [] ) ->	Acc .

fold_3( Fun , Acc , [ Ele | Rest ] ) ->
	fold_3( Fun , Fun( Ele , Acc ) , Rest ) ;
fold_3( _Fun , Acc , [] ) ->	Acc .

%% 每个执行
foreach( Fun , Set ) ->
	foreach_2( Fun , tuple_to_list( Set#hash_set.segs ) ) .

foreach_2( Fun , [ Slot ] ) ->
	lists:foreach( Fun , Slot ) ;
foreach_2( Fun , [ Slot | Rest ]  ) ->
	lists:foreach( Fun , Slot ) ,
	foreach_2( Fun , Rest ) .

























