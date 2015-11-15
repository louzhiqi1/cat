%%%----------------------------------------------------------------------
%%%
%%% @date 2012-07-29
%%% @doc 关于地图相关的一些辅助函数
%%%
%%%----------------------------------------------------------------------
-module(util).
-include("common.hrl").
-include("role.hrl").
-include_lib("kernel/include/file.hrl").

-export([any_to_iodata/1, guid/0, guid/1, guid_str/1, 
        md5_string_lowercase/1, md5_string_uppercase/1,
        url_encode/1]).
-export([start_child/2, is_app_running/1]).

%% 关于进程和名字
-export([whereis_name/1, prefix_n/3, role_name/1]).
-export([set_pid_type/1, is_role_pid/0]).

%% 随机数
-export([rand/0, rand/1, rand/2, pick_by_prob/1, pick_by_time/2]).

%% 数字
-export([sqrt/2]).

%% term 转化
-export([string_to_term/1, term_to_string/1,
        encode_term/1, decode_term/1, 
        to_lower/1, to_upper/1]).

%% 关于文件
-export([is_dir/1, is_regular/1, is_file/1, read_file_info/1,
        consult_nested/1]).

%% 关于网络
-export([peer/1, peer_str/1, get_nonlo_if/0, ip_ntoa/1, ip_aton/1, ipv4_to_n/1]).
-export([get_ip/0, get_ip_wan/0, get_ip_lan/0]).

%% 关于时间
-export([check_time/5]).
-export([now/0, now_ms/0, local_time/0]).
-export([now_sec_to_local_time/1, now_sec_to_week_num/1, local_time_to_now_sec/1,
        is_time_in_one_day/1, is_time_in_one_day/2, is_time_in_one_week/2]).
-export([localtime_string/0, localtime_string_min/0, localtime_string_min/1]).
-export([time_diff_in_days/1, time_diff_in_days/2]).

%% 关于系统
-export([msg_queue_len/0, msg_queue_len/1]).

%% 文本是否合法
-export([is_text_valid/3]).

%% 其它
-export([set_terminate_reason/1, get_terminate_reason/0]).

%% 配置相关
-export([platform/0, server_id/0]).

%% 获取服务器代码版本号
-export([get_server_code_version/0]).

%% @doc 转化成iodata
any_to_iodata(Any) when is_list(Any) ->
    Any;
any_to_iodata(Any) when is_binary(Any) ->
    Any;
any_to_iodata(Any) when is_atom(Any) ->
    atom_to_list(Any);
any_to_iodata(Any) when is_integer(Any) ->
    integer_to_list(Any);
any_to_iodata(Any) when is_float(Any) ->
    float_to_list(Any).

%% @doc 生成guid
guid() ->
    guid(undefined).

guid(undefined) ->
    erlang:md5(term_to_binary({self(), erlang:timestamp(), make_ref()}));
guid(Id) when is_integer(Id) ->
    <<_:32, Rest/bytes>> = guid(undefined),
    <<Id:32, Rest/bytes>>.

%% @doc 将GUID以string方式显示
%% 比如: 3F2504E0-4F89-11D3-9A0C-0305E82C3301 
%% 4-2-2-2-6
guid_str(GUID) when is_binary(GUID), byte_size(GUID) =:= 16 ->
    Str = mochihex:to_hex(binary_to_list(GUID)),
    {P1, Rest1} = lists:split(8, Str),
    {P2, Rest2} = lists:split(4, Rest1),
    {P3, Rest3} = lists:split(4, Rest2),
    {P4, Rest4} = lists:split(4, Rest3),
    {P5, []} = lists:split(12, Rest4),
    lists:concat([P1, "-", P2, "-", P3, "-", P4, "-", P5]).

%% @doc 生成md5 16进制字符串(小写)
md5_string_lowercase(IoData) ->
    <<N:128>> = erlang:md5(IoData),
    lists:flatten(io_lib:format("~32.16.0b", [N])).

%% @doc 生成md5 16进制字符串(大写)
md5_string_uppercase(IoData) ->
    <<N:128>> = erlang:md5(IoData),
    lists:flatten(io_lib:format("~32.16.0B", [N])).

%% @doc url encode
url_encode(D) ->
    mochiweb_util:quote_plus(D).

%% @doc 启动某个子服务
start_child(Sup, Child) ->
    case catch supervisor:start_child(Sup, Child) of
        {ok, _} ->
            ok;
        {error, {{already_started, _Pid}, _}} ->
            ok;
        Other ->
            ?ERROR(?_U("启动服务:~p失败:~p"), [Child, Other]),
            throw(Other)
    end.

%% 某个app是否运行中
is_app_running(Apps) when is_list(Apps) ->
    lists:all(fun is_app_running/1, Apps);
is_app_running(App) when is_atom(App) ->
    case lists:keyfind(App, 1, application:which_applications()) of
        false ->
            false;
        _ ->
            true
    end.

%% @doc 获取名字对应的pid,从local,后global查询
whereis_name(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            ?NONE; 
        Pid ->
            Pid
    end.

%% @doc role_server名称
role_name(Id) ->
    prefix_n("rid", Id).


%% @doc 组合某个前缀和数字，返回值为atom
prefix_n(Prefix, N) when is_list(Prefix), is_integer(N) ->
    list_to_atom(Prefix ++ ?N2S(N)).

prefix_n(Prefix, N1, N2) when is_list(Prefix), is_integer(N1), is_integer(N2) ->
    list_to_atom(Prefix ++ [$_ | ?N2S(N1)] ++ [$_ | ?N2S(N2)]).

%% @doc 设置进程类型
-define(PID_TYPE_KEY, '$pid_type').
set_pid_type(Type) ->
    erlang:put(?PID_TYPE_KEY, Type),
    ok.

%% @doc 获取进程类型
get_pid_type() ->
    case erlang:get(?PID_TYPE_KEY) of
        undefined ->
            ?PID_TYPE_UNKNOWN;
        V ->
            V
    end.

%% @doc 是否为角色进程
is_role_pid() ->
    get_pid_type() =:= ?PID_TYPE_ROLE.


%% @doc 返回1-10000之间的一个数字
rand() ->
    rand(?PROB_FULL).

%% @doc 返回一个随机数,结果为:[1,N]
rand(0) -> 0;
rand(N) when N > 0 ->
    random:uniform(N).

%% @doc 返回两个数之间的随机数
rand(Min, Min) ->
    Min;
rand(Min, Max) ->
    rand(Max - Min) + Min.

%% @doc 依据概率选择
%% 列表格式:[{对象,概率}]
%% 返回{对象,位置}
pick_by_prob(List) ->
    Point = util:rand(),
    do_pick_by_prob(List, 0, Point, 1).

do_pick_by_prob([], _Cur, _Point, _Index)->
    ?NONE;
do_pick_by_prob([E | Rest], Cur, Point, Index) ->
    case E of
        {Id, Range} ->
            Obj = Id;
        [Id, Range] ->
            Obj = Id;
        [Id, N, Range] ->
            Obj = {Id, N}
    end,
    if
        Cur < Point andalso Point =< (Range + Cur) ->
            {Obj, Index};
        true ->
            do_pick_by_prob(Rest, Range + Cur, Point, Index + 1)
    end.

%% @doc 依据时间选择数值
%% List: [{Time, Value}],Time从大到小
pick_by_time(List, Cur) ->
    do_pick_by_time(List, Cur).
do_pick_by_time([], _Cur) ->
    0;
do_pick_by_time([{Time, Value} | _T], Cur) when Cur >= Time ->
    Value;
do_pick_by_time([{_Time, _Value} | T], Cur) ->
    do_pick_by_time(T, Cur).

%% @doc 对数字进行开放
sqrt(N, B) when is_integer(N), N >= 0, is_integer(B), B > 0 ->
    do_sqrt(N div B, B, 0).
do_sqrt(0, _B, Acc) ->
    Acc;
do_sqrt(N, B, Acc) ->
    do_sqrt(N div B, B, Acc + 1).

%% @doc 将term转化为string
%% 如[1, 2]转化为"[1,2]"
term_to_string(Term) ->
    lists:flatten(io_lib:print(Term)).

%% 将string转化为term
%% 如"[1,2]"转化成[1,2]
string_to_term(Str) when is_binary(Str) ->
    string_to_term(?B2S(Str));
string_to_term(Str) when is_list(Str) ->
    case erl_scan:string(Str ++ ".") of
        {error, _, _} = Error ->
            Error;
        {ok, Tokens, _} ->
            erl_parse:parse_term(Tokens)
    end.

%% @doc term解码
decode_term(Bin) when is_binary(Bin) ->
    binary_to_term(Bin).

%% @doc term编码
encode_term(Term) ->
    term_to_binary(Term).

%% @doc 转化成小写(如果时binary首先进行unicode转化)
to_lower(S) when is_binary(S) ->
    Str = unicode:characters_to_list(S),
    to_lower(Str);
to_lower(S) when is_list(S) ->
    string:to_lower(S).

%% @doc 转化成大写(如果时binary首先进行unicode转化)
to_upper(S) when is_binary(S) ->
    Str = unicode:characters_to_list(S),
    to_upper(Str);
to_upper(S) when is_list(S) ->
    string:to_upper(S).

%%---------------
%% 关于文件
%%---------------

%% @doc 是否为目录
is_dir(File) ->
    case prim_file:read_file_info(File) of
        {ok, #file_info{type=directory}} ->
            true;
        _ ->
            false
    end.

%% @doc 是否为常规文件
is_regular(File) ->
    case prim_file:read_file_info(File) of
        {ok, #file_info{type=regular}} ->
            true;
        _ ->
            false
    end.

%% @doc 是否为文件或目录(同filelib:is_file/1)
is_file(File) ->
    case prim_file:read_file_info(File) of
        {ok, #file_info{type=regular}} ->
            true;
        {ok, #file_info{type=directory}} ->
            true;
        _ ->
            false
    end.

%% @doc 读取文件信息
read_file_info(File) ->
    prim_file:read_file_info(File).

%% @doc 嵌套的解析erlang term文件
consult_nested(File) ->
    case catch consult_nested(File, [], []) of
        {error, _} = Ret ->
            Ret;
        {Terms, _} ->
            %?WARN(?_U("************* terms:~n~p~n"), [Terms]),
            {ok, Terms}
    end.
consult_nested(FileName, Terms, Already) ->
    Config =
    case file:consult(FileName) of
        {ok, L} ->
            L;  
        Error ->
            ?ERROR("the ~p file format error!~n", [FileName]),
            throw(Error)
    end,
    Already2 = [FileName | Already],

    % 解析每一项
    {Terms2, Already3} =
    lists:mapfoldl(
    fun
        ({include, Val}, Acc) ->
            % 子配置文件列表
            Files = do_expand_files(FileName, Val, Acc),
            %?WARN(?_U("expand files list:~p"), [Files]),
            lists:mapfoldl(
            fun(F, AccIn) ->
                consult_nested(F, [], AccIn)
            end, Already2, Files);
        (_Other, Acc) ->
            {_Other, Acc}
    end, Already2, Config),
    {lists:flatten([Terms | Terms2]), Already3}.

%% 获取子配置文件
do_expand_files(Filename, Val0, Already) ->
    DirName = filename:dirname(Filename),
    Val = filename:join([DirName, Val0]),
    case lists:member($*, Val) of
        true ->
            lists:foldl(
            fun(F, Acc) ->
                case lists:member(F, Already) of
                    true ->
                        Acc;
                    false ->
                        [F | Acc]
                end
            end, [], filelib:wildcard(Val));
        false ->
            case lists:member(Val, Already) of
                true ->
                    [];
                false ->
                    [Val]
            end
    end.

%%------------
%% 网络相关
%%------------
%% @doc 获取socket peer的ip
peer_str(Sock) when is_port(Sock) ->
    inet_parse:ntoa(peer(Sock)).

%% @doc 获取socket peer的ip
peer(Sock) when is_port(Sock) ->
    case inet:peername(Sock) of
        {ok, {Addr, _Port}} ->
            Addr;
        {error, Error} ->
            throw({error, Error})
    end.

%% @doc选择非lo接口 
get_nonlo_if()->
    {ok, List} =inet:getif(),
    Set=
    lists:foldl(
    fun
        ({{127, _, _, _}, _, _}, Acc)->
            Acc;
        ({Ip, _, _}, Acc)->
            sets:add_element(Ip,Acc)
    end, sets:new(), List),
    sets:to_list(Set).

%% @doc ip tuple转换成string
ip_ntoa(Ip) ->
    inet_parse:ntoa(Ip).

%% @doc ip string转化成tuple
ip_aton(Ip) ->
    {ok, Addr} = inet_parse:address(Ip),
    Addr.

%% @doc 将ipv4转化成int
ipv4_to_n({A, B, C, D}) ->
    <<N:32>> = <<A, B, C, D>>,
    N;
ipv4_to_n(Ip) when is_list(Ip) ->
    Addr = ip_aton(Ip),
    ipv4_to_n(Addr).

%% @doc 获取本级的所有ip列表
get_ip() ->
    {ok, L} = inet:getif(),
    [Ip || {Ip, _, _} <- L, Ip =/= {127,0,0.1}].

%% @doc 获取外网ip
get_ip_wan() ->
    [Ip || {A, B, _, _} = Ip <- get_ip(),
        Ip =/= {127,0,0,1}, (A =/= 192 andalso B =/= 168)].

%% @doc 获取内网ip
get_ip_lan() ->
    [Ip || {192, 168, _, _} = Ip <- get_ip()].

%% @doc 检测时间是否合法
%% Month, Dom, Dow为0表示无限制
check_time(Month, Day, Dow, TimeStart, TimeEnd) ->
    {{_, MonthNow, DayNow} = Date, Time} = calendar:local_time(),
    DowNow = calendar:day_of_the_week(Date),
    SecondNow = calendar:time_to_seconds(Time),
    (Month =:= 0 orelse Month =:= MonthNow) 
    andalso (Day =:= 0 orelse Day =:= DayNow)
    andalso (Dow =:= 0 orelse Dow =:= DowNow)
    andalso (SecondNow >= TimeStart andalso SecondNow =< TimeEnd).

%% @doc 当前时间
now() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),
    MegaSecs * 1000000 + Secs.

now_ms() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    MegaSecs * 1000000000 + Secs * 1000 + MicroSecs div 1000.

%% @doc 获取当前时间
local_time() ->
    calendar:local_time().


%% @doc now_sec转化成local time
now_sec_to_local_time(T) when is_integer(T) ->
    T2 = {T div 1000000, T rem 1000000, 0},
    calendar:now_to_local_time(T2).

%% @doc now_sec转化成周数
now_sec_to_week_num(T) when is_integer(T) ->
    {Date, _Time} = now_sec_to_local_time(T),
    {_, Num} = calendar:iso_week_number(Date),
    Num.

%% @doc 日期转化为now sec(UTC)
%% 0-1970Seconds: 719528 * 86400.
local_time_to_now_sec({_, _} = DateTime) ->
    [DateTime2|_] = calendar:local_time_to_universal_time_dst(DateTime),
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime2),
    Seconds - 62167219200.

%% @doc 判断两天是否为同一天
is_time_in_one_day(T) ->
    is_time_in_one_day(T, util:now_sec()).

%% @doc 判断两个时间是否为同一天
is_time_in_one_day(T1, T2) ->
    {D1, _} = now_sec_to_local_time(T1),
    {D2, _} = now_sec_to_local_time(T2),
    D1 =:= D2.

%% @doc 判断两个时间是否为同一周
is_time_in_one_week(T1, T2) ->
    D1 = now_sec_to_week_num(T1),
    D2 = now_sec_to_week_num(T2),
    D1 =:= D2.

%% @doc 与当前时间相差的天数
time_diff_in_days(T) ->
    time_diff_in_days(T, util:now_sec()).
%% @doc 两个时间相差的天数
time_diff_in_days(T1, T2) ->
    {D1, _} = now_sec_to_local_time(T1),
    {D2, _} = now_sec_to_local_time(T2),
    abs(calendar:date_to_gregorian_days(D1) - calendar:date_to_gregorian_days(D2)).

%% @doc 当前时间字符串
localtime_string() ->
    {{Y, Mon, Day}, {H, M, S}} = calendar:local_time(),
    io_lib:format(<<"~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w ">>, 
                  [Y, Mon, Day, H, M, S]).

%% @doc 当前时间字符串
localtime_string_min() ->
    Time = calendar:local_time(),
    localtime_string_min(Time).
localtime_string_min({{Y, Mon, Day}, {H, M, S}}) ->
    io_lib:format(<<"~w~2..0w~2..0w-~2..0w~2..0w~2..0w ">>, [Y, Mon, Day, H, M, S]).

%% @doc 获取进程的消息队列长度
msg_queue_len() ->
    msg_queue_len(self()).
msg_queue_len(Pid) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, Len} ->
            Len;
        Other ->
            Other
    end.

%% @doc 文本内容是否合法
is_text_valid(Content, MinLen, MaxLen)->
    CList = unicode:characters_to_list(Content),
    CLen = erlang:length(string:strip(CList, both)),
    CLen >= MinLen andalso CLen =< MaxLen.

%% @doc 设置结束原因(不使用exit(Reason)，为了防止sasl报告)
set_terminate_reason(Reason) ->
    erlang:put('my_terminate_reason', Reason),
    ok.

%% @doc 获取结束原因
get_terminate_reason() ->
    erlang:get('my_terminate_reason').

%% @doc 获取平台
platform() ->
    ?CONFIG(platform).

%% @doc 获取server_id
server_id() ->
    ?CONFIG(server_id).

%% @doc 获取服务器代码版本号
get_server_code_version() ->
    L = game:module_info(compile),
    Opts = ?KV_GET(options, L), 
    case lists:keyfind('VERSION', 2, Opts) of
        false ->
            <<"">>;
        {_, 'VERSION', V} ->
            ?S2B(?A2S(V))
    end.

%%-------------------
%% EUNIT Test
%%-------------------
-ifdef(EUNIT).

sqrt_test_() ->
    [
        ?_assertEqual(0, sqrt(0, 2)),
        ?_assertEqual(0, sqrt(1, 2)),
        ?_assertEqual(1, sqrt(2, 2)),
        ?_assertEqual(2, sqrt(4, 2)),
        ?_assertEqual(2, sqrt(7, 2)),
        ?_assertEqual(3, sqrt(8, 2)),
        ?_assertEqual(3, sqrt(9, 2))
    ].

%% 测试是否为统一周
%{{2012,11,22},{15,26,43}}
t() ->
    1353569203.

d(N) ->
    86400 * N.

is_time_in_one_week_test_() ->
    [   
        ?_assertEqual(true, is_time_in_one_week(t(), t())),

        % 往前走
        ?_assertEqual(true, is_time_in_one_week(t(), t() - d(1))),
        ?_assertEqual(true, is_time_in_one_week(t(), t() - d(2))),
        ?_assertEqual(true, is_time_in_one_week(t(), t() - d(3))),
        ?_assertEqual(false, is_time_in_one_week(t(), t() - d(4))),
        ?_assertEqual(true, is_time_in_one_week(t(), t() - d(3) - 15 * 3600 - 26 * 60 - 42)),
        ?_assertEqual(true, is_time_in_one_week(t(), t() - d(3) - 15 * 3600 - 26 * 60 - 43)),
        ?_assertEqual(false, is_time_in_one_week(t(), t() - d(3) - 15 * 3600 - 26 * 60 - 44)),

        % 往后走
        ?_assertEqual(true, is_time_in_one_week(t(), t() + d(1))),
        ?_assertEqual(true, is_time_in_one_week(t(), t() + d(2))),
        ?_assertEqual(true, is_time_in_one_week(t(), t() + d(3))),
        ?_assertEqual(false, is_time_in_one_week(t(), t() + d(3) + 9 * 3600)),
        ?_assertEqual(false, is_time_in_one_week(t(), t() + d(3) + 17 + 33 * 60 + 8 * 3600)),
        ?_assertEqual(true, is_time_in_one_week(t(), t() + d(3) + 16 + 33 * 60 + 8 * 3600))
    ].  


-endif.
