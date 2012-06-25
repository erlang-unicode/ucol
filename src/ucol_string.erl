%%% Zipper interface.
%%% head -- get first element, move pointer forward
%%% fix  -- delete previous elements
%%% back -- move pointer on previos position

-module(ucol_string).
-export([new/1, 
    new_with_head/2,
    head/1, 
    back_and_skip/1, 
    back/1, 
    fix/1, 
    head_buffer/1,
    last_skipped_class/1,
    last_class/1]).

-export([decomp_class/0]).
-export([to_nfd/1]).

-include("ucol.hrl").

-type class() :: 0..255.
-type point() :: char().
-type element() :: {point(), class()}.

-record(ucol_string, {
    binary :: binary(),
    skip = [] :: [element()],
    %% zipper
    head = [] :: [element()],
    tail = [] :: [element()]
}).

-type ucol_string() :: #ucol_string{}.



-spec new(binary()) -> ucol_string().
new(Str) when is_binary(Str) ->
    #ucol_string{binary=Str}.


new_with_head(Str, Head) when is_binary(Str) ->
    #ucol_string{binary=Str, head=Head}.


-spec head(ucol_string()) -> eol | eob | {element(), #ucol_string{}}.
head(#ucol_string{tail=[], binary= <<>>}) ->
    stop;

head(Rec=#ucol_string{tail=[], head=Head, binary=Bin}) ->
    {[Cur|Tail], RemBin} = normalize(Bin, 0, []),
    {Cur, Rec#ucol_string{tail=Tail, head=[Cur|Head], binary=RemBin}};

head(Rec=#ucol_string{tail=[Cur|Tail], head=Head}) ->
    {Cur, Rec#ucol_string{tail=Tail, head=[Cur|Head]}}.


head_buffer(#ucol_string{head=Head}) -> lists:reverse(Head).


last_class(#ucol_string{
    head=[{_Char, Class}|_]}) -> Class.

last_skipped_class(#ucol_string{
    skip=[{_Char, Class}|_]}) -> Class.


fix(Rec=#ucol_string{skip=[], head=[]}) -> 
    Rec;
fix(Rec=#ucol_string{skip=[]}) -> 
    Rec#ucol_string{head=[]};
fix(Rec=#ucol_string{skip=Skip, tail=Tail}) -> 
    Rec#ucol_string{head=[], skip=[], tail=lists:reverse(Skip, Tail)}.


back(Rec=#ucol_string{head=[Cur|Head], tail=Tail}) -> 
    Rec#ucol_string{head=Head, tail=[Cur|Tail]}.


skip(Rec=#ucol_string{skip=Skip, tail=[Cur|Tail]}) -> 
    Rec#ucol_string{skip=[Cur|Skip], tail=Tail}.


back_and_skip(Rec=#ucol_string{skip=Skip, head=[Cur|Head]}) -> 
    Rec#ucol_string{skip=[Cur|Skip], head=Head}.


decomp_class() -> ?DECOMP_CLASS.


%%
%% Helpers
%%

ccc(Point) -> ucol_map:get(Point, ucol_unidata:ccc()).

%% Return [{char(), ccc()}].
decomp(Point) -> array:get(Point, ucol_unidata:decomp()).


%% Hangul decompose
%% foldl-like function, cannot be a list-comprehension.
normalize(<<Point/utf8, Rem/binary>>, _LastClass, List) 
    when ?CHAR_IS_HANGUL_S(Point) ->
    SIndex = Point - ?HANGUL_SBASE,
    %% LVT chars from Halgul
    L = ?HANGUL_LBASE + (SIndex div ?HANGUL_NCOUNT),
    V = ?HANGUL_VBASE + (SIndex rem ?HANGUL_NCOUNT) div ?HANGUL_TCOUNT,
    T = ?HANGUL_TBASE + (SIndex rem ?HANGUL_TCOUNT),

    NewList = lists:reverse(List, 
        case T of %% T is not a tail.
            ?HANGUL_TBASE -> [{L, 0}, {V, 0}];
            _ -> [{L, 0}, {V, 0}, {T, 0}]
        end),
    {NewList, Rem};

%% For example, string with classes:
%% `[0, 2, 8, 4, 0, 5]'
%% It returns 
%% `[0, 2, 4, 8]', 
%% and remains are 
%% `[0, 5]'.
%%
%% If `(LastCCC > CCC) and (CCC =/= 0)' then char is not in NF.
normalize(<<Point/utf8, Rem/binary>>, LastClass, List) ->
    Class = ccc(Point),
    H = {Point, Class},
    if
    Class =:= 0, List =:= [] -> {[H], Rem}; % Maybe deleted
    Class =:= 0              -> {lists:reverse([H|List]), Rem};
    Class =:= ?DECOMP_CLASS  -> 
        {NewLastClass, NewList} = 
            decompose(decomp(Point), LastClass, List),
        normalize(Rem, NewLastClass, NewList);
    LastClass =< Class       -> normalize(Rem, Class, [H|List]);
    % not in nf LastClass > Class
    true -> normalize(Rem, LastClass, proper_insert(H, List, []))
    end;

normalize(<<>>, _LastClass, List) -> {lists:reverse(List), <<>>}.


%% Transform a list of code points into `[{Point, CCC}]'.
to_nfd(List) -> to_nfd(List, 0, []).

to_nfd([Point|Tail], _, Acc) 
    when ?CHAR_IS_HANGUL_S(Point) ->
    SIndex = Point - ?HANGUL_SBASE,
    L = ?HANGUL_LBASE + (SIndex div ?HANGUL_NCOUNT),
    V = ?HANGUL_VBASE + (SIndex rem ?HANGUL_NCOUNT) div ?HANGUL_TCOUNT,
    T = ?HANGUL_TBASE + (SIndex rem ?HANGUL_TCOUNT),

    NewAcc =
        case T of
            ?HANGUL_TBASE -> [{V, 0}, {L, 0} | Acc];
            _ -> [{T, 0},{V, 0}, {L, 0} | Acc]
        end,
    to_nfd(Tail, 0, NewAcc);

to_nfd([Point|Tail], LastClass, Acc) -> 
    Class = ccc(Point),
    H = {Point, Class},
    if
    Class =:= 0              -> to_nfd(Tail, 0, [H|Acc]);
    Class =:= ?DECOMP_CLASS  -> 
        {NewLastClass, NewAcc} = 
            decompose(decomp(Point), LastClass, Acc),
        to_nfd(Tail, NewLastClass, NewAcc);
    LastClass =< Class       -> to_nfd(Tail, Class, [H|Acc]);
    % not in nf LastClass > Class
    true -> to_nfd(Tail, LastClass, proper_insert(H, Acc, []))
    end;

to_nfd([], _LastClass, Acc) -> lists:reverse(Acc).


%%
%% Helpers
%%



decompose([], LastClass, List) -> {LastClass, List};
decompose([H|T], LastClass, List) -> 
    {_Point, Class} = H,
    if
        Class =:= 0 -> decompose(T, Class, [H|List]);
        LastClass =< Class -> decompose(T, Class, [H|List]);
        true -> decompose(T, LastClass, proper_insert(H, List, []))
    end.


%% Finds right position for the character.
proper_insert({_Point, Class}=X, [{_PointH, ClassH}=H|T], Acc) 
    when ClassH > Class ->
    proper_insert(X, T, [H|Acc]);

proper_insert(X, T, Acc) ->
    lists:reverse([X|Acc], T).




-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(M, ucol_string).

%% [3780,1425,3805,97]
%% [{1425,220},{3805,0},{97,0}]
proper_insert_test_() ->
    [?_assertEqual(proper_insert(
        {a,4}, [{d,8}, {c,1}, {b,0}], []), [{d,8}, {a,4}, {c,1}, {b,0}])
    ,?_assertEqual(proper_insert(
        {a,4}, [{d,8}, {c,4}, {b,0}], []), [{d,8}, {a,4}, {c,4}, {b,0}])
    ].


error1_v1_test_() ->
    Bin = unicode:characters_to_binary([68159,820]),
    S1 = ?M:new(Bin),
    {{Char1, _Class1}, S2} = ?M:head(S1),
    {{Char2, _Class2}, S3} = ?M:head(S2),
    Stop = ?M:head(S3),
    [?_assertEqual(Char1, 820)
    ,?_assertEqual(Char2, 68159)
    ,?_assertEqual(Stop, stop)
    ].


error1_v2_test_() ->
    Bin = unicode:characters_to_binary([68159,97]),
    S1 = ?M:new(Bin),
    {{Char1, _Class1}, S2} = ?M:head(S1),
    {{Char2, _Class2}, S3} = ?M:head(S2),
    Stop = ?M:head(S3),
    [?_assertEqual(Char1, 68159)
    ,?_assertEqual(Char2, 97)
    ,?_assertEqual(Stop, stop)
    ].


error2_test_() ->
    Bin = unicode:characters_to_binary([4018, 98]),
    S1 = ?M:new(Bin),
    {{Char1, _Class1}, S2} = ?M:head(S1),
    {{Char2, _Class2}, S3} = ?M:head(S2),
    S4 = ucol_string:back_and_skip(S3),
    Stop = ?M:head(S4),
    S5 = ?M:fix(S4),
    {{Char3, _Class3}, S6} = ?M:head(S5),
    [?_assertEqual(Char2, Char3)].


error4_test_() ->
    %% [945,833,820] -> to nfd -> [945,820,769]
    %% [8049,820] -> to_nfd -> [945,820,769 (ccc 230)]
    Bin = unicode:characters_to_binary([945, 833, 820]),
    S1 = ?M:new(Bin),
    {{Char1, _Class1}, S2} = ?M:head(S1),
    {{Char2, _Class2}, S3} = ?M:head(S2),
    {{Char3, _Class3}, S4} = ?M:head(S3),
    Stop = ?M:head(S4),
    [?_assertEqual(Char1, 945)
    ,?_assertEqual(Char2, 820)
    ,?_assertEqual(Char3, 769)
    ].


error7_test_() ->
    Bin = unicode:characters_to_binary([1575, 1425, 1621]),
    S1 = ?M:new(Bin),
    {{Char1, _Class1}, S2} = ?M:head(S1),
    {{Char2, _Class2}, S3} = ?M:head(S2), % x
    S4 = ?M:back_and_skip(S3),
    {{Char5, _Class5}, S5} = ?M:head(S4), % y
    S6 = ?M:back_and_skip(S5),

    S7 = ?M:fix(S6),
    {{Char8, _Class8}, S8} = ?M:head(S7), % x
    {{Char9, _Class9}, S9} = ?M:head(S8), % y

    [?_assertEqual(Char2, Char8)
    ,?_assertEqual(Char5, Char9)
    ].


error11_test_() ->
    %% ux_string:to_nfd([55202]).
    %% [4370,4469,4545]
    Bin = unicode:characters_to_binary([55202]),
    S1 = ?M:new(Bin),
    {{Char1, _Class1}, S2} = ?M:head(S1),
    {{Char2, _Class2}, S3} = ?M:head(S2),
    {{Char3, _Class3}, S4} = ?M:head(S3),
    Stop = ?M:head(S4),
    [?_assertEqual(Char1, 4370)
    ,?_assertEqual(Char2, 4469)
    ,?_assertEqual(Char3, 4545)
    ,?_assertEqual(Stop, stop)
    ].


error12_test_() ->
    %% ux_string:to_nfd([44032]).        
    %% [4352,4449]
    Bin = unicode:characters_to_binary([44032]),
    S1 = ?M:new(Bin),
    {{Char1, _Class1}, S2} = ?M:head(S1),
    {{Char2, _Class2}, S3} = ?M:head(S2),
    Stop = ?M:head(S3),
    [?_assertEqual(Char1, 4352)
    ,?_assertEqual(Char2, 4449)
    ,?_assertEqual(Stop, stop)
    ].


%% 4019 3953 3968 {non_variable,9376,32,2,65535}
%% 820            {non_variable,[],124,2,65535}
error13_test_() ->
    %% ux_string:to_nfd([3960,820,3953]).               
    %% [4019,820,3953,3968]
    Bin = unicode:characters_to_binary([3960,820,3953]),
    S1 = ?M:new(Bin),
    {{Char1, _Class1}, S2} = ?M:head(S1),
    {{Char2, _Class2}, S3} = ?M:head(S2),
    {{Char3, _Class3}, S4} = ?M:head(S3),
    {{Char4, _Class4}, S5} = ?M:head(S4),
    Stop = ?M:head(S5),
    [?_assertEqual(Char1, 4019) % 0
    ,?_assertEqual(Char2, 820)  % 1
    ,?_assertEqual(Char3, 3953) % 129
    ,?_assertEqual(Char4, 3968) % 130
    ,?_assertEqual(Stop, stop)
    ].

%% 4019 3953 3968 {non_variable,9376,32,2,65535}
%% 63             {variable,[],[],[],640}
error13_case2_test_() ->
    %% ux_string:to_nfd([4019,3969,63]).                
    %% [4019,3953,3968,63]
    Bin = unicode:characters_to_binary([4019,3969,63]),
    S1 = ?M:new(Bin),
    {{Char1, _Class1}, S2} = ?M:head(S1),
    {{Char2, _Class2}, S3} = ?M:head(S2),
    {{Char3, _Class3}, S4} = ?M:head(S3),
    {{Char4, _Class4}, S5} = ?M:head(S4),
    Stop = ?M:head(S5),
    [?_assertEqual(Char1, 4019) % 0
    ,?_assertEqual(Char2, 3953) % 129
    ,?_assertEqual(Char3, 3968) % 130
    ,?_assertEqual(Char4, 63)   % 0
    ,?_assertEqual(Stop, stop)
    ].


error14_test_() ->
    Bin = unicode:characters_to_binary([3780,1425,3805,97]),
    S1 = ?M:new(Bin),
    {{Char1, _Class1}, S2} = ?M:head(S1),
    {{Char2, _Class2}, S3} = ?M:head(S2),
    {{Char3, _Class3}, S4} = ?M:head(S3),
    {{Char4, _Class4}, S5} = ?M:head(S4),
    Stop = ?M:head(S5),
    [?_assertEqual(Char1, 3780) 
    ,?_assertEqual(Char2, 1425) 
    ,?_assertEqual(Char3, 3805) 
    ,?_assertEqual(Char4, 97)   
    ,?_assertEqual(Stop, stop)

    ,?_assertEqual(normalize(<<214,145,224,187,157,97>>, 0, []), 
        {[{1425,220},{3805,0}],<<97>>})
    ].


to_nfd_test_() ->
    [?_assertEqual(?M:to_nfd("abc"), [{$a, 0}, {$b, 0}, {$c, 0}])].



-endif.
