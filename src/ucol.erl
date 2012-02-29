-module(ucol).
-export([compare/2]).

%% examples
-export([t1/0]).

t1() -> ucol_primary:compare(<<"лол">>, <<"Лол">>).


-define(RES(X), X).
-define(VAL(X), X).

-define(HANDLE_OTHER(Str, Other),
    begin
        {empty, Type} = Other,
        extract(ucol_string:fix(Str), ducet(Type), false, false)
    end).

-define(NEXT_ARRAY(Elem, PrevArr), ducet(ucol_weights:type(Elem))).
-define(WEIGHTS_MODULE, ucol_weights).

-include("ucol.hrl").
-include("ucol_base.hrl").

%% For debugging
%-define(RES(X), {X, ?LINE}).
%-define(VAL(X), erlang:element(1, X)).
%-define(DVAL(X), begin io:write(user, X), erlang:element(1, X) end).



%%
%% API
%%

-spec compare(binary(), binary()) -> equal | less | greater.

compare(S, S) -> equal;

compare(S1, S2) ->
    simple_compare(S1, S2, equal).


%%
%% Ovarloaded
%%

implicit(Point) ->
    ucol_implicit:synthesize(Point).


%% Returns a ducet array.
%% The default value is non_variable.
ducet(variable) ->
    ucol_unidata:var_ducet();

ducet(non_variable) ->
    ucol_unidata:ducet().

ducet() ->
    ucol_unidata:ducet().



%%
%% Helpers
%%

uca_compare(S1, S2) -> uca_compare(S1, S2, equal).


uca_compare(S1, S2, L3) ->
    {US1, US2} = ucol_prefix:handle(S1, S2),
    W1 = ucol_weights:new(L3),
    Arr = ducet(),
    compare_(US1, US2, W1, Arr, Arr).


simple_compare(<<Point1/utf8, Rem1/binary>> = S1, 
               <<Point2/utf8, Rem2/binary>> = S2, L3) ->
    Type1 = ?CHAR_TYPE(Point1),

    if Type1 =/= unicode ->
        Type2 = ?CHAR_TYPE(Point2),

        if Type2 =/= unicode ->
            if Point1 =:= Point2 -> 
                simple_compare(Rem1, Rem2, L3);

                Type1 =:= Type2 -> 
                    if Point1 < Point2 -> ?RES(less); true -> ?RES(greater) end;

                Type1 =:= lower, Type2 =:= upper ->
                    P1 = Point1 - 32,
                    if 
                        P1 < Point2 -> ?RES(less); 
                        P1 > Point2 -> ?RES(greater);
                        L3 =/= equal -> simple_compare(Rem1, Rem2, L3);
                        true -> simple_compare(Rem1, Rem2, ?RES(less))
                    end;

                Type2 =:= lower, Type1 =:= upper ->
                    P1 = Point1 + 32,
                    if 
                        P1 < Point2 -> ?RES(less); 
                        P1 > Point2 -> ?RES(greater);
                        L3 =/= equal -> simple_compare(Rem1, Rem2, L3);
                        true -> simple_compare(Rem1, Rem2, ?RES(greater))
                    end;

                Type1 =:= number -> ?RES(less);
                true  -> ?RES(greater) %% Type2 =:= number
            end;
                
            true -> uca_compare(S1, S2, L3)
        end;

        true -> uca_compare(S1, S2, L3)
    end;

simple_compare(<<>>, <<>>, L3) -> L3;
simple_compare(S1, S2, L3) -> uca_compare(S1, S2, ?VAL(L3)).






-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(M, ucol).
-define(CASES, true).

-ifdef(CASES).

extract_components_test_() ->
    S1 = ucol_string:new(<<"test">>),
    {_X1, S2} = ucol_string:head(S1),
    {_X2, S3} = ucol_string:head(S2),
    {_X3, S4} = ucol_string:head(S3),
    {_X4, S5} = ucol_string:head(S4),
    stop = ucol_string:head(S5),
    [].

extract_components2_test_() ->
    S1 = ucol_string:new(<<"test">>),
    {{P1, _C1}, S2} = ucol_string:head(S1),
    Arr = ducet(non_variable),
    {element, _E1} = ucol_array:get(P1, Arr),
    [].


extract_test_() ->
    S1 = ucol_string:new(<<"test">>),
    Arr = ducet(non_variable),
    LastSkippedClass = LastClass = false,
    X = {_W1, S2} = extract(S1, Arr, LastSkippedClass, LastClass),
    {_W2, S3} = extract(S2, Arr, LastSkippedClass, LastClass),
    {_W3, S4} = extract(S3, Arr, LastSkippedClass, LastClass),
    {_W4, S5} = extract(S4, Arr, LastSkippedClass, LastClass),
    stop      = extract(S5, Arr, LastSkippedClass, LastClass),
    [].

compare_test_() ->
    [?_assertEqual(?VAL(?M:compare(<<"test">>, <<"test">>)), equal)
    ,?_assertEqual(?VAL(?M:compare(<<"test">>, <<"t">>)), greater)
    ,?_assertEqual(?VAL(?M:compare(<<"abc">>, <<"def">>)), less)
    ].


%% http://unicode.org/reports/tr10/CollationTest.html
conformance_test_() ->
    [H|T] = ucol_testdata:shifted(),
    {timeout, 120, ?_test(test_conformance(T, H, 0))}.


%% ..each line in the file will order as being greater than or equal 
%% to the previous one...
test_conformance([H|T], Prev, ErrCnt) when is_binary(H) ->
    Success1 = case ?VAL(?M:compare(Prev, H)) of
        greater ->
            io:format(user, "Error case 1: ~w > ~w~n", 
                [unicode:characters_to_list(Prev), 
                 unicode:characters_to_list(H)]),
            false;
        less  -> true; 
        equal -> true
    end,

    Success2 = case ?VAL(?M:compare(H, Prev)) of
        less ->
            io:format(user, "Error case 2: ~w > ~w ~n", 
                [unicode:characters_to_list(Prev), 
                 unicode:characters_to_list(H)]),
            false;
        greater -> true; 
        equal   -> true
    end,

    Success3 = case ?VAL(uca_compare(Prev, H)) of
        greater ->
            io:format(user, "Error case 3: ~w > ~w ~n", 
                [unicode:characters_to_list(Prev), 
                 unicode:characters_to_list(H)]),
            false;
        less  -> true; 
        equal -> true
    end,

    Success4 = case ?VAL(uca_compare(H, Prev)) of
        less ->
            io:format(user, "Error case 4: ~w > ~w ~n", 
                [unicode:characters_to_list(Prev), 
                 unicode:characters_to_list(H)]),
            false;
        greater -> true; 
        equal   -> true
    end,

    Success = Success1 and Success2,

    test_conformance(T, H, ErrCnt + boolean_to_integer(not Success));

test_conformance([], Prev, ErrCnt) -> ?assertEqual(ErrCnt, 0).


boolean_to_integer(false) -> 0;
boolean_to_integer(true) -> 1.


error1_test_() ->
    S1 = unicode:characters_to_binary([820,68159]),
    S2 = unicode:characters_to_binary([68159,97]),
    [?_assertEqual(?VAL(?M:compare(S1, S2)), less) 
    ].


error2_test_() ->
    S1 = unicode:characters_to_binary([4028,65]),
    S2 = unicode:characters_to_binary([4018,98]),
    [?_assertEqual(?VAL(?M:compare(S1, S2)), less) 
    ].

%% [43710,63] lower [820,43710]
%% (ux@omicron)2> ux_uca:sort_array([43710,63]).          
%% [[non_variable,9281,32,2,43710],[variable,640,32,2,63]]
%% (ux@omicron)3> ux_uca:sort_array([820,43710]).         
%% [[non_variable,0,124,2,820],[non_variable,9281,32,2,43710]]

error3_test_() ->
    S1 = unicode:characters_to_binary([43710,63]),
    S2 = unicode:characters_to_binary([820,43710]),
    [?_assertEqual(?VAL(?M:compare(S1, S2)), less) 
    ].
    



%% A combining character sequence is called impeding if it contains any 
%% conjoining jamo, or if it contains an L1-ignorable combining mark and 
%% there is some character that canonically decomposes to a sequence 
%% containing the same base character. 
%% For example, the sequence <a, cedilla> is an impediment, because cedilla 
%% is an L1-ignorable character, and there is some character 
%% (for example, a-grave) that decomposes to a sequence containing the same 
%% base letter a. Note that although strings in Normalization Form C generally 
%% do not contain impeding sequences, there is nothing prohibiting them from
%% containing them. Conformant implementations that do not support impeding 
%% character sequences as part of their repertoire can also avoid performing 
%% the normalization in S1.1 of the algorithm

%% Error: [945,833,820] > [8049,820]

%% (ux@omicron)5> ux_uca:sort_array([8049,820]).               
%% [[non_variable,6364,32,2,945],
%%  [non_variable,0,50,2,769],
%%  [non_variable,0,124,2,820]]
%% (ux@omicron)6> ux_uca:sort_array([945,833,820]).         
%% [[non_variable,6364,32,2,945],
%%  [non_variable,0,50,2,833],
%%  [non_variable,0,124,2,820]]

error4_test_() ->
    %% [945,833,820] -> to nfd -> [945,820,769]
    %% [8049,820] -> to_nfd -> [945,820,769 (ccc 230)]
    %% [ux_unidata:ccc(X) || X <- [945,833,820]].
    %%      [0,230,1]
    %% 8049 {non_variable,6364,[32,50],[2,2],[65535,65535]}
    %% 820  {non_variable,[],124,2,65535}

    %% 945  {non_variable,6364,32,2,65535}
    %% 833  {non_variable,[],50,2,65535}}
    %% 820  {non_variable,[],124,2,65535}

    S1 = unicode:characters_to_binary([945,833,820]),
    S2 = unicode:characters_to_binary([8049,820]),
    [?_assertEqual(?VAL(?M:compare(S1, S2)), equal) 
    ,?_assertEqual(?VAL(?M:compare(S2, S1)), equal) 
    ].


%% Error: [65876,820] > [65876,33]

%% (ux@omicron)1> ux_uca:sort_array([65876,820]).
%% [[variable,5387,32,2,65501],
%%  [variable,0,0,0,376],
%%  [non_variable,0,124,2,820]]
%% (ux@omicron)2> ux_uca:sort_array([65876,33]). 
%% [[variable,5387,32,2,65501],
%%  [variable,0,0,0,376],
%%  [variable,635,32,2,33]]

%% 65876 {variable,[],[],[],5387}
%% 820   {non_variable,[],124,2,65535} (from ducet)
%% 820   none                          (from var_ducet)
%% 33    {variable,[],[],[],635}       (from ducet)
%% 33    {variable,[],[],[],635}       (from var_ducet)
error5_test_() ->
    S1 = unicode:characters_to_binary([65876,820]),
    S2 = unicode:characters_to_binary([65876,33]),
    [?_assertEqual(?VAL(?M:compare(S1, S2)), less) 
    ,?_assertEqual(?VAL(?M:compare(S2, S1)), greater) 
    ].


%% (ucol_data@omicron)3> ux_uca:sort_array([1140,1,783,97]).        
%% [[non_variable,6972,32,8,1140],
%%  [non_variable,0,101,2,783],
%%  [non_variable,5539,32,2,97]]
%% (ucol_data@omicron)4> ux_uca:sort_array([1141,98]).      
%% [[non_variable,6972,32,2,1141],[non_variable,5561,32,2,98]]
%% (ucol_data@omicron)15> ux_uca:sort_array([1140,783,97]).  
%% [[non_variable,6976,32,8,1142],[non_variable,5539,32,2,97]]

%% Error: [1140,1,783,97] > [1141,98]
error6_test_() ->
    S1 = unicode:characters_to_binary([1140,1,783,97]),
    S2 = unicode:characters_to_binary([1141,98]),
    [?_assertEqual(?VAL(?M:compare(S1, S2)), less) 
    ,?_assertEqual(?VAL(?M:compare(S2, S1)), greater) 
    ].


%% Error: [1575,1,1621,97] > [1575,1425,1621,97]
%% (ucol_data@omicron)16> ux_uca:sort_array([1575,1425,1621,97]).
%% [[non_variable,7238,32,2,1575],
%%  [non_variable,0,0,0,1425],
%% [non_variable,0,178,2,1621],
%% [non_variable,5539,32,2,97]]
%% (ucol_data@omicron)17> ux_uca:sort_array([1575,1,1621,97]).   
%% [[non_variable,7238,32,2,1575],
%%  [non_variable,0,178,2,1621],
%%  [non_variable,5539,32,2,97]]
%%
error7_test_() ->
    S1 = unicode:characters_to_binary([1575,1,1621,97]),
    S2 = unicode:characters_to_binary([1575,1425,1621,97]),
    [?_assertEqual(?VAL(?M:compare(S1, S2)), equal) 
    ].


%% NO_MORE: {ucol_string,<<>>,[],[{3953,129},{4018,0}],[{33,0}]}
%% Error: [4028,98] > [4018,3953,33]
error8_test_() ->
    S1 = unicode:characters_to_binary([4028,98]),
    S2 = unicode:characters_to_binary([4018,3953,33]),
    [?_assertEqual(?VAL(?M:compare(S1, S2)), less) 
    ].


%% Error: [194572,33] > [13470,63]
error9_test_() ->
    S1 = unicode:characters_to_binary([194572,33]),
    S2 = unicode:characters_to_binary([13470,63]),
    [?_assertEqual(?VAL(?M:compare(S1, S2)), less) 
    ].


%% (ucol_data@omicron)2> ux_string:to_nfd([44032,65]).
%% [4352,4449,65]
%% (ucol_data@omicron)3> ux_string:to_nfd([12814,97]).
%% [12814,97]
error10_test_() ->
    S1 = unicode:characters_to_binary([44032,65]),
    S2 = unicode:characters_to_binary([12814,97]),
    [?_assertEqual(?VAL(?M:compare(S1, S2)), less) 
    ].


% Error: [55202,98] > [55203,33]
error11_test_() ->
    S1 = unicode:characters_to_binary([55202,98]),
    S2 = unicode:characters_to_binary([55203,33]),
    [?_assertEqual(?VAL(?M:compare(S1, S2)), less) 
    ].


%% Error: [12910,98] > [44032,4449,33]
%%        [12910,98] > [4352,4449,4449,33]

%% ucol_array:get(12910, ucol_unidata:ducet()).
%% {element,{non_variable,{hangul_l,[12337 (L), 12463 (V), TERMINATOR]},
%%                     "  ",
%%                     [6,6],
%%                     [65535,65535]}}

%% [ucol_array:get(X, ucol_unidata:ducet()) || X <- [4352,4449,4449]]. 
%% [{element,{non_variable,{hangul_l,[12337]},32,2,65535}},
%%  {element,{non_variable,12463,32,2,65535}},
%%  {element,{non_variable,12463,32,2,65535}}]


error12_test_() ->
    S1 = unicode:characters_to_binary([12910,98]),
    S2 = unicode:characters_to_binary([44032,4449,33]),
    [?_assertEqual(?VAL(?M:compare(S1, S2)), less) 
    ].


%% (ucol_data@omicron)11> ux_string:to_nfd([4019,3969,63]).
%% [4019,3953,3968,63]
%% (ucol_data@omicron)12> ux_string:to_nfd([3960,820,3953]).
%% [4019,820,3953,3968]

%% (ucol_data@omicron)16> ux_uca:sort_array([3960,820,3953]).
%% [[non_variable,9375,32,2,3960],
%%  [non_variable,0,124,2,820],
%%  [non_variable,9366,32,2,3953]]
%% (ucol_data@omicron)17> ux_uca:sort_array([4019,3969,63]). 
%% [[non_variable,9376,32,2,3961],[variable,640,32,2,63]]

%% Error: [4019,3969,63] > [3960,820,3953]
error13_test_() ->
    S1 = unicode:characters_to_binary([4019,3969,63]),
    S2 = unicode:characters_to_binary([3960,820,3953]),
    [?_assertEqual(?VAL(?M:compare(S1, S2)), less) 
    ].

 
%% Error: [3780,1,3805,97] > [3780,1425,3805,97]
%% CCC:   [   0,0,   0, 0]   [   0, 220,   0, 0]
error14_test_() ->
    S1 = unicode:characters_to_binary([3780,1,3805,97]),
    S2 = unicode:characters_to_binary([3780,1425,3805,97]),
    [?_assertEqual(?VAL(?M:compare(S1, S2)), equal) 
    ].



%% [ucol_weights:hangul_point(X) || X <- [4370,4469,4469,98]]. 
%% [[l],v,v,x]

%% Error case 1: [55176,4469,98] > [55198,33]
%% [4370,4469,4469,98] [4370,4469,4541,33]


%%  [ucol_array:get(X, ucol_unidata:ducet()) || X <- [4370,4469,4469,98]].
%%  {element,{non_variable,{hangul_l,[12355]},32,2,65535}},
%%  {element,{non_variable,12483,32,2,65535}},
%%  {element,{non_variable,12483,32,2,65535}},
%%  {element,{non_variable,5561,32,2,65535}}]
%%  [ucol_array:get(X, ucol_unidata:ducet()) || X <- [4370,4469,4541,33]].
%%  {element,{non_variable,{hangul_l,[12355]},32,2,65535}},
%%  {element,{non_variable,12483,32,2,65535}},
%%  {element,{non_variable,12578,32,2,65535}},
%%  {element,{variable,[],[],[],635}}]

%% (ucol@omicron)6> [ucol_weights:hangul_point(X) || X <- [4370,4469,4469,98]].
%% [[l],v,v,x]
%% (ucol@omicron)7> [ucol_weights:hangul_point(X) || X <- [4370,4469,4541,33]].
%% [[l],v,x,x]
%% 4541 11BD;HANGUL JONGSEONG CIEUC;Lo;0;L;;;;;N;;;;;

error15_test_() ->
    S1 = unicode:characters_to_binary([55176,4469,98]),
    S2 = unicode:characters_to_binary([55198,33]),
    S3 = unicode:characters_to_binary([4370,4469,4469,98]),
    S4 = unicode:characters_to_binary([4370,4469,4541,33]),
    [?_assertEqual(?VAL(?M:compare(S1, S2)), less) 
    ,?_assertEqual(?VAL(?M:compare(S3, S4)), less) 
    ].

-endif.


%% UNICODE 6.1


%% Error case 1: [40908,98] > [64014,33]
%% 
%% 9FCC (40908) ; syntatic from the block 'CJK Unified Ideographs'
%% FA0E (64014) ; [.FB41.0020.0002.FA0E][.FA0E.0000.0000.FA0E] 
%%      # CJK COMPATIBILITY IDEOGRAPH-FA0E

%% E1: {non_variable,[64449,40908],32,2,[]} 
%% E2: {non_variable,[64321,64014],32,2,[65535,65535]}

error16_test_() ->
    S1 = unicode:characters_to_binary([40908,98]),
    S2 = unicode:characters_to_binary([64014,33]),
    [?_assertEqual(?VAL(?M:compare(S1, S2)), less) 
    ].



-endif.
