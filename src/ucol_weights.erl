-module(ucol_weights).
-include("ucol.hrl").

%% For generators (ucol_data)
-export([decode/1]).


-export([compare/3, 
        compare_left_remain/2,
        compare_right_remain/2,
        new/0, 
        new/1, 
        empty/0,
        empty/1,
        result/1, 
        type/1, 
        is_empty/1]).

-export([hangul_type/1,
        hangul_point/1]).

-export([implicit/1]).


-define(M, ?MODULE).

-record(state, {
    left_l1, right_l1
}).

-record(mem, {
    state, l1, l2, l3, l4
}).


%% Create a new comparator
new() -> undefined.

new(equal) -> undefined;
new(X) when X =:= less; X =:= greater -> #mem{l1=equal, l2=equal, l3=X}.

%% Create a new weight. Used as a mock for compare/3
empty() -> {non_variable, [], [], [], []}.


%% Make an empty weight of the type Type.
%% Used in ducet tables.
empty(W) -> type(W).


type({T, _, _, _, _}) -> T.

is_empty({_T, [], [], [], []}) -> true;
is_empty(_) -> false.


%% Finish comparation
result(undefined) -> equal;
result(equal) -> equal;
result(#mem{state=undefined, l1=L1, l2=L2, l3=L3, l4=L4}) ->
    result_list([L1,L2,L3,L4]);
result(#mem{state=S, l1=L1, l2=L2, l3=L3, l4=L4}) ->
    #state{left_l1=LeftL1, right_l1=RightL1} = S,
    W1L1 = hangul_result(LeftL1),
    W2L1 = hangul_result(RightL1),
    NewL1 = compare_filled_elems(L1, W1L1, W2L1),
    result_list([NewL1,L2,L3,L4]).


result_list([H|T]) ->
    case H of
        {left,_} -> greater;
        {right,_} -> less;
        equal -> result_list(T);
        X -> X
    end;
result_list([]) -> equal.


%% less, greater
%% Remains: {left, queue:queue()}, {right, queue:queue()}

%% There were no difficult weights.
compare(W1, W2, undefined) ->
    {_, W1L1, W1L2, W1L3, W1L4} = W1,
    {_, W2L1, W2L2, W2L3, W2L4} = W2,
    
    IsLeftHangulL = case W1L1 of
        {hangul_l, _} -> true;
        _ -> false end,

    if W1 =:= W2, not IsLeftHangulL -> undefined; % Can be skipped
    true ->
    {StateL, NewW1L1} = case W1L1 of
        {hangul_l, W1L1X} -> process_hangul(x, W1L1X);
        _W1L1 -> {x, W1L1} end,

    {StateR, NewW2L1} = case W2L1 of
        {hangul_l, W2L1X} -> process_hangul(x, W2L1X);
        _W2L1 -> {x, W2L1} end,

    NewState = case {StateL, StateR} of
            {x, x} -> undefined;
            {_, _} -> #state{left_l1=StateL, right_l1=StateR}
        end,

    L1 = simple_compare_elems(NewW1L1, NewW2L1),
    if 
        L1 =:= less; L1 =:= greater -> L1;
        true -> 

    L2 = simple_compare_elems(W1L2, W2L2),
    if 
        L2 =:= less; L2 =:= greater -> 
            #mem{state=NewState, l1=L1, l2=L2};
        true -> 

    L3 = simple_compare_elems(W1L3, W2L3),
    if 
        L3 =:= less; L3 =:= greater -> 
            #mem{state=NewState, l1=L1, l2=L2, l3=L3};
        true -> 

    L4 = simple_compare_elems(W1L4, W2L4),

    if L1 =:= equal, L2 =:= equal, L3 =:= equal, L4 =:= equal, 
            NewState =:= undefined ->
        undefined;
        true -> #mem{state=NewState, l1=L1, l2=L2, l3=L3, l4=L4} end

    end  % L3
    end  % L2
    end  % L1
    end; % Can be skipped?

compare(W1, W2, M=#mem{state=State, l1=X1, l2=X2, l3=X3, l4=X4}) ->
    {_, W1L1, W1L2, W1L3, W1L4} = W1,
    {_, W2L1, W2L2, W2L3, W2L4} = W2,

    {IsHangulL, W1L1X} = case W1L1 of
        {hangul_l, W1L1I} -> {true, W1L1I};
        _W1L1 -> {false, W1L1} end,

    {IsHangulR, W2L1X} = case W2L1 of
        {hangul_l, W2L1I} -> {true, W2L1I};
        _W2L1 -> {false, W2L1} end,

    {OldStateHangulL, OldStateHangulR} = case State of
        undefined -> {x, x};
        #state{left_l1=LeftL1, right_l1=RightL1} -> 
            {LeftL1, RightL1} end,

    {StateL, NewW1L1} = if
        IsHangulL; OldStateHangulL =/= x -> 
            process_hangul(OldStateHangulL, W1L1X);
        true -> {x, W1L1X} end,

    {StateR, NewW2L1} = if
        IsHangulR; OldStateHangulR =/= x -> 
            process_hangul(OldStateHangulR, W2L1X);
        true -> {x, W2L1X} end,

    NewState = case {StateL, StateR} of
            {x, x} -> undefined;
            {_, _} -> #state{left_l1=StateL, right_l1=StateR}
        end,

    NewM = M#mem{state=NewState},

    L1 = compare_filled_elems(X1, NewW1L1, NewW2L1),
    if 
        L1 =:= less; L1 =:= greater -> L1;
        true -> 

    if X2 =:= less; X2 =:= greater -> NewM#mem{l1=L1};
        true ->
    L2 = compare_filled_elems(X2, W1L2, W2L2),

    if L2 =:= less; L2 =:= greater -> NewM#mem{l1=L1, l2=L2};
        true ->

    if X3 =:= less; X3 =:= greater -> NewM#mem{l1=L1, l2=L2};
        true ->
    L3 = compare_filled_elems(X3, W1L3, W2L3),

    if L3 =:= less; L3 =:= greater -> NewM#mem{l1=L1, l2=L2, l3=L3};
        true ->

    if X4 =:= less; X4 =:= greater -> NewM#mem{l1=L1, l2=L2, l3=L3};
        true ->
    L4 = compare_filled_elems(X4, W1L4, W2L4),

    if L1 =:= equal, L2 =:= equal, L3 =:= equal, L4 =:= equal, 
        NewState =:= undefined ->
        undefined;
        true -> NewM#mem{l1=L1, l2=L2, l3=L3, l4=L4} end

    end  % L1
    end  % X2
    end  % L2
    end  % X3
    end  % L3
    end. % X4


compare_left_remain(W1, M) ->
    W2 = empty(),
    case compare(W1, W2, M) of
        #mem{l1={left,_}} -> greater;
        X -> X
    end.


compare_right_remain(W2, M) ->
    W1 = empty(),
    case compare(W1, W2, M) of
        #mem{l1={right,_}} -> less;
        X -> X
    end.
    

%% Hangul (Terminator method): 
%%      LVX = LV_X 
%%      LVL = LV_L
%%      LVT = LVT_
%%      LVV = LVV_
%%      LLV = LLV_
%% Terminator: _ = 16#24C9.

process_hangul(State, Weight) when is_integer(Weight) ->
    process_hangul(State, [Weight], []);

process_hangul(State, Weights) ->
    process_hangul(State, Weights, []).
    

process_hangul(x, [H|T], Acc) when ?IS_L1_OF_HANGUL_L(H) ->
    process_hangul(l, T, [H|Acc]);

process_hangul(l, [H|T], Acc) when ?IS_L1_OF_HANGUL_L(H) ->
    process_hangul(ll, T, [H|Acc]);

process_hangul(l, [H|T], Acc) when ?IS_L1_OF_HANGUL_V(H) ->
    process_hangul(lv, T, [H|Acc]);

process_hangul(ll, [H|T], Acc) when ?IS_L1_OF_HANGUL_V(H) ->
    process_hangul(x, T, [?COL_HANGUL_TERMINATOR, H|Acc]); %% LLV_


%%      LVX = LV_X 
%%      LVL = LV_L
%%      LVT = LVT_
%%      LVV = LVV_
process_hangul(lv, [H|T], Acc) 
    when ?IS_L1_OF_HANGUL_T(H); ?IS_L1_OF_HANGUL_V(H) ->
    process_hangul(x, T, [?COL_HANGUL_TERMINATOR, H|Acc]); %% LVV, LVT

process_hangul(lv, [H|T], Acc) when ?IS_L1_OF_HANGUL_L(H) ->
    process_hangul(l, T, [H, ?COL_HANGUL_TERMINATOR|Acc]); %% LV_L

process_hangul(lv, [H|T], Acc) ->
    process_hangul(x, T, [H, ?COL_HANGUL_TERMINATOR|Acc]); %% LV_X

process_hangul(_, [H|T], Acc) ->
    process_hangul(x, T, [H|Acc]);

process_hangul(State, [], Acc) ->
    {State, lists:reverse(Acc)}.


hangul_point(Point) -> 
    {element, {_, L1, _, _, _}} 
        = ucol_array:get(Point, ucol_unidata:ducet()),
    case L1 of
        {hangul_l, [_|_]=L1X} -> [hangul_type(X) || X <- L1X];
        {hangul_l, _} -> l;
        [_|_] -> [hangul_type(X) || X <- L1];
        _ -> hangul_type(L1)
    end.


hangul_type(X) when ?IS_L1_OF_HANGUL_L(X) -> l;
hangul_type(X) when ?IS_L1_OF_HANGUL_V(X) -> v;
hangul_type(X) when ?IS_L1_OF_HANGUL_T(X) -> t;
hangul_type(X) -> x.

%% Finish hangul comparation.
hangul_result(lv) -> [?COL_HANGUL_TERMINATOR];
hangul_result(_) -> [].



compare_filled_elems(equal, W1, W2) ->
    simple_compare_elems(W1, W2); 

compare_filled_elems({left, Q}, W1, W2) ->
    compare_left(Q, to_list(W1), to_list(W2));

compare_filled_elems({right, Q}, W1, W2) ->
    compare_right(Q, to_list(W1), to_list(W2)).


to_list(X) when is_integer(X) -> [X];
to_list(X) -> X.


compare_left(Q, W1, [W2H|W2T]) ->
    case queue:out(Q) of
    {{value, W2H}, QT} -> % equal
        compare_left(QT, W1, W2T);

    {{value, W1H}, _QT} ->
        if W1H<W2H -> less;
              true -> greater end;

    {empty, _EmptyQueue} ->
        simple_compare_elems(W1, W2T)
    end;

compare_left(Q, W1, []) -> 
    check_query(left, list_to_queue(W1, Q)).


compare_right(Q, [W1H|W1T], W2) ->
    case queue:out(Q) of
    {{value, W1H}, QT} -> % equal
        compare_right(QT, W1T, W2);

    {{value, W2H}, _QT} ->
        if W1H<W2H -> less;
              true -> greater end;

    {empty, _EmptyQueue} ->
        simple_compare_elems(W1T, W2)
    end;

compare_right(Q, [], W2) -> 
    NewQ = list_to_queue(W2, Q),
    check_query(right, list_to_queue(W2, Q)).
    

check_query(Type, Q) ->
    case queue:is_empty(Q) of
        true -> equal;
        false -> {Type, Q} end.


list_to_queue([H|T], Q) -> list_to_queue(T, queue:in(H, Q));
list_to_queue([], Q) -> Q.

    
simple_compare_elems(W1, W1) -> equal;
simple_compare_elems(W1, W2) when is_integer(W1) -> 
    if is_integer(W2) -> 
            if W1<W2 -> less;
                true -> greater end;
       true -> % is_list(W2)
            compare_lists([W1], W2) end;

simple_compare_elems(W1, W2) when is_integer(W2) -> % is_list(W1)
    compare_lists(W1, [W2]);

simple_compare_elems(W1, W2) -> % is_list(W1), is_list(W2)
    compare_lists(W1, W2).


compare_lists([H|T1], [H|T2]) -> compare_lists(T1, T2);
compare_lists([H1|_], [H2|_]) -> 
    if H1<H2 -> less;
        true -> greater end;
compare_lists([], []) -> equal;
compare_lists([], W2) -> 
    check_query(right, queue:from_list(W2));
compare_lists(W1, __) ->
    check_query(left, queue:from_list(W1)).

    

%%
%% Data functions part
%%


%% {non_variable list, variable list}
decode(Bin) when is_binary(Bin) ->
    List = bin_to_list(Bin),
    {build(shifted(List, [])), build(shifted2(List, []))}.


%% Build a turple of lists from a list of turples
build({Var,List}) ->
    Empty = {[], [], [], []},
    Tuple = lists:foldl(fun add_tuple/2, Empty, List),
    {L1, L2, L3, L4} = Tuple,
    {Var, l1_hack(check(L1)), check(L2), check(L3), check(L4)}.


%% An argument is a reversed list.
check([X]) -> X;
check(X) -> lists:reverse(X). %% MARK1


l1_hack(X) when is_integer(X), ?IS_L1_OF_HANGUL_L(X) -> {hangul_l, [X]};
l1_hack(X) when is_integer(X) -> X;
l1_hack(List) ->
    HasHangulL = lists:any(fun(X) -> ?IS_L1_OF_HANGUL_L(X) end, List),
    case HasHangulL of
        true -> {hangul_l, List};
        false -> List
    end.


%% Check and add a weight element on a level.
add_elem(0, T) -> T;
add_elem(H, T) -> [H|T]. %% MARK1: reversed insert


add_tuple(Tuple, Acc) -> 
    {L1T, L2T, L3T, L4T} = Acc,
    {L1H, L2H, L3H, L4H} = Tuple,
    { add_elem(L1H, L1T)
    , add_elem(L2H, L2T)
    , add_elem(L3H, L3T)
    , add_elem(L4H, L4T)
    }.


bin_to_list(Bin) ->
    do_bin_to_list(Bin, []).

do_bin_to_list(<<Var:8, L1:16, L2:8, L3:8, L4:16, Rem/binary>>, Res) ->
    VarAtom = case Var of
        0 -> non_variable;
        1 -> variable 
        end,
    El = {VarAtom, L1, L2, L3, L4},
    do_bin_to_list(Rem, [El|Res]);
do_bin_to_list(<<>>, Res) ->
    lists:reverse(Res).




% If it is a tertiary ignorable, then L4 = 0.
shifted([{variable,0,0,0,_}|T], Acc) ->
    shifted2(T, Acc);

shifted([{non_variable,0,0,0,_}|T], Acc) ->
    shifted(T, Acc);

% If it is a variable, then L4 = Old L1.
shifted([{variable,L1,_,_,_}|T], Acc) ->
    shifted2(T, [{0, 0, 0, L1}|Acc]);

shifted([Value|T], Acc) ->
    shifted(T, [set_l4_to_value(Value, 16#FFFF)|Acc]);

shifted([], Acc) -> 
    {non_variable, lists:reverse(Acc)}.




%% @doc This function is a version of shifted/1, but its value is
%%      after variable.
%% @end
%% @private
% If it is a ignorable, then L4 = 0.
shifted2([{_,0,_,_,_}|T], Acc) ->
    shifted2(T, Acc);

% If it is a variable, then L4 = Old L1.
shifted2([{variable, L1, _,_,_}|T], Acc) ->
    shifted2(T, [{0, 0, 0, L1}|Acc]);

shifted2([Value|T], Acc) ->
    shifted(T, [set_l4_to_value(Value, 16#FFFF)|Acc]);

shifted2([], Acc) -> 
    {variable, lists:reverse(Acc)}.



set_l4_to_value({_Variable, L1, L2, L3, _L4}, Val) ->
    {L1, L2, L3, Val}.




% Table 18. Values for Base
% -----------------------------------------------------------------------------
% Range 1: Unified_Ideograph=True AND
% ((Block=CJK_Unified_Ideograph) OR (Block=CJK_Compatibility_Ideographs))
% Base  1: FB40
% Range 2: Unified_Ideograph=True AND NOT
% ((Block=CJK_Unified_Ideograph) OR (Block=CJK_Compatibility_Ideographs))
% Base  2: FB80
% Base  3: FBC0 Any other code point
% Range 3: Ideographic AND NOT Unified_Ideograph
% -----------------------------------------------------------------------------
implicit(H)
    when ?CHAR_IS_UNIFIED_IDEOGRAPH(H)
     and (?CHAR_IS_CJK_COMPATIBILITY_IDEOGRAPH(H)
       or ?CHAR_IS_CJK_UNIFIED_IDEOGRAPH(H)) ->
    do_implicit_weight(H, 16#FB40);

implicit(H)
    when ?CHAR_IS_UNIFIED_IDEOGRAPH(H)
      and (not (?CHAR_IS_CJK_COMPATIBILITY_IDEOGRAPH(H)
             or ?CHAR_IS_CJK_UNIFIED_IDEOGRAPH(H))) ->
    do_implicit_weight(H, 16#FB80);

implicit(H) ->
    do_implicit_weight(H, 16#FBC0).




%% @doc 7.1.3 Implicit Weights 
%% The result of this process consists of collation elements that are sorted in
%% code point order, that do not collide with any explicit values in the table,
%% and that can be placed anywhere (for example, at BASE) with respect to the 
%% explicit collation element mappings. By default, implicit mappings are given
%% higher weights than all explicit collation elements.
%% @end
%% @private
do_implicit_weight(CP, BASE) when is_integer(CP), is_integer(BASE) ->
    AAAA = BASE + (CP bsr 15),
    BBBB = (CP band 16#7FFF) bor 16#8000,
    {non_variable, [AAAA, BBBB], 32, 2, []}.




-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

weight(L1,L2,L3,L4) -> {variable,L1,L2,L3,L4}.

compare_test_() ->
    [?_assertEqual(?M:compare(weight(1,2,3,4), weight(2,2,3,4), new()), less)
    ,?_assertEqual(?M:compare(weight([1,2],2,3,4), weight(2,2,3,4), new()), less)
    ].


result_test_() ->
    [?_assertEqual(?M:result(
        ?M:compare(weight(1,2,3,4), weight(1,2,3,4), new())), equal)
    ,?_assertEqual(?M:result(
        ?M:compare(weight([1],2,3,4), weight(1,2,3,4), new())), equal)
    ,?_assertEqual(?M:result(?M:compare(
        weight([1],2,3,4), weight([1,2],2,3,4), new())), less)
    ,?_assertEqual(?M:result(?M:compare(
        weight([1],2,3,4), weight([1],2,4,4), new())), less)
    ].


error1_test_() ->
%   S1 = [820,68159],
%   S2 = [68159,97],
    W820 = {non_variable,[],124,2,65535},
    W97 = {non_variable,5539,32,2,65535},
    W68159 = {non_variable,9108,32,2,[65535,65535]},
    X1 = ?M:new(),
    X2 = ?M:compare(W820, W68159, X1),
    X3 = ?M:compare(W68159, W97, X2),
    X4 = ?M:result(X3),
    [?_assertEqual(X4, less)].


error4_test_() ->
    W8049 = {non_variable,6364,[32,50],[2,2],[65535,65535]},
    W820  = {non_variable,[],124,2,65535},

    W945  = {non_variable,6364,32,2,65535},
    W833  = {non_variable,[],50,2,65535},
    W820  = {non_variable,[],124,2,65535},

    X1 = ?M:new(),
    X2 = ?M:compare(W8049, W945, X1),
    X3 = ?M:compare(W820, W833, X2),
    X4 = ?M:compare(?M:empty(), W820, X3),
    X5 = ?M:result(X4),
    [?_assertEqual(X5, equal)].


%% Shifted: Variable collation elements are reset to zero at levels one 
%%          through three. In addition, a new fourth-level weight is 
%%          appended, whose value depends on the type, as shown in Table 12. 
%%          Any subsequent primary or secondary ignorables following a variable
%%          are reset so that their weights at levels one through four are zero.
shifted_test_() ->
    %% If Variable, then L4 = Old L1; L1..L3 = 0
    [?_assertEqual(shifted([{variable,1,2,3,4}], []), {variable, [{0,0,0,1}]})
    %% If Tertiary Ignorable, then L4 = 0
    ,?_assertEqual(shifted([{non_variable,0,0,0,4}], []), {non_variable, []})
    %% Primary or Secondary Ignorable, following a Variable
    ,?_assertEqual(shifted([{variable,1,2,3,4}, {non_variable,0,1,2,3}], []), 
        {variable, [{0,0,0,1}]})
    ,?_assertEqual(shifted([{variable,1,2,3,4}, {non_variable,0,0,2,3}, 
            {non_variable,0,0,2,3}], []), 
        {variable, [{0,0,0,1}]})
    ,?_assertEqual(shifted([{variable,1,2,3,4}, {non_variable,0,0,2,3}], []), 
        {variable, [{0,0,0,1}]})
    %% Primary or Secondary Ignorable, not following a Variable
    ,?_assertEqual(shifted([{non_variable,0,0,3,4}], []), 
        {non_variable, [{0,0,3,16#FFFF}]})
    ,?_assertEqual(shifted([{non_variable,0,2,3,4}], []), 
        {non_variable, [{0,2,3,16#FFFF}]})
    %% None of the above
    ,?_assertEqual(shifted([{non_variable,1,2,3,4}], []), 
        {non_variable, [{1,2,3,16#FFFF}]})
    ].

-define(TEST_HANGUL_L1_L, (?COL_HANGUL_LBASE+5)).
-define(TEST_HANGUL_L1_V, (?COL_HANGUL_VBASE+6)).
-define(TEST_HANGUL_L1_T, (?COL_HANGUL_TBASE+4)).

process_hangul_test_() ->
    [?_assertEqual(process_hangul(l, [?TEST_HANGUL_L1_V, ?TEST_HANGUL_L1_T]),
        {x, [?TEST_HANGUL_L1_V, ?TEST_HANGUL_L1_T, ?COL_HANGUL_TERMINATOR]})
    ].

process_hangul_error12_test_() ->
    S1 = [?COL_HANGUL_LBASE, ?COL_HANGUL_VBASE, ?COL_HANGUL_VBASE],
    [?_assertEqual(process_hangul(x, S1),
        {x, S1 ++ [?COL_HANGUL_TERMINATOR]})
    ].

process_hangul_error12_var2_test_() ->
    S1 = [?COL_HANGUL_LBASE, ?COL_HANGUL_VBASE],
    S2 = [?COL_HANGUL_VBASE],
    [?_assertEqual(process_hangul(x, S1), {lv, S1}) % LV
    ,?_assertEqual(process_hangul(lv, S2), 
        {x, S2 ++ [?COL_HANGUL_TERMINATOR]}) % LVV_
    ].

process_hangul_error14_test_() ->
    S1 = [?COL_HANGUL_TBASE],
    [?_assertEqual(process_hangul(lv, S1), 
        {x, S1 ++ [?COL_HANGUL_TERMINATOR]}) % LVT_
    ].

-endif.
