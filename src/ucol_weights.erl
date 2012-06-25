-module(ucol_weights).

%% For generators (ucol_data)
-export([decode/1]).
-export([primary_weight/1,
         is_hangul/1]).

%% for ucol.erl
-export([compare/3, 
        compare_left_remain/2,
        compare_right_remain/2,
        new/0, 
        new/1, 
        empty/1,
        result/1, 
        type/1, 
        is_empty/1]).


-include("ucol.hrl").
-include("ucol_weights.hrl").



-record(mem, {
    state, l1, l2, l3, l4
}).


%% Create a new comparator
new() -> undefined.


%% Create a new comparator where L3=X
new(equal) -> undefined;
new(X) when X =:= less; X =:= greater -> #mem{l1=equal, l2=equal, l3=X}.


%% Create a new weight. Used as a mock for compare/3
%% @private
empty() -> {non_variable, [], [], [], []}.


%% Make an empty weight of the type Type.
%% Used in ducet tables.
empty(W) -> type(W).


type({T, _, _, _, _}) -> T.

is_empty({_T, [], [], [], []}) -> true;
is_empty(_) -> false.


%% Finish comparation
result(#mem{state=undefined, l1=L1, l2=L2, l3=L3, l4=L4}) ->
    result_list([L1,L2,L3,L4]);
result(#mem{state=S, l1=L1, l2=L2, l3=L3, l4=L4}) ->
    #state{left_l1=LeftL1, right_l1=RightL1} = S,
    W1L1 = hangul_result(LeftL1),
    W2L1 = hangul_result(RightL1),
    NewL1 = compare_filled_elems(L1, W1L1, W2L1),
    result_list([NewL1,L2,L3,L4]);
result(undefined) -> equal.


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

    {NewW1L1, NewW2L1, NewState} = hangul_l1(W1L1, W2L1),
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
        true -> #mem{state=NewState, l1=L1, l2=L2, l3=L3, l4=L4}
    end
    end  % L3
    end  % L2
    end; % L1

compare(W1, W2, M=#mem{state=State, l1=X1, l2=X2, l3=X3, l4=X4}) ->
    {_, W1L1, W1L2, W1L3, W1L4} = W1,
    {_, W2L1, W2L2, W2L3, W2L4} = W2,

    {NewW1L1, NewW2L1, NewState} = hangul_l1(State, W1L1, W2L1),

    L1 = compare_filled_elems(X1, NewW1L1, NewW2L1),
    if 
        L1 =:= less; L1 =:= greater -> L1;
        true -> 

    if X2 =:= less; X2 =:= greater -> #mem{state=NewState, l1=L1, l2=X2};
        true ->
    L2 = compare_filled_elems(X2, W1L2, W2L2),

    if L2 =:= less; L2 =:= greater -> #mem{state=NewState, l1=L1, l2=L2};
        true ->

    if X3 =:= less; X3 =:= greater -> #mem{state=NewState, l1=L1, l2=L2, l3=X3};
        true ->
    L3 = compare_filled_elems(X3, W1L3, W2L3),

    if L3 =:= less; L3 =:= greater -> #mem{state=NewState, l1=L1, l2=L2, l3=L3};
        true ->

    if X4 =:= less; X4 =:= greater -> 
            #mem{state=NewState, l1=L1, l2=L2, l3=L3, l4=X4};
        true ->
    L4 = compare_filled_elems(X4, W1L4, W2L4),

    if L1 =:= equal, L2 =:= equal, L3 =:= equal, L4 =:= equal, 
        NewState =:= undefined ->
        undefined;
        true -> 
            #mem{state=NewState, l1=L1, l2=L2, l3=L3, l4=L4} end

    end  % L1
    end  % X2
    end  % L2
    end  % X3
    end  % L3
    end. % X4


%%
%% Compare remains. 
%% There are remains, when one string is longer then another.
%% Called from `ucol.erl'.
%%

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

    

%%
%% Data functions part
%%


%% {non_variable list, variable list}
decode(Bin) when is_binary(Bin) ->
    List = bin_to_list(Bin),
    {build(shifted(List, [])), build(shifted2(List, []))}.


%%
%% Data helpers
%%

%% Build a turple of lists from a list of turples
build({Var,List}) ->
    Empty = {[], [], [], []},
    Tuple = lists:foldl(fun add_tuple/2, Empty, List),
    {L1, L2, L3, L4} = Tuple,
    {Var, l1_hack(check(L1)), check(L2), check(L3), check(L4)}.


%% An argument is a reversed list.
check([X]) -> X;
check(X) -> lists:reverse(X). %% MARK1


%% Check and add a weight element on a level.
add_elem(0, T) -> T;
add_elem(H, T) -> [H|T]. %% see MARK1: reversed insert


add_tuple(Tuple, Acc) -> 
    {L1T, L2T, L3T, L4T} = Acc,
    {L1H, L2H, L3H, L4H} = Tuple,
    { add_elem(L1H, L1T)
    , add_elem(L2H, L2T)
    , add_elem(L3H, L3T)
    , add_elem(L4H, L4T)
    }.


%%
%% Hangul helpers
%%

%% It is helper for __debugging__ only.
%% It converts code paint to a list of hangul types (L, V, T or X).
hangul_point(Point) -> 
    {element, {_, L1, _, _, _}} 
        = ucol_array:get(Point, ucol_unidata:ducet()),

    Types = case L1 of
        {hangul_l, [_|_]=L1X} -> [ucol_hangul:type(X) || X <- L1X];
        {hangul_l, _} -> [l];
        [_|_] -> [ucol_hangul:type(X) || X <- L1];
        _ -> [ucol_hangul:type(L1)]
    end.


hangul_result(State) -> ucol_hangul:result(State).
process_hangul(State, Weight) -> ucol_hangul:process(State, Weight).
l1_hack(L1) -> ucol_hangul:add_mark(L1).
    

primary_weight({_Type, [], _, _, _}) -> empty;
primary_weight({_Type, L1, _, _, _}) -> L1.

%% Check any of L1 elements is in L, V form.
is_hangul(X) when is_integer(X) -> is_l1_hangul(X);
is_hangul([_|_]=X) -> lists:any(fun is_l1_hangul/1, X);
is_hangul(_) -> false.


is_l1_hangul(X) -> ?IS_L1_OF_HANGUL_V(X) orelse ?IS_L1_OF_HANGUL_L(X).

%%
%% Decode Helpers
%% This functions convert weights from ux format to ucol format.
%% for `ucol_data'
%%

bin_to_list(Bin) ->
    [{case Var of
        0 -> non_variable;
        1 -> variable 
      end, L1, L2, L3, L4} 
     || <<Var:8, L1:16, L2:8, L3:8, L4:16>> <= Bin ].


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





-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(M, ?MODULE).

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
    X4 = ?M:compare(empty(), W820, X3),
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

-endif.
