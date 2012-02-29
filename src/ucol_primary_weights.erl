-module(ucol_primary_weights).

%% for ucol.erl
-export([compare/3, 
        compare_left_remain/2,
        compare_right_remain/2,
        new/0, 
        result/1]).


-include("ucol.hrl").
-include("ucol_weights.hrl").



-record(mem, {
    state, l1
}).


%% Create a new comparator
new() -> undefined.


%% Create a new weight. Used as a mock for compare/3
%% @private
empty() -> [].


%% Finish comparation
result(undefined) -> equal;
result(equal) -> equal;
result(#mem{state=undefined, l1=L1}) ->
    result_elem(L1);
result(#mem{state=S, l1=L1}) ->
    #state{left_l1=LeftL1, right_l1=RightL1} = S,
    W1L1 = hangul_result(LeftL1),
    W2L1 = hangul_result(RightL1),
    NewL1 = compare_filled_elems(L1, W1L1, W2L1),
    result_elem(NewL1).


result_elem({left,_}) -> greater;
result_elem({right,_}) -> less;
result_elem(equal) -> equal.


%% less, greater
%% Remains: {left, queue:queue()}, {right, queue:queue()}

%% There were no difficult weights.
compare(W1L1, W2L1, undefined) ->
    {NewW1L1, NewW2L1, NewState} = hangul_l1(W1L1, W2L1),
    L1 = simple_compare_elems(NewW1L1, NewW2L1),
    case L1 of 
        less -> less;
        greater -> greater;
        equal -> undefined;
        _ -> #mem{l1=L1}
    end;

compare(W1L1, W2L1, M=#mem{state=State, l1=X1}) ->
    {NewW1L1, NewW2L1, NewState} = hangul_l1(State, W1L1, W2L1),
    L1 = compare_filled_elems(X1, NewW1L1, NewW2L1),
    case L1 of 
        less -> less;
        greater -> greater;
        equal when State =:= undefined -> undefined;
        _ -> #mem{state=NewState, l1=L1}
    end.


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
%% Hangul helpers
%%

hangul_result(State) -> ucol_hangul:result(State).
process_hangul(State, Weight) -> ucol_hangul:process(State, Weight).
