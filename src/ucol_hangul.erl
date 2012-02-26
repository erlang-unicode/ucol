-module(ucol_hangul).
-include("ucol.hrl").

%% For ucol_data
-export([add_mark/1]).

%% For ucol, ucol_weights
-export([process/2, 
         type/1, 
         result/1]).


%% Check if a primary weight X is L1 a hangul weight and add a mark.
add_mark(X) when is_integer(X), ?IS_L1_OF_HANGUL_L(X) -> {hangul_l, [X]};
add_mark(X) when is_integer(X) -> X;
add_mark(List) ->
    HasHangulL = lists:any(fun(X) -> ?IS_L1_OF_HANGUL_L(X) end, List),
    case HasHangulL of
        true -> {hangul_l, List};
        false -> List
    end.


%% Hangul (Terminator method): 
%%      LVX = LV_X 
%%      LVL = LV_L
%%      LVT = LVT_
%%      LVV = LVV_
%%      LLV = LLV_
%% Terminator: _ = 16#24C9.

%% State is a current state.
%% Weight is an accumulator for a primary weight.
process(State, Weight) when is_integer(Weight) ->
    process(State, [Weight], []);

process(State, Weights) ->
    process(State, Weights, []).
    

process(x, [H|T], Acc) when ?IS_L1_OF_HANGUL_L(H) ->
    process(l, T, [H|Acc]);

process(l, [H|T], Acc) when ?IS_L1_OF_HANGUL_L(H) ->
    process(ll, T, [H|Acc]);

process(l, [H|T], Acc) when ?IS_L1_OF_HANGUL_V(H) ->
    process(lv, T, [H|Acc]);

process(ll, [H|T], Acc) when ?IS_L1_OF_HANGUL_V(H) ->
    process(x, T, [?COL_HANGUL_TERMINATOR, H|Acc]); %% LLV_

%%      LVX = LV_X 
%%      LVL = LV_L
%%      LVT = LVT_
%%      LVV = LVV_
process(lv, [H|T], Acc) 
    when ?IS_L1_OF_HANGUL_T(H); ?IS_L1_OF_HANGUL_V(H) ->
    process(x, T, [?COL_HANGUL_TERMINATOR, H|Acc]); %% LVV, LVT

process(lv, [H|T], Acc) when ?IS_L1_OF_HANGUL_L(H) ->
    process(l, T, [H, ?COL_HANGUL_TERMINATOR|Acc]); %% LV_L

process(lv, [H|T], Acc) ->
    process(x, T, [H, ?COL_HANGUL_TERMINATOR|Acc]); %% LV_X

process(_, [H|T], Acc) ->
    process(x, T, [H|Acc]);

process(State, [], Acc) ->
    {State, lists:reverse(Acc)}.


type(X) when ?IS_L1_OF_HANGUL_L(X) -> l;
type(X) when ?IS_L1_OF_HANGUL_V(X) -> v;
type(X) when ?IS_L1_OF_HANGUL_T(X) -> t;
type(X) -> x.


%% Finish hangul comparation.
result(lv) -> [?COL_HANGUL_TERMINATOR];
result(_) -> [].



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_HANGUL_L1_L, (?COL_HANGUL_LBASE+5)).
-define(TEST_HANGUL_L1_V, (?COL_HANGUL_VBASE+6)).
-define(TEST_HANGUL_L1_T, (?COL_HANGUL_TBASE+4)).

process_test_() ->
    [?_assertEqual(process(l, [?TEST_HANGUL_L1_V, ?TEST_HANGUL_L1_T]),
        {x, [?TEST_HANGUL_L1_V, ?TEST_HANGUL_L1_T, ?COL_HANGUL_TERMINATOR]})
    ].

process_error12_test_() ->
    S1 = [?COL_HANGUL_LBASE, ?COL_HANGUL_VBASE, ?COL_HANGUL_VBASE],
    [?_assertEqual(process(x, S1),
        {x, S1 ++ [?COL_HANGUL_TERMINATOR]})
    ].

process_error12_var2_test_() ->
    S1 = [?COL_HANGUL_LBASE, ?COL_HANGUL_VBASE],
    S2 = [?COL_HANGUL_VBASE],
    [?_assertEqual(process(x, S1), {lv, S1}) % LV
    ,?_assertEqual(process(lv, S2), 
        {x, S2 ++ [?COL_HANGUL_TERMINATOR]}) % LVV_
    ].

process_error14_test_() ->
    S1 = [?COL_HANGUL_TBASE],
    [?_assertEqual(process(lv, S1), 
        {x, S1 ++ [?COL_HANGUL_TERMINATOR]}) % LVT_
    ].

-endif.
