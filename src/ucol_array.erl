-module(ucol_array).
-export([new/0, set/3, get/2, fix/1]).



new() -> array:new().


fix(Arr) ->  
    array:fix(array:map(fun fix_elem/2, Arr)).


fix_elem(_Idx, Elem) -> 
    case array:is_array(Elem) of
        true -> fix(Elem);
        false -> Elem
    end.


set([H|T] = _CPList, Weights, Arr)
    when is_integer(H) ->
    Old = array:get(H, Arr),
    New = case {T, type(Old)} of
        %% H is a first element
        {[], none}    -> Weights;
        %% Already have an array.
        {[], array}   -> array:set(0, Weights, Old);
        {[], X} when X =:= element; X =:= empty -> 
            Old; % a root element is already added
        {_,  none}    -> set(T, Weights, new());
        {_,  array}   -> set(T, Weights, Old);
        %% Old is a root (0) element
        {_, X} when X =:= element; X =:= empty -> 
            set(T, Weights, array:set(0, Old, new()))
    end,
    array:set(H, New, Arr).
    

get(CP, Arr)
    when is_integer(CP) ->
    case array:get(CP, Arr) of
        undefined -> none; % not in array
        variable -> {empty, variable}; % for ucol
        non_variable -> {empty, non_variable}; 
        empty -> empty; % for ucol_primary
        Val ->
            case array:is_array(Val) of
                false -> {element, Val};
                true  -> {array, Val} 
            end
    end.

type(undefined) -> none;
type(variable) -> empty;
type(non_variable) -> empty;
type(Val) -> 
    case array:is_array(Val) of 
        true -> array; 
        false -> element end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(M, ucol_array).

type_test_() ->
    [?_assertEqual(type(undefined), none)
    ,?_assertEqual(type(array:new()), array)
    ,?_assertEqual(type(1), element)
    ].


set_and_get_test_() ->
    A1 = ?M:new(),
    A2 = ?M:set([4,5], 1, A1),
    A3 = ?M:set([4,5,1], 2, A2),
    A4 = ?M:set([4], 5, A3),
    A5 = ?M:set([4,6,1], 5, A4),

    {T1, V1} = ?M:get(4, A5),
    {T2, V2} = ?M:get(6, V1),
    {T3, V3} = ?M:get(1, V2),

    [?_assertEqual(T1, array)
    ,?_assertEqual(T2, array)
    ,?_assertEqual(T3, element)
    ,?_assertEqual(?M:get(0, V2), none)
    ,?_assertEqual(?M:get(9, A5), none)
    ].

-endif.
