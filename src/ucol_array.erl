-module(ucol_array).
-export([new/0, set/3, get/2, fix/1]).

-record(ucol_array, {
    array = array:new(),
    max = 0
}).

new() -> #ucol_array{}.


fix(UArr=#ucol_array{array=Arr}) ->  
    UArr#ucol_array{
        array=array:fix(array:map(fun fix_elem/2, Arr))
    }.


fix_elem(_Idx, Arr=#ucol_array{}) -> fix(Arr);
fix_elem(_Idx, Elem) -> Elem.


set([H|T] = _CPList, Weights, Arr)
    when is_integer(H) ->
    Old = get_(H, Arr),
    New = case {T, type(Old)} of
        %% H is a first element
        {[], none}    -> Weights;
        %% Already have an array.
        {[], array}   -> set_(0, Weights, Old);
        {[], X} when X =:= element; X =:= empty -> 
            Old; % a root element is already added
        {_,  none}    -> set(T, Weights, #ucol_array{});
        {_,  array}   -> set(T, Weights, Old);
        %% Old is a root (0) element
        {_, X} when X =:= element; X =:= empty -> 
            set(T, Weights, set_(0, Old, #ucol_array{}))
    end,
    set_(H, New, Arr).


get_(Point, #ucol_array{array=Arr})
    when is_integer(Point) ->
    array:get(Point, Arr).


set_(Point, Weights, Old=#ucol_array{array=OldArr, max=OldMax}) 
    when is_integer(Point) ->
    Old#ucol_array{
        array=array:set(Point, Weights, OldArr),
        max=max(OldMax, Point)
    }.


get(CP, #ucol_array{max=Max}) 
    when CP > Max -> none;

get(CP, #ucol_array{array=Arr})
    when is_integer(CP) ->
    case array:get(CP, Arr) of
        undefined -> none; % not in array
        variable -> {empty, variable}; % for ucol
        non_variable -> {empty, non_variable}; 
        empty -> empty; % for ucol_primary
        Val=#ucol_array{} -> {array, Val};
        Val -> {element, Val}
    end.

type(undefined) -> none;
type(variable) -> empty;
type(non_variable) -> empty;
type(#ucol_array{}) -> array;
type(_) -> element.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(M, ucol_array).

type_test_() ->
    [?_assertEqual(type(undefined), none)
    ,?_assertEqual(type(ucol_array:new()), array)
    ,?_assertEqual(type(1), element)
    ].


set_and_get_test_() ->
    A1 = ?M:new(),
    A2 = ?M:set([4,5], 1, A1),
    A3 = ?M:set([4,5,1], 2, A2),
    A4 = ?M:set([4], 5, A3),
    A5 = ?M:set([4,6,1], 5, A4),
    AF = ?M:fix(A5),

    {T1, V1} = ?M:get(4, AF),
    {T2, V2} = ?M:get(6, V1),
    {T3, V3} = ?M:get(1, V2),

    [?_assertEqual(T1, array)
    ,?_assertEqual(T2, array)
    ,?_assertEqual(T3, element)
    ,?_assertEqual(?M:get(0, V2), none)
    ,?_assertEqual(?M:get(9, A5), none)
    ].

-endif.
