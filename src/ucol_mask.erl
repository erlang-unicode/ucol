-module(ucol_mask).
-export([new/0,
         add/2,
         add_list/2,
         is_member/2,
         fix/1]).

new() ->
    gb_sets:new().


add(X, Mask) ->
    gb_sets:add(X, Mask).


add_list([H|T], Mask) -> 
    add_list(T, add(H, Mask));
add_list([], Mask) -> Mask.


is_member(Elem, Mask) -> gb_sets:is_member(Elem, Mask).

fix(Mask) -> gb_sets:balance(Mask).
