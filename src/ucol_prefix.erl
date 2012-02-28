%% It is useful when we comparing two string line:
%% 
%% * Long, long prefix 1
%% * Long, long prefix 2
%%
%% Algorithm:
%%
%% 1. Get the longest prefix.
%% 2. Go back and find last initial weight.

-module(ucol_prefix).
-include("ucol.hrl").
-export([handle/2]).

handle(<<BH:80, _/binary>> = B1, 
       <<BH:80, _/binary>> = B2) ->
    {T1, T2, Acc} = longest_common_prefix(B1, B2, []),
    case back(Acc, []) of
        false  -> make_strings(B1, B2);
        []     -> make_strings(T1, T2);
        Lefted -> 
            Head = ucol_string:to_nfd(Lefted),
            S1 = ucol_string:new_with_head(T1, Head),
            S2 = ucol_string:new_with_head(T2, Head),
            {S1, S2}
    end;

handle(B1, B2) -> make_strings(B1, B2).


make_strings(B1, B2) ->
    S1 = ucol_string:new(B1),
    S2 = ucol_string:new(B2),
    {S1, S2}.


%% == Schema ==
%% Binary: <<Prefix, Tail>>
%% Prefix: <<Skipped, Lefted>>
%% Binary: <<Skipped, Lefted, Tail>>

%% Search a last initial point.
%% back(Reversed prefix: ie "tset", Lefted Acc) -> Lefted Acc | false
back([H|T], Acc) ->
    case is_initial(H) of
        false -> back(T, [H|Acc]);
        true -> Acc
    end;
back([], Acc) -> false.
    

is_initial(Point) 
    when Point < ?HANGUL_LBASE -> not is_difficult(Point);

is_initial(Point) 
    when ?CHAR_IS_HANGUL_S(Point); 
         ?CHAR_IS_HANGUL_L(Point); 
         ?CHAR_IS_HANGUL_V(Point) -> false;

is_initial(Point) -> not is_difficult(Point).


is_difficult(Point) -> 
    ucol_mask:is_member(Point, ucol_unidata:diff_points()).


%% in:  <<testing>> <<tested>>
%% out: {<<ing>>, <<ed>>, [$t, $s, $e, $t]}
%% out: {Tail1, Tail2, Reversed prefix}
longest_common_prefix(<<H/utf8, T1/binary>>, <<H/utf8, T2/binary>>, Acc) ->
    longest_common_prefix(T1, T2, [H|Acc]);
longest_common_prefix(T1, T2, Acc) -> {T1, T2, Acc}.
