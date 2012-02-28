-module(ucol_map).
-export([from_list/1, get/2]).

-type map_value() :: 0..255.

-record(ucol_map, {
    binary :: binary(),
    array :: array(),
    min :: non_neg_integer(),
    max :: non_neg_integer(),
    separator :: non_neg_integer(),
    default :: map_value()
}).

from_list(List) ->
    NaiveSep = 10000,
    Def = 0,
    [{Min, _}|_] = SortedList = lists:sort(List),
    [{Max, _}|_] = lists:reverse(SortedList),
    Last = 0,
    %% Rem is from (16#FFFF+1) to max.
    {BinMap, Rem, Sep} = get_binary_map(
        Min, NaiveSep+1, Def, Last, SortedList, <<>>),
    TrimmedBinMap = trim_binary(Sep-Min+1, BinMap),

    #ucol_map{
        binary=TrimmedBinMap, 
        array=array:fix(array:from_orddict(orddict:from_list(Rem))),
        max=Max, separator=Sep, min=Min, default=Def
    }.


trim_binary(Len, Bin) -> 
    Bits = Len*8, 
    <<Int:Bits, _/binary>> = Bin,
    <<Int:Bits>>.
    

-spec get(non_neg_integer(), #ucol_map{}) -> map_value().
get(X, #ucol_map{default=Def, min=Min, max=Max}) when X < Min; X > Max ->
    Def;

get(X, #ucol_map{binary=BinMap, separator=Sep, min=Min}) 
    when X =< Sep ->

%% Old realization:
%   Skip = (X-Min)*8,
%   <<_:Skip, Res, _/binary>> = BinMap,
%   Res;

    %% From zero
    binary:at(BinMap, X-Min-1);

get(X, #ucol_map{array=Arr, default=Def}) ->
    case array:get(X, Arr) of
        undefined -> Def;
        Res -> Res end.


get_binary_map(Max, Max, _Def, Last, Rem, Bin) -> {Bin, Rem, Last};

get_binary_map(Pos, Max, Def, Last, [{Pos,Def}|Rem], Bin) ->
    get_binary_map(Pos+1, Max, Def, Last, Rem, <<Bin/binary, Def>>);
    
get_binary_map(Pos, Max, Def, _Last, [{Pos,Val}|Rem], Bin) ->
    get_binary_map(Pos+1, Max, Def, Pos, Rem, <<Bin/binary, Val>>);

get_binary_map(Pos, Max, Def, Last, Rem, Bin) ->
    get_binary_map(Pos+1, Max, Def, Last, Rem, <<Bin/binary, Def>>).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(M, ucol_map).

from_list_and_get_test_() ->
    M1 = ?M:from_list([{8,2}, {123456, 3}]),

    [?_assertEqual(?M:get(8, M1), 2)
    ,?_assertEqual(?M:get(2, M1), 0)
    ,?_assertEqual(?M:get(123456, M1), 3)
    ,?_assertEqual(?M:get(223456, M1), 0)
    ].


trim_binary_test_() ->
    [?_assertEqual(trim_binary(2, <<1,2,3>>), <<1,2>>)
    ].

-endif.
