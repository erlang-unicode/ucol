-module(ucol_implicit).
-include("ucol.hrl").
-export([synthesize/1, element/3]).


synthesize(Point) ->
    Base = search(Point, ucol_unidata:implicit()),
    do_implicit_weight(Point, Base).


%% Make an element for ucol_data.
element(From, To, BlockName) ->
    case block_type(BlockName) of
        false -> false;
        Base -> case trim_reserved(To, To-From) of
                false -> false;
                RealTo -> {From, RealTo, Base}
            end
    end.


trim_reserved(_To, 0) -> false;
trim_reserved(To, Max) ->
    case ux_char:type(To) of
        other -> trim_reserved(To-1, Max-1);
        _Type -> To
    end.

%%
%% Helpers
%%

search(Point, [{From, _To, _Base}|_T]) when Point < From -> ?DEFAULT_BASE;
search(Point, [{_From, To, Base}|_T]) when Point =< To -> Base;
search(Point, [_|T]) -> search(Point, T);
search(_Point, []) -> ?DEFAULT_BASE.


-spec block_type(atom()) -> integer() | false.

block_type(BlockName) -> 
    case lists:member(BlockName, ?UNIHAN_CORE_BLOCKS) of
        true -> ?UNIHAN_CORE_BASE;
        false ->
            case lists:member(BlockName, ?UNIHAN_EXT_BLOCKS) of
                true -> ?UNIHAN_EXT_BASE;
                false -> false end end.



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
