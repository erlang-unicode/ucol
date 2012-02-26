%% Shifted, Primary Collator
-module(ucol_primary).
-export([compare/2]).

-define(RES(X), X).
-define(VAL(X), X).
-define(HANDLE_OTHER(Str, Other),
    begin
        empty = Other,
        extract(ucol_string:fix(Str), ducet(), false, false)
    end).
-define(NEXT_ARRAY(Elem, PrevArr), PrevArr).
-define(WEIGHTS_MODULE, ucol_primary_weights).

-include("ucol.hrl").
-include("ucol_base.hrl").

%% For debugging
%-define(RES(X), {X, ?LINE}).
%-define(VAL(X), erlang:element(1, X)).
%-define(DVAL(X), begin io:write(user, X), erlang:element(1, X) end).



%%
%% API
%%

-spec compare(binary(), binary()) -> equal | less | greater.

compare(S, S) -> equal;

compare(S1, S2) ->
    simple_compare(S1, S2).


%%
%% Ovarloaded
%%

implicit(Point) ->
    ucol_implicit:synthesize(Point).


%% Returns a ducet array.
ducet() ->
    ucol_unidata:primary_ducet().




%%
%% Helpers
%%

uca_compare(S1, S2) -> uca_compare(S1, S2, equal).


uca_compare(S1, S2, L3) ->
    US1 = ucol_string:new(S1),
    US2 = ucol_string:new(S2),
    W1 = ucol_weights:new(L3),
    compare_(US1, US2, W1, ducet(), ducet()).



-define(IS_IGNORABLE(Point), (
       (Point < $/ andalso Point =/= $$) % 47, 36
    or ($: =< Point andalso Point =< $@) % 58, 64
)). 

%% insensitive
simple_compare(<<Point1/utf8, Rem1/binary>>, S2) 
    when ?IS_IGNORABLE(Point1) ->
        simple_compare(Rem1, S2);

simple_compare(S1, <<Point2/utf8, Rem2/binary>>) 
    when ?IS_IGNORABLE(Point2) ->
        simple_compare(S1, Rem2);

simple_compare(<<Point1/utf8, Rem1/binary>> = S1, 
               <<Point2/utf8, Rem2/binary>> = S2) ->
        

    Type1 = ?CHAR_TYPE(Point1),

    if Type1 =/= unicode ->
        Type2 = ?CHAR_TYPE(Point2),

        if Type2 =/= unicode ->
            if Point1 =:= Point2 -> 
                simple_compare(Rem1, Rem2);

                Type1 =:= Type2 -> 
                    if Point1 < Point2 -> ?RES(less); true -> ?RES(greater) end;

                Type1 =:= lower, Type2 =:= upper ->
                    P1 = Point1 - 32,
                    if 
                        P1 < Point2 -> ?RES(less); 
                        P1 > Point2 -> ?RES(greater);
                        true -> simple_compare(Rem1, Rem2)
                    end;

                Type2 =:= lower, Type1 =:= upper ->
                    P1 = Point1 + 32,
                    if 
                        P1 < Point2 -> ?RES(less); 
                        P1 > Point2 -> ?RES(greater);
                        true -> simple_compare(Rem1, Rem2)
                    end;

                Type1 =:= number -> ?RES(less);
                true  -> ?RES(greater) %% Type2 =:= number
            end;
                
            true -> uca_compare(S1, S2)
        end;

        true -> uca_compare(S1, S2)
    end;

simple_compare(<<>>, <<>>) -> equal;
simple_compare(S1, S2) -> uca_compare(S1, S2).


