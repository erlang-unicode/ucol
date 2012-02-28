
%%
%% Defines Hangul constants
%% Hangul characters can be decompize to LV or LVT forms.
%%

-define(HANGUL_SBASE,  16#AC00).
-define(HANGUL_LBASE,  16#1100). % 4352 - 4371
-define(HANGUL_VBASE,  16#1161). % 4449 - 4470
-define(HANGUL_TBASE,  16#11A7). % 4519 - 4547
-define(HANGUL_LCOUNT, 19).
-define(HANGUL_VCOUNT, 21).
-define(HANGUL_TCOUNT, 28).
-define(HANGUL_NCOUNT, 588).
-define(HANGUL_SCOUNT, 11172).

-define(HANGUL_SLAST,  (?HANGUL_SBASE + ?HANGUL_SCOUNT)).
-define(HANGUL_LLAST,  (?HANGUL_LBASE + ?HANGUL_LCOUNT)).
-define(HANGUL_VLAST,  (?HANGUL_VBASE + ?HANGUL_VCOUNT)).
-define(HANGUL_TLAST,  (?HANGUL_TBASE + ?HANGUL_TCOUNT)).



-define(CHAR_IS_HANGUL_S(Ch), (
 (Ch>=?HANGUL_SBASE) and (Ch=<?HANGUL_SLAST)
)).

-define(CHAR_IS_HANGUL_L(Ch), (
 (Ch>=?HANGUL_LBASE) and (Ch=<?HANGUL_LLAST)
)).

-define(CHAR_IS_HANGUL_V(Ch), (
 (Ch>=?HANGUL_VBASE) and (Ch=<?HANGUL_VLAST)
)).

-define(CHAR_IS_HANGUL_T(Ch), (
 (Ch>=?HANGUL_TBASE) and (Ch=<?HANGUL_TLAST)
)).


%% 12578
%% {element,{non_variable,12782,32,2,65535}}


% Hangul & UCA
-define(COL_HANGUL_LBASE,  12337). % 12337 - 12356
-define(COL_HANGUL_VBASE,  12463). % 12463 - 12484
-define(COL_HANGUL_TBASE,  12533). % 12533 - 12561      12584

-define(COL_HANGUL_LLAST,  (?COL_HANGUL_LBASE + ?HANGUL_LCOUNT)).
-define(COL_HANGUL_VLAST,  (?COL_HANGUL_VBASE + ?HANGUL_VCOUNT)).
-define(COL_HANGUL_TLAST,  (?COL_HANGUL_TBASE + ?HANGUL_TCOUNT)).

% TERMINATOR > T >  V > L
%-define(COL_HANGUL_TERMINATOR, 16#24C9). % Code Paint
-define(COL_HANGUL_TERMINATOR, 6089). % Code Paint
% {element,{non_variable,6089,32,12,65535}}

% Weight on level 1 (L1) is L1 of Hangul jamo L.
-define(IS_L1_OF_HANGUL_L(W), (
 (W>=?COL_HANGUL_LBASE) and (W=<?COL_HANGUL_LLAST)
)).

% Weight on level 1 (L1) is L1 of Hangul jamo V.
-define(IS_L1_OF_HANGUL_V(W), (
 (W>=?COL_HANGUL_VBASE) and (W=<?COL_HANGUL_VLAST)
)).

% Weight on level 1 (L1) is L1 of Hangul jamo T.
-define(IS_L1_OF_HANGUL_T(W), (
 (W>=?COL_HANGUL_TBASE) and (W=<?COL_HANGUL_TLAST)
)).


-define(DECOMP_CLASS, 100).


%%
%% Implicit weights
%%

-define(UNIHAN_CORE_BLOCKS, [
    'CJK Compatibility Ideographs',
    'CJK Unified Ideographs'
]).

-define(UNIHAN_EXT_BLOCKS, [
    'CJK Compatibility',
    'CJK Compatibility Forms',
    'CJK Compatibility Ideographs Supplement',
    'CJK Unified Ideographs Extension A',
    'CJK Unified Ideographs Extension B',
    'CJK Unified Ideographs Extension C',
    'CJK Unified Ideographs Extension D'
]).

-define(UNIHAN_CORE_BASE, 16#FB40).
-define(UNIHAN_EXT_BASE, 16#FB80).
-define(DEFAULT_BASE, 16#FBC0).



%%
%% Latin1
%%

-define(IS_SIMPLE_UPPER_CHAR(X), ($A =< (X) andalso (X) =< $Z)).
-define(IS_SIMPLE_LOWER_CHAR(X), ($a =< (X) andalso (X) =< $z)).
-define(IS_SIMPLE_NUMBER_CHAR(X), ($0 =< (X) andalso (X) =< $9)).
-define(CHAR_TYPE(X), (if
        (X) > 122 -> unicode;
        ?IS_SIMPLE_UPPER_CHAR(X) -> upper;
        ?IS_SIMPLE_LOWER_CHAR(X) -> lower;
        ?IS_SIMPLE_NUMBER_CHAR(X) -> number;
        true -> unicode 
    end)).

%% Number < Lower < Upper

-define(CHAR_GAP, 32). % $a - $A
