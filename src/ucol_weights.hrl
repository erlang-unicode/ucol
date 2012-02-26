
-record(state, {
    left_l1, right_l1
}).

compare_filled_elems(equal, W1, W2) ->
    simple_compare_elems(W1, W2); 

compare_filled_elems({left, Q}, W1, W2) ->
    compare_left(Q, to_list(W1), to_list(W2));

compare_filled_elems({right, Q}, W1, W2) ->
    compare_right(Q, to_list(W1), to_list(W2)).


to_list(X) when is_integer(X) -> [X];
to_list(X) -> X.


compare_left(Q, W1, [W2H|W2T]) ->
    case queue:out(Q) of
    {{value, W2H}, QT} -> % equal
        compare_left(QT, W1, W2T);

    {{value, W1H}, _QT} ->
        if W1H<W2H -> less;
              true -> greater end;

    {empty, _EmptyQueue} ->
        simple_compare_elems(W1, W2T)
    end;

compare_left(Q, W1, []) -> 
    check_query(left, list_to_queue(W1, Q)).


compare_right(Q, [W1H|W1T], W2) ->
    case queue:out(Q) of
    {{value, W1H}, QT} -> % equal
        compare_right(QT, W1T, W2);

    {{value, W2H}, _QT} ->
        if W1H<W2H -> less;
              true -> greater end;

    {empty, _EmptyQueue} ->
        simple_compare_elems(W1T, W2)
    end;

compare_right(Q, [], W2) -> 
    NewQ = list_to_queue(W2, Q),
    check_query(right, list_to_queue(W2, Q)).
    

check_query(Type, Q) ->
    case queue:is_empty(Q) of
        true -> equal;
        false -> {Type, Q} end.


list_to_queue([H|T], Q) -> list_to_queue(T, queue:in(H, Q));
list_to_queue([], Q) -> Q.

    
simple_compare_elems(W1, W1) -> equal;
simple_compare_elems(W1, W2) when is_integer(W1) -> 
    if is_integer(W2) -> 
            if W1<W2 -> less;
                true -> greater end;
       true -> % is_list(W2)
            compare_lists([W1], W2) end;

simple_compare_elems(W1, W2) when is_integer(W2) -> % is_list(W1)
    compare_lists(W1, [W2]);

simple_compare_elems(W1, W2) -> % is_list(W1), is_list(W2)
    compare_lists(W1, W2).


compare_lists([H|T1], [H|T2]) -> compare_lists(T1, T2);
compare_lists([H1|_], [H2|_]) -> 
    if H1<H2 -> less;
        true -> greater end;
compare_lists([], []) -> equal;
compare_lists([], W2) -> 
    check_query(right, queue:from_list(W2));
compare_lists(W1, __) ->
    check_query(left, queue:from_list(W1)).


hangul_l1(State, W1L1, W2L1) ->
    {IsHangulL, W1L1X} = case W1L1 of
        {hangul_l, W1L1I} -> {true, W1L1I};
        _W1L1 -> {false, W1L1} end,

    {IsHangulR, W2L1X} = case W2L1 of
        {hangul_l, W2L1I} -> {true, W2L1I};
        _W2L1 -> {false, W2L1} end,

    {OldStateHangulL, OldStateHangulR} = case State of
        undefined -> {x, x};
        #state{left_l1=LeftL1, right_l1=RightL1} -> 
            {LeftL1, RightL1} end,

    {StateL, NewW1L1} = if
        IsHangulL; OldStateHangulL =/= x -> 
            process_hangul(OldStateHangulL, W1L1X);
        true -> {x, W1L1X} end,

    {StateR, NewW2L1} = if
        IsHangulR; OldStateHangulR =/= x -> 
            process_hangul(OldStateHangulR, W2L1X);
        true -> {x, W2L1X} end,

    NewState = case {StateL, StateR} of
            {x, x} -> undefined;
            {_, _} -> #state{left_l1=StateL, right_l1=StateR}
        end,

    {NewW1L1, NewW2L1, NewState}.

    
hangul_l1(W1L1, W2L1) ->
    {StateL, NewW1L1} = case W1L1 of
        {hangul_l, W1L1X} -> process_hangul(x, W1L1X);
        _W1L1 -> {x, W1L1} end,

    {StateR, NewW2L1} = case W2L1 of
        {hangul_l, W2L1X} -> process_hangul(x, W2L1X);
        _W2L1 -> {x, W2L1} end,

    NewState = case {StateL, StateR} of
            {x, x} -> undefined;
            {_, _} -> #state{left_l1=StateL, right_l1=StateR}
        end,

    {NewW1L1, NewW2L1, NewState}.
