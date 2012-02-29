%% @doc Variable collation elements are reset to zero at levels one through
%% three. In addition, a new fourth-level weight is appended, whose value 
%% depends on the type, as shown in Table 12.
%% Any subsequent primary or secondary ignorables following a variable are reset
%% so that their weights at levels one through four are zero.
%% ```
%% * A combining grave accent after a space would have the value 
%%   [.0000.0000.0000.0000].
%% * A combining grave accent after a Capital A would be unchanged.'''
%% @end


compare_(US1, US2, W1, A1, A2) ->
    R1 = extract(US1, A1, false, false),
    R2 = extract(US2, A2, false, false),

    case {R1, R2} of
    {stop, stop} -> ?RES(?WEIGHTS_MODULE:result(W1));
    {stop, {E2, NewUS2}} -> compare_right_remain_(NewUS2, A2, E2, W1);
    {{E1, NewUS1}, stop} ->  compare_left_remain_(NewUS1, A1, E1, W1);
    %% `{WeightElement, NewUcolString}'
    {{E1, NewUS1}, {E2, NewUS2}} ->
%       io:format(user, "E1: ~w ~nE2: ~w~n", [E1, E2]),
        case ?WEIGHTS_MODULE:compare(E1, E2, W1) of
            less -> ?RES(less);
            greater -> ?RES(greater);
            W2 ->
                NewA1 = ?NEXT_ARRAY(E1, A1), 
                NewA2 = ?NEXT_ARRAY(E2, A2),
                compare_(NewUS1, NewUS2, W2, NewA1, NewA2)
        end
    end.


compare_right_remain_(US2, A2, E2, W1) ->
    case ?WEIGHTS_MODULE:compare_right_remain(E2, W1) of
        less -> ?RES(less);
        greater -> ?RES(greater);
        W2 ->
            NewA2 = ?NEXT_ARRAY(E2, A2), 
            R2 = extract(US2, NewA2, false, false),
            case R2 of
                stop -> ?RES(?WEIGHTS_MODULE:result(W2));
                {NewE2, NewUS2} -> 
                    compare_right_remain_(NewUS2, NewA2, NewE2, W2) 
            end
    end.


compare_left_remain_(US1, A1, E1, W1) ->
    case ?WEIGHTS_MODULE:compare_left_remain(E1, W1) of
        less -> ?RES(less);
        greater -> ?RES(greater);
        W2 ->
            NewA1 = ?NEXT_ARRAY(E1, A1), 
            R1 = extract(US1, NewA1, false, false),
            case R1 of
                stop -> ?RES(?WEIGHTS_MODULE:result(W2));
                {NewE1, NewUS1} -> 
                    compare_left_remain_(NewUS1, NewA1, NewE1, W2) 
            end
    end.



%% Produce a longest match of code points in a ducet table.
extract(Str1, Arr, LastSkippedClass, LastClass) ->
    IsFirst = LastClass =:= false,
    case ucol_string:head(Str1) of
        {{Point, Class}, Str2} ->
        %% If LastClass =:= false, then it is first point in combining weight.

        %% A non-starter in a string is called blocked if there is another 
        %% non-starter of the same canonical combining class or zero between 
        %% it and the last character of canonical combining class 0.
        HasSkipped = LastSkippedClass =/= false,
        IsBlocked = HasSkipped andalso 
            (LastSkippedClass =:= Class orelse Class =:= 0),
        %% End of a suggestion list.
        IsStopped = not IsFirst andalso LastClass > Class,
        CanSkipped = LastClass =/= false 
            andalso LastClass =< Class 
            andalso Class =/= 0,


        if IsStopped; IsBlocked ->
            handle_stopped(ucol_string:back(Str2), Arr);

            %% Last skipped point was non-blocked.
            true -> 
                case ucol_array:get(Point, Arr) of
                    {element, Elem} -> {Elem, ucol_string:fix(Str2)};

                    %% Try search longer element
                    {array, SubArr} -> 
                        extract(Str2, SubArr, LastSkippedClass, Class);

                    %% Make an implicit weight
                    none when IsFirst -> 
                        {implicit(Point), ucol_string:fix(Str2)};

                    %% Skip a char
                    none when CanSkipped -> 
                        Str3 = ucol_string:back_and_skip(Str2),
                        extract(Str3, Arr, Class, Class);

                    none -> 
                        Str3 = ucol_string:back(Str2),
                        handle_stopped(Str3, Arr);

                    Other -> 
                        %% This action can be specified.
                        ?HANDLE_OTHER(Str2, Other)
                    end
            end;
        stop when IsFirst -> stop;
        stop -> 
            case ucol_array:get(0, Arr) of
                none -> handle_no_more(Str1); % no_more
                {element, Elem} -> 
                    {Elem, ucol_string:fix(Str1)};
                _Other -> stop end % {empty, Type}
    end.


handle_stopped(Str1, Arr) -> 
    case ucol_array:get(0, Arr) of
        none -> handle_no_more(Str1); % no_more

        {element, Elem} -> 
            {Elem, ucol_string:fix(Str1)};

        Other ->
            ?HANDLE_OTHER(Str1, Other) end.


%% It is a special case, when there are a ducet weight for one and three 
%% elements, but there is no a value for two elements.
handle_no_more(Str1) ->
    Str2 = ucol_string:back_and_skip(Str1),
    Buf = ucol_string:head_buffer(Str2),
    Arr = extract_again(Buf, ducet()),
    LastSkippedClass = ucol_string:last_skipped_class(Str2),
    LastClass = ucol_string:last_class(Str2),
    extract(Str2, Arr, LastSkippedClass, LastClass).


extract_again([{H,_Class}|T], Arr) -> 
    {array, NewArr} = ucol_array:get(H, Arr),
    extract_again(T, NewArr);

extract_again([], Arr) -> Arr.



