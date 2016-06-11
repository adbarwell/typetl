-module(binarylists).

-compile(export_all).

%% -----------------------------------------------------------------------------
%% Binary functions

constructor(Xs) ->
    try list_to_binary(Xs)
    catch
        error:badarg ->
            lists:map(fun term_to_binary/1, Xs)
    end.

revert(Xs) when is_binary(Xs) ->
    binary_to_list(Xs);
revert(Xs) when is_list(Xs) ->
    lists:map(fun binary_to_term/1, Xs).


all(F, <<X>>) ->
    F(X);
all(F, <<X, Xs/binary>>) ->
    F(X) and all(F, Xs);
all(F, Xs) ->
    lists:foldr(fun(Xb, 'true') ->
                        F(binary_to_term(Xb));
                   (_, Acc) ->
                        Acc
                end, 'true', Xs).

%% Binlist must be on the left -- revert if on left
append(Xs, Ys) when is_binary(Xs) ->
    case constructor(Ys) of
        Ys_bin when is_binary(Ys_bin) ->
            <<Xs/binary, Ys_bin/binary>>;
        Ys_bin when is_list(Ys_bin) ->
            Xs_bin = lists:map(fun term_to_binary/1, binary_to_list(Xs)),
            lists:foldr(fun(X, Acc) ->
                                [X | Acc]
                        end, Ys_bin, Xs_bin)
    end;
append(Xs, Ys) when is_list(Xs) ->
    lists:append(Xs, lists:map(fun term_to_binary/1, Ys)).

delete(X, <<X, Xs/binary>>) ->
    Xs;
delete(X, <<Y, Xs/binary>>) ->
    Rest = delete(X, Xs),
    <<Y, Rest/binary>>;
delete(X, Xs) ->
    lists:delete(term_to_binary(X), Xs).

dropwhile(_, <<>>) ->
    <<>>;
dropwhile(F, <<X, Xs/binary>>) ->
    case F(X) of
        'true' ->
            dropwhile(F, Xs);
        'false' ->
            <<X, Xs/binary>>
    end;
dropwhile(_, []) ->
    [];
dropwhile(F, [X | Xs]) ->
    case F(binary_to_term(X)) of
        'true' ->
            dropwhile(F, Xs);
        'false' ->
            [X | Xs]
    end.

filter(_, <<>>) ->
    <<>>;
filter(F, <<X, Xs/binary>>) ->
    case F(X) of
        'true' ->
            Rest = filter(F, Xs),
            <<X, Rest/binary>>;
        'false' ->
            filter(F, Xs)
    end;
filter(F, Xs) ->
    lists:filter(fun(X) -> F(binary_to_term(X)) end, Xs).

flatten(Xs) when is_binary(Xs) ->
    Xs;
flatten(Xs) ->
    lists:foldr(fun(X, Acc) ->
                        X_term = binary_to_term(X),
                        case is_list(X_term) of
                            'true' ->
                                lists:foldr(fun(Y, Acc1) ->
                                                    [Y | Acc1]
                                            end, Acc,
                                            lists:map(fun term_to_binary/1,
                                                      lists:flatten(X_term)));
                            'false' ->
                                [X | Acc]
                        end
                end, [], Xs).


foldl(_, Init, <<>>) ->
    Init;
foldl(F, Init, <<X, Xs/binary>>) ->
    foldl(F, F(X, Init), Xs);
foldl(F, Init, Xs) ->
    lists:foldl(fun(X, Acc) ->
                        F(binary_to_term(X), Acc)
                end, Init, Xs).

foldr(_, Init, <<>>) ->
    Init;
foldr(F, Init, <<X, Xs/binary>>) ->
    F(X, foldr(F, Init, Xs));
foldr(F, Init, Xs) ->
    lists:foldr(fun(X, Acc) ->
                        F(binary_to_term(X), Acc)
                end, Init, Xs).

map(F, Xs) when is_binary(Xs) ->
    map_1(F, Xs, []);
map(F, Xs) ->
    lists:map(fun(X) ->
                      term_to_binary(F(binary_to_term(X)))
              end, Xs).

map_1(_, <<>>, Acc) ->
    binarylists:constructor(lists:reverse(Acc));
map_1(F, <<X, Xs/binary>>, Acc) ->
    map_1(F, Xs, [F(X) | Acc]).

member(_, <<>>) ->
    'false';
member(X, <<X, _Xs/binary>>) ->
    'true';
member(X, <<_, Xs/binary>>) ->
    member(X, Xs);
member(X, Xs) ->
    lists:foldr(fun(Y, 'false') ->
                        binary_to_term(Y) =:= X;
                   (_, True) ->
                        True
                end, 'false', Xs).

%% Binlist must be on the left -- revert if on right
merge(Xs, Ys) when is_binary(Xs) ->
    case constructor(Ys) of
        Ys_bin when is_binary(Ys_bin) ->
            merge_1(Xs, Ys_bin);
        Ys_bin when is_list(Ys_bin) ->
            Xs_bin = lists:map(fun term_to_binary/1, binary_to_list(Xs)),
            merge_1(Xs_bin, Ys_bin)
    end;
merge(Xs, Ys) when is_list(Xs) ->
    merge_1(Xs, lists:map(fun term_to_binary/1, Ys)).

merge_1(<<>>, Ys) ->
    Ys;
merge_1(Xs, <<>>) ->
    Xs;
merge_1(<<X, Xs/binary>>, <<Y, Ys/binary>>) ->
    case X =< Y of
        'true' ->
            Rest = merge_1(Xs, <<Y, Ys/binary>>),
            <<X, Rest/binary>>;
        'false' ->
            Rest = merge_1(<<X, Xs/binary>>, Ys),
            <<Y, Rest/binary>>
    end;
merge_1(Xs, Ys) ->
    lists:merge(fun(X, Y) ->
                        binary_to_term(X) =< binary_to_term(Y)
                end, Xs, Ys).

nth(1, <<X, _/binary>>) ->
    X;
nth(N, <<_, Xs/binary>>) ->
    nth(N-1, Xs);
nth(N, Xs) ->
    binary_to_term(lists:nth(N, Xs)).

nthtail(1, <<_, Xs/binary>>) ->
    Xs;
nthtail(N, <<_, Xs/binary>>) ->
    nthtail(N-1, Xs);
nthtail(N, Xs) ->
    lists:nthtail(N, Xs).

partition(F, Xs) when is_binary(Xs) ->
    partition(F, Xs, {<<>>, <<>>});
partition(F, Xs) ->
    lists:partition(fun(X) ->
                            F(binary_to_term(X))
                    end, Xs).

partition(_, <<>>, R) ->
    R;
partition(F, <<X, Xs/binary>>, {True, False}) ->
    case F(X) of
        'true' ->
            partition(F, Xs, {<<True/binary, X>>, False});
        'false' ->
            partition(F, Xs, {True, <<False/binary, X>>})
    end.

reverse(<<>>) ->
    <<>>;
reverse(<<X, Xs/binary>>) ->
    Rest = reverse(Xs),
    <<Rest/binary, X>>;
reverse(Xs) ->
    lists:reverse(Xs).

sort(<<>>) ->
    <<>>;
sort(<<X>>) ->
    <<X>>;
sort(<<X, Xs/binary>>) ->
    {LEQ, GT} = binarylists:partition(fun(Y) -> Y =< X end, Xs),
    Head = sort(LEQ),
    Tail = sort(GT),
    <<Head/binary, X, Tail/binary>>;
sort(Xs) ->
    lists:sort(fun(A, B) ->
                       binary_to_term(A) =< binary_to_term(B)
               end, Xs).

split(N, Xs) when is_binary(Xs) ->
    split(N, Xs, <<>>);
split(N, Xs) ->
    lists:split(N, Xs).

split(0, Xs, True) ->
    {True, Xs};
split(_, <<>>, _) ->
    error(badarg);
split(N, <<X, Xs/binary>>, True) ->
    split(N-1, Xs, <<True/binary, X>>).

sublist(<<>>, _) ->
    <<>>;
sublist(<<X, _/binary>>, 1) ->
    <<X>>;
sublist(<<X, Xs/binary>>, N) ->
    Rest = sublist(Xs, N-1),
    <<X, Rest/binary>>.

sublist(Xs, 1, N) when is_binary(Xs) ->
    sublist(Xs, N);
sublist(<<_, Xs/binary>>, S, N) ->
    sublist(Xs, S-1, N);
sublist(Xs, S, N) ->
    lists:sublist(Xs, S, N).

takewhile(_, <<>>) ->
    <<>>;
takewhile(F, <<X, Xs/binary>>) ->
    case F(X) of
        'true' ->
            Rest = takewhile(F, Xs),
            <<X, Rest/binary>>;
        'false' ->
            <<>>
    end;
takewhile(F, Xs) ->
    lists:takewhile(fun(X) ->
                            F(binary_to_term(X))
                    end, Xs).

%% When binary list is on the left
zipl(Xs, Ys) when is_binary(Xs) ->
    binarylists:constructor(lists:zip(binarylists:revert(Xs), Ys));
zipl([], []) ->
    [];
zipl([X | Xs], [Y | Ys]) ->
    [term_to_binary({binary_to_term(X), Y}) | zipl(Xs, Ys)].

%% When binary list is on the right
zipr(Xs, Ys) when is_binary(Ys) ->
    binarylists:constructor(lists:zip(Xs, binarylists:revert(Ys)));
zipr([], []) ->
    [];
zipr([X | Xs], [Y | Ys]) ->
    [term_to_binary({X, binary_to_term(Y)}) | zipr(Xs, Ys)].

%% When both arguments are binary lists
zip(Xs, Ys) when is_binary(Xs), is_binary(Ys) ->
    binarylists:constructor(lists:zip(binarylists:revert(Xs), binarylists:revert(Ys)));
zip([], []) ->
    [];
zip([X | Xs], [Y | Ys]) ->
    [term_to_binary({binary_to_term(X), binary_to_term(Y)}) | zip(Xs, Ys)].
