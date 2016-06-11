-module(etslists).

-compile(export_all).

%% -----------------------------------------------------------------------------
%% ETS functions

constructor(Name, Xs) ->
    Tab = ets:new(Name, [% 'bag',
                         'ordered_set',
                         'named_table', %% needed for test cleanup
                         'public' %% needed for tests
                        ]),
    insert(Tab, Xs),
    Tab.

constructor(Xs) -> %% Generate table name
    Tab = ets:new(gen_name(), [% 'bag',
                               'ordered_set'
                               %% 'named_table', %% needed for test cleanup
                               %% 'public' %% needed for tests
                              ]),
    insert(Tab, Xs),
    Tab.

gen_name() ->
    gen_name('etslist').

gen_name(Base) ->
    case lists:member(Base, ets:all()) of
        'true' ->
            gen_name(list_to_atom([$_ | atom_to_list(Base)]));
        'false' ->
            Base
    end.

insert(Tab, Xs) when is_list(Xs) ->
    lists:map(fun(X) -> ets:insert(Tab, X) end,
              lists:zip(lists:seq(1, erlang:length(Xs)), Xs)).

to_list(Xs) ->
    Snd = fun(X) -> element(2, X) end,
    lists:map(Snd, lists:sort(ets:tab2list(Xs))).

reorder_keys(Xs) ->
    Xs_lst = to_list(Xs),
    case ets:delete_all_objects(Xs) of
        'true' ->
            insert(Xs, Xs_lst);
        {'error', Reason} ->
            error(Reason)
    end.

head(Xs) ->
    element(2, hd(ets:lookup(Xs, 1))).

tail(Xs) ->
    tl(to_list(Xs)).

length(Xs) ->
    ets:info(Xs, 'size').

all(F, Xs) ->
    ets:foldl(fun(_, 'false') ->
                      'false';
                 ({_, X}, 'true') ->
                      F(X)
              end, 'true', Xs).

any(F, Xs) ->
    ets:foldl(fun(_, 'true') ->
                      'true';
                 ({_, X}, 'false') ->
                      F(X)
              end, 'false', Xs).

append(Xs, Ys) when is_pid(Xs), erlang:length(Ys) >= 1;
                    is_atom(Xs), erlang:length(Ys) >= 1 ->
    N = ets:info(Xs, 'size') + 1,
    lists:map(fun(X) -> ets:insert(Xs, X) end,
              lists:zip(lists:seq(N, N + erlang:length(Ys) - 1), Ys)).

delete(X, Xs) ->
    ets:foldl(fun({K, Y}, []) when X =:= Y ->
                      ets:delete(Xs, K),
                      reorder_keys(Xs),
                      'deleted';
                 (_, Acc) ->
                      Acc
              end, [], Xs).

droplast(Xs) ->
    case ets:last(Xs) of
        '$end_of_table' ->
            'ok';
        K ->
            ets:delete(Xs, K)
    end.

dropwhile(F, Xs) ->
    ets:foldl(fun({K, X}, 'true') ->
                      case F(X) of
                          'true' ->
                              ets:delete(Xs, K),
                              'true';
                          'false' ->
                              reorder_keys(Xs),
                              'false'
                      end;
                 (_, Acc) ->
                      Acc
              end, 'true', Xs).

filter(F, Xs) ->
    ets:foldr(fun({_, X}, Acc) ->
                      case F(X) of
                          'true' ->
                              [X | Acc];
                          'false' ->
                              Acc
                      end
              end, [], Xs).

filtermap(F, Xs) ->
    ets:foldr(fun({_, X}, Acc) ->
                      case F(X) of
                          'false' ->
                              Acc;
                          'true' ->
                              [X | Acc];
                          {'true', Value} ->
                              [Value | Acc]
                      end
              end, [], Xs).

flatlength(Xs) ->
    ets:foldr(fun({_, X}, Sum) when is_list(X) ->
                      Sum + lists:flatlength(X);
                 (_, Sum) ->
                      Sum + 1
              end, 0, Xs).

flatmap(F, Xs) ->
    Zs = ets:foldr(fun({_, X}, Acc) ->
                           case F(X) of
                               Ys when is_list(Ys) ->
                                   lists:append([Ys, Acc])
                           end
                   end, [], Xs),
    ets:delete_all_objects(Xs),
    insert(Xs, Zs).

flatten(Xs) ->
    Zs = ets:foldr(fun({_, Ys}, Acc) when is_list(Ys) ->
                           lists:foldr(fun(Y, Acc1) ->
                                               [Y | Acc1]
                                       end, Acc, lists:flatten(Ys));
                      ({_, Y}, Acc) ->
                           [Y | Acc]
                   end, [], Xs),
    ets:delete_all_objects(Xs),
    insert(Xs, Zs).

foldl(F, Init, Xs) ->
    ets:foldl(fun({_, Ys}, Acc) ->
                      F(Ys, Acc)
              end, Init, Xs).

foldr(F, Init, Xs) ->
    ets:foldr(fun({_, Ys}, Acc) ->
                      F(Ys, Acc)
              end, Init, Xs).

last(Xs) ->
    case ets:last(Xs) of
        '$end_of_table' ->
            error("No clause matching empty list.");
        K ->
            element(2, hd(ets:lookup(Xs, K)))
    end.

map(F, Xs) ->
    Ys = ets:foldr(fun({_, X}, Acc) ->
                           [F(X) | Acc]
                   end, [], Xs),
    ets:delete_all_objects(Xs),
    insert(Xs, Ys).

mapfoldl(F, Init, Xs) ->
    {Zs, R} = ets:foldl(fun({_, X}, {Ys, Acc}) ->
                                {Y, Acc1} = F(X, Acc),
                                {[Y | Ys], Acc1}
                        end, {[], Init}, Xs),
    {lists:reverse(Zs), R}.

mapfoldr(F, Init, Xs) ->
    {Zs, R} = ets:foldr(fun({_, X}, {Ys, Acc}) ->
                                {Y, Acc1} = F(X, Acc),
                                {[Y | Ys], Acc1}
                        end, {[], Init}, Xs),
    {Zs, R}.

max(Xs) ->
    ets:foldr(fun({_, X}, Acc) ->
                      case X > Acc of
                          'true' ->
                              X;
                          'false' ->
                              Acc
                      end
              end, etslists:head(Xs), Xs).

member(X, Xs) ->
    ets:foldr(fun({_, X1}, 'false') ->
                      X1 =:= X;
                 (_, 'true') ->
                      'true'
              end, 'false', Xs).

%% etslists on the left
mergel(Xs, Ys) ->
    XsYs = lists:merge(etslists:to_list(Xs), Ys),
    ets:delete_all_objects(Xs),
    lists:map(fun(XY) ->
                      ets:insert(Xs, {ets:info(Xs, 'size')+1, XY})
              end, XsYs).

min(Xs) ->
    ets:foldr(fun({_, X}, Acc) ->
                      case X < Acc of
                          'true' ->
                              X;
                          'false' ->
                              Acc
                      end
              end, etslists:head(Xs), Xs).

nth(N, Xs) ->
    element(2, hd(ets:lookup(Xs, N))).

nthtail(N, Xs) ->
    ets:foldl(fun({K, _}, 'true') ->
                      case K =< N of
                          'true' ->
                              ets:delete(Xs, K),
                              'true';
                          'false' ->
                              'false'
                      end;
                 (_, Acc) ->
                      Acc
              end, 'true', Xs),
    etslists:reorder_keys(Xs).

partition(F, Xs) ->
    ets:foldr(fun({_, X}, {True, False}) ->
                      case F(X) of
                          'true' ->
                              {[X | True], False};
                          'false' ->
                              {True, [X | False]}
                      end
              end, {[], []}, Xs).

reverse(Xs) ->
    Size = etslists:length(Xs),
    Ys = ets:foldr(fun({K, X}, Acc) ->
                           [{Size-K+1, X} | Acc]
                   end, [], Xs),
    ets:delete_all_objects(Xs),
    lists:map(fun(X) -> ets:insert(Xs, X) end, Ys).

sort(F, Xs) ->
    Ys = lists:sort(F, etslists:to_list(Xs)),
    ets:delete_all_objects(Xs),
    insert(Xs, Ys).

split(N, Xs) ->
    ets:foldr(fun({K, X}, {True, False}) ->
                      case K =< N of
                          'true' ->
                              {[X | True], False};
                          'false' ->
                              {True, [X | False]}
                      end
              end, {[], []}, Xs).

sublist(Xs, S, N) ->
    ets:foldl(fun({K, X}, 'false') ->
                      case K < S of
                          'true' ->
                              'false';
                          'false' ->
                              {1, [X]}
                      end;
                 ({_, X}, {L, Acc}) ->
                      case L < N of
                          'true' ->
                              {L+1, [X | Acc]};
                          'false' ->
                              lists:reverse(Acc)
                      end;
                 (_, Ys) ->
                      Ys
              end, 'false', Xs).

subtract(Xs, Ys) when is_pid(Xs); is_atom(Xs) ->
    Zs = ets:foldl(fun({_, X}, {Ys_acc, R_acc}) ->
                           case lists:member(X, Ys_acc) of
                               'true' ->
                                   {lists:subtract(Ys_acc, [X]), R_acc};
                               'false' ->
                                   {Ys_acc, [X | R_acc]}
                           end
                   end, {Ys, []}, Xs),
    ets:delete_all_objects(Xs),
    insert(Xs, lists:reverse(element(2, Zs)));
subtract(Ys, Xs) when is_pid(Xs); is_atom(Xs) ->
    lists:subtract(Ys, etslists:to_list(Xs)).

%% When ets table is on the left
zipl(Xs, Ys) ->
    Zs = lists:zip(to_list(Xs), Ys),
    lists:foldr(fun(V, K) ->
                        ets:update_element(Xs, K, {2, V}),
                        K-1
                end, erlang:length(Zs), Zs).

%% When ets table is on the right
zipr(Xs, Ys) ->
    Zs = lists:zip(Xs, to_list(Ys)),
    lists:foldr(fun(V, K) ->
                        ets:update_element(Ys, K, {2, V}),
                        K-1
                end, erlang:length(Zs), Zs).


%% When both arguments are ets tables
zip(Xs, Ys) ->
    Zs = lists:zip(to_list(Xs), to_list(Ys)),
    lists:foldr(fun(V, K) ->
                        ets:update_element(Xs, K, {2, V}),
                        K-1
                end, erlang:length(Zs), Zs).
