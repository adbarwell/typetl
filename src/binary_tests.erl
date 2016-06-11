-module(binary_tests).

-compile(export_all).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

binary_test_() ->
    {foreach,
     fun() ->
             Xs_io = lists:seq(1,100),
             Xs_term = lists:zip(lists:duplicate(length(Xs_io), 'test'), Xs_io),
             Xs_io_bin = binarylists:constructor(Xs_io),
             Xs_term_bin = binarylists:constructor(Xs_term),
             Ys_io = lists:seq(51, 150),
             Ys_term = lists:zip(lists:duplicate(length(Ys_io), 'test'), Ys_io),
             {Xs_io, Xs_term, Xs_io_bin, Xs_term_bin, Ys_io, Ys_term}
     end,
     [
      fun({Xs_io, Xs_term, Xs_io_bin, Xs_term_bin, Ys_io, Ys_term}) ->
              [
               ?_assertEqual(Xs_io, binarylists:revert(Xs_io_bin)),
               ?_assertEqual(Xs_term, binarylists:revert(Xs_term_bin)),
               begin
                   F1 = fun(X) -> X < 50 end,
                   F2 = fun(X) -> X < 200 end,
                   F3 = fun(X) -> element(1, X) =:= 'test' end,
                   F4 = fun(X) -> element(2, X) < 50 end,
                   [
                    ?_assertEqual(lists:all(F1, Xs_io),
                                  binarylists:all(F1, Xs_io_bin)),
                    ?_assertEqual(lists:all(F2, Xs_io),
                                  binarylists:all(F2, Xs_io_bin)),
                    ?_assertEqual(lists:all(F3, Xs_term),
                                  binarylists:all(F3, Xs_term_bin)),
                    ?_assertEqual(lists:all(F4, Xs_term),
                                  binarylists:all(F4, Xs_term_bin))
                   ]
               end,
               ?_assertEqual(lists:append(Xs_io, Ys_io),
                             binarylists:revert(binarylists:append(Xs_io_bin, Ys_io))),
               ?_assertEqual(lists:append(Xs_io, Ys_term),
                             binarylists:revert(binarylists:append(Xs_io_bin, Ys_term))),
               ?_assertEqual(lists:append(Xs_term, Ys_io),
                             binarylists:revert(binarylists:append(Xs_term_bin, Ys_io))),
               ?_assertEqual(lists:append(Xs_term, Ys_term),
                             binarylists:revert(binarylists:append(Xs_term_bin, Ys_term))),
               ?_assertEqual(lists:delete(50, Xs_io),
                             binarylists:revert(binarylists:delete(50, Xs_io_bin))),
               ?_assertEqual(lists:delete({'test', 50}, Xs_term),
                             binarylists:revert(binarylists:delete({'test', 50}, Xs_term_bin))),
               begin
                   F5 = fun(X) -> X < 50 end,
                   F6 = fun(X) -> element(1, X) =:= 'test' end,
                   F7 = fun(X) -> element(2, X) < 50 end,
                   [?_assertEqual(lists:dropwhile(F5, Xs_io),
                                  binarylists:revert(binarylists:dropwhile(F5, Xs_io_bin))),
                    ?_assertEqual(lists:dropwhile(F6, Xs_term),
                                  binarylists:revert(binarylists:dropwhile(F6, Xs_term_bin))),
                    ?_assertEqual(lists:dropwhile(F7, Xs_term),
                                  binarylists:revert(binarylists:dropwhile(F7, Xs_term_bin))),
                    ?_assertEqual(lists:filter(F5, Xs_io),
                                  binarylists:revert(binarylists:filter(F5, Xs_io_bin))),
                    ?_assertEqual(lists:filter(F6, Xs_term),
                                  binarylists:revert(binarylists:filter(F6, Xs_term_bin))),
                    ?_assertEqual(lists:filter(F7, Xs_term),
                                  binarylists:revert(binarylists:filter(F7, Xs_term_bin)))
                   ]
               end,
               ?_assertEqual(lists:flatten(Xs_io),
                             binarylists:revert(binarylists:flatten(Xs_io_bin))),
               begin
                   Xs_deep = lists:duplicate(10, Xs_term),
                   Xs_deep_bin = binarylists:constructor(Xs_deep),
                   ?_assertEqual(lists:flatten(Xs_deep),
                                 binarylists:revert(binarylists:flatten(Xs_deep_bin)))
               end,
               begin
                   F8 = fun(X, Acc) -> X + Acc end,
                   F9 = fun(X, Acc) -> X - Acc end,
                   [
                    ?_assertEqual(lists:foldl(F8, 0, Xs_io),
                                  binarylists:foldl(F8, 0, Xs_io_bin)),
                    ?_assertEqual(lists:foldl(F9, 0, Xs_io),
                                  binarylists:foldl(F9, 0, Xs_io_bin)),
                    ?_assertEqual(lists:foldr(F8, 0, Xs_io),
                                  binarylists:foldr(F8, 0, Xs_io_bin)),
                    ?_assertEqual(lists:foldr(F9, 0, Xs_io),
                                  binarylists:foldr(F9, 0, Xs_io_bin))
                   ]
               end,
               begin
                   F10 = fun(X, Acc) -> element(2, X) + Acc end,
                   F11 = fun(X, Acc) -> element(2, X) - Acc end,
                   [
                    ?_assertEqual(lists:foldl(F10, 0, Xs_term),
                                  binarylists:foldl(F10, 0, Xs_term_bin)),
                    ?_assertEqual(lists:foldl(F11, 0, Xs_term),
                                  binarylists:foldl(F11, 0, Xs_term_bin)),
                    ?_assertEqual(lists:foldr(F10, 0, Xs_term),
                                  binarylists:foldr(F10, 0, Xs_term_bin)),
                    ?_assertEqual(lists:foldr(F11, 0, Xs_term),
                                  binarylists:foldr(F11, 0, Xs_term_bin))
                   ]
               end,
               begin
                   F12 = fun(X) -> X + 1 end,
                   F13 = fun(X) -> {X, X} end,
                   F14 = fun(X) -> element(2, X) + 1 end,
                   F15 = fun(X) -> {'result', element(2, X) + 1} end,
                   [
                    ?_assertEqual(lists:map(F12, Xs_io),
                                  binarylists:revert(binarylists:map(F12, Xs_io_bin))),
                    ?_assertEqual(lists:map(F13, Xs_io),
                                  binarylists:revert(binarylists:map(F13, Xs_io_bin))),
                    ?_assertEqual(lists:map(F14, Xs_term),
                                  binarylists:revert(binarylists:map(F14, Xs_term_bin))),
                    ?_assertEqual(lists:map(F15, Xs_term),
                                  binarylists:revert(binarylists:map(F15, Xs_term_bin)))
                   ]
               end,
               ?_assertEqual(lists:member(50, Xs_io),
                             binarylists:member(50, Xs_io_bin)),
               ?_assertEqual(lists:member(300, Xs_io),
                             binarylists:member(300, Xs_io_bin)),
               ?_assertEqual(lists:member({'test', 50}, Xs_term),
                             binarylists:member({'test', 50}, Xs_term_bin)),
               ?_assertEqual(lists:member('false', Xs_term),
                             binarylists:member('false', Xs_term_bin)),
               ?_assertEqual(lists:merge(Xs_io, Ys_io),
                             binarylists:revert(binarylists:merge(Xs_io_bin, Ys_io))),
               ?_assertEqual(lists:merge(Xs_io, Ys_term),
                             binarylists:revert(binarylists:merge(Xs_io_bin, Ys_term))),
               ?_assertEqual(lists:merge(Xs_term, Ys_io),
                             binarylists:revert(binarylists:merge(Xs_term_bin, Ys_io))),
               ?_assertEqual(lists:merge(Xs_term, Ys_term),
                             binarylists:revert(binarylists:merge(Xs_term_bin, Ys_term))),
               ?_assertEqual(lists:nth(50, Xs_io),
                             binarylists:nth(50, Xs_io_bin)),
               ?_assertEqual(lists:nth(50, Xs_term),
                             binarylists:nth(50, Xs_term_bin)),
               ?_assertError(function_clause, binarylists:nth(300, Xs_io_bin)),
               ?_assertError(function_clause, binarylists:nth(300, Xs_term_bin)),
               ?_assertEqual(lists:nthtail(50, Xs_io),
                             binarylists:revert(binarylists:nthtail(50, Xs_io_bin))),
               ?_assertEqual(lists:nthtail(50, Xs_term),
                             binarylists:revert(binarylists:nthtail(50, Xs_term_bin))),
               ?_assertError(function_clause, binarylists:nthtail(300, Xs_io_bin)),
               ?_assertError(function_clause, binarylists:nthtail(300, Xs_term_bin)),
               begin
                   F16 = fun(X) -> X < 50 end,
                   F17 = fun(X) -> X < 200 end,
                   F18 = fun(X) -> element(1, X) =:= 'test' end,
                   F19 = fun(X) -> element(2, X) < 50 end,
                   [
                    ?_assertEqual(tuple_to_list(lists:partition(F16, Xs_io)),
                                  lists:map(fun binarylists:revert/1,
                                            tuple_to_list(binarylists:partition(F16, Xs_io_bin)))),
                    ?_assertEqual(tuple_to_list(lists:partition(F17, Xs_io)),
                                  lists:map(fun binarylists:revert/1,
                                            tuple_to_list(binarylists:partition(F17, Xs_io_bin)))),
                    ?_assertEqual(tuple_to_list(lists:partition(F18, Xs_term)),
                                  lists:map(fun binarylists:revert/1,
                                            tuple_to_list(binarylists:partition(F18, Xs_term_bin)))),
                    ?_assertEqual(tuple_to_list(lists:partition(F19, Xs_term)),
                                  lists:map(fun binarylists:revert/1,
                                            tuple_to_list(binarylists:partition(F19, Xs_term_bin))))
                   ]
               end,
               ?_assertEqual(lists:reverse(Xs_io),
                             binarylists:revert(binarylists:reverse(Xs_io_bin))),
               ?_assertEqual(lists:reverse(Xs_term),
                             binarylists:revert(binarylists:reverse(Xs_term_bin))),
               ?_assertEqual(lists:sort(Xs_io),
                             binarylists:revert(binarylists:sort(Xs_io_bin))),
               ?_assertEqual(lists:sort(lists:reverse(Xs_io)),
                             binarylists:revert(binarylists:sort(binarylists:constructor(lists:reverse(Xs_io))))),
               ?_assertEqual(lists:sort(Xs_term),
                             binarylists:revert(binarylists:sort(Xs_term_bin))),
               ?_assertEqual(lists:sort(lists:reverse(Xs_term)),
                             binarylists:revert(binarylists:sort(binarylists:constructor(lists:reverse(Xs_term))))),
               ?_assertEqual(tuple_to_list(lists:split(50, Xs_io)),
                             lists:map(fun binarylists:revert/1,
                                       tuple_to_list(binarylists:split(50, Xs_io_bin)))),
               ?_assertError(badarg, lists:map(fun binarylists:revert/1,
                                               tuple_to_list(binarylists:split(200, Xs_io_bin)))),
               ?_assertEqual(tuple_to_list(lists:split(50, Xs_term)),
                             lists:map(fun binarylists:revert/1,
                                       tuple_to_list(binarylists:split(50, Xs_term_bin)))),
               ?_assertError(badarg,
                             lists:map(fun binarylists:revert/1,
                                       tuple_to_list(binarylists:split(200, Xs_term_bin)))),
               ?_assertEqual(lists:sublist(Xs_io, 5, 10),
                             binarylists:revert(binarylists:sublist(Xs_io_bin, 5, 10))),
               ?_assertEqual(lists:sublist(Xs_term, 5, 10),
                             binarylists:revert(binarylists:sublist(Xs_term_bin, 5, 10))),
               ?_assertEqual(lists:sublist(Xs_io, 95, 50),
                             binarylists:revert(binarylists:sublist(Xs_io_bin, 95, 50))),
               ?_assertEqual(lists:sublist(Xs_term, 95, 50),
                             binarylists:revert(binarylists:sublist(Xs_term_bin, 95, 50))),
               begin
                   F20 = fun(X) -> X < 50 end,
                   F21 = fun(X) -> X < 200 end,
                   F22 = fun(X) -> element(1, X) =:= 'test' end,
                   F23 = fun(X) -> element(2, X) < 50 end,
                   [
                    ?_assertEqual(lists:takewhile(F20, Xs_io),
                                  binarylists:revert(binarylists:takewhile(F20, Xs_io_bin))),
                    ?_assertEqual(lists:takewhile(F21, Xs_io),
                                  binarylists:revert(binarylists:takewhile(F21, Xs_io_bin))),
                    ?_assertEqual(lists:takewhile(F22, Xs_term),
                                  binarylists:revert(binarylists:takewhile(F22, Xs_term_bin))),
                    ?_assertEqual(lists:takewhile(F23, Xs_term),
                                  binarylists:revert(binarylists:takewhile(F23, Xs_term_bin)))
                   ]
               end,
               ?_assertEqual(lists:zip(Xs_io, Xs_term),
                             binarylists:revert(binarylists:zipl(Xs_io_bin, Xs_term))),
               ?_assertEqual(lists:zip(Xs_io, Ys_io),
                             binarylists:revert(binarylists:zipl(Xs_io_bin, Ys_io))),
               ?_assertEqual(lists:zip(Xs_term, Xs_io),
                             binarylists:revert(binarylists:zipl(Xs_term_bin, Xs_io))),
               ?_assertEqual(lists:zip(Xs_term, Ys_term),
                             binarylists:revert(binarylists:zipl(Xs_term_bin, Ys_term))),
               ?_assertEqual(lists:zip(Xs_term, Xs_io),
                             binarylists:revert(binarylists:zipr(Xs_term, Xs_io_bin))),
               ?_assertEqual(lists:zip(Ys_io, Xs_io),
                             binarylists:revert(binarylists:zipr(Ys_io, Xs_io_bin))),
               ?_assertEqual(lists:zip(Xs_io, Xs_term),
                             binarylists:revert(binarylists:zipr(Xs_io, Xs_term_bin))),
               ?_assertEqual(lists:zip(Ys_term, Xs_term),
                             binarylists:revert(binarylists:zipr(Ys_term, Xs_term_bin))),
               ?_assertEqual(lists:zip(Xs_io, Ys_io),
                             binarylists:revert(binarylists:zip(Xs_io_bin,
                                                                binarylists:constructor(Ys_io)))),
               ?_assertEqual(lists:zip(Xs_term, Ys_term),
                             binarylists:revert(binarylists:zip(Xs_term_bin,
                                                                binarylists:constructor(Ys_term))))
              ]
      end
     ]
    }.

-endif.
