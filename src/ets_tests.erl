-module(ets_tests).

-compile(export_all).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

constructor_test_() ->
    {foreach,
     fun() ->
             Xs = lists:seq(1, 100),
             Xs_ets = etslists:constructor('etslist', Xs),
             Ys = lists:seq(51, 150),
             {Xs, Xs_ets, Ys}
     end,
     fun(_) ->
             ets:delete('etslist')
     end,
     [
      fun({Xs, Xs_ets, Ys}) ->
              [
               ?_assertEqual(Xs, (etslists:to_list(Xs_ets))),
               ?_assertEqual(Xs, begin
                                     etslists:reorder_keys(Xs_ets),
                                     etslists:to_list(Xs_ets)
                                 end),
               ?_assertEqual((hd(Xs)), (etslists:head(Xs_ets))),
               ?_assertEqual((tl(Xs)), (etslists:tail(Xs_ets))),
               ?_assertEqual((length(Xs)), (etslists:length(Xs_ets))),
               begin
                   F1 = fun(X) -> X < 1000 end,
                   F2 = fun(X) -> X < 50 end,
                   [
                    ?_assertEqual((lists:all(F1, Xs)), (etslists:all(F1, Xs_ets))),
                    ?_assertEqual((lists:all(F2, Xs)), (etslists:all(F2, Xs_ets))),
                    ?_assertEqual((lists:any(F1, Xs)), (etslists:any(F1, Xs_ets))),
                    ?_assertEqual((lists:any(F2, Xs)), (etslists:any(F2, Xs_ets))),
                    ?_assertEqual((lists:append(Xs, Ys)),
                                  begin
                                      etslists:append(Xs_ets, Ys),
                                      etslists:to_list(Xs_ets)
                                  end)
                   ]
               end,
               begin
                   F3 = fun(X) -> X < 50 end,
                   F4 = fun(X) ->
                                case X < 50 of
                                    'true' ->
                                        {'true', X+1};
                                    _Otherwise ->
                                        'false'
                                end
                        end,
                   [
                    ?_assertEqual(lists:filter(F3, Xs),
                                  etslists:filter(F3, Xs_ets)),
                    ?_assertEqual(lists:filtermap(F3, Xs),
                                  etslists:filtermap(F3, Xs_ets)),
                    ?_assertEqual(lists:filtermap(F4, Xs),
                                  etslists:filtermap(F4, Xs_ets))
                   ]
               end
              ]
      end,
      fun({Xs, Xs_ets, Ys}) ->
              ?_assertEqual((lists:delete(50, Xs)),
                            begin
                                etslists:delete(50, Xs_ets),
                                etslists:to_list(Xs_ets)
                            end)
      end,
      fun({Xs, Xs_ets, Ys}) ->
              ?_assertEqual((lists:droplast(Xs)),
                            begin
                                etslists:droplast(Xs_ets),
                                etslists:to_list(Xs_ets)
                            end)
      end,
      fun({Xs, Xs_ets, Ys}) ->
              F5 = fun(X) -> X < 50 end,
              [
               ?_assertEqual((lists:dropwhile(F5, Xs)),
                             begin
                                 etslists:dropwhile(F5, Xs_ets),
                                 etslists:to_list(Xs_ets)
                             end)
              ]
      end,
      fun({Xs, Xs_ets, Ys}) ->
              Xs_deep = lists:duplicate(10, Xs),
              Xs_deep_ets = etslists:constructor(Xs_deep),
              F6 = fun(X) -> [X + 1] end,
              [
               ?_assertEqual(lists:flatlength(Xs),
                             etslists:flatlength(Xs_ets)),
               ?_assertEqual(lists:flatlength(Xs_deep),
                             etslists:flatlength(Xs_deep_ets)),
               ?_assertEqual(lists:flatmap(F6, Xs),
                             begin
                                 etslists:flatmap(F6, Xs_ets),
                                 etslists:to_list(Xs_ets)
                             end)
              ]
      end,
      fun({Xs, Xs_ets, Ys}) ->
              F7 = fun(X) -> [X, X, X] end,
              [
               ?_assertEqual(lists:flatmap(F7, Xs),
                             begin
                                 etslists:flatmap(F7, Xs_ets),
                                 etslists:to_list(Xs_ets)
                             end)
              ]
      end,
      fun({Xs, Xs_ets, Ys}) ->
              F8 = fun(X, Sum) -> X+Sum end,
              Empty_ets = etslists:constructor([]),
              [
               ?_assertEqual(lists:flatten(Xs),
                             begin
                                 etslists:flatten(Xs_ets),
                                 etslists:to_list(Xs_ets)
                             end),
               ?_assertEqual(lists:foldl(F8, 0, Xs),
                             etslists:foldl(F8, 0, Xs_ets)),
               ?_assertEqual(lists:foldr(F8, 0, Xs),
                             etslists:foldr(F8, 0, Xs_ets)),
               ?_assertEqual(lists:last(Xs),
                             etslists:last(Xs_ets)),
               ?_assertError("No clause matching empty list.",
                             etslists:last(Empty_ets))
              ]
      end,
      fun({Xs, Xs_ets, Ys}) ->
              F9 = fun(X) -> X*2 end,
              [
               ?_assertEqual(lists:map(F9, Xs),
                             begin
                                 etslists:map(F9, Xs_ets),
                                 etslists:to_list(Xs_ets)
                             end)
              ]
      end,
      fun({Xs, Xs_ets, Ys}) ->
              F10 = fun(X, Sum) -> {2*X, X+Sum} end,
              F11 = fun(X, Sum) -> {2/X, X-Sum} end,
              [
               ?_assertEqual(lists:mapfoldl(F10, 0, Xs),
                             etslists:mapfoldl(F10, 0, Xs_ets)),
               ?_assertEqual(lists:mapfoldr(F10, 0, Xs),
                             etslists:mapfoldr(F10, 0, Xs_ets)),
               ?_assertEqual(lists:mapfoldl(F11, 0, Xs),
                             etslists:mapfoldl(F11, 0, Xs_ets)),
               ?_assertEqual(lists:mapfoldr(F11, 0, Xs),
                             etslists:mapfoldr(F11, 0, Xs_ets)),
               ?_assertEqual(lists:max(Xs),
                             etslists:max(Xs_ets)),
               ?_assertEqual(lists:member(5, Xs),
                             etslists:member(5, Xs_ets)),
               ?_assertEqual(lists:member('nothere', Xs),
                             etslists:member('nothere', Xs_ets)),
               ?_assertEqual(lists:min(Xs),
                             etslists:min(Xs_ets)),
               ?_assertEqual(lists:merge(Xs, Ys),
                             begin
                                 etslists:mergel(Xs_ets, Ys),
                                 etslists:to_list(Xs_ets)
                             end)
              ]
      end,
      fun({Xs, Xs_ets, Ys}) ->
              [
               ?_assertEqual(lists:nth(5, Xs),
                             etslists:nth(5, Xs_ets)),
               ?_assertEqual(lists:nthtail(5, Xs),
                             begin
                                 etslists:nthtail(5, Xs_ets),
                                 etslists:to_list(Xs_ets)
                             end)
              ]
      end,
      fun({Xs, Xs_ets, Ys}) ->
              F12 = fun(X) -> X < 50 end,
              [
               ?_assertEqual(lists:partition(F12, Xs),
                             etslists:partition(F12, Xs_ets)),
               ?_assertEqual(lists:reverse(Xs),
                             begin
                                 etslists:reverse(Xs_ets),
                                 etslists:to_list(Xs_ets)
                             end)
              ]
      end,
      fun({Xs, Xs_ets, Ys}) ->
              F13 = fun(A, B) -> A =< B end,
              [
               ?_assertEqual(lists:sort(F13, Xs),
                             begin
                                 etslists:sort(F13, Xs_ets),
                                 etslists:to_list(Xs_ets)
                             end)
              ]
      end,
      fun({Xs, Xs_ets, Ys}) ->
              [
               ?_assertEqual(lists:split(50, Xs),
                             etslists:split(50, Xs_ets)),
               ?_assertEqual(lists:sublist(Xs, 5, 10),
                             etslists:sublist(Xs_ets, 5, 10)),
               ?_assertEqual(lists:subtract(Xs, Ys),
                             begin
                                 etslists:subtract(Xs_ets, Ys),
                                 etslists:to_list(Xs_ets)
                             end)
              ]
      end,
      fun({Xs, Xs_ets, Ys}) ->
              ?_assertEqual(lists:subtract(Ys, Xs),
                            etslists:subtract(Ys, Xs_ets))
      end,
      fun({Xs, Xs_ets, Ys}) ->
              ?_assertEqual(lists:zip(Xs, Ys),
                            begin
                                etslists:zipl(Xs_ets, Ys),
                                etslists:to_list(Xs_ets)
                            end)
      end,
      fun({Xs, Xs_ets, Ys}) ->
              ?_assertEqual(lists:zip(Ys, Xs),
                            begin
                                etslists:zipr(Ys, Xs_ets),
                                etslists:to_list(Xs_ets)
                            end)
      end,
      fun({Xs, Xs_ets, Ys}) ->
              ?_assertEqual(lists:zip(Xs, Ys),
                            begin
                                etslists:zip(Xs_ets, etslists:constructor(Ys)),
                                etslists:to_list(Xs_ets)
                            end)
      end,
      fun({Xs, Xs_ets, Ys}) ->
              F14 = fun(A, B) -> A + B end,
              ?_assertEqual(lists:zipwith(F14, Xs, Ys),
                            begin
                                etslists:zipwithl(F14, Xs_ets, Ys),
                                etslists:to_list(Xs_ets)
                            end)
      end,
      fun({Xs, Xs_ets, Ys}) ->
              F15 = fun(A, B) -> A + B end,
              ?_assertEqual(lists:zipwith(F15, Ys, Xs),
                            begin
                                etslists:zipwithr(F15, Ys, Xs_ets),
                                etslists:to_list(Xs_ets)
                            end)
      end,
      fun({Xs, Xs_ets, Ys}) ->
              F16 = fun(A, B) -> A + B end,
              ?_assertEqual(lists:zipwith(F16, Xs, Ys),
                            begin
                                etslists:zipwith(F16, Xs_ets, etslists:constructor(Ys)),
                                etslists:to_list(Xs_ets)
                            end)
      end
     ]
    }.

-endif.
