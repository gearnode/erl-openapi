-module(openapi_utils_tests).

-include_lib("eunit/include/eunit.hrl").

flatten_deep_object_test_() ->
  Flatten = fun openapi_utils:flatten_deep_object/1,
  [?_assertEqual(#{},
                 Flatten([])),
   ?_assertEqual(#{<<"[0]">> => 1,
                   <<"[1]">> => 2,
                   <<"[2]">> => <<"foo">>},
                 Flatten([1,2,<<"foo">>])),
   ?_assertEqual(#{},
                 Flatten(#{})),
   ?_assertEqual(#{<<"a">> => 1,
                   <<"ef gh[i]">> => 2,
                   <<"ef gh[j]">> => 3},
                 Flatten(#{<<"a">> => 1,
                           bcd =>
                             #{},
                           <<"ef gh">> =>
                             #{i => 2,
                               j => 3}})),
   ?_assertEqual(#{},
                 Flatten([#{}, #{}])),
   ?_assertEqual(#{<<"[0][a]">> => 1,
                   <<"[1][c][0]">> => 2,
                   <<"[1][c][1]">> => 3,
                   <<"[2][e][foo]">> => <<"bar">>},
                 Flatten([#{a => 1},
                          #{b => [],
                            c => [2,3]},
                          #{d => #{},
                            e => #{<<"foo">> => <<"bar">>}}]))].
