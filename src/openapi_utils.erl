%% Copyright (c) 2022 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(openapi_utils).

-export([binary/1, format/2, flatten_deep_object/1]).

-spec binary(unicode:chardata()) -> binary().
binary(Data) ->
  case unicode:characters_to_binary(Data) of
    Bin when is_binary(Bin) ->
      Bin;
    _ ->
      error({invalid_character_data, Data})
  end.

-spec format(io:format(), [term()]) -> binary().
format(Format, Args) ->
  binary(io_lib:format(Format, Args)).

-spec flatten_deep_object(#{binary() | atom() := term()} | list()) ->
        #{binary() := term()}.
flatten_deep_object(Value) ->
  flatten_deep_object(Value, <<"">>, #{}).

-spec flatten_deep_object(#{binary() | atom() := term()} | list(),
                          binary(), #{binary() := term()}) ->
        #{binary() := term()}.
flatten_deep_object(Value, Prefix, Acc) when is_map(Value) ->
  maps:fold(fun (K, V, Acc2) ->
                Prefix2 =
                  case Prefix of
                    <<"">> ->
                      K;
                    _ ->
                      format("~ts[~ts]", [Prefix, K])
                  end,
                if
                  is_map(V); is_list(V) ->
                    flatten_deep_object(V, Prefix2, Acc2);
                  true ->
                    Acc2#{Prefix2 => V}
                end
            end, Acc, Value);
flatten_deep_object(Value, Prefix, Acc) when is_list(Value) ->
  Indices = lists:seq(0, length(Value)-1),
  lists:foldl(fun ({I,E}, Acc2) ->
                  Prefix2 = format("~ts[~b]", [Prefix, I]),
                  if
                    is_map(E); is_list(E) ->
                      flatten_deep_object(E, Prefix2, Acc2);
                    true ->
                      Acc2#{Prefix2 => E}
                  end
              end, Acc, lists:zip(Indices, Value)).
