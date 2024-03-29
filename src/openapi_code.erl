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

-module(openapi_code).

-export([comment/4, snake_case/1, camel_case/1, pascal_case/1]).

-spec comment(iodata(), iodata(), non_neg_integer(),
              openapi:generate_options()) ->
        iodata().
comment(Prefix, Text0, IdentSize, Options) ->
  FillColumn = maps:get(fill_column, Options, 74),
  Text = openapi_string:text_reflow(Text0, FillColumn),
  openapi_string:text_prefix(Prefix, Text, IdentSize).

-spec pascal_case(binary()) -> binary().
pascal_case(<<>>) ->
  <<>>;
pascal_case(<<C/utf8, Rest/binary>>) ->
  C1 = string:to_upper(C),
  C2 = camel_case(Rest),
  <<C1/utf8, C2/binary>>.

-spec camel_case(binary()) -> binary().
camel_case(Name) ->
  camel_case(Name, <<>>).

-spec camel_case(binary(), binary()) -> binary().
camel_case(<<>>, Acc) ->
  Acc;
camel_case(<<$_, C/utf8, Rest/binary>>, Acc) ->
  C1 = string:to_upper(C),
  camel_case(Rest, <<Acc/binary, C1/utf8>>);
camel_case(<<C/utf8, Rest/binary>>, Acc) ->
  camel_case(Rest, <<Acc/binary, C/utf8>>).

-spec snake_case(binary()) -> binary().
snake_case(Name) ->
  snake_case(Name, <<>>, undefined).

-spec snake_case(binary(), binary(), pos_integer() | undefined) -> binary().
snake_case(<<>>, Acc, _) ->
  string:lowercase(Acc);
snake_case(<<C/utf8, Rest/binary>>, Acc, undefined) ->
  snake_case(Rest, <<Acc/binary, C/utf8>>, C);
snake_case(<<C/utf8, Rest/binary>>, Acc, LastC) when C >= $A, C =< $Z ->
  if
    LastC >= $A, LastC =< $Z ->
      case Rest of
        <<NextC/utf8, _/binary>> when NextC >= $a, NextC =< $z ->
          snake_case(Rest, <<Acc/binary, $_, C/utf8>>, C);
        _ ->
          snake_case(Rest, <<Acc/binary, C/utf8>>, C)
      end;
    LastC /= $_ ->
      snake_case(Rest, <<Acc/binary, $_, C/utf8>>, C);
    true ->
      snake_case(Rest, <<Acc/binary, C/utf8>>, C)
  end;
snake_case(<<"."/utf8, Rest/binary>>, Acc,  _) ->
  snake_case(Rest, <<Acc/binary, "_"/utf8>>, $.);
snake_case(<<C/utf8, Rest/binary>>, Acc, _) ->
  snake_case(Rest, <<Acc/binary, C/utf8>>, C).
