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

-export([comment/4, snake_case/1]).

-spec comment(iodata(), iodata(), non_neg_integer(),
              openapi:generate_options()) ->
        iodata().
comment(Prefix, Text0, IdentSize, Options) ->
  FillColumn = maps:get(fill_column, Options, 74),
  Text = openapi_string:text_reflow(Text0, FillColumn),
  openapi_string:text_prefix(Prefix, Text, IdentSize).

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
