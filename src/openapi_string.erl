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

-module(openapi_string).

-export([text_reflow/2,
         text_prefix/3,
         text_indent/1]).

-spec text_reflow(iodata(), non_neg_integer()) -> iodata().
text_reflow(Text, FillColumn) ->
  TrimedText = text_reflow_trim(Text),
  F = fun (LineData) ->
          case unicode:characters_to_list(LineData) of
            "" ->
              prettypr:break(prettypr:text(""));
            Line ->
              prettypr:text_par(Line)
          end
      end,
  Paragraphs = lists:map(F, string:split(TrimedText, "\n", all)),
  prettypr:format(prettypr:sep(Paragraphs), FillColumn).

-spec text_reflow_trim(iodata()) -> iodata().
text_reflow_trim(Text) ->
  text_reflow_trim(iolist_to_binary(Text), []).

-spec text_reflow_trim(binary(), iodata()) -> iodata().
text_reflow_trim(<<>>, Acc) ->
  lists:reverse(Acc);
text_reflow_trim(<<$\n, $\n, Rest/binary>>, Acc) ->
  text_reflow_trim(Rest, ["\n\n" | Acc]);
text_reflow_trim(<<$\n, Rest/binary>>, Acc) ->
  text_reflow_trim(Rest, [" " | Acc]);
text_reflow_trim(<<C, Rest/binary>>, Acc) ->
  text_reflow_trim(Rest, [C | Acc]).

-spec text_prefix(iodata(), iodata(), non_neg_integer()) -> iodata().
text_prefix(Prefix, Text, Size) ->
  Replacement = ["\n", text_indent(Size), Prefix, " "],
  [Prefix, " ", string:replace(Text, "\n", Replacement, all)].

-spec text_indent(non_neg_integer()) -> iodata().
text_indent(0) ->
  "";
text_indent(Size) ->
  lists:map(fun (_) -> $\s end, lists:seq(1, Size)).
