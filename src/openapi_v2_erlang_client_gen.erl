%% Copyright (c) 2021-2022 Exograd SAS.
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

-module(openapi_v2_erlang_client_gen).

-behaviour(openapi_code_generator).

-export([template_dir/0,
         cast_type/1,
         typedef/1,
         reserved_words/0,
         comment/1,
         escape_reserved_word/1]).

template_dir() ->
  "/v2/erlang/client".

cast_type(#{type := null}) ->
  "null";
cast_type(#{type := number}) ->
  typedef("number");
cast_type(#{type := integer}) ->
  typedef("integer");
cast_type(#{type := boolean}) ->
  typedef("boolean");
cast_type(#{type := string, format := date}) ->
  typedef({"calendar", "datetime"});
cast_type(#{type := string, format := 'date-time'}) ->
  typedef({"calendar", "datetime"});
cast_type(#{type := string, format := 'int-or-string'}) ->
  [typedef("integer"),  " | ", typedef("binary")];
cast_type(#{type := string}) ->
  typedef("binary");
cast_type(#{type := array, in_types := InTypes}) ->
  case InTypes of
    [InType] ->
      [$[, cast_type(InType), $]];
    _ ->
      lists:join("\n|", [cast_type(Type) || Type <- InTypes])
  end;
cast_type(#{type := array}) ->
  typedef("list");
cast_type(_) ->
  typedef({"json", "value"}).

typedef({Module, Name}) ->
  [Module, ":", Name, "()"];
typedef(Name) ->
  [Name, "()"].

reserved_words() ->
  [<<"after">>, <<"and">>, <<"andalso">>, <<"band">>, <<"begin">>, <<"bnot">>,
   <<"bor">>, <<"bsl">>, <<"bsr">>, <<"bxor">>, <<"case">>, <<"catch">>,
   <<"cond">>, <<"div">>, <<"end">>, <<"fun">>, <<"if">>, <<"let">>, <<"not">>,
   <<"of">>, <<"or">>, <<"orelse">>, <<"receive">>, <<"rem">>, <<"try">>,
   <<"when">>, <<"xor">>].

comment(Lines) ->
  F = fun (Line) ->
          case unicode:characters_to_list(Line) of
            "" ->
              prettypr:break(prettypr:text(""));
            String ->
              prettypr:text_par(String)
          end
      end,
  Paragraphs = lists:map(F, string:split(Lines, "\n", all)),
  FilledText = prettypr:format(prettypr:sep(Paragraphs), 77),
  ["%% ", string:replace(FilledText, "\n", "\n%% ", all), $\n].

escape_reserved_word(Word0) ->
  Word = string:replace(Word0, "'", "\\'", all),
  iolist_to_binary([$', Word, $']).
