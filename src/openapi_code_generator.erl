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

-module(openapi_code_generator).

-export_type([oas_type/0]).

-export([unicode_iolist_to_binary/1,
         remove_trailing_whitespaces/1,
         header_line/1,
         is_reserved_word/2,
         generate/4]).

-type oas_type() ::
        #{type := term(),
          format => term(),
          in_types => [oas_type()]}.

-callback template_dir() -> file:name_all().

-callback cast_type(oas_type()) -> iodata().

-callback reserved_words() -> [binary()].

-callback comment(iodata()) -> iodata().

-callback escape_reserved_word(binary()) -> binary().

-callback typedef(iodata() | {iodata(), iodata()}) -> iodata().

-spec unicode_iolist_to_binary(iolist()) ->
        {ok, binary()} | {error, openapi:error_reason()}.
unicode_iolist_to_binary(Data) ->
  case unicode:characters_to_binary(Data) of
    Bin when is_binary(Bin) ->
      {ok, Bin};
    {error, _, Rest} ->
      {error, {invalid_unicode_data, Rest}};
    {incomplete, _, Rest} ->
      {error, {incomplete_unicode_data, Rest}}
  end.

-spec remove_trailing_whitespaces(iodata()) -> iodata().
remove_trailing_whitespaces(Data) ->
  re:replace(Data, " +\n", "\n", [global]).

-spec header_line(module()) -> iodata().
header_line(Mod) ->
  Now = os:system_time(second),
  Datetime = calendar:system_time_to_rfc3339(Now, [{offset, "Z"}]),
  Line = ["File generated by erl-openapi on ", Datetime, "."],
  [Mod:comment(Line)].

-spec is_reserved_word(module(), binary()) -> boolean().
is_reserved_word(Mod, Word) ->
  lists:member(Word, Mod:reserved_words()).

generate(Mod, _Spec, _OutDir, _Options) ->
  Data =
    lists:join("\n", [header_line(Mod), $\n]),
  io:format("~s", [Data]).
