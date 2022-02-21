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

-module(openapi_generator).

-export_type([error_reason/0]).

-export([generate/3]).

-type error_reason() ::
        {unsupported_language, atom()}
      | {unsupported_generator, atom()}
      | openapi_erlang_client_code_gen:error_reason().

-spec supported_generators() -> map().
supported_generators() ->
  #{erlang =>
      #{client => openapi_erlang_client_code_gen}}.

-spec generate(json:value(), file:name_all(), openapi:generate_options()) ->
        ok | {error, error_reason()}.
generate(Data, OutDir,
         #{language := Language, generator := Generator} = Options) ->
  case maps:find(Language, supported_generators()) of
    {ok, Generators} ->
      case maps:find(Generator, Generators) of
        {ok, Mod} ->
          Mod:generate(Data, OutDir, Options);
        error ->
          {error, {unsupported_generator, Generator}}
      end;
    error ->
      {error, {unsupported_language, Language}}
  end.
