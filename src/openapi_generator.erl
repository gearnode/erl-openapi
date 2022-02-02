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

-export_type([specification/0,
              error_reason/0]).

-export([generate/4]).

-type specification() ::
        openapi_v2:specification()
      | openapi_v3:specification().

-type error_reason() ::
        {unsupported_language, atom()}
      | {unsupported_generator, atom()}
      | openapi_v2:error_reason()
      | openapi_v3:error_reason().

-callback supported_generator() ->
  #{atom() := #{atom() := module()}}.

-callback validate_spec(json:value()) ->
  {error, openapi:error_reason()} | {ok, specification()}.

-spec generate(module(), json:value(), file:name_all(),
               openapi:generate_options()) ->
        ok | {error, error_reason()}.
generate(Mod, Data, OutputDir, Options) ->
  Language = maps:get(language, Options),
  Generator = maps:get(generator, Options),
  case maps:find(Language, Mod:supported_generator()) of
    {ok, SupportedGenerators} ->
      case maps:find(Generator, SupportedGenerators) of
        {ok, GeneratorMod} ->
          case Mod:validate_spec(Data) of
            {ok, Spec} ->
              GeneratorMod:generate(Spec, OutputDir, Options);
            {error, Reason} ->
              {error, Reason}
          end;
        error ->
          {error, {unsupported_generator, Generator}}
      end;
    error ->
      {error, {unsupported_language, Language}}
  end.