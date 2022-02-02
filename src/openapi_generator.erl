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
      | {invalid_specification, [jsv:value_error()]}
      | openapi_v2:error_reason()
      | openapi_v3:error_reason().

-callback supported_generator() ->
  #{atom() := #{atom() := module()}}.

-callback definition() -> jsv:definition().

-spec generate(module(), json:value(), file:name_all(),
               openapi:generate_options()) ->
        ok | {error, error_reason()}.
generate(Mod, Data, OutDir, Options) ->
  Language = maps:get(language, Options),
  Generator = maps:get(generator, Options),
  JSVOptions = #{type_map => openapi_jsv:type_map(),
                 unknown_member_handling => keep,
                 format_value_errors => true},
  case maps:find(Language, Mod:supported_generator()) of
    {ok, SupportedGenerators} ->
      case maps:find(Generator, SupportedGenerators) of
        {ok, GeneratorMod} ->
          case validate(Data, Mod:definition()) of
            {ok, Spec} ->
              openapi_code_generator:generate(GeneratorMod, Spec, OutDir,
                                              Options);
            {error, Reason} ->
              {error, {invalid_specification, Reason}}
          end;
        error ->
          {error, {unsupported_generator, Generator}}
      end;
    error ->
      {error, {unsupported_language, Language}}
  end.

-spec validate(json:value(), jsv:definition()) ->
        {error, [jsv:value_error()]} | {ok, specification()}.
validate(Data, Definition) ->
  Options = #{type_map => openapi_jsv:type_map(),
              unknown_member_handling => keep,
              format_value_errors => true},
  jsv:validate(Data, Definition, Options).
