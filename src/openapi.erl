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

-module(openapi).

-export([generate/3]).

-export_type([generate_options/0,
              error_reason/0,
              ref/0]).

-type error_reason() ::
        {file_error, term(), file:name_all()}
      | {invalid_json_data, json:error()}
      | {invalid_output_directory, file:posix()}
      | openapi_generator:error_reason().

-type ref() ::
        json_pointer:pointer() | {URI :: binary(), json_pointer:pointer()}.

-type generate_options() ::
        #{language := atom(),
          generator := atom(),
          package_name := binary(),
          model_package => binary(),
          model_name_suffix => binary(),
          model_name_prefix => binary(),
          api_package => binary(),
          api_name_suffix => binary(),
          api_name_prefix => binary()}.

-spec generate(file:name_all(), file:name_all(), generate_options()) ->
        ok | {error, error_reason()}.
generate(Filename, OutDir0, Options) ->
  OutDir = list_to_binary([filename:absname(OutDir0), $/]),
  case file:read_file(Filename) of
    {ok, File} ->
      case json:parse(File) of
        {ok, Data} ->
          case filelib:ensure_dir(OutDir) of
            ok ->
              case maps:is_key(<<"swagger">>, Data) of
                true ->
                  openapi_generator:generate(openapi_v2, Data, OutDir, Options);
                false ->
                  openapi_generator:generate(openapi_v3, Data, OutDir, Options)
              end;
            {error, Reason} ->
              {error, {invalid_output_directory, Reason}}
          end;
        {error, Reason} ->
          {error, {invalid_json_data, Reason}}
      end;
    {error, Reason} ->
      {error, {file_error, Reason, Filename}}
  end.
