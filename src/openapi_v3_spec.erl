%% Copyright (c) 2021 Exograd SAS.
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

-module(openapi_v3_spec).

-export([read_file/1, read/1, read_value/1]).

-spec read_file(file:name_all()) ->
        {ok, openapi_v3:specification()} | {error, openapi_v3:error_reason()}.
read_file(Path) ->
  case file:read_file(Path) of
    {ok, Data} ->
      read(Data);
    {error, Reason} ->
      {error, {file_error, Reason, Path}}
  end.

-spec read(binary()) ->
        {ok, openapi_v3:specification()} | {error, openapi_v3:error_reason()}.
read(Data) ->
  case json:parse(Data) of
    {ok, Value} ->
      read_value(Value);
    {error, Error} ->
      {error, {invalid_json_data, Error}}
  end.

-spec read_value(json:value()) ->
        {ok, openapi_v3:specification()} | {error, openapi_v3:error_reason()}.
read_value(Value) ->
  Options = #{type_map => openapi_v3_jsv:type_map(),
              unknown_member_handling => keep,
              format_value_errors => true},
  case jsv:validate(Value, {ref, openapi_v3, specification}, Options) of
    {ok, Spec} ->
      {ok, Spec};
    {error, Errors} ->
      {error, {invalid_specification, Errors}}
  end.
