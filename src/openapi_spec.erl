-module(openapi_spec).

-export([read_file/1, read/1, read_value/1]).

-spec read_file(file:name_all()) ->
        {ok, openapi:specification()} | {error, openapi:error_reason()}.
read_file(Path) ->
  case file:read_file(Path) of
    {ok, Data} ->
      read(Data);
    {error, Reason} ->
      {error, {file_error, Reason, Path}}
  end.

-spec read(binary()) ->
        {ok, openapi:specification()} | {error, openapi:error_reason()}.
read(Data) ->
  case json:parse(Data) of
    {ok, Value} ->
      read_value(Value);
    {error, Error} ->
      {error, {invalid_json_data, Error}}
  end.

-spec read_value(json:value()) ->
        {ok, openapi:specification()} | {error, openapi:error_reason()}.
read_value(Value) ->
  Options = #{type_map => openapi_jsv:type_map(),
              invalid_member_handling => keep,
              format_value_errors => true},
  case jsv:validate(Value, {ref, openapi, specification}, Options) of
    {ok, Spec} ->
      {ok, Spec};
    {error, Errors} ->
      {error, {invalid_specification, Errors}}
  end.
