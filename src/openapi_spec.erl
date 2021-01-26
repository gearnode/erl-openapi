-module(openapi_spec).

-export([load_file/1]).

-export_type([error_reason/0, spec/0]).

-type error_reason() ::
        {file_error, term(), file:name_all()}
      | {invalid_json_data, json:error()}
      | {invalid_spec, [jsv:value_error()]}.

-type spec() ::
        #{}.

-spec load_file(file:name_all()) -> ok | {error, error_reason()}.
load_file(Path) ->
  case file:read_file(Path) of
    {ok, Data} ->
      read(Data);
    {error, Reason} ->
      {error, {file_error, Reason, Path}}
  end.

-spec read(binary()) -> ok | {error, error_reason()}.
read(Data) ->
  case json:parse(Data) of
    {ok, Value} ->
      read_value(Value);
    {error, Error} ->
      {error, {invalid_data, Error}}
  end.

-spec read_value(json:value()) -> {ok, spec()} | {error, error_reason()}.
read_value(Value) ->
  case jsv:validate(Value, {ref, openapi, spec}) of
    {ok, Spec} ->
      {ok, Spec};
    {error, Errors} ->
      {error, {invalid_spec, Errors}}
  end.
