-module(openapi_spec).

-export([read_file/1, read/1, read_value/1]).

-export_type([error_reason/0]).

-type error_reason() ::
        {file_error, term(), file:name_all()}
      | {invalid_json_data, json:error()}
      | {invalid_specification, [jsv:value_error()]}
      | {invalid_ref_uri, uri:error_reason(), binary()}
      | {unsupported_external_ref, binary()}
      | {unsupported_circular_ref, binary()}
      | {invalid_ref_pointer, json_pointer:error_reason(), binary()}
      | {invalid_ref, binary()}.

-spec read_file(file:name_all()) -> ok | {error, error_reason()}.
read_file(Path) ->
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
      {error, {invalid_json_data, Error}}
  end.

-spec read_value(json:value()) ->
        {ok, openapi:specification()} | {error, error_reason()}.
read_value(Value) ->
  case resolve_refs(Value) of
    {ok, Value2} ->
      Options = #{invalid_member_handling => keep},
      case jsv:validate(Value2, {ref, openapi, specification}, Options) of
        {ok, Spec} ->
          {ok, Spec};
        {error, Errors} ->
          {error, {invalid_specification, Errors}}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec resolve_refs(json:value()) ->
        {ok, json:value()} | {error, error_reason()}.
resolve_refs(Spec) ->
  try
    {ok, resolve_refs(Spec, Spec, #{})}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec resolve_refs(json:value(), json:value(), #{binary() := boolean()}) ->
        json:value().
resolve_refs(Value, Spec, Refs) when is_list(Value) ->
  lists:map(fun (Element) -> resolve_refs(Element, Spec, Refs) end, Value);
resolve_refs(Value = #{<<"$ref">> := Ref}, Spec, Refs) when is_binary(Ref) ->
  Value2 = maps:without([<<"$ref">>], Value),
  maps:merge(resolve_refs(Value2, Spec, Refs),
             resolve_ref(Ref, Spec, Refs));
resolve_refs(Value, Spec, Refs) when is_map(Value) ->
  maps:fold(fun (K, V, Acc) ->
                Acc#{K => resolve_refs(V, Spec, Refs)}
            end, #{}, Value);
resolve_refs(Value, _, _) ->
  Value.

-spec resolve_ref(binary(), json:value(), #{binary() := boolean()}) ->
        json:value().
resolve_ref(URIString, _, Refs) when is_map_key(URIString, Refs) ->
  throw({error, {unsupported_circular_ref, URIString}});
resolve_ref(URIString, Spec, Refs) ->
  case uri:parse(URIString) of
    {ok, #{host := _}} ->
      throw({error, {unsupported_external_ref, URIString}});
    {ok, URI} ->
      Fragment = uri:fragment(URI),
      case json_pointer:parse(Fragment) of
        {ok, Pointer} ->
          case json_pointer:find(Pointer, Spec) of
            {ok, Value} ->
              resolve_refs(Value, Spec, Refs#{URIString => true});
            {error, Reason} ->
              throw({error, {invalid_ref, Reason, URIString}})
          end;
        {error, Reason} ->
          throw({error, {invalid_ref_pointer, Reason, URIString}})
      end;
    {error, Reason} ->
      throw({error, {invalid_ref_uri, Reason, URIString}})
  end.
