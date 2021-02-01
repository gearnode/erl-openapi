-module(openapi_model_gen).

-export([generate/1, generate/2]).

-spec generate(openapi:specification()) ->
        {ok, iodata()} | {error, openapi:error_reason()}.
generate(Spec) ->
  generate(Spec, #{}).

-spec generate(openapi:specification(), openapi_gen:options()) ->
        {ok, iodata()} | {error, openapi:error_reason()}.
generate(Spec, Options) ->
  try
    Data = do_generate(Spec, Options),
    case maps:get(return_binary, Options, false) of
      true ->
        case unicode:characters_to_binary(Data) of
          Bin when is_binary(Bin) ->
            {ok, Bin};
          {error, _, Rest} ->
            {error, {invalid_unicode_data, Rest}};
          {incomplete, _, Rest} ->
            {error, {incomplete_unicode_data, Rest}}
        end;
      false ->
        {ok, Data}
    end
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec do_generate(openapi:specification(), openapi_gen:options()) -> iodata().
do_generate(Spec = #{definitions := Definitions}, Options) ->
  ModuleName = [maps:get(module_prefix, Options, ""), "model"],
  Types = maps:fold(fun (Name, Schema, Acc) ->
                        [generate_model(Name, Schema, Spec, Options) | Acc]
                    end, [], Definitions),
  [openapi_gen:header(),
   openapi_gen:module_declaration(ModuleName), $\n,
   openapi_gen:export_type_declaration([Type || Type <- Types]), $\n,
   lists:join($\n, [openapi_gen:type_declaration(Type) || Type <- Types])].

-spec generate_model(DefinitionName :: binary(),
                     openapi:schema(), openapi:specification(),
                     openapi_gen:options()) ->
        openapi_gen:type().
generate_model(DefinitionName, Schema, _Spec, Options) ->
  Name = openapi_gen:name(DefinitionName, Options),
  Desc = case maps:find(description, Schema) of
           {ok, String} -> ["\n\n", String];
           error -> []
         end,
  #{name => Name,
    args => [],
    data => generate_type(Schema, Options),
    comment => [DefinitionName, Desc]}.

-spec generate_type(openapi:schema(), openapi_gen:options()) -> iodata().
generate_type(_Schema = #{'$ref' := [<<"definitions">>, DefName]}, Options) ->
  ModuleName = [maps:get(module_prefix, Options, ""), "model"],
  Name = openapi_gen:name(DefName, Options),
  [ModuleName, $:, Name, "()"];
generate_type(_Schema = #{'$ref' := Pointer}, _Options) ->
  throw({error, {invalid_schema_ref, json_pointer:serialize(Pointer)}});
generate_type(Schema = #{type := Types}, Options) when is_list(Types) ->
  generate_type_union([Schema#{type => Type} || Type <- Types], Options);
generate_type(_Schema = #{type := null}, _Options) ->
  "null";
generate_type(_Schema = #{type := string}, _Options) ->
  "binary()";
generate_type(_Schema = #{type := number}, _Options) ->
  "number()";
generate_type(_Schema = #{type := integer}, _Options) ->
  "integer()";
generate_type(_Schema = #{type := boolean}, _Options) ->
  "boolean()";
generate_type(Schema = #{type := array}, Options) ->
  case maps:find(items, Schema) of
    {ok, ItemSchemas} when is_list(ItemSchemas) ->
      ["[", generate_type_union(ItemSchemas, Options), "]"];
    {ok, ItemSchema} ->
      ["[", generate_type(ItemSchema, Options), "]"];
    error ->
      "list()"
  end;
generate_type(Schema = #{type := object}, Options) ->
  Required = maps:get(required, Schema, []),
  Properties = maps:get(properties, Schema, #{}),
  PTypes =
    maps:fold(fun (PName, PSchema, Acc) ->
                  Op = case lists:member(PName, Required) of
                         true -> " := ";
                         false -> " => "
                       end,
                  PType = generate_type(PSchema, Options),
                  [[openapi_gen:atom(PName), Op, PType] | Acc]
              end, [], Properties),
  AdditionalType =
    case maps:find(additionalProperties, Schema) of
      {ok, true} ->
        ["_ := json:value()"];
      {ok, false} ->
        [];
      {ok, AdditionalSchema} ->
        [["_ := ", generate_type(AdditionalSchema, Options)]];
      error ->
        []
      end,
  ["#{",
   lists:join(",\n", PTypes ++ AdditionalType),
   "}"];
generate_type(_Schema, _Options) ->
  "json:value()".

-spec generate_type_union([openapi:schema()], openapi_gen:options()) ->
        iodata().
generate_type_union(Schemas, Options) ->
  lists:join("\n| ", [generate_type(S, Options) || S <- Schemas]).
