-module(openapi_jsv_gen).

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
  ModuleName = [maps:get(module_prefix, Options, ""), "jsv"],
  [openapi_gen:header(),
   openapi_gen:module_declaration(ModuleName), $\n,
   openapi_gen:export_declaration(["catalog/0"]), $\n,
   generate_catalog_fun(Spec, Options), $\n,
   lists:join($\n, [generate_jsv_definition_fun(Name, Def, Options) ||
                     {Name, Def} <- maps:to_list(Definitions)])].

-spec generate_catalog_fun(openapi:specification(), openapi_gen:options()) ->
        iodata().
generate_catalog_fun(#{definitions := Definitions}, _Options) ->
  JSVDefs = maps:fold(fun (DefName, _Def, Acc) ->
                          Name = openapi_gen:name(DefName),
                          [{Name, <<Name/binary, "_definition()">>} | Acc]
                      end, [], Definitions),
  Body = ["  #{",
          lists:join("\n  ",
                     [[N, " =>\n    ", Fun] || {N, Fun} <- JSVDefs]),
          "}.\n"],
  ["-type catalog() -> jsv:catalog().\n",
   "catalog() ->\n",
   openapi_gen:indent(Body, 2)].

-spec generate_jsv_definition_fun(binary(), openapi:schema(),
                                  openapi_gen:options()) -> iodata().
generate_jsv_definition_fun(DefName, Schema, Options) ->
  Name = openapi_gen:name(DefName),
  Body = ["  ", generate_jsv_definition(Schema, Options), ".\n"],
  Desc = case maps:find(description, Schema) of
           {ok, String} -> ["\n\n", String];
           error -> []
         end,
  [openapi_gen:comment([DefName, Desc]),
   "-spec ", Name, "_definition() -> jsv:definition().\n",
   Name, "_definition() ->\n",
   openapi_gen:indent(Body, 2)].

-spec generate_jsv_definition(openapi:schema(), openapi_gen:options()) ->
        iodata().
generate_jsv_definition(_Schema = #{type := null}, _Options) ->
  "null";
generate_jsv_definition(_Schema = #{type := string}, _Options) ->
  "string";
generate_jsv_definition(_Schema = #{type := number}, _Options) ->
  "number";
generate_jsv_definition(_Schema = #{type := integer}, _Options) ->
  "integer";
generate_jsv_definition(_Schema = #{type := boolean}, _Options) ->
  "boolean";
generate_jsv_definition(Schema = #{type := array}, Options) ->
  case maps:find(items, Schema) of
    {ok, ItemSchemas} when is_list(ItemSchemas) ->
      ["{array,\n",
       "  #{element =>\n",
       "      ", generate_one_of_jsv_definition(ItemSchemas, Options),"}}"];
    {ok, ItemSchema} ->
      ["{array,\n",
       "  #{element =>\n",
       "      ", generate_jsv_definition(ItemSchema, Options),"}}"];
    error ->
      "array"
  end;
generate_jsv_definition(Schema = #{type := object}, Options) ->
  Required = maps:get(required, Schema, []),
  Required2 = lists:join(",\n", [openapi_gen:atom(M) || M <- Required]),
  RequiredConstraint = ["required =>\n  ",
                        openapi_gen:indent([$[, Required2, $]], 3)],
  Properties = maps:get(properties, Schema, #{}),
  Members =
    maps:fold(fun (MName, MSchema, Acc) ->
                  MType = generate_jsv_definition(MSchema, Options),
                  [[openapi_gen:atom(MName), " =>\n  ",
                    openapi_gen:indent(MType, 2)] | Acc]
              end, [], Properties),
  Members2 = lists:join(",\n", Members),
  MembersConstraint = ["members =>\n  ",
                       openapi_gen:indent(["#{", Members2, $}], 4)],
  Constraints1 = [MembersConstraint, RequiredConstraint],
  Constraints2 =
    case maps:find(additionalProperties, Schema) of
      {ok, true} ->
        [["value =>\n  any"] | Constraints1];
      {ok, false} ->
        Constraints1;
      {ok, AdditionalSchema} ->
        Value = generate_jsv_definition(AdditionalSchema, Options),
        [["value =>\n  ", Value] | Constraints1];
      error ->
        Constraints1
      end,
  ["#{",
   openapi_gen:indent(lists:join(",\n", Constraints2), 2), "}"];
generate_jsv_definition(_Schema, _Options) ->
  "any".

-spec generate_one_of_jsv_definition([openapi:schema()],
                                     openapi_gen:options()) ->
        iodata().
generate_one_of_jsv_definition(Schemas, Options) ->
  Defs = [generate_jsv_definition(S, Options) || S <- Schemas],
  ["{one_of,\n"
   "  [", openapi_gen:indent(lists:join("\n,", Defs), 3),"]}"].
