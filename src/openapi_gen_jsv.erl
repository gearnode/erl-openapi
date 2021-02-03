-module(openapi_gen_jsv).

-behaviour(openapi_gen).

-export([module_name/1, generate/2]).

-spec module_name(openapi_gen:options()) -> binary().
module_name(Options) ->
  <<(maps:get(module_prefix, Options, <<>>))/binary, "jsv">>.

-spec generate(openapi:specification(), openapi_gen:options()) ->
        {ok, iodata()} | {error, openapi:error_reason()}.
generate(Spec, Options) ->
  try
    Data0 = do_generate(Spec, Options),
    Data = openapi_gen:remove_trailing_whitespaces(Data0),
    case maps:get(return_binary, Options, false) of
      true ->
        openapi_gen:unicode_iolist_to_binary(Data);
      false ->
        {ok, Data}
    end
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec do_generate(openapi:specification(), openapi_gen:options()) -> iodata().
do_generate(Spec = #{definitions := Definitions}, Options) ->
  ModuleName = module_name(Options),
  [openapi_gen:header(),
   openapi_gen:module_declaration(ModuleName), $\n,
   openapi_gen:export_declaration(["catalog/0"]), $\n,
   generate_catalog_fun(Spec, Options), $\n,
   lists:join($\n, [generate_jsv_definition_fun(Name, Def, Options) ||
                     {Name, Def} <- maps:to_list(Definitions)])].

-spec generate_catalog_fun(openapi:specification(), openapi_gen:options()) ->
        iodata().
generate_catalog_fun(#{definitions := Definitions}, Options) ->
  JSVDefs0 = maps:fold(fun (DefName, _Def, Acc) ->
                           Name = openapi_gen:name(DefName, Options),
                           [{Name, <<Name/binary, "_definition()">>} | Acc]
                       end, [], Definitions),
  JSVDefs = lists:sort(fun({N1, _}, {N2, _}) ->
                           N1 =< N2
                       end, JSVDefs0),
  Body = ["  #{",
          lists:join(",\n  ",
                     [[N, " =>\n    ", Fun] || {N, Fun} <- JSVDefs]),
          "}.\n"],
  ["-spec catalog() -> jsv:catalog().\n",
   "catalog() ->\n",
   openapi_gen:indent(Body, 2)].

-spec generate_jsv_definition_fun(binary(), openapi:schema(),
                                  openapi_gen:options()) -> iodata().
generate_jsv_definition_fun(DefName, Schema, Options) ->
  Name = openapi_gen:name(DefName, Options),
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
generate_jsv_definition(_Schema = #{'$ref' := [<<"definitions">>, DefName]},
                        Options) ->
  Name = openapi_gen:name(DefName, Options),
  ["{ref, ", Name, $}];
generate_jsv_definition(_Schema = #{'$ref' := Pointer}, _Options) ->
  throw({error, {invalid_schema_ref, json_pointer:serialize(Pointer)}});
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
  MembersConstraint =
    case maps:find(properties, Schema) of
      {ok, Properties} ->
        Members =
          maps:fold(fun (MName, MSchema, Acc) ->
                        MType = generate_jsv_definition(MSchema, Options),
                        [[openapi_gen:atom(MName), " =>\n  ",
                          openapi_gen:indent(MType, 2)] | Acc]
                    end, [], Properties),
        Members2 = lists:join(",\n", Members),
        ["members =>\n  ", openapi_gen:indent(["#{", Members2, $}], 4)];
      error ->
        undefined
    end,
  RequiredConstraint =
    case maps:find(required, Schema) of
      {ok, Required} ->
        Required2 = lists:join(",\n", [openapi_gen:atom(M) || M <- Required]),
        ["required =>\n  ", openapi_gen:indent([$[, Required2, $]], 3)];
      error ->
        undefined
    end,
  ValueConstraint =
    case maps:find(additionalProperties, Schema) of
      {ok, true} ->
        ["value =>\n  any"];
      {ok, false} ->
        undefined;
      {ok, AdditionalSchema} ->
        Value = generate_jsv_definition(AdditionalSchema, Options),
        ["value =>\n  ", Value];
      error ->
        undefined
      end,
  Constraints0 = [MembersConstraint, RequiredConstraint, ValueConstraint],
  Constraints = [C || C <- Constraints0, C /= undefined],
  ConstraintsData = lists:join(",\n", Constraints),
  ["{object,\n",
   " #{", openapi_gen:indent(ConstraintsData, 3), "}}"];
generate_jsv_definition(_Schema, _Options) ->
  "any".

-spec generate_one_of_jsv_definition([openapi:schema()],
                                     openapi_gen:options()) ->
        iodata().
generate_one_of_jsv_definition(Schemas, Options) ->
  Defs = [generate_jsv_definition(S, Options) || S <- Schemas],
  ["{one_of,\n"
   "  [", openapi_gen:indent(lists:join("\n,", Defs), 3),"]}"].
