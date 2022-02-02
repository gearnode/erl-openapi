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

-module(openapi_gen_model).

-behaviour(openapi_gen).

-export([module_name/1, generate/2]).

-spec module_name(openapi_gen:options()) -> binary().
module_name(Options) ->
  <<(maps:get(module_prefix, Options, <<>>))/binary, "model">>.

-spec generate(openapi_v2:specification(), openapi_gen:options()) ->
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

-spec do_generate(openapi_v2:specification(), openapi_gen:options()) -> iodata().
do_generate(Spec = #{definitions := Definitions}, Options) ->
  ModuleName = module_name(Options),
  Types0 = maps:fold(fun (Name, Schema, Acc) ->
                         [generate_model(Name, Schema, Spec, Options) | Acc]
                     end, [], Definitions),
  Types = lists:sort(fun (#{name := T1Name}, #{name := T2Name}) ->
                         T1Name =< T2Name
                     end, Types0),
  DefinitionIdType = generate_definition_id_type(Spec, Options),
  AllTypes = [DefinitionIdType | Types],
  [openapi_gen:header(),
   openapi_gen:module_declaration(ModuleName), $\n,
   openapi_gen:export_type_declaration([Type || Type <- AllTypes]), $\n,
   lists:join($\n, [openapi_gen:type_declaration(Type) || Type <- AllTypes])].

-spec generate_definition_id_type(openapi_v2:specification(),
                                  openapi_gen:options()) ->
        openapi_gen:type().
generate_definition_id_type(#{definitions := Definitions}, Options) ->
  Names0 = [definition_name(N, Options) || N <- maps:keys(Definitions)],
  Names = lists:sort(Names0),
  [FirstName | OtherNames] = Names,
  Data = [["    ", FirstName, "\n| "], lists:join(" \n| ", OtherNames)],
  #{name => <<"definition_id">>,
    args => [],
    data => Data}.

-spec generate_model(DefinitionName :: binary(),
                     openapi_v2:schema(), openapi_v2:specification(),
                     openapi_gen:options()) ->
        openapi_gen:type().
generate_model(DefinitionName, Schema, _Spec, Options) ->
  Name = definition_name(DefinitionName, Options),
  Desc = case maps:find(description, Schema) of
           {ok, String} -> ["\n\n", String];
           error -> []
         end,
  #{name => Name,
    args => [],
    data => generate_type(Schema, Options),
    comment => [DefinitionName, Desc]}.

-spec generate_type(openapi_v2:schema(), openapi_gen:options()) -> iodata().
generate_type(_Schema = #{'$ref' := [<<"definitions">>, DefName]}, Options) ->
  ModuleName = [maps:get(module_prefix, Options, ""), "model"],
  Name = definition_name(DefName, Options),
  [ModuleName, $:, Name, "()"];
generate_type(_Schema = #{'$ref' := Pointer}, _Options) ->
  throw({error, {invalid_schema_ref, json_pointer:serialize(Pointer)}});
generate_type(Schema = #{type := Types}, Options) when is_list(Types) ->
  generate_type_union([Schema#{type => Type} || Type <- Types], Options);
generate_type(_Schema = #{type := null}, _Options) ->
  "null";
generate_type(Schema = #{type := string}, _Options) ->
  case maps:find(format, Schema) of
    {ok, <<"date">>} ->
      "calendar:date()";
    {ok, <<"date-time">>} ->
      "calendar:datetime()";
    {ok, <<"int-or-string">>} ->
      "integer() | binary()";
    {ok, _} ->
      "binary()";
    error ->
      "binary()"
  end;
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

-spec generate_type_union([openapi_v2:schema()], openapi_gen:options()) ->
        iodata().
generate_type_union(Schemas, Options) ->
  lists:join("\n| ", [generate_type(S, Options) || S <- Schemas]).

-spec definition_name(binary(), openapi_gen:options()) -> binary().
definition_name(DefName, Options) ->
  case openapi_gen:name(DefName, Options) of
    <<"definition_id">> ->
      <<"definition_id_">>;
    Name ->
      Name
  end.
