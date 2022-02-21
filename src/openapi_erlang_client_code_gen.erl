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

-module(openapi_erlang_client_code_gen).

-import(prettypr,
        [text/1,
         null_text/1,
         nest/2,
         above/2,
         beside/2,
         sep/1,
         par/1,
         par/2,
         floating/3,
         floating/1,
         break/1,
         follow/2,
         follow/3,
         empty/0]).

-export_type([error_reason/0]).

-export([generate/3]).

-type error_reason() :: term().

-spec generate(openapi:specification(), file:name_all(),
               openapi:generate_options()) ->
        ok | {error, error_reason()}.
generate(Spec, OutDir, Options) ->
  Now = os:system_time(second),
  Datetime = calendar:system_time_to_rfc3339(Now, [{offset, "Z"}]),
  PackageName = maps:get(package_name, Options),

  F1 = generate_openapi_file(Datetime, PackageName, Spec, Options),
  F2 = generate_model_file(Datetime, PackageName, Spec, Options),
  F3 = generate_jsv_file(Datetime, PackageName, Spec, Options),

  file:write_file(filename:join(OutDir, <<PackageName/binary, "_openapi.erl">>), F1),
  file:write_file(filename:join(OutDir, <<PackageName/binary, "_schemas.erl">>), F2),
  file:write_file(filename:join(OutDir, <<PackageName/binary, "_jsv.erl">>), F3),
  ok.


generate_jsv_file(Datetime, PackageName, Spec, Options) ->
  Schemas = maps:get(schemas, maps:get(components, Spec, #{}), #{}),
  Data = #{datetime => Datetime,
           package_name => <<PackageName/binary, "_jsv">>,
           functions => generate_functions(Schemas, Options)},
  openapi_mustache:render(<<"erlang-client/jsv.erl">>, Data).

generate_functions(Schemas, Options) ->
  Iterator = maps:iterator(Schemas),
  generate_functions(maps:next(Iterator), Options, []).

generate_functions(none, _, Acc) ->
  lists:reverse(Acc);
generate_functions({Name0, Schema, I}, Options, Acc) ->
  Name = openapi_code:snake_case(Name0),
  Data = ["-spec ", Name, "_definition() -> jsv:definition().\n",
   Name, "_definition() ->\n",
   schema_to_jsv(Schema, Options), $.],
  Func = unicode:characters_to_binary(Data),
  generate_functions(maps:next(I), Options, [Func | Acc]).

schema_to_jsv(#{type := number, nullable := true}, _) ->
  "{one_of, [number, null]}";
schema_to_jsv(#{type := number}, _) ->
  "number";
schema_to_jsv(#{type := integer, nullable := true}, _) ->
  "{one_of, [integer, null]}";
schema_to_jsv(#{type := integer}, _) ->
  "integer";
schema_to_jsv(#{type := boolean, nullable := true}, _) ->
  "{one_of, [boolean, null]}";
schema_to_jsv(#{type := boolean}, _) ->
  "boolean";
schema_to_jsv(#{type := string, enum := Values} = Schema, _) ->
  A = lists:map(fun (X) -> [$', X, $'] end, Values),
  B = lists:join(",", A),
  C = ["{string, #{values => [", B, "]}}"],
  case maps:get(nullable, Schema, false) of
    true ->
      ["{one_of, [", C, ", null]}"];
    false ->
      C
  end;
schema_to_jsv(#{type := string, nullable := true}, _) ->
  "{one_of, [string, null]}";
schema_to_jsv(#{type := string}, _) ->
  "string";
schema_to_jsv(#{'$ref' := Ref}, _) ->
  ["{ref, ", openapi_code:snake_case(lists:last(Ref)), "}"];
schema_to_jsv(#{type := array} = Schema, Options) ->
  case maps:find(items, Schema) of
    {ok, ItemSchemas} ->
      ["{array, #{element =>" , schema_to_jsv(ItemSchemas, Options), "}}"];
    error ->
      "array"
  end;
schema_to_jsv(#{anyOf := Schemas}, Options) ->
  A = lists:map(fun (X) -> schema_to_jsv(X, Options) end, Schemas),
  B = lists:join(",", A),
  ["{one_of, [", B, "]}"];
schema_to_jsv(#{type := object} = Schema, Options) ->
  MembersConstraint =
    case maps:find(properties, Schema) of
      {ok, Properties} ->
        Members =
          maps:fold(fun (MName, MSchema, Acc) ->
                        MType = schema_to_jsv(MSchema, Options),
                        [[openapi_code:snake_case(MName), " => ", MType] | Acc]
                    end, [], Properties),
        Members2 = lists:join(", ", Members),
        ["members => ", "#{", Members2, $}];
      error ->
        undefined
    end,
  RequiredConstraint =
    case maps:find(required, Schema) of
      {ok, Required} ->
        Required2 = lists:join(", ", [openapi_code:snake_case(M) || M <- Required]),
        ["required =>  ", $[, Required2, $]];
      error ->
        undefined
    end,
  ValueConstraint =
    case maps:find(additionalProperties, Schema) of
      {ok, true} ->
        ["value => any"];
      {ok, false} ->
        undefined;
      {ok, AdditionalSchema} ->
        Value = schema_to_jsv(AdditionalSchema, Options),
        ["value => ", Value];
      error ->
        undefined
      end,
  Constraints0 = [MembersConstraint, RequiredConstraint, ValueConstraint],
  Constraints = [C || C <- Constraints0, C /= undefined],
  ConstraintsData = lists:join(", ", Constraints),
  X = ["{object, #{", ConstraintsData, "}}"],
  case maps:get(nullable, Schema, false) of
    true ->
      ["{one_of, [", X, ", null]}"];
    false ->
      X
  end;
schema_to_jsv(_, _) ->
  ["any"].

generate_model_file(Datetime, PackageName, Spec, Options) ->
  Schemas = maps:get(schemas, maps:get(components, Spec, #{}), #{}),
  Data = #{datetime => Datetime,
           package_name => <<PackageName/binary, "_schemas">>,
           types => generate_types(Schemas, Options),
           functions => []},
  openapi_mustache:render(<<"erlang-client/model.erl">>, Data).

generate_types(Schemas, Options) ->
  Iterator = maps:iterator(Schemas),
  generate_types(maps:next(Iterator), Options, []).

generate_types(none, _, Acc) ->
  lists:reverse(Acc);
generate_types({Name, Schema, I}, Options, Acc) ->
  Type = #{comment => type_comment(Name, Schema, Options),
           name => openapi_code:snake_case(Name),
           value => unicode:characters_to_binary(schema_to_typespec(Schema))},
  generate_types(maps:next(I), Options, [Type | Acc]).

-spec type_comment(binary(), openapi:schema(), openapi:generate_options()) -> binary().
type_comment(Name, Schema, Options) ->
  Text =
    case maps:find(description, Schema) of
      {ok, Value} when Value =/= <<>> ->
        [Name, "\n\n", Value];
      _ ->
        Name
    end,
  unicode:characters_to_binary(
    openapi_code:comment("%%", Text, 0, Options)).


-spec schema_to_typespec(openapi:schema()) -> iodata().
schema_to_typespec(#{type := object, properties := Props} = Schema) ->
  Required = maps:get(required, Schema, []),
  F = fun (Name, Schema2, Acc) ->
          Operator =
            case lists:member(Name, Required) of
              true -> " := ";
              false -> " => "
            end,
          [[$', Name, $', Operator, schema_to_typespec(Schema2)] | Acc]
      end,
  Definition = maps:fold(F, [], Props),
  [$#, ${, lists:join(", ", Definition), $}];
schema_to_typespec(#{type := object, nullable := true}) ->
  "json:value() | null";
schema_to_typespec(#{type := object}) ->
  "json:value()";
schema_to_typespec(#{type := integer, nullable := true}) ->
  "integer() | null";
schema_to_typespec(#{type := integer}) ->
  "integer()";
schema_to_typespec(#{type := number, nullable := true}) ->
  "number() | null";
schema_to_typespec(#{type := number}) ->
  "number()";
schema_to_typespec(#{type := boolean, nullable := true}) ->
  "boolean() | null";
schema_to_typespec(#{type := boolean}) ->
  "boolean()";
schema_to_typespec(#{type := array, nullable := true} = Schema) ->
  case maps:find(items, Schema) of
    {ok, ItemSchema} ->
      [$[, schema_to_typespec(ItemSchema), $], " | null"];
    error ->
      "list() | null"
  end;
schema_to_typespec(#{type := array} = Schema) ->
  case maps:find(items, Schema) of
    {ok, ItemSchema} ->
      [$[, schema_to_typespec(ItemSchema), $]];
    error ->
      "list()"
  end;
schema_to_typespec(#{type := string, enum := Enum, nullable := true}) ->
  lists:join(" | ", lists:map(fun (X) -> [$', X, $'] end, Enum ++ ["null"]));
schema_to_typespec(#{type := string, enum := Enum}) ->
  lists:join(" | ", lists:map(fun (X) -> [$', X, $'] end, Enum));
schema_to_typespec(#{type := string, nullable := true}) ->
  "binary() | null";
schema_to_typespec(#{type := string}) ->
  "binary()";
schema_to_typespec(#{anyOf := Schemas}) ->
  lists:join(" | ", lists:map(fun schema_to_typespec/1, Schemas));
schema_to_typespec(#{'$ref' := Ref}) ->
  [openapi_code:snake_case(lists:last(Ref)), "()"];
schema_to_typespec(_) ->
  "json:value()".

generate_openapi_file(Datetime, PackageName, Spec, Options) ->
  Data = #{datetime => Datetime,
           package_name => <<PackageName/binary, "_openapi">>,
           functions => [info_fun_0(Spec), servers_fun_0(Spec)]},
  openapi_mustache:render(<<"erlang-client/openapi.erl">>, Data).

-spec info_fun_0(openapi:specification()) -> binary().
info_fun_0(#{info := Info}) ->
  Data = io_lib:format("~p.", [Info]),
  Lines = string:split(Data, "\n", all),
  Documents = lists:map(fun (Line) -> text(Line) end, Lines),
  iolist_to_binary(
    prettypr:format(
      sep(
        [beside(
           break(text("-spec info() -> info().")),
           break(text("info() ->"))),
         nest(4, beside(
                   sep(Documents),
                   empty())),
         empty()]))).

-spec servers_fun_0(openapi:specification()) -> iodata().
servers_fun_0(#{servers := Servers}) ->
  Data = io_lib:format("~p.", [Servers]),
  Lines = string:split(Data, "\n", all),
  Documents = lists:map(fun (Line) -> text(Line) end, Lines),
  iolist_to_binary(
    prettypr:format(
      sep(
        [beside(
           break(text("-spec servers() -> [server()].")),
           break(text("servers() ->"))),
         nest(4, beside(
                   sep(Documents),
                   empty())),
         empty()])));
servers_fun_0(_) ->
  "".
