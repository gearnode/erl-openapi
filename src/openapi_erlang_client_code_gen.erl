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


%% -spec atom(binary()) -> binary().
%% atom(Name) ->
%%   case re:run(Name, "^[a-z][A-Za-z_@]*$") of
%%     {match, _} ->
%%       Name;
%%     nomatch ->
%%       %% Escape
%%       ok
%%   end.

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
  F4 = generate_client_file(Datetime, PackageName, Spec, Options),

  write_and_format_file(OutDir, <<PackageName/binary, "_openapi.erl">>, F1),
  write_and_format_file(OutDir, <<PackageName/binary, "_schemas.erl">>, F2),
  write_and_format_file(OutDir, <<PackageName/binary, "_jsv.erl">>, F3),
  write_and_format_file(OutDir, <<PackageName/binary, "_client.erl">>, F4),
  ok.

-spec write_and_format_file(binary(), binary(), iodata()) -> ok.
write_and_format_file(OutDir, Filename0, Content) ->
  Filename = binary_to_list(filename:join(OutDir, Filename0)),
  State = rebar3_formatter:new(default_formatter,
                               #{output_dir => OutDir, action => format},
                               undefined),
  file:write_file(Filename, Content),
  rebar3_formatter:format_file(Filename, State),
  ok.

generate_client_file(Datetime, PackageName, Spec, Options) ->
  Paths = openapi:paths(Spec),
  Data = #{datetime => Datetime,
           types => generate_client_function_request_types(Paths, Options),
           package_name => <<PackageName/binary, "_client">>,
           functions => generate_client_functions(Paths, Options)},
  openapi_mustache:render(<<"erlang-client/client.erl">>, Data, #{disable_html_escaping => true}).


generate_client_function_request_types(Paths, _Options) ->
  maps:fold(
    fun (_, PathItemObject, Acc) ->
        PathOperations = openapi_path:operations(PathItemObject),
        Acc ++ lists:map(
          fun
            ({_, OperationObject}) ->
              Id = openapi_operation:operation_id(OperationObject),
              TypeName = [openapi_code:snake_case(Id), "_request"],
              Parameters = openapi_operation:parameters(OperationObject),

              QueryParameters = openapi_parameter:queries(Parameters),
              PathParameters = openapi_parameter:paths(Parameters),
              HeaderParameters = openapi_parameter:headers(Parameters),
              CookieParameters = openapi_parameter:cookies(Parameters),

              F = fun (X) ->
                      Op = case openapi_parameter:required(X) of
                             true ->
                               " := ";
                             false ->
                               " => "
                           end,
                      Name = openapi_parameter:name(X),
                      KeyName = openapi_code:snake_case(Name),
                      Schema = maps:get(schema, X),
                      [KeyName, Op, schema_to_typespec(Schema)]
                  end,

              TypeDef =
                ["-type ", TypeName, "() :: #{",
                 lists:join(
                   ", ",
                   [["query => ",
                     if
                       length(QueryParameters) =:= 0 ->
                         "map()";
                       true ->
                         ["#{", lists:join(", ", lists:map(F, QueryParameters)), $}]
                     end],
                    ["header => ",
                     if
                       length(HeaderParameters) =:= 0 ->
                         "map()";
                       true ->
                         ["#{", lists:join(", ", lists:map(F, HeaderParameters)), $}]
                    end],
                    ["cookie => ",
                     if
                       length(CookieParameters) =:= 0 ->
                         "map()";
                       true ->
                         ["#{", lists:join(", ", lists:map(F, CookieParameters)), $}]
                     end],
                    ["body => {binary(), term()}"]] ++ lists:map(F, PathParameters)),
                 "}."],

              #{name => unicode:characters_to_binary(TypeName),
                def => unicode:characters_to_binary(TypeDef)}
          end, PathOperations)
    end, [], Paths).

generate_client_functions(Paths, Options) ->
  %% unicode:characters_to_binary(
    maps:fold(
      fun (Path, PathItemObject, Acc) ->
          {PathFormat, VariablesNames} = openapi_path_template:parse(Path),
          %% TODO manage global parametgers not needed for stripe
          %% io:format("XXX ~p~n", [maps:get(parameters, PathItemObject, [])]),
          %% io:format("XXX ~p~n", [maps:get(securitySchemes]),

          PathOperations = openapi_path:operations(PathItemObject),
          lists:map(
            fun
              ({Verb, OperationObject}) ->

                Id = openapi_operation:operation_id(OperationObject),
                Parameters = openapi_operation:parameters(OperationObject),

                QueryParameters = openapi_parameter:queries(Parameters),
                PathParameters = openapi_parameter:paths(Parameters),
                HeaderParameters = openapi_parameter:headers(Parameters),
                %% Responses = openapi_operation:responses(OperationObject),

                FuncName = openapi_code:snake_case(Id),

                #{f_name => FuncName,
                  path_parameters =>
                    lists:map(
                      fun (#{name := Name}) ->
                          #{pascal_name => openapi_code:pascal_case(Name),
                            snake_name => openapi_code:snake_case(Name)}
                      end, PathParameters),
                  query_parameters =>
                    lists:map(
                      fun (#{name := Name} = Parameter) ->
                          #{snake_name => openapi_code:snake_case(Name),
                            real_name => Name,
                            style => maps:get(style, Parameter, form),
                            explode => maps:get(explode, Parameter, false)}
                      end, QueryParameters),
                  header_parameters =>
                    lists:map(
                      fun (#{name := Name} = Parameter) ->
                          #{snake_name => openapi_code:snake_case(Name),
                            real_name => Name,
                            style => maps:get(style, Parameter, simple),
                            explode => maps:get(explode, Parameter, false)}
                      end, HeaderParameters),
                  method => Verb,
                  target_host => maps:get(default_host, Options, <<>>),
                  path_format => unicode:characters_to_binary(PathFormat),
                  path_args =>
                    unicode:characters_to_binary(
                      ["[",
                       lists:join(
                         ",",
                         lists:map(
                           fun (Name) ->
                               ["Var", openapi_code:pascal_case(Name)]
                           end, VariablesNames)), "]"]),
                  responses =>
                    []}


                %%   "_ReqBody = [],",
                %%   "target => #{scheme => <<\"https\">>, host => <<\"api.stripe.com\">>, path => iolist_to_binary(ReqPath), query => ReqQuery}",
                %%   "},",
                %%

                %%   "case mhttp_response:status(Response) of\n",
                %%   maps:fold(
                %%     fun
                %%       (StatusCode, _ResponseObject, Acc3) when
                %%           StatusCode =/= <<"default">> ->
                %%         [[StatusCode, " ->\n",
                %%           "{ok, Response};"] | Acc3];
                %%           %% case maps:find(content, ResponseObject) of
                %%           %%   {ok, Contents} ->
                %%           %%     ["if\n",
                %%           %%      maps:fold(
                %%           %%        fun (_, _, Acc4) ->
                %%           %%            [["mhttp_media_range:match() =:= true ->\n",
                %%           %%              "hello;\n"] | Acc4]
                %%           %%        end, [], Contents),
                %%           %%      "true ->\n",
                %%           %%      "{ok, Response}\n",
                %%           %%      "end;"];
                %%           %%   error ->
                %%           %%     "{ok, Response};"
                %%           %% end] | Acc3];
                %%       (_, _, Acc3) ->
                %%                Acc3
                %%     end, [], Responses),
                %%   "_ ->\n",
            end, PathOperations) ++ Acc
      end, [], Paths).

generate_jsv_file(Datetime, PackageName, Spec, Options) ->
  Schemas = maps:get(schemas, openapi:components(Spec), #{}),
  Data = #{datetime => Datetime,
           package_name => <<PackageName/binary, "_jsv">>,
           functions => [generate_jsv_catalog(Schemas, Options)] ++ generate_functions(Schemas, Options)},
  openapi_mustache:render(<<"erlang-client/jsv.erl">>, Data).


generate_jsv_catalog(Schemas, _Options) ->
  Values =
    maps:fold(fun (Name0, _, Acc) ->
                  Name = openapi_code:snake_case(Name0),
                  [[Name, " => ", [Name, "_definition()"]] | Acc]
              end, [], Schemas),
  Func =
    ["-spec catalog() -> jsv:catalog().\n",
     "catalog() ->\n",
     "#{", lists:join(", ", Values), "}."],
  #{func => unicode:characters_to_binary(Func), name => <<"catalog">>}.

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
  X = #{func => Func, name => <<Name/binary, "_definition">>},
  generate_functions(maps:next(I), Options, [X | Acc]).

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
                        [[$', openapi_code:snake_case(MName), $', " => ", MType] | Acc]
                    end, [], Properties),
        Members2 = lists:join(", ", Members),
        ["members => ", "#{", Members2, $}];
      error ->
        undefined
    end,
  RequiredConstraint =
    case maps:find(required, Schema) of
      {ok, Required} ->
        Required2 = lists:join(", ", [[$', openapi_code:snake_case(M), $'] || M <- Required]),
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
  Schemas = maps:get(schemas, openapi:components(Spec), #{}),
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

generate_openapi_file(Datetime, PackageName, Spec, _Options) ->
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
