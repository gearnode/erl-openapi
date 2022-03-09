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

-export_type([error_reason/0]).

-export([generate/3]).

-export_type([state/0]).


-type state() ::
        #{name := binary(),
          function :=
            #{parameters :=
                #{query := [parameter()],
                  path := [parameter()],
                  header := [parameter()],
                  cookie := [parameter()]},
              http_verb := atom(),
              http_host := binary(),
              http_path := binary(),
              http_path_arg := binary()},
          types :=
            #{request => binary(),
              query => binary(),
              header => binary(),
              cookie => binary(),
              body => binary(),
              response => binary()}}.

-type parameter() ::
        #{pascal_name := binary(),
          snake_name := binary(),
          real_name := binary(),
          style := atom(),
          explode := boolean()}.


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
  write_and_format_file(OutDir, <<PackageName/binary, "_model.erl">>, F2),
  write_and_format_file(OutDir, <<PackageName/binary, "_jsv.erl">>, F3),
  write_and_format_file(OutDir, <<PackageName/binary, "_client.erl">>, F4),
  ok.

-spec write_and_format_file(binary(), binary(), iodata()) -> ok.
write_and_format_file(OutDir, Filename0, Content) ->
  Filename = binary_to_list(filename:join(OutDir, Filename0)),
  State = rebar3_formatter:new(default_formatter,
                               #{output_dir => OutDir, action => format},
                               undefined),
  ok = file:write_file(Filename, Content),
  rebar3_formatter:format_file(Filename, State),
  ok.

generate_client_file(Datetime, PackageName, Spec, Options) ->
  Paths = openapi:paths(Spec),
  Data = #{datetime => Datetime,
           package_name => <<PackageName/binary, "_client">>,
           functions => generate_client_functions(Paths, PackageName, Options)},
  openapi_mustache:render(<<"erlang-client/client.erl">>, Data, #{disable_html_escaping => true}).

generate_client_functions(Paths, PackageName, Options) ->
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
              CookieParameters = openapi_parameter:cookies(Parameters),
              Responses = openapi_operation:responses(OperationObject),

              RequestBody = openapi_operation:request_body(OperationObject),

              TReqBody =
                lists:join(
                  " | ",
                  lists:map(
                    fun ({_ContentType, MediaType}) ->
                        Schema = openapi_media_type:schema(MediaType),
                        schema_to_typespec(Schema, #{})
                    end, openapi_request_body:contents(RequestBody))),

              FuncName = openapi_code:snake_case(Id),
              Contents0 =
                maps:fold(
                  fun (_, V, X) ->
                      case maps:find(content, V) of
                        {ok, Contents} ->
                          maps:fold(
                            fun
                              (_, V2, X2) ->
                                case maps:find(schema, V2) of
                                  error ->
                                    X2;
                                  {ok, V3} ->
                                    [V3 | X2]
                                end
                            end, X, Contents);
                        error ->
                          X
                      end
                  end, [], Responses),

              F =
                fun (X) ->
                    Op = case openapi_parameter:required(X) of
                           true -> " := ";
                           false -> " => "
                         end,
                    Name = openapi_parameter:name(X),
                    KeyName = openapi_code:snake_case(Name),
                    Schema = maps:get(schema, X),
                    [KeyName, Op, schema_to_typespec(Schema, #{})]
                end,

              BodyOp =
                case openapi_request_body:required(RequestBody) of
                  true ->
                    " := ";
                  false ->
                    " => "
                end,

              ParameterOp =
                fun (P) ->
                    case lists:any(fun openapi_parameter:required/1, P) of
                      true ->
                        " := ";
                      false ->
                        " => "
                    end
                end,

              TRequest =
                ["#{",
                 "query", ParameterOp(QueryParameters), FuncName, "_request_query(),",
                 "header", ParameterOp(HeaderParameters), FuncName, "_request_header(),",
                 "cookie", ParameterOp(CookieParameters), FuncName, "_request_cookie(),",
                 "path", ParameterOp(PathParameters), FuncName, "_request_path(),",
                 "body", BodyOp, "{binary(), ", FuncName, "_request_body()}}"],

              ResponseType =
                lists:join(
                  " | ",
                  lists:map(
                    fun (V) ->
                        schema_to_typespec(V, #{namespace => <<PackageName/binary, "_model">>})
                    end, Contents0)),

              MaybeMap =
                fun ([]) -> "map()";
                    (P) -> ["#{", lists:join(",", lists:map(F, P)), "}"]
                end,
              BuildParameter =
                fun (#{name := PName} = ParameterObject) ->
                    #{pascal_name => openapi_code:pascal_case(PName),
                      real_name => PName,
                      style => maps:get(style, ParameterObject, form),
                      explode => maps:get(explode, ParameterObject, false)}
                end,
              Binary = fun unicode:characters_to_binary/1,

              State =
                #{name => FuncName,
                  function =>
                    #{http_verb => Verb,
                      http_host => maps:get(default_host, Options, <<>>),
                      http_path => Binary(PathFormat),
                      http_path_args =>
                        Binary(["[",
                                lists:join(
                                  ",",
                                  lists:map(
                                    fun (Name) ->
                                        ["Var", openapi_code:pascal_case(Name)]
                                    end, VariablesNames)), "]"]),
                      parameters =>
                        #{query => lists:map(BuildParameter, QueryParameters),
                          path => lists:map(BuildParameter, PathParameters),
                          header => lists:map(BuildParameter, HeaderParameters),
                          cookie => lists:map(BuildParameter, CookieParameters)}},
                  types =>
                    #{request => Binary(TRequest),
                      query => Binary(MaybeMap(QueryParameters)),
                      header => Binary(MaybeMap(HeaderParameters)),
                      cookie => Binary(MaybeMap(CookieParameters)),
                      path => Binary(MaybeMap(PathParameters)),
                      body => Binary(TReqBody),
                      response => Binary(ResponseType)}},

              State1 =
                State#{responses =>
                         maps:fold(
                           fun
                             (StatusCode, ResponseObject, Acc2) ->
                               Content = maps:get(content, ResponseObject, #{}),
                               Value =
                                 maps:fold(
                                   fun (MediaType, MediaTypeObject, Acc3) ->
                                       Schema = maps:get(schema, MediaTypeObject, #{}),
                                       {ok, MT} = mhttp_media_type:parse(MediaType),
                                       [#{media_type => MT,
                                          jsv_schema =>
                                            unicode:characters_to_binary(
                                              io_lib:format("~p",
                                              [schema_to_jsv(Schema,
                                                            #{namespace => PackageName})]))} | Acc3]
                                   end, [], Content),

                               [#{status => StatusCode, media_types => Value} | Acc2]
                           end, [], maps:without([<<"default">>], Responses))},

              State2 =
                State1#{default_response =>
                          maps:fold(
                            fun (_, ResponseObject, Acc5) ->
                                Contents = maps:get(content, ResponseObject, #{}),
                                maps:fold(
                                  fun (MediaType, MediaTypeObject, Acc6) ->
                                      Schema = maps:get(schema, MediaTypeObject, #{}),
                                      {ok, MT} = mhttp_media_type:parse(MediaType),
                                      [#{media_type => MT,
                                         jsv_schema =>
                                           unicode:characters_to_binary(
                                             io_lib:format("~p",
                                             [schema_to_jsv(Schema,
                                                           #{namespace => PackageName})]))} | Acc6]
                                  end, Acc5, Contents)
                            end, [], maps:with([<<"default">>], Responses))},

              State2
          end, PathOperations) ++ Acc
    end, [], Paths).


generate_jsv_file(Now, PackageName, Spec, Options) ->
  DefaultContext =
    #{datetime => Now,
      package_name => <<PackageName/binary, "_jsv">>},
  Context =
    maps:merge(DefaultContext, jsv_mustache_context(Spec, Options)),
  openapi_mustache:render(<<"erlang-client/jsv.erl">>, Context).

-spec jsv_mustache_context(openapi:specification(), map()) -> map().
jsv_mustache_context(Specification, Options) ->
  Components = openapi:components(Specification),
  Schemas = openapi_components:schema(Components),

  #{definitions => jsv_definitions(Schemas, Options),
    catalog => jsv_catalog(Schemas, Options)}.

-spec jsv_definitions(Schemas, Options) -> Context
          when Schemas :: #{binary() := openapi:schema()},
               Options :: map(),
               Context :: [#{name := binary(),
                             definition := jsv:definition()}].
jsv_definitions(Schemas, Options) ->
  lists:reverse(
    maps:fold(
      fun (Key, Schema, Acc) ->
          Context =
            #{name => openapi_code:snake_case(Key),
              definition => schema_to_jsv(Schema, Options)},

          [Context | Acc]
      end, [], Schemas)).

-spec jsv_catalog(Schemas, Options) -> Context
          when Schemas :: #{binary() := openapi:schema()},
               Options :: map(),
               Context :: #{name := binary(), definition := binary()}.
jsv_catalog(Schemas, Options) ->
  F =
    fun (Key, _, Acc) ->
        Name = openapi_code:snake_case(Key),
        [[Name, " => ", Name, "_definition()"] | Acc]
    end,
  PackageName = maps:get(package_name, Options),
  KV = maps:fold(F, [], Schemas),
  Map = ["#{", lists:join(",", KV), "}"],
  #{name => openapi_code:snake_case(PackageName),
    definition => unicode:characters_to_binary(Map)}.

-spec schema_to_jsv(openapi:schema(), map()) -> jsv:definition().
schema_to_jsv(#{type := number, nullable := true}, _) ->
  {one_of, [number, null]};
schema_to_jsv(#{type := number}, _) ->
  number;
schema_to_jsv(#{type := integer, nullable := true}, _) ->
  {one_of, [integer, null]};
schema_to_jsv(#{type := integer}, _) ->
  integer;
schema_to_jsv(#{type := boolean, nullable := true}, _) ->
  {one_of, [boolean, null]};
schema_to_jsv(#{type := boolean}, _) ->
  boolean;
schema_to_jsv(#{type := string, enum := Values} = Schema, _) ->
  Definition =
    {string,
     #{values => lists:map(fun binary_to_atom/1, Values)}},

  case openapi_schema:nullable(Schema) of
    true ->
      {one_of, [Definition, null]};
    false ->
      Definition
  end;
schema_to_jsv(#{type := string, nullable := true}, _) ->
  {one_of, [string, null]};
schema_to_jsv(#{type := string}, _) ->
  string;
schema_to_jsv(#{'$ref' := Ref}, Options) ->
  RefName = binary_to_atom(openapi_code:snake_case(lists:last(Ref))),
  case maps:find(namespace, Options) of
    {ok, Namespace} ->
      {ref, binary_to_atom(Namespace), RefName};
    error ->
      {ref, RefName}
  end;
schema_to_jsv(#{type := array} = Schema, Options) ->
  case maps:find(items, Schema) of
    {ok, ItemSchemas} ->
      {array,
       #{element => schema_to_jsv(ItemSchemas, Options)}};
    error ->
      array
  end;
schema_to_jsv(#{anyOf := Schemas, nullable := true}, Options) ->
  Definitions =
    lists:map(fun (X) ->
                  schema_to_jsv(X, Options)
              end, Schemas),
  {one_of, Definitions ++ [null]};
schema_to_jsv(#{anyOf := Schemas}, Options) ->
  Definitions =
    lists:map(fun (X) ->
                  schema_to_jsv(X, Options)
              end, Schemas),
  {one_of, Definitions};
schema_to_jsv(#{type := object} = Schema, Options) ->
  Definition =
    #{members =>
        maps:fold(fun (Key, Value, Acc) ->
                      Definition = schema_to_jsv(Value, Options),
                      Name = binary_to_atom(Key),
                      Acc#{Name => Definition}
                  end, #{}, openapi_schema:properties(Schema)),
      required =>
        lists:map(fun (Key) ->
                      binary_to_atom(Key)
                  end, openapi_schema:required(Schema))},

  %% Additionalproperties cannot currently be handle by JSV.
  %% We use 'keep' option to handle the additionalproperties.
  %%
  %% Definition1 =
  %%   case openapi_schema:additional_properties(Schema) of
  %%     true ->
  %%       Definition;
  %%     false ->
  %%       Definition;
  %%     AdditionalProperties ->
  %%       throw({mdr}),
  %%       Definition#{value => schema_to_jsv(AdditionalProperties, Options)}
  %%   end,

  case openapi_schema:nullable(Schema) of
    true ->
      {one_of, [{object, Definition}, null]};
    false ->
      {object, Definition}
  end;
schema_to_jsv(_, _) ->
  any.

generate_model_file(Datetime, PackageName, Spec, Options) ->
  Components = openapi:components(Spec),
  Schemas = openapi_components:schema(Components),

  Data = #{datetime => Datetime,
           package_name => <<PackageName/binary, "_model">>,
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
           value => unicode:characters_to_binary(schema_to_typespec(Schema, #{}))},
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


-spec schema_to_typespec(openapi:schema(), map()) -> iodata().
schema_to_typespec(#{type := object, properties := Props} = Schema, Options) ->
  Required = openapi_schema:required(Schema),
  F = fun (Name, Schema2, Acc) ->
          Operator =
            case lists:member(Name, Required) of
              true -> " := ";
              false -> " => "
            end,
          [[$', Name, $', Operator, schema_to_typespec(Schema2, Options)] | Acc]
      end,
  AdditionalType =
    case openapi_schema:additional_properties(Schema) of
      true ->
        ["_ := json:value()"];
      false ->
        [];
      AdditionalSchema ->
        [["_ := ", schema_to_typespec(AdditionalSchema, Options)]]
      end,
  ["#{",
   lists:join(",", maps:fold(F, [], Props) ++ AdditionalType),
   "}"];
schema_to_typespec(#{type := object, nullable := true}, _) ->
  "json:value() | null";
schema_to_typespec(#{type := object}, _) ->
  "json:value()";
schema_to_typespec(#{type := integer, nullable := true}, _) ->
  "integer() | null";
schema_to_typespec(#{type := integer}, _) ->
  "integer()";
schema_to_typespec(#{type := number, nullable := true}, _) ->
  "number() | null";
schema_to_typespec(#{type := number}, _) ->
  "number()";
schema_to_typespec(#{type := boolean, nullable := true}, _) ->
  "boolean() | null";
schema_to_typespec(#{type := boolean}, _) ->
  "boolean()";
schema_to_typespec(#{type := array, nullable := true} = Schema, Options) ->
  case maps:find(items, Schema) of
    {ok, ItemSchema} ->
      [$[, schema_to_typespec(ItemSchema, Options), $], " | null"];
    error ->
      "list() | null"
  end;
schema_to_typespec(#{type := array} = Schema, Options) ->
  case maps:find(items, Schema) of
    {ok, ItemSchema} ->
      [$[, schema_to_typespec(ItemSchema, Options), $]];
    error ->
      "list()"
  end;
schema_to_typespec(#{type := string, enum := Enum, nullable := true}, _) ->
  lists:join(" | ", lists:map(fun (X) -> [$', X, $'] end, Enum ++ ["null"]));
schema_to_typespec(#{type := string, enum := Enum}, _) ->
  lists:join(" | ", lists:map(fun (X) -> [$', X, $'] end, Enum));
schema_to_typespec(#{type := string, nullable := true}, _) ->
  "binary() | null";
schema_to_typespec(#{type := string}, _) ->
  "binary()";
schema_to_typespec(#{anyOf := Schemas}, Options) ->
  lists:join(" | ", lists:map(fun (X) -> schema_to_typespec(X, Options) end, Schemas));
schema_to_typespec(#{'$ref' := Ref}, Options) ->
  case maps:find(namespace, Options) of
    {ok, Namespace} ->
      [Namespace, ":", openapi_code:snake_case(lists:last(Ref)), "()"];
    error ->
      [openapi_code:snake_case(lists:last(Ref)), "()"]
  end;
schema_to_typespec(_, _) ->
  "json:value()".

generate_openapi_file(Datetime, PackageName, Specification, _Options) ->
  Info = openapi:info(Specification),
  Servers = openapi:servers(Specification),

  Context =
    #{datetime => Datetime,
      package_name => <<PackageName/binary, "_openapi">>,
      info => Info,
      servers => Servers},
  openapi_mustache:render(<<"erlang-client/openapi.erl">>, Context).
