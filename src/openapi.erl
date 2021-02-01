-module(openapi).

-export([definition/2,
         generate_model_data/1, generate_model_data/2, generate_model_file/3,
         generate_jsv_data/1, generate_jsv_data/2, generate_jsv_file/3]).

-export_type([error_reason/0,
              specification/0,
              info/0, contact/0, license/0,
              path/0, operation/0, schema/0, schema_type/0, xml/0,
              parameter/0, parameter_location/0, parameter_type/0,
              parameter_collection_format/0,
              items/0, items_type/0, items_collection_format/0,
              response/0, header/0, header_type/0,
              security/0, security_scheme/0, security_scheme_location/0,
              oauth2_flow/0, security_requirement/0, tag/0,
              external_documentation/0, ref/0]).

-type error_reason() ::
        {file_error, term(), file:name_all()}
      | {invalid_json_data, json:error()}
      | {invalid_specification, [jsv:value_error()]}
      | {invalid_unicode_data, unicode:chardata()}
      | {incomplete_unicode_data, unicode:chardata()}.

-type specification() ::
        #{info := info(),
          host => binary(),
          basePath => binary(),
          schemes => [binary()],
          consumes => [binary()],
          produces => [binary()],
          paths => #{binary() := path()},
          definitions => #{binary() := schema()},
          parameters => #{binary() := parameter()},
          responses := #{binary() := response()},
          securityDefinitions => security(),
          security => [security_requirement()],
          tags => [tag()],
          externalDocs => external_documentation()}.

-type info() ::
        #{title := binary(),
          description => binary(),
          termsOfService => binary(),
          contact => contact(),
          license => license(),
          version := binary()}.

-type contact() ::
        #{name => binary(),
          url => binary(),
          email => binary()}.

-type license() ::
        #{name => binary(),
          url => binary()}.

-type path() ::
        #{'$ref' => ref(),
          get => operation(),
          put => operation(),
          post => operation(),
          delete => operation(),
          options => operation(),
          head => operation(),
          patch => operation(),
          parameters => [parameter()]}.

-type operation() ::
        #{tags => [binary()],
          summary => binary(),
          description => binary(),
          externalDocs => external_documentation(),
          operationId => binary(),
          consumes => [binary()],
          produces => [binary()],
          parameters => [parameter()],
          responses := #{binary() := response()},
          schemes => [binary()],
          deprecated => boolean(),
          security => [security_requirement()]}.

-type schema() ::
        #{'$ref' => ref(),
          discriminator => binary(),
          readOnly => boolean(),
          xml => xml(),
          externalDocs => external_documentation(),
          example => json:value(),
          type => schema_type() | [schema_type()],
          format => binary(),
          title => binary(),
          description => binary(),
          default => json:value(),
          maximum => number(),
          exclusiveMaximum => number(),
          minimum => number(),
          exclusiveMinimum => number(),
          maxLength => integer(),
          minLength => integer(),
          pattern => binary(),
          maxItems => integer(),
          minItems => integer(),
          uniqueItems => boolean(),
          enum => [json:value()],
          multipleOf => number(),
          required => [binary()],
          items => schema() | [schema()],
          allOf => [schema()],
          properties => #{binary() := schema()},
          additionalProperties => boolean() | schema()}.

-type schema_type() ::
        null | string | number | integer | boolean | array | object.

-type xml() ::
        #{name => binary(),
         namespace => binary(),
         prefix => binary(),
         attribute => boolean(),
         wrapped => boolean()}.

-type parameter() ::
        #{'$ref' => ref(),
          name := binary(),
          in := parameter_location(),
          description => binary(),
          required => boolean(),
          schema => schema(),
          type => parameter_type(),
          format => binary(),
          allow_empty_value => boolean(),
          items => items(),
          collection_format => parameter_collection_format(),
          default => json:value(),
          maximum => number(),
          exclusiveMaximum => number(),
          minimum => number(),
          exclusiveMinimum => number(),
          maxLength => integer(),
          minLength => integer(),
          pattern => binary(),
          maxItems => integer(),
          minItems => integer(),
          uniqueItems => boolean(),
          enum => [json:value()],
          multipleOf => number()}.

-type parameter_location() ::
        query | header | path | formData | body.

-type parameter_type() ::
        string | number | integer | boolean | array | file.

-type parameter_collection_format() ::
        csv | ssv | tsv | pipes | multi.

-type items() ::
        #{type := items_type(),
          format => binary(),
          items => items(),
          collection_format => items_collection_format(),
          default => json:value(),
          maximum => number(),
          exclusiveMaximum => number(),
          minimum => number(),
          exclusiveMinimum => number(),
          maxLength => integer(),
          minLength => integer(),
          pattern => binary(),
          maxItems => integer(),
          minItems => integer(),
          uniqueItems => boolean(),
          enum => [json:value()],
          multipleOf => number()}.

-type items_type() ::
        string | number | integer | boolean | array.

-type items_collection_format() ::
        csv | ssv | tsv | pipes.

-type response() ::
        #{'$ref' => ref(),
          description := binary(),
          schema => schema(),
          headers => #{binary() := header()},
          examples => #{binary() := json:value()}}.

-type header() ::
        #{description => binary(),
          type := header_type(),
          format => binary(),
          items => items(),
          collection_format => items_collection_format(),
          default => json:value(),
          maximum => number(),
          exclusiveMaximum => number(),
          minimum => number(),
          exclusiveMinimum => number(),
          maxLength => integer(),
          minLength => integer(),
          pattern => binary(),
          maxItems => integer(),
          minItems => integer(),
          uniqueItems => boolean(),
          enum => [json:value()],
          multipleOf => number()}.

-type header_type() ::
        string | number | integer | boolean | array.

-type security() ::
        #{binary() := security_scheme()}.

-type security_scheme() ::
        #{type := binary(),
          description => binary(),
          name => binary(),
          in => security_scheme_location(),
          flow => oauth2_flow(),
          authorizationUrl => binary(),
          tokenUrl => binary(),
          scopes => [binary()]}.

-type security_scheme_location() ::
        query | header.

-type oauth2_flow() ::
        implicit | password | application | accessCode.

-type security_requirement() ::
        #{binary() := [binary()]}.

-type tag() ::
        #{name := binary(),
          description => binary(),
          externalDocs => external_documentation()}.

-type external_documentation() ::
        #{description => binary(),
          url := binary()}.

-type ref() ::
        json_pointer:pointer() | {URI :: binary(), json_pointer:pointer()}.

-spec definition(binary(), specification()) -> {ok, schema()} | error.
definition(Name, #{definitions := Definitions}) ->
  maps:find(Name, Definitions).

-spec generate_model_data(Input :: file:name_all()) ->
        {ok, iodata()} | {error, openapi:error_reason()}.
generate_model_data(Input) ->
  generate_model_data(Input, #{}).

-spec generate_model_data(Input :: file:name_all(), openapi_gen:options()) ->
        {ok, iodata()} | {error, openapi:error_reason()}.
generate_model_data(Input, Options) ->
  case openapi_spec:read_file(Input) of
    {ok, Spec} ->
      openapi_model_gen:generate(Spec, Options);
    {error, Reason} ->
      {error, Reason}
  end.

-spec generate_model_file(Input :: file:name_all(), Output :: file:name_all(),
                          openapi_gen:options()) ->
        ok | {error, openapi:error_reason()}.
generate_model_file(Input, Output, Options) ->
  case generate_model_data(Input, Options) of
    {ok, Data} ->
      case file:write_file(Output, Data) of
        ok ->
          ok;
        {error, Reason} ->
          {error, {file_error, Reason, Output}}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec generate_jsv_data(Input :: file:name_all()) ->
        {ok, iodata()} | {error, openapi:error_reason()}.
generate_jsv_data(Input) ->
  generate_jsv_data(Input, #{}).

-spec generate_jsv_data(Input :: file:name_all(), openapi_gen:options()) ->
        {ok, iodata()} | {error, openapi:error_reason()}.
generate_jsv_data(Input, Options) ->
  case openapi_spec:read_file(Input) of
    {ok, Spec} ->
      openapi_jsv_gen:generate(Spec, Options);
    {error, Reason} ->
      {error, Reason}
  end.

-spec generate_jsv_file(Input :: file:name_all(), Output :: file:name_all(),
                        openapi_gen:options()) ->
        ok | {error, openapi:error_reason()}.
generate_jsv_file(Input, Output, Options) ->
  case generate_jsv_data(Input, Options) of
    {ok, Data} ->
      case file:write_file(Output, Data) of
        ok ->
          ok;
        {error, Reason} ->
          {error, {file_error, Reason, Output}}
      end;
    {error, Reason} ->
      {error, Reason}
  end.
