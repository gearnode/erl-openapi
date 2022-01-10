%% Copyright (c) 2021 Exograd SAS.
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

-module(openapi_v3).

-export([definition/2,
         generate/3,
         generate_data/2, generate_data/3]).

-export_type([error_reason/0,
              specification/0,
              info/0,
              contact/0,
              license/0,
              server/0,
              server_variable/0,
              components/0,
              path/0,
              operation/0,
              external_documentation/0,
              parameter/0,
              parameter_location/0,
              parameter_style/0,
              request_body/0,
              media_type/0,
              encoding/0,
              response/0,
              callback/0,
              example/0,
              links/0,
              tag/0,
              ref/0,
              schema/0,
              discriminator/0,
              xml/0,
              security_scheme/0,
              security_scheme_location/0,
              oauth2_flows/0,
              oauth2_flow/0,
              security_requirement/0]).

-type error_reason() ::
        {file_error, term(), file:name_all()}
      | {invalid_json_data, json:error()}
      | {invalid_specification, [jsv:value_error()]}
      | {invalid_unicode_data, unicode:chardata()}
      | {incomplete_unicode_data, unicode:chardata()}
      | {invalid_schema_ref, binary()}
      | {invalid_response_status, binary()}.

-type specification() ::
        #{info := info(),
          server => [server()],
          paths := #{binary() => path()},
          components => components(),
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
        #{name := binary(),
          url => binary()}.

-type server() ::
        #{url := binary(),
          description => binary(),
          variables => #{binary() := server_variable()}}.

-type server_variable() ::
        #{enum => [binary()],
          default := binary(),
          description => binary()}.

-type components() ::
        #{schemas => #{binary() := schema() | ref()},
          responses => #{binary() := response() | ref()},
          parameters => #{binary() := parameter() | ref()},
          examples => #{binary() := example() | ref()},
          requestBodies => #{binary() := request_body() | ref()},
          headers => #{binary() := header() | ref()},
          securitySchemes =>
            #{binary() := security_scheme() | ref()},
          links => #{binary() := links() | ref()},
          callbacks => #{binary() := callback() | ref()}}.

-type header() ::
        #{description => binary(),
          required => boolean(),
          deprecated => boolean(),
          allowEmptyValue => boolean(),
          style => binary(),
          explode => boolean(),
          allowReserved => boolean(),
          schema => schema(),
          example => json:value(),
          examples => #{binary() := example() | ref()},
          content => #{binary() := media_type()},
          '$ref' => ref()}.

-type path() ::
        #{'$ref' => ref(),
          summary => binary(),
          description => binary(),
          get => operation(),
          put => operation(),
          post => operation(),
          delete => operation(),
          options => operation(),
          head => operation(),
          patch => operation(),
          trace => operation(),
          servers => [server()],
          parameters => [parameter() | ref()]}.

-type operation() ::
        #{tags => [binary()],
          summary => binary(),
          description => binary(),
          externalDocs => external_documentation(),
          operationId => binary(),
          parameters => [parameter() | ref()],
          requestBody => request_body() | ref(),
          responses := response() | ref(),
          callbacks => #{binary() := callback() | ref()},
          deprecated => boolean(),
          security => [security_requirement()],
          server => [server()]}.

-type external_documentation() ::
        #{description => binary(),
          url := binary()}.

-type parameter() ::
        #{name := binary(),
          in := parameter_location(),
          description => binary(),
          required := boolean(),
          deprecated => boolean(),
          allowEmptyValue => boolean(),
          style => parameter_style(),
          explode => boolean(),
          allowReserved => boolean(),
          schema => schema(),
          example => json:value(),
          examples => #{binary() := example() | ref()},
          content => #{binary() := media_type()}}.

-type parameter_location() ::
        query | header | path | cookie.

-type parameter_style() ::
        matrix
      | label
      | form
      | simple
      | spaceDelimited
      | pipeDelimited
      | deepObject.

-type request_body() ::
        #{description => binary(),
          content := #{binary() := media_type()},
          required => boolean()}.

-type media_type() ::
        #{schema => schema() | ref(),
          example => json:value(),
          examples => #{binary() := example() | ref()},
          encoding => #{binary() := encoding()}}.

-type encoding() ::
        #{contentType => binary(),
          headers => #{binary() := header() | ref()},
          style => binary(),
          explode => boolean(),
          allowReserved => boolean()}.

-type response() ::
        #{description => binary(),
         headers => #{binary() := header() | ref()},
         content => #{binary() := media_type()},
         links => #{binary() := links() | ref()}}.

-type callback() ::
        #{binary() := path()}.

-type example() ::
        #{summary => binary(),
          description => binary(),
          value => term(),
          externalValue => binary()}.

-type links() ::
        #{operationRef => binary(),
          operationId => binary(),
          parameters => #{binary() := term()},
          requestBody => term(),
          description => binary(),
          server => server()}.

-type tag() ::
        #{name := binary(),
          description => binary(),
          externalDocs => external_documentation()}.

-type ref() ::
        #{'$ref' := json_pointer:pointer() | {URI :: binary(), json_pointer:pointer()}}.


-type schema() ::
        #{title => binary(),
          multipleOf => pos_integer(),
          maximum => number(),
          exclusiveMaximum =>  boolean(),
          minimum => number(),
          exclusiveMinimum => boolean(),
          maxLength => non_neg_integer(),
          minLength => non_neg_integer(),
          pattern => binary(),
          maxItems => non_neg_integer(),
          minItems => non_neg_integer(),
          uniqueItems => boolean(),
          maxProperties => non_neg_integer(),
          minProperties => non_neg_integer(),
          required => nonempty_list(binary()),
          enum => nonempty_list(json:value()),

          type => binary(),
          allOf => [schema()],
          oneOf => [schema()],
          anyOf => [schema()],
          'not' => schema(),
          items => schema(),
          properties => #{binary := schema()},
          additionalProperties => boolean() | schema(),
          description => binary(),
          format => binary(),
          default => json:value(),

          nullable => boolean(),
          discriminator => discriminator(),
          readOnly => boolean(),
          writeOnly => boolean(),
          xml => xml(),
          externalDocs => external_documentation(),
          example => json:value(),
          deprecated => boolean()}.

-type discriminator() ::
        #{propertyName := binary(),
           mapping => #{binary() := binary()}}.

-type xml() ::
        #{name => binary(),
         namespace => binary(),
         prefix => binary(),
         attribute => boolean(),
         wrapped => boolean()}.

-type security_scheme() ::
        #{type := binary(),
          description => binary(),
          name => binary(),
          in => security_scheme_location(),
          scheme => binary(),
          bearerFormat => binary(),
          flows => oauth2_flows(),
          openIdConnectUrl => binary()}.

-type security_scheme_location() ::
        query | header | cookie.

-type oauth2_flows() ::
        #{implicit => oauth2_flow(),
          password => oauth2_flow(),
          clientCredentials => oauth2_flow(),
          authorizationCode => oauth2_flow()}.

-type oauth2_flow() ::
        #{authorizationUrl := binary(),
          tokenUrl := binary(),
          refreshUrl => binary(),
          scopes := #{binary() := binary()}}.

-type security_requirement() ::
        #{binary() := [binary()]}.

-spec definition(binary(), specification()) -> {ok, schema()} | error.
definition(Name, #{definitions := Definitions}) ->
  maps:find(Name, Definitions).

-spec generate(Input :: file:name_all(), OutputDir :: file:name_all(),
               openapi_v3_gen:options()) ->
        ok | {error, openapi_v3:error_reason()}.
generate(Input, OutputDir, Options) ->
  case openapi_v3_spec:read_file(Input) of
    {ok, Spec} ->
      Mods = [openapi_v3_gen_model,
              openapi_v3_gen_jsv],
      Fun = fun
              F([]) ->
                ok;
              F([H | T]) ->
                case H:generate(Spec, Options) of
                  {ok, Data} ->
                    Output0 = [OutputDir, $/, H:module_name(Options), ".erl"],
                    Output = list_to_binary(Output0),
                    case file:write_file(Output, Data) of
                      ok ->
                        F(T);
                      {error, Reason} ->
                        {error, {file_error, Reason, Output}}
                    end;
                  {error, Reason} ->
                    {error, Reason}
                end
            end,
      Fun(Mods);
    {error, Reason} ->
      {error, Reason}
  end.

-spec generate_data(module(), Input :: file:name_all()) ->
        {ok, iodata()} | {error, openapi_v3:error_reason()}.
generate_data(Module, Input) ->
  generate_data(Module, Input, #{}).

-spec generate_data(module(), Input :: file:name_all(),
                    openapi_v3_gen:options()) ->
        {ok, iodata()} | {error, openapi_v3:error_reason()}.
generate_data(Module, Input, Options) ->
  case openapi_v3_spec:read_file(Input) of
    {ok, Spec} ->
      Module:generate(Spec, Options);
    {error, Reason} ->
      {error, Reason}
  end.
