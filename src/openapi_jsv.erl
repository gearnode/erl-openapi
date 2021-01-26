-module(openapi_jsv).

-export([catalog/0]).

-spec catalog() -> jsv:catalog().
catalog() ->
  #{specification => specification_definition(),
    info => info_definition(),
    contact => contact_definition(),
    license => license_definition(),
    path => path_definition(),
    operation => operation_definition(),
    schema => schema_definition(),
    parameters => parameters_definition(),
    parameter => parameter_definition(),
    responses => responses_definition(),
    response => response_definition(),
    examples => examples_definition(),
    headers => headers_definition(),
    header => header_definition(),
    security => security_definition(),
    security_scheme => security_scheme_definition(),
    security_requirement => security_requirement_definition(),
    tag => tag_definition(),
    external_documentation => external_documentation_definition()}.

-spec specification_definition() -> jsv:definition().
specification_definition() ->
  {object,
   #{members =>
       #{swagger => {string, #{values => ['2.0']}},
         info => {ref, info},
         host => string,
         basePath => string,
         schemes => {array, #{element => string}},
         consumes => {array, #{element => string}},
         produces => {array, #{element => string}},
         paths => {object, #{value => {ref, path}}},
         definitions => {object, #{value => {ref, schema}}},
         parameters => {ref, parameters},
         responses => {ref, responses},
         securityDefinitions => {ref, security},
         security => {array, #{element => {ref, security_requirement}}},
         tags => {array, #{element => {ref, tag}}},
         externalDocs => {ref, external_documentation}},
     required =>
       [swagger, info, paths]}}.

-spec info_definition() -> jsv:definition().
info_definition() ->
  {object,
   #{members =>
       #{title => string,
         description => string,
         termsOfService => string,
         contact => {ref, contact},
         license => {ref, license},
         version => string},
     required =>
       [title, version]}}.

-spec contact_definition() -> jsv:definition().
contact_definition() ->
  {object,
   #{members =>
       #{name => string,
         url => uri,
         email => string}}}.

-spec license_definition() -> jsv:definition().
license_definition() ->
  {object,
   #{members =>
       #{name => string,
         url => uri}}}.

-spec path_definition() -> jsv:definition().
path_definition() ->
  {object,
   #{members =>
       #{'$ref' => uri,
         get => {ref, operation},
         put => {ref, operation},
         post => {ref, operation},
         delete => {ref, operation},
         options => {ref, operation},
         head => {ref, operation},
         patch => {ref, operation},
         parameters => {array, #{element => {ref, parameter}}}},
     required =>
       []}}.

-spec operation_definition() -> jsv:definition().
operation_definition() ->
  {object,
   #{members =>
       #{tags => {array, #{element => string}},
         summary => string,
         description => string,
         externalDocs => {ref, external_documentation},
         operationId => string,
         consumes => {array, #{element => string}},
         produces => {array, #{element => string}},
         parameters => {array, #{element => {ref, parameter}}},
         responses => {ref, responses},
         schemes => {array, #{element => string}},
         deprecated => boolean,
         security => {array, #{element => {ref, security_requirement}}}},
     required =>
       [responses]}}.

-spec schema_definition() -> jsv:definition().
schema_definition() ->
  %% TODO add missing member definitions
  {object,
   #{members =>
       #{'$ref' => uri,
         discriminator => string,
         readOnly => boolean,
         format => string,
         externalDocs => {ref, external_documentation},
         example => any},
     required =>
       []}}.

-spec parameters_definition() -> jsv:definition().
parameters_definition() ->
  {object,
   #{value => {ref, parameter}}}.

-spec parameter_definition() -> jsv:definition().
parameter_definition() ->
  %% TODO add missing member definitions
  {object,
   #{members =>
       #{'$ref' => uri,
         name => string,
         in => {string, #{values => [query, header, path, formData, body]}},
         description => string,
         required => boolean,
         type => {string,
                  #{values => [string, number, integer, boolean, array, file]}},
         format => string,
         allowEmptyValue => boolean,
         collectionFormat => {string,
                              #{values => [csv, ssv, tsv, pipes, multi]}},
         default => any,
         uniqueItems => boolean},
     required =>
       [name, in]}}.

-spec responses_definition() -> jsv:definition().
responses_definition() ->
  {object,
   #{members =>
       #{'$ref' => uri,
         default => {ref, response}},
     value =>
       {ref, response}}}.

-spec response_definition() -> jsv:definition().
response_definition() ->
  {object,
   #{members =>
       #{'$ref' => uri,
         description => string,
         schema => {ref, schema},
         headers => {ref, headers},
         examples => {ref, examples}},
     required =>
       [description]}}.

-spec examples_definition() -> jsv:definition().
examples_definition() ->
  {object,
   #{value => any}}.

-spec headers_definition() -> jsv:definition().
headers_definition() ->
  {object,
   #{value => {ref, header}}}.

-spec header_definition() -> jsv:definition().
header_definition() ->
  %% TODO add missing member definitions
  {object,
   #{members =>
       #{description => string,
         type => {string, #{values => [string, number, integer, boolean,
                                       array]}},
         format => string,
         default => string},
     required =>
       [type]}}.

-spec security_definition() -> jsv:definition().
security_definition() ->
  {object,
   #{value => {ref, security_scheme}}}.

-spec security_scheme_definition() -> jsv:definition().
security_scheme_definition() ->
  %% OpenAPI mandates that various fields (name, in, flow, authorizationUrl,
  %% tokenUrl, scopes) are required. In practice, lots of specifications do
  %% not include them (e.g. the Kubernetes API specification).
  {object,
   #{members =>
       #{type => string,
         description => string,
         name => string,
         in => {string, #{values => [query, header]}},
         flow => {string, #{values => [implicit, password, application,
                                       accessCode]}},
         authorizationUrl => uri,
         tokenUrl => uri,
         scopes => {object, #{value => string}}},
     required =>
       [type]}}.

-spec security_requirement_definition() -> jsv:definition().
security_requirement_definition() ->
  {object,
   #{value => {array, #{element => string}}}}.

-spec tag_definition() -> jsv:definition().
tag_definition() ->
  {object,
   #{members =>
       #{name => string,
         description => string,
         externalDocs => {ref, external_documentation}},
     required =>
       [name]}}.

-spec external_documentation_definition() -> jsv:definition().
external_documentation_definition() ->
  {object,
   #{members =>
       #{description => string,
         url => uri},
     required =>
       [uri]}}.
