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

-module(openapi_jsv).

-export([catalog/0, type_map/0]).

-spec type_map() -> jsv:type_map().
type_map() ->
  maps:merge(jsv:default_type_map(),
             #{reference => openapi_jsv_reference}).

-spec catalog() -> jsv:catalog().
catalog() ->
  #{specification => specification_definition(),
    info => info_definition(),
    contact => contact_definition(),
    license => license_definition(),
    server => server_definition(),
    server_variable => server_variable_definition(),
    component => component_definition(),
    path => path_definition(),
    operation => operation_definition(),
    external_documentation => external_documentation_definition(),
    parameter => parameter_definition(),
    style => style_definition(),
    request_body => request_body_definition(),
    media_type => media_type_definition(),
    encoding => encoding_definition(),
    response => response_definition(),
    callback => callback_definition(),
    example => example_definition(),
    link => link_definition(),
    header => header_definition(),
    tag => tag_definition(),
    reference => reference_definition(),
    schema => schema_definition(),
    discriminator => discriminator_definition(),
    xml => xml_definition(),
    security_scheme => security_scheme_definition(),
    oauth_flows => oauth_flows_definition(),
    oauth_flow => oauth_flow_definition(),
    security_requirement => security_requirement_definition()}.

-spec specification_definition() -> jsv:definition().
specification_definition() ->
  {object,
   #{members =>
       #{openapi => {string, #{values => ['3.0.0', '3.0.1', '3.0.2', '3.0.3']}},
         info => {ref, info},
         servers => {array, #{element => {ref, server}}},
         paths => {object, #{value => {ref, path}}},
         components => {ref, component},
         security => {array, #{element => {ref, security_requirement}}},
         tags => {array, #{element => {ref, tag}}},
         externalDocs => {ref, external_documentation}},
     required =>
       [openapi, info, paths]}}.

-spec info_definition() -> jsv:definition().
info_definition() ->
  {object,
   #{members =>
       #{title => string,
         description => string,
         termsOfService => string,
         contact => {ref, contact},
         license => {ref, contact},
         version => string},
     required =>
       [title, version]}}.

-spec contact_definition() -> jsv:definition().
contact_definition() ->
  {object,
   #{members =>
       #{name => string,
         url => uri,
         email => email_address}}}.

-spec license_definition() -> jsv:definition().
license_definition() ->
  {object,
   #{members =>
       #{name => string,
         url => uri},
     required =>
       [name]}}.

-spec server_definition() -> jsv:definition().
server_definition() ->
  {object,
   #{members =>
       #{url => uri,
         description => string,
         variables => {object, #{value => {ref, server_variable}}}},
     required =>
       [url]}}.

-spec server_variable_definition() -> jsv:definition().
server_variable_definition() ->
  {object,
   #{members =>
       #{enum => {array, #{element => string, min_length => 1}},
         default => string,
         description => string},
     required =>
       [default]}}.

-spec component_definition() -> jsv:definition().
component_definition() ->
  {object,
   #{members =>
       #{schemas =>
           {object,
            #{value =>
                {one_of, [{ref, reference}, {ref, schema}]}}},
         responses =>
           {object,
            #{value =>
                {one_of, [{ref, reference}, {ref, response}]}}},
         parameters =>
           {object,
            #{value =>
                {one_of, [{ref, reference}, {ref, parameter}]}}},
         examples =>
           {object,
            #{value =>
                {one_of, [{ref, reference}, {ref, example}]}}},
         requestBodies =>
           {object,
            #{value =>
                {one_of, [{ref, reference}, {ref, request_body}]}}},
         headers =>
           {object,
            #{value =>
                {one_of, [{ref, reference}, {ref, header}]}}},
          securitySchemes =>
           {object,
            #{value =>
                {one_of, [{ref, reference}, {ref, security_scheme}]}}},
         links =>
           {object,
            #{value =>
                {one_of, [{ref, reference}, {ref, link}]}}},
         callbacks =>
           {object,
            #{value =>
                {one_of, [{ref, reference}, {ref, callback}]}}}}}}.

-spec path_definition() -> jsv:definition().
path_definition() ->
  {object,
   #{members =>
       #{'$ref' => reference,
         summary => string,
         description => string,
         get => {ref, operation},
         put => {ref, operation},
         post => {ref, operation},
         delete => {ref, operation},
         options => {ref, operation},
         head => {ref, operation},
         patch => {ref, operation},
         trace => {ref, operation},
         servers => {array, #{element => {ref, server}}},
         parameters =>
           {array,
            #{element =>
                {one_of, [{ref, reference}, {ref, parameter}]}}}}}}.

-spec operation_definition() -> jsv:definition().
operation_definition() ->
  {object,
   #{members =>
       #{tags => {array, #{element => string}},
         summary => string,
         description => string,
         externalDocs => {ref, external_documentation},
         operationId => string,
         parameters =>
           {array,
            #{element =>
                {one_of, [{ref, reference}, {ref, parameter}]}}},
         requestBody =>
           {one_of, [{ref, reference}, {ref, request_body}]},
         responses =>
           {object,
            #{value =>
                {one_of, [{ref, reference}, {ref, response}]}}},
         callbacks =>
           {object,
            #{value =>
                {one_of, [{ref, reference}, {ref, callback}]}}},
         deprecated => boolean,
         security =>
           {array, #{element => {ref, security_requirement}}},
         server =>
           {array, #{element => {ref, server}}}},
     required =>
       [responses]}}.

-spec external_documentation_definition() -> jsv:definition().
external_documentation_definition() ->
  {object,
   #{members =>
      #{description => string,
         url => uri},
     required =>
       [url]}}.

-spec parameter_definition() -> jsv:definition().
parameter_definition() ->
  {object,
   #{members =>
       #{name => string,
         in => {string, #{values => [query, header, path, cookie]}},
         description => string,
         required => boolean,
         deprecated => boolean,
         allowEmptyValue => boolean,
         style => {ref, style},
         explode => boolean,
         allowReserved => boolean,
         schema => {one_of, [{ref, reference}, {ref, schema}]},
         example => any,
         examples =>
           {object,
            #{value =>
                {one_of, [{ref, reference}, {ref, example}]}}},
         content =>
           {object,
            #{value =>
                {ref, media_type}}}},
     required =>
       [name, in]}}.

-spec style_definition() -> jsv:definition().
style_definition() ->
  {string, #{values => [matrix, label, form, simple, spaceDelimited,
                        pipeDelimited, deepObject]}}.

-spec request_body_definition() -> jsv:definition().
request_body_definition() ->
  {object,
   #{members =>
       #{description => string,
         content =>
           {object,
            #{value =>
                {ref, media_type}}},
         required => boolean},
     required =>
       [content]}}.

-spec media_type_definition() -> jsv:definition().
media_type_definition() ->
  {object,
   #{members =>
       #{schema => {one_of, [{ref, reference}, {ref, schema}]},
         example => any,
         examples =>
           {object,
            #{value =>
                {one_of, [{ref, reference}, {ref, example}]}}},
         encoding =>
           {object,
            #{value =>
                {ref, encoding}}}}}}.

-spec encoding_definition() -> jsv:definition().
encoding_definition() ->
  {object,
   #{members =>
       #{contentType => string,
         header =>
           {object,
            #{value =>
                {one_of, [{ref, reference}, {ref, header}]}}},
         style => string,
         explode => boolean,
         allowReserved => boolean}}}.

-spec response_definition() -> jsv:definition().
response_definition() ->
  {object,
   #{members =>
       #{description => string,
         headers =>
           {object,
            #{value => {one_of, [{ref, reference}, {ref, header}]}}},
         content =>
           {object,
            #{value => {ref, media_type}}},
         links =>
           {object,
            #{value => {one_of, [{ref, reference}, {ref, link}]}}}}}}.

-spec callback_definition() -> jsv:definition().
callback_definition() ->
  {object,
   #{value => {ref, path}}}.

-spec example_definition() -> jsv:definition().
example_definition() ->
  {object,
   #{members =>
       #{summary => string,
         description => string,
         value => any,
         externalValue => string}}}.

-spec link_definition() -> jsv:definition().
link_definition() ->
  {object,
   #{members =>
       #{operationRef => reference,
         operationId => string,
         parameters =>
           {object,
            #{value =>
                {one_of, [any, string]}}},
         requestBody => {one_of, [any, string]},
         description => string,
         server => {ref, server}}}}.

-spec header_definition() -> jsv:definition().
header_definition() ->
  {object,
   #{members =>
       #{name => string,
         in => {string, #{values => [query, header, path, cookie]}},
         description => string,
         required => boolean,
         deprecated => boolean,
         allowEmptyValue => boolean,
         style => {ref, style},
         explode => boolean,
         allowReserved => boolean,
         schema => {one_of, [{ref, reference}, {ref, schema}]},
         example => any,
         examples =>
           {object,
            #{value =>
                {one_of, [{ref, reference}, {ref, example}]}}},
         content =>
           {object,
            #{value =>
                {ref, media_type}}}}}}.

-spec tag_definition() -> jsv:definition().
tag_definition() ->
  {object,
   #{members =>
       #{name => string,
         description => string,
         externalDocs => {ref, external_documentation}},
     required =>
       [name]}}.

-spec reference_definition() -> jsv:definition().
reference_definition() ->
  {object,
   #{members =>
       #{'$ref' => reference},
     required =>
       ['$ref']}}.

-spec schema_definition() -> jsv:definition().
schema_definition() ->
  {object,
   #{members =>
       #{title => string,
         multipleOf => {number, #{min => 1}},
         maximum => number,
         exclusiveMaximum => boolean,
         minimum => number,
         exclusiveMinimum => boolean,
         maxLength => {number, #{min => 0}},
         minLength => {number, #{min => 0}},
         pattern => string,
         maxItems => {number, #{min => 0}},
         minItems => {number, #{min => 0}},
         uniqueItems => boolean,
         maxProperties => {number, #{min => 0}},
         minProperties => {number, #{min => 0}},
         required => {array, #{element => string, min_length => 1}},
         enum =>
           {array,
            #{element => any, unique_elements => true, min_length => 1}},
         type => {string, #{values => [object, string, integer, number, array, boolean, '$ref']}},
         allOf =>
           {array,
            #{element =>
                {one_of, [{ref, reference}, {ref, schema}]}}},
         oneOf =>
           {array,
            #{element =>
                {one_of, [{ref, reference}, {ref, schema}]}}},
         anyOf =>
           {array,
            #{element =>
                {one_of, [{ref, reference}, {ref, schema}]}}},
         'not' => {one_of, [{ref, reference}, {ref, schema}]},
         items => {one_of, [{ref, reference}, {ref, schema}]},
         properties =>
           {object,
            #{value =>
                {one_of, [{ref, reference}, {ref, schema}]}}},
         description => string,
         format => string,
         default => any,
         nullable => boolean,
         discriminator => {ref, discriminator},
         readOnly => boolean,
         writeOnly => boolean,
         xml => {ref, xml},
         externalDocs => {ref, external_documentation},
         example => any,
         deprecated => boolean}}}.

-spec discriminator_definition() -> jsv:definition().
discriminator_definition() ->
  {object,
   #{members =>
       #{propertyName => string,
         mapping =>
           {object, #{value => string}}},
     required =>
       [propertyName]}}.

-spec xml_definition() -> jsv:definition().
xml_definition() ->
  {object,
   #{members =>
       #{name => string,
         namespace => string,
         prefix => string,
         attribute => string,
         wrapped => boolean}}}.

-spec security_scheme_definition() -> jsv:definition().
security_scheme_definition() ->
  {object,
   #{members =>
       #{type => string,
         description => string,
         name => string,
         in => {string, #{values => [query, header, cookie]}},
         scheme => string,
         bearerFormat => string,
         flows => {ref, oauth_flows},
         openIdConnectUrl => string},
     required =>
       [type]}}.

-spec oauth_flows_definition() -> jsv:definition().
oauth_flows_definition() ->
  {object,
   #{members =>
       #{implicit => {ref, oauth_flow},
         password => {ref, oauth_flow},
         clientCredentials => {ref, oauth_flow},
         authorizationCode => {ref, oauth_flow}}}}.

-spec oauth_flow_definition() -> jsv:definition().
oauth_flow_definition() ->
  {object,
   #{members =>
       #{authorizationUrl => uri,
         tokenUrl => uri,
         refreshUrl => uri,
         scopes => {object, #{value => string}}},
     required =>
       [authorizationUrl, tokenUrl, scopes]}}.

-spec security_requirement_definition() -> jsv:definition().
security_requirement_definition() ->
  {object,
   #{value =>
       {array,
        #{element => string}}}}.
