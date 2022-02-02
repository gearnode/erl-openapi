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

-module(openapi_v2_jsv).

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
    schema_type => schema_type_definition(),
    xml => xml_definition(),
    parameter => parameter_definition(),
    items => items_definition(),
    response => response_definition(),
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
         parameters => {object, #{value => {ref, parameter}}},
         responses => {object, #{value => {ref, response}}},
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
       #{'$ref' => reference,
         get => {ref, operation},
         put => {ref, operation},
         post => {ref, operation},
         delete => {ref, operation},
         options => {ref, operation},
         head => {ref, operation},
         patch => {ref, operation},
         parameters => {array, #{element => {ref, parameter}}}}}}.

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
         responses => {object, #{value => {ref, response}}},
         schemes => {array, #{element => string}},
         deprecated => boolean,
         security => {array, #{element => {ref, security_requirement}}}},
     required =>
       [responses]}}.

-spec schema_definition() -> jsv:definition().
schema_definition() ->
  {object,
   #{members =>
       #{'$ref' => reference,
         discriminator => string,
         readOnly => boolean,
         xml => {ref, xml},
         externalDocs => {ref, external_documentation},
         example => any,
         type => {one_of, [{ref, schema_type},
                           {array, #{element => {ref, schema_type}}}]},
         format => string,
         title => string,
         description => string,
         default => any,
         maximum => number,
         exclusiveMaximum => number,
         minimum => number,
         exclusiveMinimum => number,
         maxLength => integer,
         minLength => integer,
         pattern => string,
         maxItems => integer,
         minItems => integer,
         uniqueItems => boolean,
         enum => array,
         multipleOf => number,
         required => {array, #{element => string}},
         items => {one_of, [{ref, schema},
                            {array, #{element => {ref, schema}}}]},
         allOf => {array, #{element => {ref, schema},
                            min_length => 1}},
         properties => {object, #{value => {ref, schema}}},
         additionalProperties => {one_of, [boolean, {ref, schema}]}},
     required =>
       []}}.

-spec schema_type_definition() -> jsv:definition().
schema_type_definition() ->
  {string,
   #{values =>
       [null, string, number, integer, boolean, array, object]}}.

-spec xml_definition() -> jsv:definition().
xml_definition() ->
  {object,
   #{members =>
       #{name => string,
         namespace => string,
         prefix => string,
         attribute => boolean,
         wrapped => boolean}}}.

-spec parameter_definition() -> jsv:definition().
parameter_definition() ->
  {object,
   #{members =>
       #{'$ref' => reference,
         name => string,
         in => {string, #{values => [query, header, path, formData, body]}},
         description => string,
         required => boolean,
         schema => {ref, schema},
         type => {string,
                  #{values => [string, number, integer, boolean, array,
                               file]}},
         format => string,
         allowEmptyValue => boolean,
         items => {ref, items},
         collectionFormat => {string,
                              #{values => [csv, ssv, tsv, pipes, multi]}},
         default => any,
         maximum => number,
         exclusiveMaximum => number,
         minimum => number,
         exclusiveMinimum => number,
         maxLength => integer,
         minLength => integer,
         pattern => string,
         maxItems => integer,
         minItems => integer,
         uniqueItems => boolean,
         enum => array,
         multipleOf => number},
     required =>
       [name, in]}}.

-spec items_definition() -> jsv:definition().
items_definition() ->
  {object,
   #{members =>
       #{type => {string,
                  #{values => [string, number, integer, boolean, array]}},
         format => string,
         items => {ref, items},
         collectionFormat => {string, #{values => [csv, ssv, tsv, pipes]}},
         default => any,
         maximum => number,
         exclusiveMaximum => number,
         minimum => number,
         exclusiveMinimum => number,
         maxLength => integer,
         minLength => integer,
         pattern => string,
         maxItems => integer,
         minItems => integer,
         uniqueItems => boolean,
         enum => array,
         multipleOf => number},
     required =>
       [type]}}.

-spec response_definition() -> jsv:definition().
response_definition() ->
  {object,
   #{members =>
       #{'$ref' => reference,
         description => string,
         schema => {ref, schema},
         headers => {object, #{value => {ref, header}}},
         examples => object},
     required =>
       [description]}}.

-spec header_definition() -> jsv:definition().
header_definition() ->
  {object,
   #{members =>
       #{description => string,
         type => {string, #{values => [string, number, integer, boolean,
                                       array]}},
         format => string,
         items => {ref, items},
         collectionFormat => {string,
                              #{values => [csv, ssv, tsv, pipes]}},
         default => any,
         maximum => number,
         exclusiveMaximum => number,
         minimum => number,
         exclusiveMinimum => number,
         maxLength => integer,
         minLength => integer,
         pattern => string,
         maxItems => integer,
         minItems => integer,
         uniqueItems => boolean,
         enum => array,
         multipleOf => number},
     required =>
       [type]}}.

-spec security_definition() -> jsv:definition().
security_definition() ->
  {object,
   #{value => {ref, security_scheme}}}.

-spec security_scheme_definition() -> jsv:definition().
security_scheme_definition() ->
  {object,
   #{members =>
       #{type => string,
         description => string,
         name => string,
         in => {string, #{values => [query, header]}},
         flow => {string,
                  #{values => [implicit, password, application, accessCode]}},
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
       [url]}}.
