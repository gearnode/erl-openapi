%% Copyright (c) 2022 Exograd SAS.
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

-module(openapi_schema).

-export([additional_properties/1, required/1, properties/1]).

-spec additional_properties(openapi:schema()) -> boolean() | openapi:schema().
additional_properties(#{additionalProperties := Value}) ->
  Value;
additional_properties(_) ->
  true.

-spec required(openapi:schema()) -> [binary()].
required(#{required := Required}) ->
  Required;
required(_) ->
  [].

-spec properties(openapi:schema()) -> #{binary() := openapi:schema()}.
properties(#{properties := Properties}) ->
  Properties;
properties(_) ->
  #{}.
