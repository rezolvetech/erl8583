% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%% @author CA Meijer
%% @copyright 2011 CA Meijer
%% @doc This module will provide a function to get how a field is
%%      encoded according to the 2003 version of the ISO 8583 
%%      specification. 
-module(erl8583_fields_2003).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include("erl8583_field_ids.hrl").
-include("erl8583_types.hrl").

%%
%% Exported Functions
%%
-export([get_encoding/1]).

%%
%% API Functions
%%

%% @doc Returns how a field is encoded as a triple consisting of the content (e.g. ans, b, z, etc),
%%      the format (e.g. llvar, lllvar or fixed) and the maximum length.
%%
%% @spec get_encoding(integer()) -> field_encoding()
-spec(get_encoding(integer()) -> field_encoding()).

get_encoding(_FieldId) ->
	throw("No support yet for the 2003 version of ISO 8583.").
