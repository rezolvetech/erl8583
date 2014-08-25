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
%% @doc This module provides a function to get how a field is
%%      encoded according to the 1993 version of the ISO 8583 
%%      specification. 
-module(erl8583_fields_1993).

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
%%      the format (e.g. llvar, lllvar or fixed) and the maximum length. The field can be passed as
%%      an integer (e.g. 127 to determine how to encode field 127) or a list of integers (e.g. [127,2] to
%%      determine how to encode subfield 127.2)
%%
%% @spec get_encoding(FieldId::integer()|list(integer())) -> field_encoding()
-spec(get_encoding(integer()|list(integer())) -> field_encoding()).

get_encoding(?DATE_AND_TIME_LOCAL_TRAN) ->
	{n, fixed, 12};
get_encoding(?DATE_EFF) ->
	{n, fixed, 4};
get_encoding(?DATE_SETTLE) ->
	{n, fixed, 6};
get_encoding(?POS_DATA_CODE) ->
	{an, fixed, 12};
get_encoding(?FUNCTION_CODE) ->
	{n, fixed, 3};
get_encoding(?MESSAGE_REASON_CODE) ->
	{n, fixed, 4};
get_encoding(?CARD_ACCEPTOR_BUSINESS_CODE) ->
	{n, fixed, 4};
get_encoding(?AMOUNT_ORIGINAL) ->
	{n, fixed, 24};
get_encoding(?ACQUIRER_REFERENCE_DATA) ->
	{n, llvar, 48};
get_encoding(?RESP_CODE) ->
	{an, fixed, 3};
get_encoding(?CARD_ACCEPTOR_NAME_LOCATION) ->
	{an, llvar, 99};
get_encoding(?SECURITY_RELATED_CONTROL_INFO) ->
	{an, llvar, 8};
get_encoding(?ICC_SYSTEM_RELATED_DATA) ->
	{ans, lllvar, 999};
get_encoding(?ORIGINAL_DATA_ELEMENTS_1993) ->
	{an, llvar, 35};
get_encoding(?MESSAGE_SECURITY_CODE) ->
	{an, llvar, 18};
get_encoding([FieldId]) when is_integer(FieldId) ->
	get_encoding(FieldId);
get_encoding(FieldId) ->
	erl8583_fields:get_encoding(FieldId).
