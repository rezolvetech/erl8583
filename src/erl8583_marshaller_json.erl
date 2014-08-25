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
%% @doc This module marshals an iso8583message() 
%%      into a JSON document and can unmarshal a JSIBL document into
%%      an iso8583message(). This module also exposes
%%      functions for explicitly marshalling/unmarshalling
%%      the MTI, bitmap and fields of a message. This is
%%      to conform to the design of other marshallers and
%%      the contract required by the erl8583_marshaller.
-module(erl8583_marshaller_json).

%%
%% Include files
%%
%% @headerfile "../include/erl8583_types.hrl"
-include("erl8583_types.hrl").
-include("erl8583_marshallers.hrl").

%%
%% Exported Functions
%%
-export([marshal/1,
		 unmarshal/1,
		 marshal_init/1,
		 unmarshal_init/2,
		 marshal_mti/1,
		 unmarshal_mti/1,
		 marshal_bitmap/1,
		 unmarshal_bitmap/1,
		 marshal_field/3,
		 unmarshal_field/3,
		 marshal_end/2,
		 unmarshal_end/2]).

%%
%% API Functions
%%

%% @doc Constructs a JSON document of
%%      an iso8583message().
%%
%% @spec marshal(iso8583message()) -> string()
-spec(marshal(iso8583message()) -> string()).

marshal(Message) ->
	erl8583_marshaller:marshal(Message, ?MARSHALLER_JSON).

%% @doc Constructs an iso8583message() from a JSON document 
%%      representation of the message.
%%
%% @spec unmarshal(string()) -> iso8583message()
-spec(unmarshal(string()) -> iso8583message()).

unmarshal(Marshalled) ->
	erl8583_marshaller:unmarshal(Marshalled, ?MARSHALLER_JSON).

%% @doc Creates an ISO 8583 message by extracting the attributes in a JSON
%%      document and returns the message and the JSON document as 
%%      a 2-tuple. No fields of the message are populated by this function; only
%%      the attributes of the message are set.
%%
%% @spec unmarshal_init(string(), string()) -> {iso8583message(), string()}
-spec(unmarshal_init(string(), string()) -> {iso8583message(), string()}).

unmarshal_init(Message, Marshalled) ->
	{struct, JsonData} = mochijson2:decode(Marshalled),
	case proplists:is_defined(<<"attributes">>, JsonData) of
		false ->
			{Message, Marshalled};
		true ->
			{struct, Attributes} = proplists:get_value(<<"attributes">>, JsonData),
			Keys = proplists:get_keys(Attributes),
			Attrs =[{binary_to_list(Key), binary_to_list(proplists:get_value(Key, Attributes))} || Key <- Keys],
			{erl8583_message:set_attributes(Attrs, Message), Marshalled}
	end.

%% @doc Starts the marshalling of a message and returns the
%%      initial marshalled data and message as a 2-tuple.
%%
%% @spec marshal_init(iso8583message()) -> {string(), iso8583message()}
-spec(marshal_init(iso8583message()) -> {string(), iso8583message()}).

marshal_init(Message) ->
	{[], Message}.


%% @doc Extracts the MTI from a JSON document.
%%      The MTI and the JSON document are 
%%      returned as a 2-tuple.
%%
%% @spec unmarshal_mti(string()) -> {string(), string()}
-spec(unmarshal_mti(string()) -> {string(), string()}).

unmarshal_mti(Marshalled) ->
	{struct, JsonData} = mochijson2:decode(Marshalled),
	{struct, FieldsData} = proplists:get_value(<<"fields">>, JsonData),
	MtiBin = proplists:get_value(<<"0">>, FieldsData),
	{binary_to_list(MtiBin), Marshalled}.

%% @doc Marshals the MTI into a JSON field.
%%
%% @spec marshal_mti(string()) -> string()
-spec(marshal_mti(string()) -> string()).

marshal_mti(Mti) ->
	encode_id(0) ++ " : " ++ encode_value(Mti).

%% @doc Extracts a list of field IDs from a JSON 
%%      representation of an ISO 8583 message. The result is returned
%%      as a 2-tuple of the field IDs and the JSON document.
%%
%% @spec unmarshal_bitmap(string()) -> {list(integer()), string()}
-spec(unmarshal_bitmap(string()) -> {list(integer()), string()}).

unmarshal_bitmap(Marshalled) ->
	{struct, JsonData} = mochijson2:decode(Marshalled),
	{struct, FieldsData} = proplists:get_value(<<"fields">>, JsonData),
	Fields = proplists:get_keys(FieldsData),
	FieldIds = [list_to_integer(binary_to_list(Id)) || Id <- Fields] -- [0],
	{lists:sort(FieldIds), Marshalled}.

%% @doc Returns an empty string and the message as a 2-tuple.
%%
%% @spec marshal_bitmap(list(integer())) -> string()
-spec(marshal_bitmap(list(integer())) -> string()).

marshal_bitmap(Message) ->
	{[], Message}.

%% @doc Extracts a field value from a JSON document.  The field value,
%%      the document and an empty list is returned as a 3-tuple.
%%      A module that specifies how the field is encoded must be passed
%%      as an argument.
%%
%% @spec unmarshal_field(integer(), string(), module()) -> {iso8583field_value(), string(), list(integer())}
-spec(unmarshal_field(integer(), string(), module()) -> {iso8583field_value(), string(), list(integer())}).

unmarshal_field(FieldId, Marshalled, EncodingRules) ->
	{struct, JsonData} = mochijson2:decode(Marshalled),
	{struct, FieldsProps} = proplists:get_value(<<"fields">>, JsonData),
	FieldValue = proplists:get_value(list_to_binary(integer_to_list(FieldId)), FieldsProps),
	case FieldValue of
		{struct, PropList} ->
			Value = unmarshal_complex_field([FieldId], erl8583_message:new(), PropList, EncodingRules);
		_ ->
			Value = unmarshal_simple_field(FieldId, FieldValue, EncodingRules)
	end,
	{Value, Marshalled, []}.

%% @doc Marshals a field value into a JSON field. The
%%      encoding rules module argument is ignored.
%%
%% @spec marshal_field(integer(), iso8583field_value(), module()) -> string()
-spec(marshal_field(integer(), iso8583field_value(), module()) -> string()).

marshal_field(FieldId, FieldValue, _EncodingRules) when is_list(FieldValue) ->
	", " ++ encode_id(FieldId) ++ " : " ++ encode_value(FieldValue);
marshal_field(FieldId, FieldValue, _EncodingRules) when is_binary(FieldValue) ->
	", " ++ encode_id(FieldId) ++ " : " ++ encode_value(FieldValue);
marshal_field(FieldId, FieldValue, _EncodingRules) ->
	true = erl8583_message:is_message(FieldValue),
	KeyValueList = erl8583_message:to_list(FieldValue),
	", " ++ encode_id(FieldId) ++ " : " ++ "{" ++ encode_values(KeyValueList, []) ++ "}".


%% @doc Finishes the unmarshalling of a message and returns the
%%      message.
%%
%% @spec unmarshal_end(iso8583message(), string()) -> iso8583message()
-spec(unmarshal_end(iso8583message(), string()) -> iso8583message()).

unmarshal_end(Message, _Marshalled) ->
	Message.

%% @doc Wraps the marshalled fields into an enclosing element
%%      and returns the marshalled result.
%%
%% @spec marshal_end(iso8583message(), string()) -> string()
-spec(marshal_end(iso8583message(), string()) -> string()).

marshal_end(Message, Marshalled) ->
	"{\"fields\" : {" ++ Marshalled ++ "}" ++ marshal_attributes(Message) ++ "}".

%%
%% Local Functions
%%
unmarshal_simple_field(FieldId, FieldValue, EncodingRules) ->
	case EncodingRules:get_encoding(FieldId) of
		{b, _, _} ->
			erl8583_convert:ascii_hex_to_binary(binary_to_list(FieldValue));
		_ ->
			binary_to_list(FieldValue)
	end.

unmarshal_complex_field(FieldId, Message, PropList, EncodingRules) ->
	ConstructMessageFun = fun(Id, MessageAccum) ->
								  IdInt = list_to_integer(binary_to_list(Id)),
								  UpdatedId = FieldId ++ [IdInt],
					  			  FieldValue = proplists:get_value(Id, PropList),
								  case FieldValue of
									  {struct, PropList2} ->
										  Value = unmarshal_complex_field(UpdatedId, 
																		  erl8583_message:new(),
																		  PropList2, 
																		  EncodingRules);
									  _ ->
										  Value = unmarshal_simple_field(UpdatedId, 
																		 FieldValue, 
																		 EncodingRules)
								  end,
					  			  erl8583_message:set(IdInt, Value, MessageAccum)
			  end,
	SubFieldIds = proplists:get_keys(PropList),
	lists:foldl(ConstructMessageFun, Message, SubFieldIds).

encode_values([], Result) ->
	Result;
encode_values([{Id, Value}|Tail], []) ->
	encode_values(Tail, encode_id(Id) ++ " : " ++ encode_value(Value));
encode_values([{Id, Value}|Tail], Result) ->
	encode_values(Tail,  Result ++ ", " ++ encode_id(Id) ++ " : " ++ encode_value(Value)).

encode_id(Id) ->
	"\"" ++ integer_to_list(Id) ++ "\"".

encode_value(Value) when is_list(Value) ->
	"\"" ++ Value ++ "\"";
encode_value(Value) when is_binary(Value) ->
	"\"" ++ erl8583_convert:binary_to_ascii_hex(Value) ++ "\"";
encode_value(Value) ->
	true = erl8583_message:is_message(Value),
	KeyValueList = erl8583_message:to_list(Value),
	"{" ++ encode_values(KeyValueList, []) ++ "}".

marshal_attributes(Message) ->
	Attrs = erl8583_message:get_attributes(Message),
	case Attrs of
		[] ->
			[];
		_ ->
			", \"attributes\" : {" ++ marshal_attributes_list(Attrs) ++ "}"
	end.

marshal_attributes_list([{Key, Value}]) ->
	"\"" ++ Key ++ "\" : \"" ++ Value ++ "\"";
marshal_attributes_list([{Key, Value}|Tail]) ->
	"\"" ++ Key ++ "\" : \"" ++ Value ++ "\", " ++ marshal_attributes_list(Tail).


