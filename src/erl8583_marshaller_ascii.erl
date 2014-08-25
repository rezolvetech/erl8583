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
%% @doc This module provides functions to marshal an iso8583message() 
%%      into an ASCII string, to unmarshal an ASCII string into an
%%      iso8583message(). There are also functions to marshal/unmarshal the MTI, bitmap
%%      and fields of an ISO 8583 message.
%%      
%%      The functions to unmarshal the MTI, bitmap or a 
%%      field of a message assume that the component
%%      to be unmarshalled is at the start of the string. The
%%      returned value is a 2-tuple containing the unmarshalled value
%%      and the remainder of the string that needs to be unmarshalled.
-module(erl8583_marshaller_ascii).

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
		 marshal_bitmap/1,
		 unmarshal_bitmap/1,
		 marshal_field/3, 
		 unmarshal_field/3,
		 marshal_mti/1, 
		 unmarshal_mti/1,
		 marshal_end/2,
		 unmarshal_end/2]).

%%
%% API Functions
%%

%% @doc Constructs an ASCII string representation of
%%      an iso8583message().
%%
%% @spec marshal(iso8583message()) -> string()
-spec(marshal(iso8583message()) -> string()).

marshal(Message) ->
	erl8583_marshaller:marshal(Message, ?MARSHALLER_ASCII).
	
%% @doc Constructs an iso8583message() from an ASCII string 
%%      marshalling of the message.
%%
%% @spec unmarshal(string()) -> iso8583message()
-spec(unmarshal(string()) -> iso8583message()).

unmarshal(Marshalled) ->
	erl8583_marshaller:unmarshal(Marshalled, ?MARSHALLER_ASCII).

%% @doc Constructs an ASCII string representation of the
%%      bitmap for an iso8583message().
%%
%% @spec marshal_bitmap(list(integer())) -> string()
-spec(marshal_bitmap(list(integer())) -> string()).

marshal_bitmap(Message) ->
	FieldIds = erl8583_message:get_fields(Message) -- [0],
	case lists:max(FieldIds) > 64 of
		true ->
			Fields = [1] ++ FieldIds,
			SecondaryBitmap = erl8583_convert:list_to_bitmap(Fields, 64),
			UpdatedMessage = erl8583_message:set(1, SecondaryBitmap, Message);
		false ->
			Fields = FieldIds,
			UpdatedMessage = Message
	end,
	{erl8583_convert:binary_to_ascii_hex(erl8583_convert:list_to_bitmap(Fields, 0)), UpdatedMessage}.

%% @doc Extracts a list of field IDs from an ASCII string 
%%      representation of an ISO 8583 message. The result is returned
%%      as a 2-tuple of the field IDs and the remainder of the 
%%      the message (encoding the field values but not the bit map).
%%
%% @spec unmarshal_bitmap(string()) -> {list(integer()), string()}
-spec(unmarshal_bitmap(string()) -> {list(integer()), string()}).

unmarshal_bitmap(AsciiMessage) ->
	{AsciiBitmap, Fields} = lists:split(16, AsciiMessage),
  request_logger:debug("Bitmap",AsciiBitmap),
  Bitmap = erl8583_convert:ascii_hex_to_binary(AsciiBitmap),
  request_logger:debug("Binary bitmap",Bitmap),
  {erl8583_convert:bitmap_to_list(Bitmap, 0), Fields}.

%% @doc Marshals a field value into an ASCII string using a specified
%%      encoding rules module.
%%
%% @spec marshal_field(integer(), iso8583field_value(), module()) -> string()
-spec(marshal_field(integer(), iso8583field_value(), module()) -> string()).

marshal_field(FieldId, FieldValue, EncodingRules) ->
	Pattern = EncodingRules:get_encoding(FieldId),
	marshal_data_element(Pattern, FieldValue).

%% @doc Extracts a field value from the start of a string.  The field value,
%%      the rest of the unmarshalled string and a list of additional field IDs 
%%      that need to be unmarshalled is returned as a 3-tuple.
%%      A module that specifies how the field is encoded must be passed
%%      as an argument.
%%
%% @spec unmarshal_field(integer(), string(), module()) -> {iso8583field_value(), string(), list(integer())}
-spec(unmarshal_field(integer(), string(), module()) -> {iso8583field_value(), string(), list(integer())}).

unmarshal_field(1, AsciiFields, _EncodingRules) ->
	{Value, Rest} = unmarshal_data_element({b, fixed, 64}, AsciiFields),
	{Value, Rest, erl8583_convert:bitmap_to_list(Value, 64)};
unmarshal_field(FieldId, AsciiFields, EncodingRules) ->
  request_logger:debug("Unmarshall data field"),
  request_logger:debug("Field id",FieldId),
  Pattern = EncodingRules:get_encoding(FieldId),
  request_logger:debug("Encoding rules",Pattern),
	{FieldValue, MarshalledRest} = unmarshal_data_element(Pattern, AsciiFields),
  request_logger:debug("Field value",FieldValue),
  {FieldValue, MarshalledRest, []}.

%% @doc Marshals the MTI into an ASCII string.
%%
%% @spec marshal_mti(string()) -> string()
-spec(marshal_mti(string()) -> string()).

marshal_mti(Mti) ->
	marshal_field(0, Mti, erl8583_fields).

%% @doc Extracts the MTI from the start of a string.  The MTI 
%%      and the rest of the unmarshalled string is returned as a 2-tuple.
%%
%% @spec unmarshal_mti(string()) -> {string(), string()}
-spec(unmarshal_mti(string()) -> {string(), string()}).

unmarshal_mti(Marshalled) ->
	{Mti, Rest, []} = unmarshal_field(0, Marshalled, erl8583_fields),
	{Mti, Rest}.

%% @doc Completes the marshalling of a message and returns the marshalled
%%      form.
%%
%% @spec marshal_end(iso8583message(), string()) -> string()
-spec(marshal_end(iso8583message(), string()) -> string()).

marshal_end(_Message, Marshalled) ->
	Marshalled.

%% @doc Completes the unmarshalling of a message and returns the
%%      message.
%%
%% @spec unmarshal_end(iso8583message(), Marshalled::string()) -> iso8583message()
-spec(unmarshal_end(iso8583message(), Marshalled::string()) -> iso8583message()).

unmarshal_end(Message, []) ->
	erl8583_message:remove_fields([1], Message).

%%
%% Local Functions
%%
marshal_data_element({n, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:integer_to_string(length(FieldValue), 2) ++ FieldValue;
marshal_data_element({n, lllvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:integer_to_string(length(FieldValue), 3) ++ FieldValue;
marshal_data_element({ns, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:integer_to_string(length(FieldValue), 2) ++ FieldValue;
marshal_data_element({an, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:integer_to_string(length(FieldValue), 2) ++ FieldValue;
marshal_data_element({an, lllvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:integer_to_string(length(FieldValue), 3) ++ FieldValue;
marshal_data_element({ans, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:integer_to_string(length(FieldValue), 2) ++ FieldValue;
marshal_data_element({ans, lllvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:integer_to_string(length(FieldValue), 3) ++ FieldValue;
marshal_data_element({n, fixed, Length}, FieldValue) when length(FieldValue) =< Length ->
	IntValue = list_to_integer(FieldValue),
	erl8583_convert:integer_to_string(IntValue, Length);
marshal_data_element({an, fixed, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:pad_with_trailing_spaces(FieldValue, Length);
marshal_data_element({ans, fixed, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:pad_with_trailing_spaces(FieldValue, Length);
marshal_data_element({x_n, fixed, Length}, [Head | FieldValue]) when Head =:= $C orelse Head =:= $D ->
	IntValue = list_to_integer(FieldValue),
	[Head] ++ erl8583_convert:integer_to_string(IntValue, Length);
marshal_data_element({z, llvar, Length}, FieldValue) when length(FieldValue) =< Length ->
	erl8583_convert:integer_to_string(length(FieldValue), 2) ++ FieldValue;
marshal_data_element({b, fixed, Length}, FieldValue) when size(FieldValue) =:= Length div 8 ->
	erl8583_convert:binary_to_ascii_hex(FieldValue).

unmarshal_data_element({n, llvar, _MaxLength}, AsciiFields) ->
	{N, Rest} = lists:split(2, AsciiFields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({n, lllvar, _MaxLength}, AsciiFields) ->
	{N, Rest} = lists:split(3, AsciiFields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({ns, llvar, _MaxLength}, AsciiFields) ->
	{N, Rest} = lists:split(2, AsciiFields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({an, llvar, _MaxLength}, AsciiFields) ->
	{N, Rest} = lists:split(2, AsciiFields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({an, lllvar, _MaxLength}, AsciiFields) ->
	{N, Rest} = lists:split(3, AsciiFields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({ans, llvar, _MaxLength}, AsciiFields) ->
	{N, Rest} = lists:split(2, AsciiFields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({ans, lllvar, _MaxLength}, AsciiFields) ->
	{N, Rest} = lists:split(3, AsciiFields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({n, fixed, Length}, AsciiFields) ->
	lists:split(Length, AsciiFields);
unmarshal_data_element({an, fixed, Length}, AsciiFields) ->
	lists:split(Length, AsciiFields);
unmarshal_data_element({ans, fixed, Length}, AsciiFields) ->
	lists:split(Length, AsciiFields);
unmarshal_data_element({x_n, fixed, Length}, [Head|Tail]) when Head =:= $C orelse Head =:= $D ->
	lists:split(Length+1, [Head|Tail]);
unmarshal_data_element({z, llvar, _MaxLength}, AsciiFields) ->
	{N, Rest} = lists:split(2, AsciiFields),
	lists:split(list_to_integer(N), Rest);
unmarshal_data_element({b, fixed, Length}, AsciiFields) ->
	{ValueStr, Rest} = lists:split(Length div 4, AsciiFields),
	Value = erl8583_convert:ascii_hex_to_binary(ValueStr),
	{Value, Rest}.
