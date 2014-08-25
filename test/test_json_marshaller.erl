-module(test_json_marshaller).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("erl8583_marshallers.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%
marshal_init_test() ->
	Msg = erl8583_message:new(),
	{[], Msg} = erl8583_marshaller_json:marshal_init(Msg).

marshal_string_test() ->
	MarshalledField = erl8583_marshaller_json:marshal_field(2, "0123456789", ?MODULE),
	", \"2\" : \"0123456789\"" = MarshalledField.
	
marshal_binary_test() ->
	MarshalledField = erl8583_marshaller_json:marshal_field(64, <<0,1,16,255>>, ?MODULE),
	", \"64\" : \"000110FF\"" = MarshalledField.

marshal_message_1_test() ->
	FieldValue = erl8583_message:new(),
	MarshalledField = erl8583_marshaller_json:marshal_field(63, FieldValue, ?MODULE),
	", \"63\" : {}" = MarshalledField.

marshal_message_2_test() ->
	FieldValue1 = erl8583_message:new(),
	FieldValue2 = erl8583_message:set(1, "hello", FieldValue1),
	MarshalledField = erl8583_marshaller_json:marshal_field(62, FieldValue2, ?MODULE),
	", \"62\" : {\"1\" : \"hello\"}" = MarshalledField.

marshal_message_3_test() ->
	FieldValue1 = erl8583_message:new(),
	FieldValue2 = erl8583_message:set(1, "hello", FieldValue1),
	FieldValue3 = erl8583_message:set(2, <<5>>, FieldValue2),
	MarshalledField = erl8583_marshaller_json:marshal_field(62, FieldValue3, ?MODULE),
	", \"62\" : {\"1\" : \"hello\", \"2\" : \"05\"}" = MarshalledField.

marshal_message_4_test() ->
	FieldValue1 = erl8583_message:new(),
	FieldValue2 = erl8583_message:set(1, "hello", FieldValue1),
	FieldValue3 = erl8583_message:set(3, <<5>>, FieldValue2),
	FieldValue4 = erl8583_message:set(2, erl8583_message:new(), FieldValue3),
	MarshalledField = erl8583_marshaller_json:marshal_field(62, FieldValue4, ?MODULE),
	", \"62\" : {\"1\" : \"hello\", \"2\" : {}, \"3\" : \"05\"}" = MarshalledField.

marshal_message_5_test() ->
	FieldValue1 = erl8583_message:new(),
	FieldValue2 = erl8583_message:set(1, "hello", FieldValue1),
	FieldValue3 = erl8583_message:set(3, <<5>>, FieldValue2),
	FieldValue5 = erl8583_message:set([2, 7], "123", FieldValue3),
	MarshalledField = erl8583_marshaller_json:marshal_field(62, FieldValue5, ?MODULE),
	", \"62\" : {\"1\" : \"hello\", \"2\" : {\"7\" : \"123\"}, \"3\" : \"05\"}" = MarshalledField.

marshal_unmarshal_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set_mti("0200", Message1),
	Message3 = erl8583_message:set(2, "1234567890123456", Message2),
	Message4 = erl8583_message:set(64, <<1,2,3,4,5,6,7,8>>, Message3),
	Marshalled = erl8583_marshaller_json:marshal(Message4),
	Message4 = erl8583_marshaller_json:unmarshal(Marshalled).

marshal_unmarshal_complex_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set_mti("0200", Message1),
	Message3 = erl8583_message:set(2, "1234567890123456", Message2),
	Message4 = erl8583_message:set(64, <<1,2,3,4,5,6,7,8>>, Message3),
	Message5 = erl8583_message:set(63, erl8583_message:new(), Message4),
	Marshalled = erl8583_marshaller_json:marshal(Message5),
	Message5 = erl8583_marshaller_json:unmarshal(Marshalled).

marshal_end_test() ->
	Message1 = erl8583_message:new(),
	"{\"fields\" : {}}" = erl8583_marshaller_json:marshal_end(Message1, "").

marshal_end_attrs_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set_attributes([{"foo", "1"}, {"bar", "2"}], Message1),
	"{\"fields\" : {}, \"attributes\" : {\"foo\" : \"1\", \"bar\" : \"2\"}}" = erl8583_marshaller_json:marshal_end(Message2, "").

marshal_unmarshal_attrs_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set(0, "0200", Message1),
	Message3 = erl8583_message:set(2, "12345678", Message2),
	Message4 = erl8583_message:set_attributes([{"foo", "1"}, {"bar", "2"}], Message3),
	Marshalled = erl8583_marshaller_json:marshal(Message4),
	Message4 = erl8583_marshaller_json:unmarshal(Marshalled).


%%
%% Local Functions
%%

