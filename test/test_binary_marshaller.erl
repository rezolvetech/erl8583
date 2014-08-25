%% Author: carl
%% Created: 19 Feb 2011
%% Description: TODO: Add description to test_binary_marshaller
-module(test_binary_marshaller).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("erl8583_field_ids.hrl").
-include("erl8583_marshallers.hrl").

%%
%% Exported Functions
%%
-export([marshal_mti/1, marshal_field/3, marshal_bitmap/1]).

%%
%% API Functions
%%
marshal_field(3, 3, _EncodingRules) ->
	[3];
marshal_field(4, 4, _EncodingRules) ->
	[4];
marshal_field(0, "0200", _EncodingRules) ->
	[255];
marshal_field(FieldId, FieldValue, EncodingRules) ->
	erl8583_marshaller_binary:marshal_field(FieldId, FieldValue, EncodingRules).

marshal_bitmap(Message) ->
	{[254], Message}.

marshal_mti(Mti) ->
	marshal_field(0, Mti, erl8583_fields).

%%
%% Local Functions
%%

field_2_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0210", Msg1),
	Msg3 = erl8583_message:set(?PAN, "15234567890123456", Msg2),
	[2, 16, 64, 0, 0, 0, 0, 0, 0, 0, 23, 21, 35, 69, 103, 137, 1, 35, 69, 96] 
		= erl8583_marshaller:marshal(Msg3, ?MARSHALLER_BINARY).
	
fields_2_3_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(?PAN, "1234567890123456789", Msg2),
	Msg4 = erl8583_message:set(?PROC_CODE, "1234", Msg3),
	[2, 0, 96, 0, 0, 0, 0, 0, 0, 0, 25, 18, 52, 86, 120, 144, 18, 52, 86, 120, 144, 0, 18, 52]
		= erl8583_marshaller:marshal(Msg4, ?MARSHALLER_BINARY).

field_4_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(4, "123", Msg2),
	[2, 0, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 35]
		= erl8583_marshaller:marshal(Msg3, ?MARSHALLER_BINARY).

field_5_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0210", Msg1),
	Msg3 = erl8583_message:set(5, "10", Msg2),
	[2, 16, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16]
		= erl8583_marshaller:marshal(Msg3, ?MARSHALLER_BINARY).

field_6_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0220", Msg1),
	Msg3 = erl8583_message:set(6, "098765", Msg2),
	[2, 32, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 135, 101]
		= erl8583_marshaller:marshal(Msg3, ?MARSHALLER_BINARY).

fields_8_9_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0220", Msg1),
	Msg3 = erl8583_message:set(8, "88", Msg2),
	Msg4 = erl8583_message:set(9, "99", Msg3),
	[2, 32, 1, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 136, 0, 0, 0, 153]
		= erl8583_marshaller:marshal(Msg4, ?MARSHALLER_BINARY).

field_18_19_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0220", Msg1),
	Msg3 = erl8583_message:set(18, "1234", Msg2),
	Msg4 = erl8583_message:set(19, "567", Msg3),
	[2, 32, 0, 0, 96, 0, 0, 0, 0, 0, 18, 52, 5, 103]
		= erl8583_marshaller:marshal(Msg4, ?MARSHALLER_BINARY).

fields_28_29_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0221", Msg1),
	Msg3 = erl8583_message:set(28, "C1", Msg2),
	Msg4 = erl8583_message:set(29, "D22", Msg3),
	[2, 33, 0, 0, 0, 24, 0, 0, 0, 0, 67, 0, 0, 0, 1, 68, 0, 0, 0, 34]
		= erl8583_marshaller:marshal(Msg4, ?MARSHALLER_BINARY).

fields_28_29_b_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0221", Msg1),
	Msg3 = erl8583_message:set(28, "C1", Msg2),
	Msg4 = erl8583_message:set(29, "D22", Msg3),
	[2, 33, 0, 0, 0, 24, 0, 0, 0, 0, 67, 0, 0, 0, 1, 68, 0, 0, 0, 34]
		= erl8583_marshaller_binary:marshal(Msg4).

fields_33_34_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0221", Msg1),
	Msg3 = erl8583_message:set(33, "12345", Msg2),
	Msg4 = erl8583_message:set(34, "567890", Msg3),
	[2, 33, 0, 0, 0, 0, 192, 0, 0, 0, 5, 18, 52, 80, 6, 53, 54, 55, 56, 57, 48]
		= erl8583_marshaller:marshal(Msg4, ?MARSHALLER_BINARY).

fields_35_36_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0221", Msg1),
	Msg3 = erl8583_message:set(35, ";1234123412341234=0305101193010877?", Msg2),
	Msg4 = erl8583_message:set(36, "ABC123", Msg3),
	[2, 33, 0, 0, 0, 0, 48, 0, 0, 0, 53, 177, 35, 65, 35, 65, 35, 65, 35, 77, 3, 5, 16, 17, 147, 1, 8, 119, 240, 0, 6, 65, 66, 67, 49, 50, 51] =
		erl8583_marshaller:marshal(Msg4, ?MARSHALLER_BINARY).

fields_35_36b_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0221", Msg1),
	Msg3 = erl8583_message:set(35, ";12?", Msg2),
	Msg4 = erl8583_message:set(36, lists:duplicate(104, $A), Msg3),
	[2, 33, 0, 0, 0, 0, 48, 0, 0, 0, 4, 177, 47, 1, 4, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 
		65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 
		65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 
	    65, 65, 65, 65, 65, 65, 65, 65, 65] = erl8583_marshaller:marshal(Msg4, ?MARSHALLER_BINARY).

field_37_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0221", Msg1),
	Msg3 = erl8583_message:set(37, "A1", Msg2),
	[2, 33, 0, 0, 0, 0, 8, 0, 0, 0, 65, 49, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32] =
		erl8583_marshaller:marshal(Msg3, ?MARSHALLER_BINARY).

field_41_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(41, "CATI 1", Msg2),
	[2, 0, 0, 0, 0, 0, 0, 128, 0, 0, 67, 65, 84, 73, 32, 49, 32, 32] =
		erl8583_marshaller:marshal(Msg3, ?MARSHALLER_BINARY).

field_44_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(44, "additional rep0n5e data", Msg2),
	[2, 0, 0, 0, 0, 0, 0, 16, 0, 0, 35, 97, 100, 100, 105, 116, 105, 111, 110, 97, 108, 32, 114, 101, 112, 48, 110, 53, 101, 32, 100, 97, 116, 97] =
		erl8583_marshaller:marshal(Msg3, ?MARSHALLER_BINARY).

field_52_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(2, "1234567890123456", Msg2),
	Msg4 = erl8583_message:set(52, <<"12345678">>, Msg3),
	[2, 0, 64, 0, 0, 0, 0, 0, 16, 0, 22, 18, 52, 86, 120, 144, 18, 52, 86, 49, 50, 51, 52, 53, 54, 55, 56] =
		erl8583_marshaller:marshal(Msg4, ?MARSHALLER_BINARY).

field_66_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(2, "1234567890123456", Msg2),
	Msg4 = erl8583_message:set(66, "1", Msg3),
	[2, 0, 192, 0, 0, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 22, 18, 52, 86, 120, 144, 18, 52, 86, 1] =
		erl8583_marshaller:marshal(Msg4, ?MARSHALLER_BINARY).

field_101_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(2, "1234567890123456", Msg2),
	Msg4 = erl8583_message:set(101, "FileName", Msg3),
	[2, 0, 192, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 22, 18, 52, 86, 120, 144, 18, 52, 86, 8, 70, 105, 108, 101, 78, 97, 109, 101] =
		erl8583_marshaller:marshal(Msg4, ?MARSHALLER_BINARY).

custom_marshaller_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(3, 3, Msg2),
	Msg4 = erl8583_message:set(4, 4, Msg3),
	[255, 48, 0, 0, 0, 0, 0, 0, 0, 3, 4] 
		= erl8583_marshaller:marshal(Msg4, [{mti_marshaller, ?MODULE}, {field_marshaller, ?MODULE}, {bitmap_marshaller, erl8583_marshaller_binary}]).

custom_bitmap_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(3, 3, Msg2),
	Msg4 = erl8583_message:set(4, 4, Msg3),
	[255, 254, 3, 4] 
		= erl8583_marshaller:marshal(Msg4, [{mti_marshaller, ?MODULE}, {field_marshaller, ?MODULE}, {bitmap_marshaller, ?MODULE}]).
	