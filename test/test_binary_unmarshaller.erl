%% Author: carl
%% Created: 19 Feb 2011
%% Description: TODO: Add description to test_binary_unmarshaller
-module(test_binary_unmarshaller).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("erl8583_field_ids.hrl").
-include("erl8583_marshallers.hrl").

%%
%% Exported Functions
%%
-export([unmarshal_field/3, unmarshal_bitmap/1, unmarshal_mti/1]).

%%
%% API Functions
%%
unmarshal_field(3, [3, 0, 4, 0], _EncodingRules) ->
	{"3", [4, 0], []};
unmarshal_field(4, [4, 0], _EncodingRule) ->
	{"4", [], []};
unmarshal_field(0, Binary, _EncodingRule) ->
	[255|B] = Binary,
	{"0200", B, []}.

unmarshal_mti(Marshalled) ->
	{X, Y, []} = unmarshal_field(0, Marshalled, undefined),
	{X, Y}.

unmarshal_bitmap([254, 3, 0, 4, 0]) ->
	{[3, 4], [3, 0, 4, 0]}.

%%
%% Local Functions
%%
field_2_test() ->
	Msg1 = erl8583_marshaller:unmarshal([2, 16, 64, 0, 0, 0, 0, 0, 0, 0, 23, 21, 35, 69, 103, 137, 1, 35, 69, 96], ?MARSHALLER_BINARY),
	"0210" = erl8583_message:get(0, Msg1),
	[0, 2] = erl8583_message:get_fields(Msg1),
	"15234567890123456" = erl8583_message:get(2, Msg1).

field_2_3_test() ->
	Msg1 = erl8583_marshaller:unmarshal([2, 16, 96, 0, 0, 0, 0, 0, 0, 0, 22, 18, 52, 86, 120, 144, 18, 52, 86, 1, 35, 69], ?MARSHALLER_BINARY),
	"0210" = erl8583_message:get(0, Msg1),
	[0, 2, 3] = erl8583_message:get_fields(Msg1),
	"1234567890123456" = erl8583_message:get(2, Msg1),
	"012345" = erl8583_message:get(?PROC_CODE, Msg1).

field_2_3_b_test() ->
	Msg1 = erl8583_marshaller_binary:unmarshal([2, 16, 96, 0, 0, 0, 0, 0, 0, 0, 22, 18, 52, 86, 120, 144, 18, 52, 86, 1, 35, 69]),
	"0210" = erl8583_message:get(0, Msg1),
	[0, 2, 3] = erl8583_message:get_fields(Msg1),
	"1234567890123456" = erl8583_message:get(2, Msg1),
	"012345" = erl8583_message:get(?PROC_CODE, Msg1).

field_4_test() ->
	Msg = erl8583_marshaller:unmarshal([1, 2, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 35], ?MARSHALLER_BINARY),
	"0102" = erl8583_message:get(0, Msg),
	[0, 4] = erl8583_message:get_fields(Msg),
	"000000000123" = erl8583_message:get(4, Msg).

field_5_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 17, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16], ?MARSHALLER_BINARY),
	"0211" = erl8583_message:get(0, Msg),
	[0, 5] = erl8583_message:get_fields(Msg),
	"000000000010" = erl8583_message:get(5, Msg).
	
field_6_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 32, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 135, 101], ?MARSHALLER_BINARY),
	"0220" = erl8583_message:get(0, Msg),
	[0, 6] = erl8583_message:get_fields(Msg),
	"000000098765" = erl8583_message:get(6, Msg).

field_6_7_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 32, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 152, 118, 84, 2, 34, 25, 6, 18], ?MARSHALLER_BINARY),
	"0220" = erl8583_message:get(0, Msg),
	[0, 6, 7] = erl8583_message:get_fields(Msg),
	"000000987654" = erl8583_message:get(6, Msg),
	"0222190612" =  erl8583_message:get(7, Msg).
	
field_18_19_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 32, 0, 0, 96, 0, 0, 0, 0, 0, 18, 52, 5, 103], ?MARSHALLER_BINARY),
	"0220" = erl8583_message:get(0, Msg),
	[0, 18, 19] = erl8583_message:get_fields(Msg),
	"1234" = erl8583_message:get(18, Msg),
	"567" =  erl8583_message:get(19, Msg).

field_26_27_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 33, 0, 0, 0, 96, 0, 0, 0, 0, 34, 1], ?MARSHALLER_BINARY),
	"0221" = erl8583_message:get(0, Msg),
	[0, 26, 27] = erl8583_message:get_fields(Msg),
	"22" = erl8583_message:get(26, Msg),
	"1" =  erl8583_message:get(27, Msg).

fields_28_29_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 33, 0, 0, 0, 24, 0, 0, 0, 0, 67, 0, 0, 0, 1, 68, 0, 0, 0, 34], ?MARSHALLER_BINARY),
	"0221" = erl8583_message:get(0, Msg),
	[0, 28, 29] = erl8583_message:get_fields(Msg),
	"C00000001" = erl8583_message:get(28, Msg),
	"D00000022" =  erl8583_message:get(29, Msg).
	
fields_33_34_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 33, 0, 0, 0, 0, 192, 0, 0, 0, 5, 18, 52, 80, 6, 53, 54, 55, 56, 57, 48], ?MARSHALLER_BINARY),
	"0221" = erl8583_message:get(0, Msg),
	[0, 33, 34] = erl8583_message:get_fields(Msg),
	"12345" = erl8583_message:get(33, Msg),
	"567890" =  erl8583_message:get(34, Msg).

fields_35_36_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 33, 0, 0, 0, 0, 48, 0, 0, 0, 53, 177, 35, 65, 35, 65, 35, 65, 35, 77, 3, 5, 16, 17, 147, 1, 8, 119, 240, 0, 6, 65, 66, 67, 49, 50, 51], ?MARSHALLER_BINARY),
	"0221" = erl8583_message:get(0, Msg),
	[0, 35, 36] = erl8583_message:get_fields(Msg),
	";1234123412341234=0305101193010877?" = erl8583_message:get(35, Msg),
	"ABC123" =  erl8583_message:get(36, Msg).
	
fields_35_36b_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 33, 0, 0, 0, 0, 48, 0, 0, 0, 4, 177, 47, 1, 4, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 
										   65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 
										   65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 
										   65, 65, 65, 65, 65, 65, 65, 65, 65], ?MARSHALLER_BINARY),
	[0, 35, 36] = erl8583_message:get_fields(Msg),
	";12?" = erl8583_message:get(35, Msg),
	Expected = lists:duplicate(104, $A),
	Expected =  erl8583_message:get(36, Msg).

field_37_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 33, 0, 0, 0, 0, 8, 0, 0, 0, 65, 49, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32], ?MARSHALLER_BINARY),
	[0, 37] = erl8583_message:get_fields(Msg),
	"A1          " = erl8583_message:get(?RETRIEVAL_REF_NUM, Msg).

field_41_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 0, 0, 0, 0, 0, 0, 128, 0, 0, 67, 65, 84, 73, 32, 49, 32, 32], ?MARSHALLER_BINARY),
	[0, 41] = erl8583_message:get_fields(Msg),
	"CATI 1  " = erl8583_message:get(?CARD_ACCEPTOR_TERMINAL_ID, Msg).

field_44_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 0, 0, 0, 0, 0, 0, 16, 0, 0, 35, 97, 100, 100, 105, 116, 105, 111, 110, 97, 108, 32, 114, 101, 112, 48, 110, 53, 101, 32, 100, 97, 116, 97], ?MARSHALLER_BINARY),
	[0, 44] = erl8583_message:get_fields(Msg),
	"additional rep0n5e data" = erl8583_message:get(44, Msg).
	
field_46_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 0, 64, 0, 0, 0, 0, 4, 0, 0, 25, 18, 52, 86, 120, 144, 18, 52, 86, 120, 144, 0, 33, 97, 100, 100, 105, 116, 105, 111, 110, 97, 108, 32, 100, 97, 116, 97, 32, 45, 32, 105, 115, 111], ?MARSHALLER_BINARY),
	[0, 2, 46] = erl8583_message:get_fields(Msg),
	"additional data - iso" = erl8583_message:get(46, Msg).
	
field_52_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 0, 64, 0, 0, 0, 0, 0, 16, 0, 22, 18, 52, 86, 120, 144, 18, 52, 86, 49, 50, 51, 52, 53, 54, 55, 56], ?MARSHALLER_BINARY),
	[0, 2, 52] = erl8583_message:get_fields(Msg),
	<<"12345678">> = erl8583_message:get(52, Msg).

field_53_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 0, 64, 0, 0, 0, 0, 0, 8, 0, 22, 18, 52, 86, 120, 144, 18, 52, 86, 0, 0, 0, 0, 0, 0, 0, 1], ?MARSHALLER_BINARY),
	[0, 2, 53] = erl8583_message:get_fields(Msg),
	"0000000000000001" = erl8583_message:get(53, Msg).

field_55_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 0, 64, 0, 0, 0, 0, 0, 2, 0, 22, 18, 52, 86, 120, 
										  144, 18, 52, 86, 1, 35, 65, 65, 65, 65, 65, 65, 65, 
										  65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 
										  65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 
										  65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 
										  65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 
										  65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 
										  65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 
										  65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 
										  65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 
										  65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65], ?MARSHALLER_BINARY),
	Expected = lists:duplicate(123, $A),
	Expected = erl8583_message:get(55, Msg).

field_64_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 0, 64, 0, 0, 0, 0, 0, 0, 1, 22, 18, 52, 86, 120, 144, 18, 52, 86, 0, 0, 0, 0, 0, 0, 0, 0], ?MARSHALLER_BINARY),
	<<0, 0, 0, 0, 0, 0, 0, 0>> = erl8583_message:get(64, Msg).

field_66_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 0, 192, 0, 0, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 22, 18, 52, 86, 120, 144, 18, 52, 86, 1], ?MARSHALLER_BINARY),
	[0, 2, 66] = erl8583_message:get_fields(Msg),
	"1" = erl8583_message:get(66, Msg).

field_91_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 0, 192, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 0, 0, 0, 0, 22, 18, 52, 86, 120, 144, 18, 52, 86, 65], ?MARSHALLER_BINARY),
	"A" = erl8583_message:get(91, Msg).

field_101_test() ->
	Msg = erl8583_marshaller:unmarshal([2, 0, 192, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 22, 18, 52, 86, 120, 144, 18, 52, 86, 8, 70, 105, 108, 101, 78, 97, 109, 101], ?MARSHALLER_BINARY),
	"FileName" = erl8583_message:get(101, Msg).

custom_marshaller_test() ->
	Msg = erl8583_marshaller:unmarshal([255, 48, 0, 0, 0, 0, 0, 0, 0, 3, 0, 4, 0], [{field_marshaller, ?MODULE}, {bitmap_marshaller, erl8583_marshaller_binary}, {mti_marshaller, ?MODULE}]),
	"3" = erl8583_message:get(3, Msg),
	"4" = erl8583_message:get(4, Msg).

custom_bitmap_test() ->
	Msg = erl8583_marshaller:unmarshal([255, 254, 3, 0, 4, 0], [{mti_marshaller, ?MODULE}, {bitmap_marshaller, ?MODULE}, {field_marshaller, ?MODULE}]),
	"0200" = erl8583_message:get(0, Msg),
	"3" = erl8583_message:get(3, Msg),
	"4" = erl8583_message:get(4, Msg).
	