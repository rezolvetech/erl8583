%% Author: carl
%% Created: 12 Feb 2011
%% Description: TODO: Add description to test_ascii_marshaller
-module(test_ascii_marshaller).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("erl8583_field_ids.hrl").
-include("erl8583_marshallers.hrl").

%%
%% Exported Functions
%%
-export([marshal_field/3, marshal_bitmap/1, marshal_mti/1]).

%%
%% API Functions
%%

% Self-shunting for marshalling fields in tests.
marshal_field(3, _Msg, _) ->
	"Field 3";
marshal_field(4, _Msg, _) ->
	"Field 4";
marshal_field(0, "0200", _) ->
	"X";
marshal_field(FieldId, Msg, Encoding) ->
	erl8583_marshaller_ascii:marshal_field(FieldId, Msg, Encoding).

% Self-shunting for marshalling bit map in tests.
marshal_bitmap(Msg) ->
	{"bitmap", Msg}.

marshal_mti(Value) ->
	marshal_field(0, Value, undefined).

pan_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(2, "5234567890123456", Msg2),	
	"02004000000000000000165234567890123456" = erl8583_marshaller:marshal(Msg3, ?MARSHALLER_ASCII).

proc_code_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0100", Msg1),
	Msg3 = erl8583_message:set(3, "01234", Msg2),	
	"01002000000000000000001234" = erl8583_marshaller:marshal(Msg3, ?MARSHALLER_ASCII).

amount_tran_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(4, "30000", Msg2),	
	"02001000000000000000000000030000" = erl8583_marshaller:marshal(Msg3, ?MARSHALLER_ASCII).
	
amount_settle_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(5, "1", Msg2),	
	"02000800000000000000000000000001" = erl8583_marshaller:marshal(Msg3, ?MARSHALLER_ASCII).

fields_5_6_7_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(5, "1", Msg2),	
	Msg4 = erl8583_message:set(6, "2", Msg3),	
	Msg5 = erl8583_message:set(7, "0131081200", Msg4),	
	"02000E000000000000000000000000010000000000020131081200" = erl8583_marshaller:marshal(Msg5, ?MARSHALLER_ASCII).

fields_5_6_7_b_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(5, "1", Msg2),	
	Msg4 = erl8583_message:set(6, "2", Msg3),	
	Msg5 = erl8583_message:set(7, "0131081200", Msg4),	
	"02000E000000000000000000000000010000000000020131081200" = erl8583_marshaller_ascii:marshal(Msg5).
	
fields_8_9_10_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0300", Msg1),
	Msg3 = erl8583_message:set(8, "1", Msg2),	
	Msg4 = erl8583_message:set(9, "2", Msg3),	
	Msg5 = erl8583_message:set(10, "3", Msg4),	
	"030001C0000000000000000000010000000200000003" = erl8583_marshaller:marshal(Msg5, ?MARSHALLER_ASCII).

fields_11_12_13_14_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(11, "1234", Msg2),	
	Msg4 = erl8583_message:set(12, "150755", Msg3),	
	Msg5 = erl8583_message:set(13, "2012", Msg4),
	Msg6 = erl8583_message:set(14, "1206", Msg5),
	"0200003C00000000000000123415075520121206" = erl8583_marshaller:marshal(Msg6, ?MARSHALLER_ASCII).

fields_21_to_25_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(?FORWARDING_INST_COUNTRY_CODE, "1", Msg2),	
	Msg4 = erl8583_message:set(?POS_ENTRY_MODE, "2", Msg3),	
	Msg5 = erl8583_message:set(?CARD_SEQUENCE_NUMBER, "3", Msg4),
	Msg6 = erl8583_message:set(?POS_CONDITION_CODE, "5", Msg5),
	Msg7 = erl8583_message:set(?NETWORK_INTERNATIONAL_ID, "4", Msg6),
	"020000000F800000000000100200300405" = erl8583_marshaller:marshal(Msg7, ?MARSHALLER_ASCII).
	
fields_27_to_33_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(27, "9", Msg2),	
	Msg4 = erl8583_message:set(28, "C11", Msg3),
	Msg5 = erl8583_message:set(29, "D22", Msg4),
	Msg6 = erl8583_message:set(30, "C33", Msg5),
	Msg7 = erl8583_message:set(31, "C44", Msg6),
	Msg8 = erl8583_message:set(32, "555", Msg7),
	Msg9 = erl8583_message:set(33, "12345678901", Msg8),
	"02000000003F800000009C00000011D00000022C00000033C00000044035551112345678901"
		= erl8583_marshaller:marshal(Msg9, ?MARSHALLER_ASCII).

fields_34_35_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(34, "12341234123412341234567890", Msg2),	
	Msg4 = erl8583_message:set(35, ";1234123412341234=0305101193010877?", Msg3),
	"02000000000060000000261234123412341234123456789035;1234123412341234=0305101193010877?" =
		erl8583_marshaller:marshal(Msg4, ?MARSHALLER_ASCII).

fields_36_37_38_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(36, "1234567890", Msg2),	
	Msg4 = erl8583_message:set(37, "Query123456", Msg3),
	Msg5 = erl8583_message:set(38, "123", Msg4),
	"0200000000001C0000000101234567890Query123456 123   " = erl8583_marshaller:marshal(Msg5, ?MARSHALLER_ASCII).

field_39_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(39, "R7", Msg2),
	"02000000000002000000R7" = erl8583_marshaller:marshal(Msg3, ?MARSHALLER_ASCII).

field_40_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(?SERVICE_RESTRICTION_CODE, "R 1", Msg2),
	"02000000000001000000R 1" = erl8583_marshaller:marshal(Msg3, ?MARSHALLER_ASCII).
	
field_41_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(41, "Term#1", Msg2),
	"02000000000000800000Term#1  " = erl8583_marshaller:marshal(Msg3, ?MARSHALLER_ASCII).

field_42_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(?CARD_ACCEPTOR_ID_CODE, "CA ID 123", Msg2),
	"02000000000000400000CA ID 123      " = erl8583_marshaller:marshal(Msg3, ?MARSHALLER_ASCII).

field_43_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(43, "NAME                                  ZA", Msg2),
	"02000000000000200000NAME                                  ZA" = erl8583_marshaller:marshal(Msg3, ?MARSHALLER_ASCII).

field_44_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(44, "Foo123", Msg2),
	"0200000000000010000006Foo123" = erl8583_marshaller:marshal(Msg3, ?MARSHALLER_ASCII).
	
field_45_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(?TRACK_1_DATA, "Foo123", Msg2),
	"0200000000000008000006Foo123" = erl8583_marshaller:marshal(Msg3, ?MARSHALLER_ASCII).
	
field_46_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(2, "5234567890123456789", Msg2),
	Msg4 = erl8583_message:set(?ADDITIONAL_DATA_ISO, "Hello, world!", Msg3),
	"02004000000000040000195234567890123456789013Hello, world!" = erl8583_marshaller:marshal(Msg4, ?MARSHALLER_ASCII).

fields_47_48_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(47, "Hello!", Msg2),
	Msg4 = erl8583_message:set(48, "Goodbye!", Msg3),
	"02000000000000030000006Hello!008Goodbye!" = erl8583_marshaller:marshal(Msg4, ?MARSHALLER_ASCII).

fields_49_50_51_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(?CURRENCY_CODE_TRAN, "A", Msg2),
	Msg4 = erl8583_message:set(?CURRENCY_CODE_SETTLE, "B", Msg3),
	Msg5 = erl8583_message:set(?CURRENCY_CODE_CARDHOLDER_BILLING, "C", Msg4),
	"0200000000000000E000A  B  C  " = erl8583_marshaller:marshal(Msg5, ?MARSHALLER_ASCII).

field_52_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(52, <<253, 0, 1, 2, 3, 4, 5, 127>>, Msg2),
	"02000000000000001000FD0001020304057F" = erl8583_marshaller:marshal(Msg3, ?MARSHALLER_ASCII).

field_53_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(2, "5234567890123456789", Msg2),
	Msg4 = erl8583_message:set(?SECURITY_RELATED_CONTROL_INFO, "17", Msg3),
	"020040000000000008001952345678901234567890000000000000017" = erl8583_marshaller:marshal(Msg4, ?MARSHALLER_ASCII).
	
field_54_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(2, "5234567890123456789", Msg2),
	Msg4 = erl8583_message:set(?ADDITIONAL_AMOUNTS, "Additi0nal Am0unt", Msg3),
	"02004000000000000400195234567890123456789017Additi0nal Am0unt" = erl8583_marshaller:marshal(Msg4, ?MARSHALLER_ASCII).

fields_55_56_57_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(2, "5234567890123456789", Msg2),
	Msg4 = erl8583_message:set(55, "A1", Msg3),
	Msg5 = erl8583_message:set(56, "B22", Msg4),
	Msg6 = erl8583_message:set(57, "C333", Msg5),
	"02004000000000000380195234567890123456789002A1003B22004C333" = erl8583_marshaller:marshal(Msg6, ?MARSHALLER_ASCII).
	
field_64_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(64, <<128, 255, 1, 2, 3, 4, 5, 127>>, Msg2),
	"0200000000000000000180FF01020304057F" = erl8583_marshaller:marshal(Msg3, ?MARSHALLER_ASCII).

field_66_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(?SETTLE_CODE, "9", Msg2),
	"0200800000000000000040000000000000009" = erl8583_marshaller:marshal(Msg3, ?MARSHALLER_ASCII),
	Msg3 = erl8583_marshaller:unmarshal(erl8583_marshaller:marshal(Msg3, ?MARSHALLER_ASCII), ?MARSHALLER_ASCII).
	
fields_67_to_70_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(?EXTENDED_PAYMENT_CODE, "1", Msg2),
	Msg4 = erl8583_message:set(?RECEIVING_INSTITUTION_COUNTRY_CODE, "2", Msg3),
	Msg5 = erl8583_message:set(?SETTLE_INSTITUTION_COUNTRY_CODE, "3", Msg4),
	Msg6 = erl8583_message:set(?NETWORK_MANAGEMENT_INFORMATION_CODE, "4", Msg5),
	"020080000000000000003C0000000000000001002003004" = erl8583_marshaller:marshal(Msg6, ?MARSHALLER_ASCII).
	
fields_71_72_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(?MESSAGE_NUMBER, "1", Msg2),
	Msg4 = erl8583_message:set(?MESSAGE_NUMBER_LAST, "2", Msg3),
	"02008000000000000000030000000000000000010002" = erl8583_marshaller:marshal(Msg4, ?MARSHALLER_ASCII).

fields_74_to_77_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(?CREDITS_NUMBER, "1", Msg2),
	Msg4 = erl8583_message:set(?CREDITS_REVERSAL_NUMBER, "22", Msg3),
	Msg5 = erl8583_message:set(?DEBITS_NUMBER, "333", Msg4),
	Msg6 = erl8583_message:set(?DEBITS_REVERSAL_NUMBER, "4444", Msg5),
	"0200800000000000000000780000000000000000000001000000002200000003330000004444" = erl8583_marshaller:marshal(Msg6, ?MARSHALLER_ASCII).

fields_83_to_90_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(?CREDITS_TRANSACTION_FEE_AMOUNT, "1", Msg2),
	Msg4 = erl8583_message:set(?DEBITS_PROCESSING_FEE_AMOUNT, "22", Msg3),
	Msg5 = erl8583_message:set(?DEBITS_TRANSACTION_FEE_AMOUNT, "333", Msg4),
	Msg6 = erl8583_message:set(?CREDITS_AMOUNT, "4444", Msg5),
	Msg7 = erl8583_message:set(?CREDITS_REVERSAL_AMOUNT, "55555", Msg6),
	Msg8 = erl8583_message:set(?DEBITS_AMOUNT, "666666", Msg7),
	Msg9 = erl8583_message:set(?DEBITS_REVERSAL_AMOUNT, "7777777", Msg8),
	Msg10 = erl8583_message:set(?ORIGINAL_DATA_ELEMENTS, "88888888", Msg9),
	"0200800000000000000000003FC0000000000000000000010000000000220000000003330000000000004444000000000005555500000000006666660000000007777777000000000000000000000000000000000088888888" = erl8583_marshaller:marshal(Msg10, ?MARSHALLER_ASCII).

fields_95_96_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(?REPLACEMENT_AMOUNTS, "1", Msg2),
	Msg4 = erl8583_message:set(?MESSAGE_SECURITY_CODE, <<1,0,0,0,0,0,0,255>>, Msg3),
	"0200800000000000000000000003000000001                                         01000000000000FF" = erl8583_marshaller:marshal(Msg4, ?MARSHALLER_ASCII).
	
fields_99_100_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(?SETTLE_INSTITUTION_ID_CODE, "11", Msg2),
	Msg4 = erl8583_message:set(?RECEIVING_INSTITUTION_ID_CODE, "2222", Msg3),
	"0200800000000000000000000000300000000211042222" = erl8583_marshaller:marshal(Msg4, ?MARSHALLER_ASCII).
	
field_101_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(?FILE_NAME, "A loong file name", Msg2),
	"02008000000000000000000000000800000017A loong file name" = erl8583_marshaller:marshal(Msg3, ?MARSHALLER_ASCII).
	
custom_marshaller_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0200", Msg1),
	Msg3 = erl8583_message:set(3, "1", Msg2),
	Msg4 = erl8583_message:set(4, "4", Msg3),
	"X3000000000000000Field 3Field 4" = 
		erl8583_marshaller:marshal(Msg4, [{mti_marshaller, ?MODULE}, {field_marshaller, ?MODULE}, {bitmap_marshaller, erl8583_marshaller_ascii}]).
	
custom_bitmap_test() ->
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set(0, "0210", Msg1),
	Msg3 = erl8583_message:set(3, "1", Msg2),
	Msg4 = erl8583_message:set(4, "4", Msg3),
	"0210bitmapField 3Field 4" = 
		erl8583_marshaller:marshal(Msg4, [{mti_marshaller, erl8583_marshaller_ascii}, {field_marshaller, ?MODULE}, {bitmap_marshaller, ?MODULE}]).
	
%%
%% Local Functions
%%

