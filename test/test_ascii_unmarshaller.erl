%% Author: carl
%% Created: 13 Feb 2011
%% Description: TODO: Add description to test_ascii_unmarshaller
-module(test_ascii_unmarshaller).

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
unmarshal_field(3, [$3|Tail], _Encoder) ->
	{"Field 3", Tail, []};
unmarshal_field(4, [$4|Tail], _Encoder) ->
	{"Field 4", Tail, []};
unmarshal_field(0, [$X|Tail], _Encoder) ->
	{"0200", Tail, []};
unmarshal_field(Id, Str, Encoder) ->
	erl8583_marshaller_ascii:unmarshal_field(Id, Str, Encoder).
	
unmarshal_mti(Marshalled) ->
	{X, Y, []} = unmarshal_field(0, Marshalled, erl8583_fields),
	{X, Y}.

% bit map unmarshaller
unmarshal_bitmap([$B|Rest]) ->
	{[3, 4], Rest}.

%%
%% Local Functions
%%
pan_test() ->
	Msg = erl8583_marshaller:unmarshal("02004000000000000000165234567890123456", ?MARSHALLER_ASCII),
	"0200" = erl8583_message:get(0, Msg),
	[0, 2] = erl8583_message:get_fields(Msg),
	"5234567890123456" = erl8583_message:get(2, Msg).
	
field_8_9_10_test() ->
	Msg = erl8583_marshaller:unmarshal("030001C0000000000000000000010000000200000003", ?MARSHALLER_ASCII),
	"0300" = erl8583_message:get(0, Msg),
	[0, 8, 9, 10] = erl8583_message:get_fields(Msg),
	"00000001" = erl8583_message:get(8, Msg),
	"00000002" = erl8583_message:get(9, Msg),
	"00000003" = erl8583_message:get(10, Msg).

field_8_9_10_b_test() ->
	Msg = erl8583_marshaller_ascii:unmarshal("030001C0000000000000000000010000000200000003"),
	"0300" = erl8583_message:get(0, Msg),
	[0, 8, 9, 10] = erl8583_message:get_fields(Msg),
	"00000001" = erl8583_message:get(8, Msg),
	"00000002" = erl8583_message:get(9, Msg),
	"00000003" = erl8583_message:get(10, Msg).

fields_11_12_13_14_test() ->
	Msg = erl8583_marshaller:unmarshal("0200003C00000000000000123415075520121206", ?MARSHALLER_ASCII),
	[0, 11, 12, 13, 14] = erl8583_message:get_fields(Msg),
	"150755" = erl8583_message:get(12, Msg),
	"001234" = erl8583_message:get(11, Msg).

fields_15_to_20_test() ->
	Msg = erl8583_marshaller:unmarshal("02000003F000000000000001000200030004005006", ?MARSHALLER_ASCII),
	[0, 15, 16, 17, 18, 19, 20] = erl8583_message:get_fields(Msg),
	"0001" = erl8583_message:get(15, Msg),
	"0003" = erl8583_message:get(17, Msg),
	"0004" = erl8583_message:get(18, Msg),
	"006" = erl8583_message:get(20, Msg).

field_26_test() ->
	Msg = erl8583_marshaller:unmarshal("0200000008400000000000106", ?MARSHALLER_ASCII),
	[0, 21, 26] = erl8583_message:get_fields(Msg),
	"06" = erl8583_message:get(?POS_CAPTURE_CODE, Msg).

field_27_to_33_test() ->
	Msg = erl8583_marshaller:unmarshal("02000000003F800000009C00000011D00000022C00000033C00000044035551112345678901", ?MARSHALLER_ASCII),
	"9" = erl8583_message:get(?AUTHORIZING_ID_RESP_LEN, Msg),
	"C00000011" = erl8583_message:get(?AMOUNT_TRAN_FEE, Msg).

fields_34_35_test() ->
	Msg = erl8583_marshaller:unmarshal("02000000000060000000261234123412341234123456789035;1234123412341234=0305101193010877?", ?MARSHALLER_ASCII),
	[0, 34, 35] = erl8583_message:get_fields(Msg),
	"12341234123412341234567890" = erl8583_message:get(?PAN_EXTENDED, Msg),
	";1234123412341234=0305101193010877?" = erl8583_message:get(?TRACK_2_DATA, Msg).

fields_36_37_38_test() ->
	Msg = erl8583_marshaller:unmarshal("0200000000001C0000000101234567890Query123456 123   ", ?MARSHALLER_ASCII),
	[0, 36, 37, 38] = erl8583_message:get_fields(Msg),
	"1234567890" = erl8583_message:get(?TRACK_3_DATA, Msg),
	"Query123456 " = erl8583_message:get(37, Msg),
	"123   " = erl8583_message:get(38, Msg).

field_39_test() ->
	Msg = erl8583_marshaller:unmarshal("020000000000020000007 ", ?MARSHALLER_ASCII),
	[0, 39] = erl8583_message:get_fields(Msg),
	"7 " = erl8583_message:get(?RESP_CODE, Msg).

field_40_test() ->
	Msg = erl8583_marshaller:unmarshal("02000000000001000000R 1", ?MARSHALLER_ASCII),
	"R 1" = erl8583_message:get(40, Msg).

field_41_test() ->
	Msg = erl8583_marshaller:unmarshal("02000000000000800000Term#1  ", ?MARSHALLER_ASCII),
	"Term#1  " = erl8583_message:get(?CARD_ACCEPTOR_TERMINAL_ID, Msg).

field_42_test() ->
	Msg = erl8583_marshaller:unmarshal("02000000000000400000CA ID 123      ", ?MARSHALLER_ASCII),
	"CA ID 123      " = erl8583_message:get(42, Msg).

field_43_test() ->
	Msg = erl8583_marshaller:unmarshal("02000000000000200000NAME                                  ZA", ?MARSHALLER_ASCII),
	"NAME                                  ZA" = erl8583_message:get(?CARD_ACCEPTOR_NAME_LOCATION, Msg).
	
field_44_test() ->
	Msg = erl8583_marshaller:unmarshal("0200000000000010000006Foo123", ?MARSHALLER_ASCII),
	"Foo123" = erl8583_message:get(?ADDITIONAL_RESP_DATA, Msg).

field_45_test() ->
	Msg = erl8583_marshaller:unmarshal("0200000000000008000006Foo123", ?MARSHALLER_ASCII),
	"Foo123" = erl8583_message:get(45, Msg).

field_46_test() ->
	Msg = erl8583_marshaller:unmarshal("02004000000000040000195234567890123456789013Hello, world!", ?MARSHALLER_ASCII),
	[0, 2, 46] = erl8583_message:get_fields(Msg),
	"Hello, world!" = erl8583_message:get(46, Msg).

fields_47_48_test() ->
	Msg = erl8583_marshaller:unmarshal("02000000000000030000006Hello!008Goodbye!", ?MARSHALLER_ASCII),
	[0, 47, 48] = erl8583_message:get_fields(Msg),
	"Hello!" = erl8583_message:get(?ADDITIONAL_DATA_NATIONAL, Msg),
	"Goodbye!" = erl8583_message:get(?ADDITIONAL_DATA_PRIVATE, Msg).

fields_49_50_51_test() ->
	Msg = erl8583_marshaller:unmarshal("0200000000000000E000A  B  C  ", ?MARSHALLER_ASCII),
	[0, 49, 50, 51] = erl8583_message:get_fields(Msg),
	"A  " = erl8583_message:get(49, Msg),
	"B  " = erl8583_message:get(50, Msg),
	"C  " = erl8583_message:get(51, Msg).

field_52_test() ->
	Msg = erl8583_marshaller:unmarshal("02000000000000001000FD0001020304057F", ?MARSHALLER_ASCII),
	[0, 52] = erl8583_message:get_fields(Msg),
	<<253, 0, 1, 2, 3, 4, 5, 127>> = erl8583_message:get(52, Msg).

field_53_test() ->
	Msg = erl8583_marshaller:unmarshal("020040000000000008001952345678901234567890000000000000017", ?MARSHALLER_ASCII),
	[0, 2, 53] = erl8583_message:get_fields(Msg),
	"0000000000000017" = erl8583_message:get(?SECURITY_RELATED_CONTROL_INFO, Msg).

field_54_test() ->
	Msg = erl8583_marshaller:unmarshal("02004000000000000400195234567890123456789017Additi0nal Am0unt", ?MARSHALLER_ASCII),
	[0, 2, 54] = erl8583_message:get_fields(Msg),
	"Additi0nal Am0unt" = erl8583_message:get(?ADDITIONAL_AMOUNTS, Msg).

fields_55_56_57_test() ->
	Msg = erl8583_marshaller:unmarshal("02004000000000000380195234567890123456789002A1003B22004C333", ?MARSHALLER_ASCII),
	[0, 2, 55, 56, 57] = erl8583_message:get_fields(Msg),
	"A1" = erl8583_message:get(?RESERVED_ISO1, Msg),
	"B22" = erl8583_message:get(?RESERVED_ISO2, Msg),
	"C333" = erl8583_message:get(?RESERVED_NATIONAL1, Msg).

fields_58_to_63_test() ->
	Msg = erl8583_marshaller:unmarshal("0200400000000000007E195234567890123456789002A1003B22004C333005D4444006E55555007F666666", ?MARSHALLER_ASCII),
	"A1" = erl8583_message:get(?RESERVED_NATIONAL2, Msg),
	"B22" = erl8583_message:get(?RESERVED_NATIONAL3, Msg),
	"C333" = erl8583_message:get(?RESERVED_PRIVATE1, Msg),
	"D4444" = erl8583_message:get(?RESERVED_PRIVATE2, Msg),
	"E55555" = erl8583_message:get(?RESERVED_PRIVATE3, Msg),
	"F666666" = erl8583_message:get(?RESERVED_PRIVATE4, Msg).
	
field_64_test() ->
	Msg = erl8583_marshaller:unmarshal("0200000000000000000180FF01020304057F", ?MARSHALLER_ASCII),
	[0, 64] = erl8583_message:get_fields(Msg),
	<<128, 255, 1, 2, 3, 4, 5, 127>> = erl8583_message:get(?MESSAGE_AUTHENTICATION_CODE, Msg).

field_66_test() ->
	Msg = erl8583_marshaller:unmarshal("0200800000000000000040000000000000001", ?MARSHALLER_ASCII),
	[0, 66] = erl8583_message:get_fields(Msg),
	"1" = erl8583_message:get(66, Msg).

fields_67_to_70_test() ->
	Msg = erl8583_marshaller:unmarshal("020080000000000000003C0000000000000001002003004", ?MARSHALLER_ASCII),
	[0, 67, 68, 69, 70] = erl8583_message:get_fields(Msg),
	"01" = erl8583_message:get(67, Msg),
	"002" = erl8583_message:get(68, Msg),
	"003" = erl8583_message:get(69, Msg),
	"004" = erl8583_message:get(70, Msg).
	
fields_71_72_test() ->
	Msg = erl8583_marshaller:unmarshal("02008000000000000000030000000000000000010002", ?MARSHALLER_ASCII),
	[0, 71, 72] = erl8583_message:get_fields(Msg),
	"0001" = erl8583_message:get(?MESSAGE_NUMBER, Msg),
	"0002" = erl8583_message:get(?MESSAGE_NUMBER_LAST, Msg).

field_73_test() ->
	Msg = erl8583_marshaller:unmarshal("020080000000000000000080000000000000110219", ?MARSHALLER_ASCII),
	[0, 73] = erl8583_message:get_fields(Msg),
	"110219" = erl8583_message:get(?DATE_ACTION, Msg).

field_78_to_82_test() ->
	Msg = erl8583_marshaller:unmarshal("020080000000000000000007C000000000000000000001000000002200000003330000004444000000055555", ?MARSHALLER_ASCII),
	[0, 78, 79, 80, 81, 82] = erl8583_message:get_fields(Msg),
	"0000000001" = erl8583_message:get(?TRANSFER_NUMBER, Msg),
	"0000000022" = erl8583_message:get(?TRANSFER_NUMBER_REVERSAL, Msg),
	"0000000333" = erl8583_message:get(?INQUIRIES_NUMBER, Msg),
	"0000004444" = erl8583_message:get(?AUTHORIZATIONS_NUMBER, Msg),
	"000000055555" = erl8583_message:get(?CREDITS_PROCESSING_FEE_AMOUNT, Msg).
	
fields_91_to_94_test() ->
	Msg = erl8583_marshaller:unmarshal("020080000000000000000000003C00000000A2255555DDDDDDD", ?MARSHALLER_ASCII),
	[0, 91, 92, 93, 94] = erl8583_message:get_fields(Msg),
	"A" = erl8583_message:get(?FILE_UPDATE_CODE, Msg),
	"22" = erl8583_message:get(?FILE_SECURITY_CODE, Msg),
	"55555" = erl8583_message:get(?RESP_INDICATOR, Msg),
	"DDDDDDD" = erl8583_message:get(?SERVICE_INDICATOR, Msg).
	
field_97_test() ->
	Msg = erl8583_marshaller:unmarshal("020080000000000000000000000080000000C0000000000000001", ?MARSHALLER_ASCII),
	[0, 97] = erl8583_message:get_fields(Msg),
	"C0000000000000001" = erl8583_message:get(?AMOUNT_NET_SETTLE, Msg).

field_98_test() ->
	Msg = erl8583_marshaller:unmarshal("020080000000000000000000000040000000Payee                    ", ?MARSHALLER_ASCII),
	[0, ?PAYEE] = erl8583_message:get_fields(Msg),
	"Payee                    " = erl8583_message:get(?PAYEE, Msg).

field_101_test() ->
	Msg = erl8583_marshaller:unmarshal("02008000000000000000000000000800000017A loong file name", ?MARSHALLER_ASCII),
	[0, ?FILE_NAME] = erl8583_message:get_fields(Msg),
	"A loong file name" = erl8583_message:get(?FILE_NAME, Msg).

fields_102_103_104_128_test() ->
	Msg = erl8583_marshaller:unmarshal("02008000000000000000000000000700000104ID 1281234567890123456789012345678009txn desc.0000000000000000", ?MARSHALLER_ASCII),
	[0, ?ACCOUNT_ID1, ?ACCOUNT_ID2, ?TRAN_DESCRIPTION, ?MESSAGE_AUTHENTICATION_CODE2] = erl8583_message:get_fields(Msg),
	<<0, 0, 0, 0, 0, 0, 0, 0>> = erl8583_message:get(?MESSAGE_AUTHENTICATION_CODE2, Msg).

custom_marshaller_test() ->
	Msg = erl8583_marshaller:unmarshal("0200300000000000000034", [{mti_marshaller, ?MODULE}, {field_marshaller, ?MODULE}, {bitmap_marshaller, erl8583_marshaller_ascii}]),
	"Field 3" = erl8583_message:get(3, Msg),
	"Field 4" = erl8583_message:get(4, Msg).

custom_bit_map_test() ->
	Msg = erl8583_marshaller:unmarshal("XB34", [{mti_marshaller, ?MODULE}, {bitmap_marshaller, ?MODULE}, {field_marshaller, ?MODULE}]),
	"Field 3" = erl8583_message:get(3, Msg),
	"Field 4" = erl8583_message:get(4, Msg).
	
