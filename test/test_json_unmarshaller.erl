%% Author: carl
%% Created: 06 Nov 2011
%% Description: TODO: Add description to test_json_unmarshaller
-module(test_json_unmarshaller).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("erl8583_marshallers.hrl").

%%
%% Exported Functions
%%
-export([get_encoding/1]).

%%
%% API Functions
%%
get_encoding([126,3]) ->
	{b, fixed, 3};
get_encoding([126,4,2]) ->
	{ans, llvar, 13};
get_encoding([126,4,3]) ->
	{ans, llvar, 13};
get_encoding([127,2]) ->
	{n, llvar, 20}.

mti_unmarshal_test() ->
	Msg = "{\"fields\" : {\"0\" : \"0210\"}}",
	{"0210", Msg} = erl8583_marshaller_json:unmarshal_mti(Msg),
	Msg2 = "{\"fields\" : {\"0\" : \"0200\"}}",
	{"0200", Msg2} = erl8583_marshaller_json:unmarshal_mti(Msg2).

bitmap_unmarshal_1_test() ->
	Msg = "{\"fields\" : {\"1\" : \"hello\", \"127\" : \"good bye\"}}",
	{[1, 127], Msg} = erl8583_marshaller_json:unmarshal_bitmap(Msg).

bitmap_unmarshal_2_test() ->
	Msg = "{\"fields\" : {\"0\" : \"hello\", \"126\" : {\"0\" : \"good bye\"}}}",
	{[126], Msg} = erl8583_marshaller_json:unmarshal_bitmap(Msg).

field_unmarshal_test() ->
	Msg = "{\"fields\" : {\"0\" : \"0200\", \"2\" : \"12345678\"}}",
	{"12345678", Msg, []} = erl8583_marshaller_json:unmarshal_field(2, Msg, erl8583_fields).

binary_field_unmarshal_test() ->
	Msg = "{\"fields\" : {\"0\" : \"0200\", \"1\" : \"1234567890\", \"64\" : \"0011A0FF000000AA\"}}",
	{<<0,17,160,255,0,0,0,170>>, Msg, []} = erl8583_marshaller_json:unmarshal_field(64, Msg, erl8583_fields).
	
complex_message_test() ->
	Msg = "{\"fields\" : {\"0\" : \"0200\", \"127\" : {\"2\" : \"13579\"}}}",
	{Field127, Msg, []} = erl8583_marshaller_json:unmarshal_field(127, Msg, ?MODULE),
	true = erl8583_message:is_message(Field127),
	"13579" = erl8583_message:get(2, Field127).

complex_message2_test() ->
	Msg = "{\"fields\" : {\"0\" : \"0200\", \"126\" : {\"3\" : \"a100fe\"}}}",
	{Field126, Msg, []} = erl8583_marshaller_json:unmarshal_field(126, Msg, ?MODULE),
	true = erl8583_message:is_message(Field126),
	<<161,0,254>> = erl8583_message:get(3, Field126).

complex_message3_test() ->
	Msg = "{\"fields\" : {\"0\" : \"0200\", \"126\" : {\"3\" : \"a100fe\", \"4\" : {\"3\" : \"hello\", \"2\" : \"good bye\"}}}}",
	{Field126, Msg, []} = erl8583_marshaller_json:unmarshal_field(126, Msg, ?MODULE),
	true = erl8583_message:is_message(Field126),
	<<161,0,254>> = erl8583_message:get(3, Field126),
	[2,3] = erl8583_message:get_fields(erl8583_message:get(4, Field126)),
	"hello" = erl8583_message:get([4,3], Field126),
	"good bye" = erl8583_message:get([4,2], Field126).

unmarshal_init_no_attrs_test() ->
	Marshalled = "{\"fields\" : {\"0\" : \"0200\", \"2\" : \"1234567890\", \"64\" : \"0011A0FF000000AA\"}}",
	Message = erl8583_message:new(),
	{Message, Marshalled} = erl8583_marshaller_json:unmarshal_init(Message, Marshalled).

unmarshal_init_with_attrs_test() ->
	Marshalled = "{\"attributes\" : {\"foo\" : \"bar\"}, \"fields\" : {\"0\" : \"0200\", \"2\" : \"1234567890\", \"64\" : \"0011A0FF000000AA\"}}",
	Message = erl8583_message:new(),
	{Message2, Marshalled} = erl8583_marshaller_json:unmarshal_init(Message, Marshalled),
	[{"foo", "bar"}] = erl8583_message:get_attributes(Message2).

	
unmarshal_message_test() ->
	Msg = "{\"fields\" : {\"0\" : \"0200\", \"2\" : \"1234567890\", \"64\" : \"0011A0FF000000AA\"}}",
	{[2,64], Msg} = erl8583_marshaller_json:unmarshal_bitmap(Msg),
	Unmarshalled = erl8583_marshaller_json:unmarshal(Msg),
	"1234567890" = erl8583_message:get(2, Unmarshalled),
	<<0,17,160,255,0,0,0,170>> = erl8583_message:get(64, Unmarshalled).

	