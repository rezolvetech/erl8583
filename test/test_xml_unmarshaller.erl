%% Author: carl
%% Created: 29 Jan 2011
%% Description: TODO: Add description to test_xml_unmarshaller
-module(test_xml_unmarshaller).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("erl8583_marshallers.hrl").

%%
%% Exported Functions
%%
-export([foo/0]).

%%
%% API Functions
%%
xml_unmarshal_test() ->
	XmlMessage = "<isomsg>" ++
					 "<field id=\"0\" value=\"0800\"/>" ++
					 "<field id=\"3\" value=\"333333\"/>" ++
					 "</isomsg>",
	Message = erl8583_marshaller:unmarshal(XmlMessage, ?MARSHALLER_XML),
	?assertEqual("0800", erl8583_message:get(0, Message)),
	?assertEqual("333333", erl8583_message:get(3, Message)),
	?assertEqual([0, 3], erl8583_message:get_fields(Message)).
	
xml_unmarshal_with_text_test() ->
	XmlMessage = "<isomsg>Some text" ++
					 "<field value=\"0800\" id=\"0\">more text</field>" ++
					 "<field id=\"3\" value=\"333333\"/>" ++
					 "</isomsg>",
	Message = erl8583_marshaller:unmarshal(XmlMessage, ?MARSHALLER_XML),
	?assertEqual("0800", erl8583_message:get(0, Message)),
	?assertEqual("333333", erl8583_message:get(3, Message)),
	?assertEqual([0, 3], erl8583_message:get_fields(Message)).
	
xml_unmarshal_complex_test() ->
	XmlMessage = "<isomsg>" ++
      "<field id=\"0\" value=\"0810\"/>" ++
      "<field id=\"3\" value=\"333333\"/>" ++
      "<field id=\"39\" value=\"00\"/>" ++
      "<isomsg id=\"48\">" ++
        "<field id=\"1\" value=\"hello\"/>" ++
      "</isomsg>" ++
    "</isomsg>",
	Message = erl8583_marshaller:unmarshal(XmlMessage, ?MARSHALLER_XML),
	?assertEqual("0810", erl8583_message:get(0, Message)),
	?assertEqual("333333", erl8583_message:get(3, Message)),
	?assertEqual("00", erl8583_message:get(39, Message)),
	?assertEqual([0, 3, 39, 48], erl8583_message:get_fields(Message)),
	BitMap = erl8583_message:get(48, Message),
	[1] = erl8583_message:get_fields(BitMap),
	"hello" = erl8583_message:get(1, BitMap).

xml_unmarshal_with_attributes_test() ->
	Message = erl8583_marshaller:unmarshal("<isomsg foo=\"bar\"""><field id=\"0\" value=\"0100\"/></isomsg>", ?MARSHALLER_XML),
	[{"foo", "bar"}] = erl8583_message:get_attributes(Message).

xml_unmarshal_with_attributes2_test() ->
	Message = erl8583_marshaller:unmarshal("<isomsg><field id=\"0\" value=\"0200\"/><isomsg id=\"48\" foo=\"bar\"""/></isomsg>", ?MARSHALLER_XML),
	Field = erl8583_message:get(48, Message),
	[{"foo", "bar"}] = erl8583_message:get_attributes(Field).
	
xml_unmarshal_complex2_test() ->
	Message = erl8583_marshaller:unmarshal("<isomsg><field id=\"0\" value=\"0100\"/><isomsg id=\"48\"><isomsg id=\"105\" baz=\"hello\"""/></isomsg></isomsg>", ?MARSHALLER_XML),
	[0, 48] = erl8583_message:get_fields(Message),
	Message2 = erl8583_message:get(48, Message),
	[105] = erl8583_message:get_fields(Message2),
	Message3 = erl8583_message:get(105, Message2),
	[] = erl8583_message:get_fields(Message3),
	[{"baz", "hello"}] = erl8583_message:get_attributes(Message3).
	
foo() ->
	{ok, Sock} = gen_tcp:connect("localhost", 8000, [list, {packet, 0}, {active, true}]),
	IsoMsg1 = erl8583_message:new(),
	IsoMsg2 = erl8583_message:set(0, "0800", IsoMsg1),
	IsoMsg3 = erl8583_message:set(3, "333333", IsoMsg2),
	Field = erl8583_message:new(),
	Field2 = erl8583_message:set(1, "help", Field),
	IsoMsg4 = erl8583_message:set(40, Field2, IsoMsg3),
	Marshalled = erl8583_marshaller_xml:marshal(IsoMsg4),
	ok = gen_tcp:send(Sock, Marshalled),
	ok = gen_tcp:send(Sock, "\n").
	%receive {tcp, _, IsoResp} -> IsoResp end,
	%IsoResp1 = erl8583_marshaller_xml:unmarshal(IsoResp),
	%erl8583_message:get(39, IsoResp1).	

unmarshal_wrapping_test() ->
	Marshalled = "<isomsg foo=\"bar\" baz=\"2\"></isomsg>",
	{Message, Marshalled} = erl8583_marshaller_xml:unmarshal_init(erl8583_message:new(), Marshalled),
	2 = length(erl8583_message:get_attributes(Message)),
	[] = erl8583_message:get_attributes(Message) -- [{"foo", "bar"}, {"baz", "2"}].

unmarshal_bitmap_test() ->
	Marshalled = "<isomsg><field id=\"0\" value=\"3\"/><!-- hello --><field id=\"2\" value=\"3\"/><field id=\"50\" value=\"3\"/></isomsg>",
	{[2, 50], Marshalled} = erl8583_marshaller_xml:unmarshal_bitmap(Marshalled).
	
unmarshal_field_test() ->
	Marshalled = "<isomsg>" ++
      "<field id=\"0\" value=\"0810\"/>" ++
      "<field id=\"3\" value=\"333333\"/>" ++
      "<field id=\"39\" value=\"00\"/>" ++
      "<field id=\"40\" value=\"12A7F\" type=\"binary\"/>" ++
      "<isomsg id=\"48\">" ++
        "<field id=\"1\" value=\"hello\"/>" ++
      "</isomsg>" ++
    "</isomsg>",
	?assertError(_, erl8583_marshaller_xml:unmarshal_field(7, Marshalled, undefined)),
	{"0810", Marshalled, []} = erl8583_marshaller_xml:unmarshal_field(0, Marshalled, undefined),
	{<<1, 42, 127>>, Marshalled, []} = erl8583_marshaller_xml:unmarshal_field(40, Marshalled, undefined),
	{Msg, Marshalled, []} = erl8583_marshaller_xml:unmarshal_field(48, Marshalled, undefined),
	[1] = erl8583_message:get_fields(Msg),
	"hello" = erl8583_message:get(1, Msg).

unmarshal_mti_test() ->
	Marshalled = "<isomsg>" ++
      "<field id=\"0\" value=\"0810\"/>" ++
      "<field id=\"3\" value=\"333333\"/>" ++
      "<field id=\"39\" value=\"00\"/>" ++
      "<field id=\"40\" value=\"12A7F\" type=\"binary\"/>" ++
      "<isomsg id=\"48\">" ++
        "<field id=\"1\" value=\"hello\"/>" ++
      "</isomsg>" ++
    "</isomsg>",
	{"0810", Marshalled} = erl8583_marshaller_xml:unmarshal_mti(Marshalled).


%%
%% Local Functions
%%

