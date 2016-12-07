%% Author: carl
%% Created: 20 Jan 2011
%% Description: TODO: Add description to test_iso8583_message
-module(test_iso8583_message).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include("../include/erl8583_types.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%

%%
%% Tests
%%

%% Check that "new" creates the expected tuple.
new_test() ->
	#iso8583_message{attributes=[]} = erl8583_message:new().

%% Check that we can add a field with ID 0
set_test() ->
	Message = erl8583_message:new(),
	erl8583_message:set(0, "0200", Message).

%% Check that we can't add a field with a negative ID.
set_negative_id_test() ->
	Message = erl8583_message:new(),
	?assertError(_, erl8583_message:set_field(-1, "0200", Message)).

%% Check that we can't add a field more than once.
set_field_twice_test() ->
	Message = erl8583_message:new(),
	Message2 = erl8583_message:set(0, "0200", Message),
	?assertError(_, erl8583_message:set(0, "0210", Message2)).

set_field_not_integer_test() ->
	Message = erl8583_message:new(),
	?assertError(_, erl8583_message:set(foo, "0210", Message)).

set_negative_field_test() ->
	Message = erl8583_message:new(),
	?assertError(_, erl8583_message:set(-1, "0210", Message)).

%% Error should be thrown if we read an unset field.
get_unset_field_test() ->
	Message = erl8583_message:new(),
	?assertError(_, erl8583_message:get(0, Message)).

%% Should be able to read a set/unset field.
find_field_test() ->
	Message = erl8583_message:new(),
	Message2 = erl8583_message:set(0, <<"0200">>, Message),
	?assertEqual(<<"0200">>, erl8583_message:get(0, Message2)),
	?assertEqual({ok, <<"0200">>}, erl8583_message:find(0, Message2)),
	?assertEqual(error, erl8583_message:find(3, Message2)).

%% Should be able to read a set field.
get_field_test() ->
	Message = erl8583_message:new(),
	Message2 = erl8583_message:set(0, "0200", Message),
	?assertEqual("0200", erl8583_message:get(0, Message2)).

%% Test that we can get fields.
get_fields_test() ->
	Message = erl8583_message:new([{mapper, ?MODULE}]),
	Message2 = erl8583_message:set(180, "hello", Message),
	Message3 = erl8583_message:set(0, "0200", Message2),
	?assertEqual([0, 180], erl8583_message:get_fields(Message3)).

%% Test to_list.
to_list_test() ->
	Message = erl8583_message:new(),
	Message2 = erl8583_message:set(0, "0200", Message),
	Message3 = erl8583_message:set(39, 0, Message2),
	[{0, "0200"}, {39, 0}] = erl8583_message:to_list(Message3).

from_list_test() ->
	Message = erl8583_message:from_list([{0, "0200"}, {39, 0}]),
	#iso8583_message{attributes=[]} = Message,
	[{0, "0200"}, {39, 0}] = erl8583_message:to_list(Message).

get_attributes_test() ->
	Message = erl8583_message:new([{"foo", "bar"}, {"hello", "world"}]),
	[{"foo", "bar"}, {"hello", "world"}] = erl8583_message:get_attributes(Message).

set_attributes_test() ->
	Msg = erl8583_message:new(),
	UpdatedMsg = erl8583_message:set_attributes([{"foo", "bar"}, {"hello", "world"}], Msg),
	[{"foo", "bar"}, {"hello", "world"}] = erl8583_message:get_attributes(UpdatedMsg).

update_test() ->
	Msg = erl8583_message:new(),
	UpdatedMsg = erl8583_message:update(3, "foo", Msg),
	"foo" = erl8583_message:get(3, UpdatedMsg),
	ChangedMsg = erl8583_message:update(3, "bar", UpdatedMsg),
	"bar" = erl8583_message:get(3, ChangedMsg).

repeat_test() ->
	Msg = erl8583_message:new(),
	Msg1 = erl8583_message:set(0, "0301", Msg),
	Msg1Rep = erl8583_message:repeat(Msg1),
	"0301" = erl8583_message:get(0, Msg1Rep),
	Msg3 = erl8583_message:set(0, "0203", Msg),
	Msg3Rep = erl8583_message:repeat(Msg3),
	"0203" = erl8583_message:get(0, Msg3Rep),
	Msg5 = erl8583_message:set(0, "0205", Msg),
	Msg5Rep = erl8583_message:repeat(Msg5),
	"0205" = erl8583_message:get(0, Msg5Rep),
	Msg0 = erl8583_message:set(0, "0200", Msg),
	Msg0Rep = erl8583_message:repeat(Msg0),
	"0201" = erl8583_message:get(0, Msg0Rep),
	Msg2 = erl8583_message:set(0, "0402", Msg),
	Msg2Rep = erl8583_message:repeat(Msg2),
	"0403" = erl8583_message:get(0, Msg2Rep),
	Msg4 = erl8583_message:set(0, "0104", Msg),
	Msg4Rep = erl8583_message:repeat(Msg4),
	"0105" = erl8583_message:get(0, Msg4Rep).

clone_fields_test() ->
	Message = erl8583_message:new([{mapper, ?MODULE}]),
	Message2 = erl8583_message:set(10, "hello", Message),
	Message3 = erl8583_message:set(0, "0200", Message2),
	Message4 = erl8583_message:set(2, "hello2", Message3),
	Clone1 = erl8583_message:clone_fields([0, 2, 10], Message4),
	[0, 2, 10] = erl8583_message:get_fields(Clone1),
	Clone2 = erl8583_message:clone_fields([0, 10], Message4),
	[0, 10] = erl8583_message:get_fields(Clone2),
	"hello" = erl8583_message:get(10, Clone2),
	"0200" = erl8583_message:get(0, Clone2).

response_1_test() ->
	Message = erl8583_message:new([{mapper, ?MODULE}]),
	Message2 = erl8583_message:set(10, "hello", Message),
	Message3 = erl8583_message:set(0, "0200", Message2),
	Message4 = erl8583_message:set(2, "hello2", Message3),
	Response1 = erl8583_message:response([0, 10], Message4),
	[0, 10] = erl8583_message:get_fields(Response1),
	"hello" = erl8583_message:get(10, Response1),
	"0210" = erl8583_message:get(0, Response1),
	Response2 = erl8583_message:response([2, 10], Message4),
	[0, 2, 10] = erl8583_message:get_fields(Response2),
	"hello2" = erl8583_message:get(2, Response2),
	"0210" = erl8583_message:get(0, Response2).

response_2_test() ->
	Message = erl8583_message:new([{mapper, ?MODULE}]),
	Message2 = erl8583_message:set(10, "hello", Message),
	Message3 = erl8583_message:set(0, "0220", Message2),
	Message4 = erl8583_message:set(2, "hello2", Message3),
	Response = erl8583_message:response(Message4),
	[0, 2, 10] = erl8583_message:get_fields(Response),
	"hello2" = erl8583_message:get(2, Response),
	"0230" = erl8583_message:get(0, Response).

% response shouldn't use the keep the repeat digit.
response_3_test() ->
	Message = erl8583_message:new([{mapper, ?MODULE}]),
	Message2 = erl8583_message:set(10, "hello", Message),
	Message3 = erl8583_message:set(0, "0221", Message2),
	Response = erl8583_message:response(Message3),
	[0, 10] = erl8583_message:get_fields(Response),
	"hello" = erl8583_message:get(10, Response),
	"0230" = erl8583_message:get(0, Response).

% response shouldn't use the keep the repeat digit.
response_4_test() ->
	Message = erl8583_message:new([{mapper, ?MODULE}]),
	Message2 = erl8583_message:set(0, "0423", Message),
	Response = erl8583_message:response(Message2),
	[0] = erl8583_message:get_fields(Response),
	"0432" = erl8583_message:get(0, Response).

remove_fields_test() ->
	Message = erl8583_message:new([{mapper, ?MODULE}]),
	Message2 = erl8583_message:set(10, "hello", Message),
	Message3 = erl8583_message:set(0, "0220", Message2),
	Message4 = erl8583_message:set(2, "hello2", Message3),
	Message5 = erl8583_message:set(3, "hello3", Message4),
	UpdatedMessage = erl8583_message:remove_fields([2, 3], Message5),
	[0, 10] = erl8583_message:get_fields(UpdatedMessage),
	"hello" = erl8583_message:get(10, UpdatedMessage),
	"0220" = erl8583_message:get(0, UpdatedMessage).

set_list_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set(4, "baz", Message1),
	Fields = [{2, "foo"}, {3, "bar"}],
	Message3 = erl8583_message:set(Fields, Message2),
	[2, 3, 4] = erl8583_message:get_fields(Message3),
	"foo" = erl8583_message:get(2, Message3),
	?assertError(_, erl8583_message:set(Fields, Message3)).

update_list_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set(4, "baz", Message1),
	Fields1 = [{2, "foo"}, {3, "bar"}],
	Message3 = erl8583_message:update(Fields1, Message2),
	[2, 3, 4] = erl8583_message:get_fields(Message3),
	"foo" = erl8583_message:get(2, Message3),
	Fields2 = [{2, "foobar"}, {5, "baz"}],
	Message4 = erl8583_message:update(Fields2, Message3),
	[2, 3, 4, 5] = erl8583_message:get_fields(Message4),
	"foobar" = erl8583_message:get(2, Message4).

get_list1_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set(4, "baz", Message1),
	"baz" = erl8583_message:get([4], Message2),
	?assertError(_, erl8583_message:get([[4]], Message2)).

get_list2_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set(4, "baz", erl8583_message:new()),
	Message3 = erl8583_message:set(2, Message2, Message1),
	Message2 = erl8583_message:get([2], Message3),
	"baz" = erl8583_message:get([2, 4], Message3).

set_list1_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set([4], "baz", Message1),
	"baz" = erl8583_message:get([4], Message2),
	"baz" = erl8583_message:get(4, Message2).

set_list2_test() ->
	Message1 = erl8583_message:set(5, erl8583_message:new(), erl8583_message:new()),
	Message2 = erl8583_message:set([5, 2], "foobar", Message1),
	"foobar" = erl8583_message:get([5, 2], Message2).

update_list1_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set([4], "baz", Message1),
	Message3 = erl8583_message:update([4], "foobar", Message2),
	"foobar" = erl8583_message:get([4], Message3).

update_list2_test() ->
	Message1 = erl8583_message:set(5, erl8583_message:new(), erl8583_message:new()),
	Message2 = erl8583_message:set([5, 2], "foobar", Message1),
	"foobar" = erl8583_message:get([5, 2], Message2),
	?assertError(_, erl8583_message:set([5, 2], "foobar2", Message2)),
	Message3 = erl8583_message:update([5, 2], "foobar2", Message2),
	"foobar2" = erl8583_message:get([5, 2], Message3).

set_list3_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set([5, 2], "foo", Message1),
	Message3 = erl8583_message:set([5, 3, 1], "bar", Message2),
	SubMessage = erl8583_message:get(5, Message3),
	[2, 3] = erl8583_message:get_fields(SubMessage),
	"foo" = erl8583_message:get(2, SubMessage),
	"bar" = erl8583_message:get([3, 1], SubMessage).

update_list3_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:update([5, 2], "foo", Message1),
	Message3 = erl8583_message:update([5, 3, 1], "bar", Message2),
	SubMessage = erl8583_message:get(5, Message3),
	[2, 3] = erl8583_message:get_fields(SubMessage),
	"foo" = erl8583_message:get(2, SubMessage),
	"bar" = erl8583_message:get([3, 1], SubMessage).

get_numeric_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:update([5, 2], "0123", Message1),
	Message3 = erl8583_message:update(6, "77", Message2),
	123 = erl8583_message:get_numeric([5, 2], Message3),
	77 = erl8583_message:get_numeric(6, Message3).

set_numeric_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set_numeric([5, 2], 123, 12, Message1),
	Message3 = erl8583_message:set_numeric(6, 77, 2, Message2),
	"000000000123" = erl8583_message:get([5, 2], Message3),
	"77" = erl8583_message:get(6, Message3).

update_numeric_test() ->
	Message1 = erl8583_message:new(),
	Message2 = erl8583_message:set_numeric([5, 2], 123, 12, Message1),
	Message3 = erl8583_message:update_numeric([5, 2], 77, 3, Message2),
	"077" = erl8583_message:get([5, 2], Message3).

is_message_test() ->
	false = erl8583_message:is_message(3),
	true = erl8583_message:is_message(#iso8583_message{}).

clone_with_attributes_test() ->
	Msg1 = erl8583_message:new([{"foo", "bar"}]),
	Msg2 = erl8583_message:clone_fields([], Msg1),
	[{"foo", "bar"}] = erl8583_message:get_attributes(Msg2).

get_attribute_test() ->
	Msg = erl8583_message:new([{"foo", "bar"}, {"bar", "baz"}]),
	"bar" = erl8583_message:get_attribute("foo", Msg),
	"baz" = erl8583_message:get_attribute("bar", Msg).

set_attribute_test() ->
	Msg1 = erl8583_message:new([{"foo", "bar"}, {"bar", "baz"}]),
	Msg2 = erl8583_message:set_attribute("baz", "3", Msg1),
	"3" = erl8583_message:get_attribute("baz", Msg2),
	?assertError(_, erl8583_message:set_attribute("bar", "3", Msg2)).

update_attribute_test() ->
	Msg1 = erl8583_message:new([{"foo", "bar"}, {"bar", "baz"}]),
	Msg2 = erl8583_message:update_attribute("baz", "3", Msg1),
	Msg3 = erl8583_message:update_attribute("foo", "3", Msg2),
	"3" = erl8583_message:get_attribute("baz", Msg3),
	"3" = erl8583_message:get_attribute("foo", Msg3).

delete_attribute_test() ->
	Msg1 = erl8583_message:new([{"foo", "bar"}, {"bar", "baz"}]),
	Msg2 = erl8583_message:delete_attribute("bar", Msg1),
	"baz" = erl8583_message:get_attribute("bar", Msg1),
	?assertError(_, erl8583_message:get_attribute("bar", Msg2)).
