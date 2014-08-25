%% An example that demonstrates setting and getting data elements in an
%% iso8583message().
-module(example_1_message).

%%
%% Include files
%%
-include_lib("erl8583/include/erl8583_field_ids.hrl").

%%
%% Exported Functions
%%
-export([test/0]).

%%
%% API Functions
%%
test() ->
	% Create a message.
	Msg1 = erl8583_message:new(),
	
	% Set the MTI (field 0), card acceptor name/location (field 43), a MAC (field 64) and
	% a private use field (field 127) whose contents is not defined by ISO.
	% This code shows how a data element can be a numeric value (field 0), a string
	% (field 43), a binary (field 64) or another message (field 127) containing 
	% subfields (fields 127.2 and 127.12). 
	Msg2 = erl8583_message:set_mti("0200", Msg1),
	Msg3 = erl8583_message:set(?CARD_ACCEPTOR_NAME_LOCATION, "ZIB Head Office ATM    V/I Lagos    01NG", Msg2),
	Msg4 = erl8583_message:set(?MESSAGE_AUTHENTICATION_CODE, <<1,2,3,4,5,6,7,8>>, Msg3),
	% Field 127 is a message containing subfields 127.2 and 127.12.
	% Notice that we use two different ways of setting the value of a subfield.
	% The way we set subfield 127.12 is the preferred idiom.
	SubMsg1 = erl8583_message:new(),
	SubMsg2 = erl8583_message:set(2, "0000387020", SubMsg1),
	Msg5 = erl8583_message:set(127, SubMsg2, Msg4),
	Msg6 = erl8583_message:set([127, 12], "ZIBeTranzSnk", Msg5),
	
	% Display the fields defined for the message.
	io:format("Fields: ~w~n", [erl8583_message:get_fields(Msg6)]),
	
	% Display fields 0, 43, 64, 127.2 and 127.12.
	io:format("MTI:                         ~s~n", [erl8583_message:get(0, Msg6)]),
	io:format("Card acceptor name/location: ~s~n", [erl8583_message:get(43, Msg6)]),
	io:format("MAC:                         ~p~n", [erl8583_message:get(64, Msg6)]),
	io:format("Field 127.2:                 ~s~n", [erl8583_message:get([127, 2], Msg6)]),
	io:format("Field 127.12:                ~s~n", [erl8583_message:get([127, 12], Msg6)]).
