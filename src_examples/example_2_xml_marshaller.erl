%% An example that demonstrates marshalling an
%% iso8583message().
-module(example_2_xml_marshaller).

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
	
	% Set some fields. 
	Msg2 = erl8583_message:set_mti("0200", Msg1),
	Msg3 = erl8583_message:set(?CARD_ACCEPTOR_NAME_LOCATION, "ZIB Head Office ATM    V/I Lagos    01NG", Msg2),
	Msg4 = erl8583_message:set(?MESSAGE_AUTHENTICATION_CODE, <<1,2,3,4,5,6,7,8>>, Msg3),
	Msg5 = erl8583_message:set([127, 2], "0000387020", Msg4),
	Msg6 = erl8583_message:set([127, 12], "ZIBeTranzSnk", Msg5),
	
	% Marshal the message and display it.
	Marshalled = erl8583_marshaller_xml:marshal(Msg6),
	io:format("~s~n", [Marshalled]).
