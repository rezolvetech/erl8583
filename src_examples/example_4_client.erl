%% An example that demonstrates a minimal ISO 8583 client
%% that uses ASCII marshalling.
-module(example_4_client).
-export([test/0]).

test() ->
	% Create a message.
	Msg1 = erl8583_message:new(),
	Msg2 = erl8583_message:set_mti("0800", Msg1),
	Msg3 = erl8583_message:set(3, "300000", Msg2),
	Msg4 = erl8583_message:set(24, "045", Msg3),
	Msg5 = erl8583_message:set(41, "11111111", Msg4),
	Msg6 = erl8583_message:set(42, "222222222222222", Msg5),
	Msg7 = erl8583_message:set(63, "This is a Test Message", Msg6),
	
	% Marshal the message using our custom marshaller and send the result. 
	AsciiMessage = example_4_marshaller:marshal(Msg7),
	{ok, Sock} = gen_tcp:connect("localhost", 8583, [list, {packet, 0}, {active, false}]),
	io:format("Sending:~n~p~n~n", [AsciiMessage]),
	ok = gen_tcp:send(Sock, AsciiMessage),
	
	% Get the response to the request and unmarshal it.
	AsciiResponse = do_recv(Sock, []),
	io:format("Received:~n~s~n", [AsciiResponse]),
	Response = erl8583_marshaller_ascii:unmarshal(AsciiResponse),
	
	% Display the MTI from the response.
	io:format("~nMTI: ~s~n", [erl8583_message:get(0, Response)]).	

% A pretty standard function to read data from a socket.
do_recv(Sock, Bs) ->
	{ok, B} = gen_tcp:recv(Sock, 0),
	UpdatedBs = Bs ++ B,
	if
		% There's a 2 byte length header. Use it to check if
		% we have received the whole response.
		length(UpdatedBs) < 2 ->
			do_recv(Sock, UpdatedBs);
		true ->
			{[Len1, Len2], Rest} = lists:split(2, UpdatedBs),
			Len = Len1 * 256 + Len2 + 2,
			if 
				Len =:= length(UpdatedBs) ->
					% Got the whole response, return the response
					% but not the length header.
					Rest;
				Len < length(UpdatedBs) ->
					% Haven't got all the response data.
					do_recv(Sock, UpdatedBs)
			end
    end.	
