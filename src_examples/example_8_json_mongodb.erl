%% A module to test JSON marshalling.
-module(example_8_json_mongodb).

-export([test/0]).

test() ->
	% A Postilion message that we want to convert to JSON.
	Marshalled = "30323030F23E049508E0810000000000" ++
					 "04000022313630353730303130353132" ++
					 "32393839383433313030303030303030" ++
					 "30303030303030303130313231333437" ++
					 "33353030303031313133343733353130" ++
					 "31323037313231303131303031303043" ++
					 "30303030303030304330303030303030" ++
					 "30303636323736323930303030303033" ++
					 "38373032303130353730303131303031" ++
					 "2020202020202020202020205A494220" ++
					 "48656164204F66666963652041544D20" ++
					 "202020562F49204C61676F7320202020" ++
					 "30314E47353636303034313531303130" ++
					 "34303930313236363539303135323131" ++
					 "32303132303331343430303230303031" ++
					 "3135601C100000000000313030303030" ++
					 "3338373032305A656E69746841544D73" ++
					 "63725A4942655472616E7A536E6B3030" ++
					 "303030323030303031315A656E697468" ++
					 "54472020202031325A4942655472616E" ++
					 "7A536E6B303132333431303030303120" ++
					 "20203536365A454E4954482042323030" ++
					 "3630393231",
	Message = example_7_unmarshaller:unmarshal(Marshalled),
	JsonDocument = erl8583_marshaller_json:marshal(Message),
	
	% Create a connection to the MongoDB server. We assume that the server is
	% running on the local host on the default port (27017).
	application:start(emongo),
	emongo:add_pool(test_pool, "localhost", 27017, "test", 1),
	
	% Use mochijson and a little munging to get the document
	% into the form that emongo needs it.
	{struct, JsonData} = mochijson2:decode(JsonDocument),
	MongoData = munge_json(JsonData),
	
	% Update the database.
	emongo:insert(test_pool, "erl8583", MongoData).
	
%% Function to munge the mochijson data for emongo.
munge_json({Key, {struct, Value}}) ->
	munge_json({Key, Value});
munge_json({Key, Value}) when is_binary(Value) ->
	{binary_to_list(Key), binary_to_list(Value)};
munge_json({Key, Value}) when is_list(Value) ->
	{binary_to_list(Key), munge_json(Value)};
munge_json([]) ->
	[];
munge_json([H|T]) ->
	[munge_json(H)] ++ munge_json(T).
	