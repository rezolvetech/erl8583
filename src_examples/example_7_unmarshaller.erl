% This unmarshaller can unmarshal a Postilion message with an
% unusual encoding of field 127.
-module(example_7_unmarshaller).

-export([unmarshal/1, unmarshal_init/2, unmarshal_field/3]).

unmarshal(Marshalled) ->
	MarshallingOptions = [{mti_marshaller, erl8583_marshaller_ascii},
						  {bitmap_marshaller, erl8583_marshaller_binary},
						  {field_marshaller, ?MODULE},
						  {init_marshaller, ?MODULE},
						  {end_marshaller, erl8583_marshaller_ascii}],
	erl8583_marshaller:unmarshal(Marshalled, MarshallingOptions).

unmarshal_init(Message, Marshalled) ->
	MarshalledBin = erl8583_convert:ascii_hex_to_binary_list(Marshalled),
	{Message, MarshalledBin}.

% This function is called when a data element needs to be unmarshalled.
%
% Special handling for field 127.
% The length of field 127 is encoded in 6 bytes rather than 3.
% After extracting the value of field 127 we unmarshal it since
% it contains subfields.
unmarshal_field(127, Marshalled, _EncodingRules) ->
	{LenStr, Rest} = lists:split(6, Marshalled),
	Len = list_to_integer(LenStr),
	{Value, MarshalledRem} = lists:split(Len, Rest),
	
	% Notice:
	% 1. That we need to specify how the subfields of field 127 are
	%    encoded.
	% 2. We don't specify a marshaller for the MTI (since there isn't
	%    a message type).
	MarshallingOptions = [{bitmap_marshaller, erl8583_marshaller_binary},
						  {field_marshaller, ?MODULE},
						  {encoding_rules, example_7_field127_rules}],
	Message127 = erl8583_marshaller:unmarshal(Value, MarshallingOptions),
	
	% Return the unmarshalled message as the value of field 127.
	{Message127, MarshalledRem, []};

% Use the binary marshaller to unmarshal binary fields and
% the ASCII marshaller to unmarshal all other fields.
unmarshal_field(FieldId, Marshalled, EncodingRules) ->
	case EncodingRules:get_encoding(FieldId) of
		{b, _, _} ->
			erl8583_marshaller_binary:unmarshal_field(FieldId, Marshalled, EncodingRules);
		{_, _, _} ->
			erl8583_marshaller_ascii:unmarshal_field(FieldId, Marshalled, EncodingRules)
	end.
