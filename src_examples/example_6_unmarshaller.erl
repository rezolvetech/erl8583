% This unmarshaller can unmarshal a Postilion message with an
% unusual encoding of field 127.
-module(example_6_unmarshaller).

-export([unmarshal/1, unmarshal_init/2, unmarshal_field/3]).

% Use:
%      The ASCII marshaller to unmarshal the MTI
%      The binary marshaller to unmarshal the bitmap
%      This module to unmarshal data elements
% Also call the unmarshal_init method of this module before
% trying to unmarshal the message.
unmarshal(Marshalled) ->
	MarshallingOptions = [{mti_marshaller, erl8583_marshaller_ascii},
						  {bitmap_marshaller, erl8583_marshaller_binary},
						  {field_marshaller, ?MODULE},
						  {init_marshaller, ?MODULE},
						  {end_marshaller, erl8583_marshaller_ascii}],
	erl8583_marshaller:unmarshal(Marshalled, MarshallingOptions).

% The marshalled message is encoded as ASCII hex. Convert the ASCII hex
% to a list of bytes before unmarshalling further.
unmarshal_init(Message, Marshalled) ->
	MarshalledBin = erl8583_convert:ascii_hex_to_binary_list(Marshalled),
	{Message, MarshalledBin}.

% This function is called when a data element needs to be unmarshalled.
%
% Special handling for field 127.
% The length of field 127 is encoded in 6 bytes rather than 3.
unmarshal_field(127, Marshalled, _EncodingRules) ->
	{LenStr, Rest} = lists:split(6, Marshalled),
	Len = list_to_integer(LenStr),
	{Value, MarshalledRem} = lists:split(Len, Rest),
	{Value, MarshalledRem, []};
% Use the binary marshaller to unmarshal binary fields and
% the ASCII marshaller to unmarshal all other fields.
unmarshal_field(FieldId, Marshalled, EncodingRules) ->
	case EncodingRules:get_encoding(FieldId) of
		{b, _, _} ->
			erl8583_marshaller_binary:unmarshal_field(FieldId, Marshalled, EncodingRules);
		{_, _, _} ->
			erl8583_marshaller_ascii:unmarshal_field(FieldId, Marshalled, EncodingRules)
	end.
