%% This unmarshaller can unmarshal AMEX messages.
-module(example_5_unmarshaller).

-export([unmarshal/1, unmarshal_field/3]).

unmarshal(Marshalled) ->
	% We use the EBCDIC marshaller to unmarshal the MTI,
	%        the binary marshaller to unmarshal the bitmap,
	%        this module unmarshals the data elements
	MarshallingOptions = [{mti_marshaller, erl8583_marshaller_ebcdic},
						  {bitmap_marshaller,  erl8583_marshaller_binary},
						  {field_marshaller,  ?MODULE}],
	erl8583_marshaller:unmarshal(Marshalled, MarshallingOptions).

% We use the binary marshaller to unmarshal binary data elements
% and the EBCDIC marshaller to unmarshal all other data elements.
unmarshal_field(FieldId, Marshalled, EncodingRules) ->
	% We use the encoding rules to get whether a data element is binary
	% or anything else.
	case EncodingRules:get_encoding(FieldId) of
		{b, _, _} ->
			erl8583_marshaller_binary:unmarshal_field(FieldId, Marshalled, EncodingRules);
		{_, _, _} ->
			erl8583_marshaller_ebcdic:unmarshal_field(FieldId, Marshalled, EncodingRules)
	end.
