% This module allows us to decode the subfields of field 127 from
% a Postilion node.
-module(example_7_field127_rules).

-export([get_encoding/1]).

% We provide clauses only for field IDs 2, 3, 12, 13, 14 and 20
% since these are the only (sub)fields needed for example 7.
% For completeness we could add clauses for other fields.
get_encoding(2) ->
	{ans, llvar, 32};
get_encoding(3) ->
	{ans, fixed, 48};
get_encoding(12) ->
	{ans, llvar, 25};
get_encoding(13) ->
	{ans, fixed, 17};
get_encoding(14) ->
	{ans, fixed, 8};
get_encoding(20) ->
	{n, fixed, 8}.
