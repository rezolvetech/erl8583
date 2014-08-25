%% Author: carl
%% Created: 10 Nov 2011
%% Description: TODO: Add description to test_field_encoders
-module(test_field_encoders).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%
fields_1987_test() ->
	Encoding = erl8583_fields:get_encoding(3),
	Encoding = erl8583_fields:get_encoding([3]).

fields_1993_test() ->
	Encoding = erl8583_fields_1993:get_encoding(12),
	Encoding = erl8583_fields_1993:get_encoding([12]).
	

%%
%% Local Functions
%%

