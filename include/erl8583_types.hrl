% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

%% Defines types used by other modules.

%% @type max_length() = integer(). The maximum length of a field in bytes.
-type(max_length() :: integer()).

%% @type field_encoding() = {n|b|an|ans|x_n|ns|z, fixed|llvar|lllvar, max_length()}. 
%% How a field is encoded.
-type(field_encoding() :: {n|b|an|ans|x_n|ns|z, fixed|llvar|lllvar, max_length()}).

%% An attribute of an ISO 8583 message is a {Key, Value} pair.
%%
%% @type iso8583attribute() = {string(), string()}. An attribute expressed as
%% a {Key, Value} pair.
-type(iso8583attribute() :: {string(), string()}).

%% An encapsulation of an ISO 8583 message.
%%
%% @type iso8583message() = #iso8583_message{attributes=[iso8583attribute()], values=dict()}. An 
%% encapsulation of an ISO 8583 message.
-record(iso8583_message, {attributes=[], values=dict:new()}).
-type(iso8583message() :: #iso8583_message{}).

%% Valid types for the field of an ISO 8583 message.
%%she
%% @type iso8583field_value() = string()|binary()|iso8583message(). Valid
%% types for an ISO 8583 field. 
-type(iso8583field_value() :: string()|binary()|iso8583message()).

