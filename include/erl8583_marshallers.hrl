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

% Defines macros for popular ways to marshal ISO 8583 messages.
-define(MARSHALLER_ASCII, [{field_marshaller, erl8583_marshaller_ascii},
						   {bitmap_marshaller, erl8583_marshaller_ascii},
						   {mti_marshaller, erl8583_marshaller_ascii},
						   {end_marshaller, erl8583_marshaller_ascii}]).

-define(MARSHALLER_BINARY, [{field_marshaller, erl8583_marshaller_binary}, 
						    {bitmap_marshaller, erl8583_marshaller_binary},
						    {mti_marshaller, erl8583_marshaller_binary},
						    {end_marshaller, erl8583_marshaller_binary}]).


-define(MARSHALLER_EBCDIC, [{field_marshaller, erl8583_marshaller_ebcdic},
							{bitmap_marshaller, erl8583_marshaller_ebcdic},
							{mti_marshaller, erl8583_marshaller_ebcdic},
							{end_marshaller, erl8583_marshaller_ebcdic}]).

-define(MARSHALLER_JSON, [{field_marshaller, erl8583_marshaller_json}, 
						  {bitmap_marshaller, erl8583_marshaller_json},
						  {mti_marshaller, erl8583_marshaller_json},
						  {init_marshaller, erl8583_marshaller_json},
						  {end_marshaller, erl8583_marshaller_json}]).

-define(MARSHALLER_XML, [{field_marshaller, erl8583_marshaller_xml}, 
						 {bitmap_marshaller, erl8583_marshaller_xml},
						 {mti_marshaller, erl8583_marshaller_xml},
						 {init_marshaller, erl8583_marshaller_xml},
						 {end_marshaller, erl8583_marshaller_xml}]).
