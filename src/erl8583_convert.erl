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

%% @author CA Meijer
%% @copyright 2011 CA Meijer
%% @doc This module provides a number of functions for converting
%%      between representations of data common in ISO 8583.
%%      Particular data representations are ASCII and EBCDIC strings,
%%      BCD encoding and the encoding used for track 2 data.
-module(erl8583_convert).

%%
%% Exported Functions
%%
-export([string_to_ascii_hex/1, 
		 ascii_hex_to_string/1, 
		 integer_to_string/2, 
		 pad_with_trailing_spaces/2,
		 binary_to_ascii_hex/1,
		 binary_list_to_ascii_hex/1,
		 ascii_hex_to_binary/1,
		 ascii_hex_to_binary_list/1,
		 integer_to_bcd/2,
		 ascii_hex_to_bcd/2,
		 bcd_to_integer/1,
		 bcd_to_ascii_hex/3,
		 track2_to_string/2,
		 string_to_track2/1,
		 ascii_hex_to_digit/1,
		 digit_to_ascii_hex/1,
		 strip_trailing_spaces/1,
		 strip_leading_zeroes/1,
		 ascii_to_ebcdic/1,
		 ebcdic_to_ascii/1,
		 list_to_bitmap/2,
		 bitmap_to_list/2]).

%%
%% API Functions
%%

%% @doc Converts a string of characters to a list containing
%%      the ASCII hex character codes.
%%
%% @spec string_to_ascii_hex(string()) -> string()
-spec(string_to_ascii_hex(string()) -> string()).

string_to_ascii_hex(Str) ->
	string_to_ascii_hex(Str, []).

%% @doc Converts a string containing ASCII hex characters
%%      to an equivalent ASCII string().
%%
%% @spec ascii_hex_to_string(string()) -> string()
-spec(ascii_hex_to_string(string()) -> string()).

ascii_hex_to_string(HexStr) ->
	ascii_hex_to_string(HexStr, []).

%% @doc Converts an integer to an ASCII string of fixed length with
%%      leading zeroes if necessary.
%%
%% @spec integer_to_string(integer(), integer()) -> string()
-spec(integer_to_string(integer(), integer()) -> string()).

integer_to_string(IntValue, Length) ->
	pad_with_zeroes(Length, integer_to_list(IntValue)).

%% @doc Pads an ASCII string with a number of spaces so that the
%%      resultant string has specified length.
%%
%% @spec pad_with_trailing_spaces(string(), integer()) -> string()
-spec(pad_with_trailing_spaces(string(), integer()) -> string()).

pad_with_trailing_spaces(Str, Length) ->
	lists:reverse(pad_with_leading_spaces(lists:reverse(Str), Length)).

%% @doc Returns the ASCII hex encoding of a binary value.
%%
%% @spec binary_to_ascii_hex(binary()) -> string()
-spec(binary_to_ascii_hex(binary()) -> string()).

binary_to_ascii_hex(BinValue) ->
	binary_to_ascii_hex(binary_to_list(BinValue), []).

%% @doc Returns the ASCII hex encoding of a list of bytes.
%%
%% @spec binary_list_to_ascii_hex(binary()) -> string()
-spec(binary_list_to_ascii_hex(list(byte())) -> string()).

binary_list_to_ascii_hex(BinList) ->
	binary_to_ascii_hex(BinList, []).

%% @doc Returns the binary value corresponding to an ASCII hex string.
%%
%% @spec ascii_hex_to_binary(string()) -> binary()
-spec(ascii_hex_to_binary(string()) -> binary()).

ascii_hex_to_binary(HexStr) ->
	list_to_binary(ascii_hex_to_binary_list(HexStr)).
	
%% @doc Returns a binary list corresponding to an ASCII hex string.
%%
%% @spec ascii_hex_to_binary_list(string()) -> list(byte())
-spec(ascii_hex_to_binary_list(string()) -> list(byte())).

ascii_hex_to_binary_list(HexStr) ->
	case length(HexStr) rem 2 of
		0 ->
			ascii_hex_to_bytes(HexStr, []);
		1 ->
			ascii_hex_to_bytes([$0|HexStr], [])
	end.
	
%% @doc Converts an integer to a list of specified length 
%%      of BCD encoded bytes.
%%
%% @spec integer_to_bcd(integer(), integer()) -> list(byte())
-spec(integer_to_bcd(integer(), integer()) -> list(byte())).

integer_to_bcd(IntValue, Length) ->
	integer_to_bcd(IntValue, Length, []).

%% @doc Converts an ASCII hex encoded string to a list of BCD
%%      encoded bytes ()padded with a specified padding character
%%      if the string has odd length).
%%
%% @spec ascii_hex_to_bcd(string(), char()) -> list(byte())
-spec(ascii_hex_to_bcd(string(), char()) -> list(byte())).

ascii_hex_to_bcd(HexStr, PaddingChar) when length(HexStr) rem 2 =:= 1 ->
	ascii_hex_to_bcd(HexStr ++ PaddingChar, PaddingChar);
ascii_hex_to_bcd(HexStr, _PaddingChar) ->
	ascii_hex_to_bcd2(HexStr, []).	
	
%% @doc Converts a list of BCD encoded bytes to an integer.
%%
%% @spec bcd_to_integer(list(byte())) -> integer()
-spec(bcd_to_integer(list(byte())) -> integer()).

bcd_to_integer(BcdList) ->
	F = fun(Value, Acc) ->
				Dig1 = Value div 16,
				Dig2 = Value rem 16,
				100 * Acc + 10 * Dig1 + Dig2
		end,
	lists:foldl(F, 0, BcdList).

%% @doc Converts a BCD encoding of a value of specified length (possibly
%%      padded with a padding character) to an ASCII hex string.
%%
%% @spec bcd_to_ascii_hex(list(byte()), integer(), char()) -> string()
-spec(bcd_to_ascii_hex(list(byte()), integer(), char()) -> string()).

bcd_to_ascii_hex(BcdList, Length, PaddingChar) when length(BcdList) =:= (Length + 1) div 2 ->
	IntValue = bcd_to_integer(BcdList),
	case Length rem 2 of
		0 ->
			integer_to_string(IntValue, Length);
		1 ->
			StrippedValue = IntValue - ascii_hex_to_digit(PaddingChar),
			0 = StrippedValue rem 10,
			integer_to_string(StrippedValue div 10, Length)
	end.

%% @doc Converts a list of track 2 nibbles to a string containing
%%      an ASCII encoding of the same data.
%%
%% @spec track2_to_string(list(byte()), integer()) -> string()
-spec(track2_to_string(list(byte()), integer()) -> string()).

track2_to_string(Track2Data, Length) ->
	lists:sublist(track2_to_string2(Track2Data, []), 1, Length).

%% @doc Converts a string of ASCII characters to a track 2
%%      encoding.
%%
%% @spec string_to_track2(string()) -> list(byte())
-spec(string_to_track2(string()) -> list(byte())).

string_to_track2(Str) ->
	string_to_track2(Str, [], 0, true).

%% @doc Converts a string containing 1 ASCII hex character
%%      to its value. The ASCII hex digits A-F can be encoded
%%      as upper-case ("A" - "F") or lower-case ("a" - "f")
%%      characters.
%%
%% @spec ascii_hex_to_digit(HexDigit::string()) -> integer()
-spec(ascii_hex_to_digit(HexDigit::string()) -> integer()).

ascii_hex_to_digit([A]) when A >= $0 andalso A =< $9 ->
	A - $0;
ascii_hex_to_digit([A]) when A >= $A andalso A =< $F ->
	A - 55;
ascii_hex_to_digit([A]) when A >= $a andalso A =< $f ->
	A - 87.

%% @doc Converts a value in the range 0-15 to a 1 character
%%      ASCII string containing the equivalent hexadecimal digit.
%%      Values 10 - 15 are converted to the upper case strings
%%      "A" - "F".
%%
%% @spec digit_to_ascii_hex(IntValue::integer()) -> string()
-spec(digit_to_ascii_hex(IntValue::integer()) -> string()).

digit_to_ascii_hex(D) when D >= 0 andalso D =< 9 ->
	[48+D];
digit_to_ascii_hex(D) when D >= 10 andalso D =< 15 ->
	[55+D].

%% @doc Strips trailing spaces from an ASCII string.
%%
%% @spec strip_trailing_spaces(string()) -> string()
-spec(strip_trailing_spaces(string()) -> string()).

strip_trailing_spaces(Str) ->
	lists:reverse(strip_leading_spaces(lists:reverse(Str))).

%% @doc Strips leading zeroes from an ASCII string.
%%
%% @spec strip_leading_zeroes(Str::string()) -> string()
-spec(strip_leading_zeroes(Str::string()) -> string()).

strip_leading_zeroes([$0|Tail]) ->
	strip_leading_zeroes(Tail);
strip_leading_zeroes(Str) ->
	Str.

%% @doc Converts an ASCII string to an EBCDIC string.
%%
%% @spec ascii_to_ebcdic(string()) -> list(byte())
-spec(ascii_to_ebcdic(string()) -> list(byte())).

ascii_to_ebcdic(Str) ->
	[ascii_to_ebcdic_char(C) || C <- Str].

%% @doc Converts an EBCDIC string to an ASCII string.
%%
%% @spec ebcdic_to_ascii(list(byte())) -> string()
-spec(ebcdic_to_ascii(list(byte())) -> string()).

ebcdic_to_ascii(EbcdicStr) ->
	[ebcdic_to_ascii_char(C) || C <- EbcdicStr].

%% @doc Converts a list of integer IDs to a 64-bit bitmap.
%%      Values in the range [Offset+1, Offset+64] are
%%      encoded. Values outside the specified range are
%%      ignored.
%%
%% @spec list_to_bitmap(list(integer()), integer()) -> binary()
-spec(list_to_bitmap(list(integer()), integer()) -> binary()).

list_to_bitmap(FieldIds, Offset) ->
	list_to_bitmap(FieldIds, Offset, array:from_list(lists:duplicate(8, 0))).

%% @doc Converts a 64-bit bitmap to a list of integers
%%      starting at a specified offset.
%%
%% @spec bitmap_to_list(binary(), integer()) -> list(integer())
-spec(bitmap_to_list(binary(), integer()) -> list(integer())).

bitmap_to_list(Bitmap, Offset) when size(Bitmap) =:= 8 ->
	<< BitmapInt:64/big >> = Bitmap,
	bitmap_int_to_list(BitmapInt, Offset, 0, []).

%%
%% Local Functions
%%
string_to_ascii_hex([], Result) ->
	lists:reverse(Result);
string_to_ascii_hex([Char|Tail], Result) ->
	Msb = Char div 16,
	Lsb = Char rem 16,
	string_to_ascii_hex(Tail, digit_to_ascii_hex(Lsb) ++ 
							digit_to_ascii_hex(Msb) ++
							Result).

ascii_hex_to_string([], Result) ->
	lists:reverse(Result);
ascii_hex_to_string([Dig1, Dig2 | Tail], Result) ->
	Char = [ascii_hex_to_digit([Dig1]) * 16 + ascii_hex_to_digit([Dig2])],
	ascii_hex_to_string(Tail, Char ++ Result).

pad_with_zeroes(Length, Value) when Length =:= length(Value) ->
	Value;
pad_with_zeroes(Length, Value) when Length > length(Value) ->
	pad_with_zeroes(Length, "0" ++ Value).

pad_with_leading_spaces(List, Length) when length(List) =:= Length ->
	List;
pad_with_leading_spaces(List, Length) when length(List) < Length ->
	pad_with_leading_spaces(" " ++ List, Length).

binary_to_ascii_hex([], Result) ->
	lists:reverse(Result);
binary_to_ascii_hex([H|T], Result) ->
	Msn = H div 16,
	Lsn = H rem 16,
	binary_to_ascii_hex(T, digit_to_ascii_hex(Lsn) ++ digit_to_ascii_hex(Msn) ++ Result).

ascii_hex_to_bytes([], Result) ->
	lists:reverse(Result);
ascii_hex_to_bytes([Msd, Lsd | Tail], Result) ->
	Msn = ascii_hex_to_digit([Msd]),
	Lsn = ascii_hex_to_digit([Lsd]),
	ascii_hex_to_bytes(Tail, [Msn * 16 + Lsn] ++ Result).

integer_to_bcd(0, 0, List) ->
	[Head|Tail] = List,
	case length(List) rem 2 of
		0 ->
			concat_adjacent_bytes(List, []);
		1 when Head =< 9 ->
			[Head|concat_adjacent_bytes(Tail, [])]
	end;	
integer_to_bcd(Value, Length, List) when Length > 0 ->
	integer_to_bcd(Value div 10, Length-1, [Value rem 10|List]).

concat_adjacent_bytes([], Result) ->
	lists:reverse(Result);
concat_adjacent_bytes([Dig1, Dig2|Tail], Result) ->
	concat_adjacent_bytes(Tail, [Dig1 * 16 + Dig2|Result]).

ascii_hex_to_bcd2([], Result) ->
	lists:reverse(Result);
ascii_hex_to_bcd2([Dig1, Dig2|Tail], Result) ->
	Byte = ascii_hex_to_digit([Dig1]) * 16 + ascii_hex_to_digit([Dig2]),
	ascii_hex_to_bcd2(Tail, [Byte|Result]).

track2_to_string2([], Result) ->
	lists:reverse(Result);
track2_to_string2(Data, Result) ->
	[H|Tail] = Data,
	track2_to_string2(Tail, [H rem 16 + $0, H div 16 + $0 | Result]).

string_to_track2([], Result, 0, true) ->
	lists:reverse(Result);
string_to_track2([], Result, X, false) ->
	lists:reverse(Result) ++ [X*16];
string_to_track2([H|T], Result, 0, true) ->
	string_to_track2(T, Result, H-$0, false);
string_to_track2([H|T], Result, X, false) ->
	Xupdated = X*16 + H - $0,
	string_to_track2(T, [Xupdated|Result], 0, true).

strip_leading_spaces([$ |Tail]) ->
	strip_leading_spaces(Tail);
strip_leading_spaces(Str) ->
	Str.

ascii_to_ebcdic_char(H) when H >= $0 andalso H =< $9 ->
	H - $0 + 240;
ascii_to_ebcdic_char(H) when H >= $a andalso H =< $i ->
	H - $a + 129;
ascii_to_ebcdic_char(H) when H >= $j andalso H =< $r ->
	H - $j + 145;
ascii_to_ebcdic_char(H) when H >= $s andalso H =< $z ->
	H - $s + 162;
ascii_to_ebcdic_char(H) when H >= $A andalso H =< $I ->
	H - $A + 193;
ascii_to_ebcdic_char(H) when H >= $J andalso H =< $R ->
	H - $J + 209;
ascii_to_ebcdic_char(H) when H >= $S andalso H =< $Z ->
	H - $S + 226;
ascii_to_ebcdic_char($ ) ->
	64;
ascii_to_ebcdic_char($.) ->
	75;
ascii_to_ebcdic_char($<) ->
	76;
ascii_to_ebcdic_char($() ->
	77;
ascii_to_ebcdic_char($+) ->
	78;
ascii_to_ebcdic_char($|) ->
	79;
ascii_to_ebcdic_char($&) ->
	80;
ascii_to_ebcdic_char($!) ->
	90;
ascii_to_ebcdic_char($$) ->
	91;
ascii_to_ebcdic_char($*) ->
	92;
ascii_to_ebcdic_char($)) ->
	93;
ascii_to_ebcdic_char($;) ->
	94;
ascii_to_ebcdic_char($-) ->
	96;
ascii_to_ebcdic_char($/) ->
	97;
ascii_to_ebcdic_char($,) ->
	107;
ascii_to_ebcdic_char($%) ->
	108;
ascii_to_ebcdic_char($_) ->
	109;
ascii_to_ebcdic_char($>) ->
	110;
ascii_to_ebcdic_char($?) ->
	111;
ascii_to_ebcdic_char($`) ->
	121;
ascii_to_ebcdic_char($:) ->
	122;
ascii_to_ebcdic_char($#) ->
	123;
ascii_to_ebcdic_char($@) ->
	124;
ascii_to_ebcdic_char($') ->
	125;
ascii_to_ebcdic_char($=) ->
	126;
ascii_to_ebcdic_char($") ->
	127;
ascii_to_ebcdic_char($~) ->
	161;
ascii_to_ebcdic_char($^) ->
	176;
ascii_to_ebcdic_char($[) ->
	186;
ascii_to_ebcdic_char($]) ->
	187;
ascii_to_ebcdic_char(${) ->
	192;
ascii_to_ebcdic_char($}) ->
	208;
ascii_to_ebcdic_char(92) ->
	224.

ebcdic_to_ascii_char(H) when H >= 129 andalso H =< 137 ->
	H - 129 + $a;
ebcdic_to_ascii_char(H) when H >= 145 andalso H =< 153 ->
	H - 145 + $j;
ebcdic_to_ascii_char(H) when H >= 162 andalso H =< 169 ->
	H - 162 + $s;
ebcdic_to_ascii_char(H) when H >= 193 andalso H =< 201 ->
	H - 193 + $A;
ebcdic_to_ascii_char(H) when H >= 209 andalso H =< 217 ->
	H - 209 + $J;
ebcdic_to_ascii_char(H) when H >= 226 andalso H =< 233 ->
	H - 226 + $S;
ebcdic_to_ascii_char(H) when H >= 240 andalso H =< 249 ->
	H - 240 + $0;
ebcdic_to_ascii_char(64) ->
	$ ;
ebcdic_to_ascii_char(75) ->
	$.;
ebcdic_to_ascii_char(76) ->
	$<;
ebcdic_to_ascii_char(77) ->
	$(;
ebcdic_to_ascii_char(78) ->
	$+;
ebcdic_to_ascii_char(79) ->
	$|;
ebcdic_to_ascii_char(80) ->
	$&;
ebcdic_to_ascii_char(90) ->
	$!;
ebcdic_to_ascii_char(91) ->
	$$;
ebcdic_to_ascii_char(92) ->
	$*;
ebcdic_to_ascii_char(93) ->
	$);
ebcdic_to_ascii_char(94) ->
	$;;
ebcdic_to_ascii_char(96) ->
	$-;
ebcdic_to_ascii_char(97) ->
	$/;
ebcdic_to_ascii_char(107) ->
	$,;
ebcdic_to_ascii_char(108) ->
	$%;
ebcdic_to_ascii_char(109) ->
	$_;
ebcdic_to_ascii_char(110) ->
	$>;
ebcdic_to_ascii_char(111) ->
	$?;
ebcdic_to_ascii_char(121) ->
	$`;
ebcdic_to_ascii_char(122) ->
	$:;
ebcdic_to_ascii_char(123) ->
	$#;
ebcdic_to_ascii_char(124) ->
	$@;
ebcdic_to_ascii_char(125) ->
	$';
ebcdic_to_ascii_char(126) ->
	$=;
ebcdic_to_ascii_char(127) ->
	$";
ebcdic_to_ascii_char(161) ->
	$~;
ebcdic_to_ascii_char(176) ->
	$^;
ebcdic_to_ascii_char(186) ->
	$[;
ebcdic_to_ascii_char(187) ->
	$];
ebcdic_to_ascii_char(192) ->
	${;
ebcdic_to_ascii_char(208) ->
	$};
ebcdic_to_ascii_char(224) ->
	$\\.

list_to_bitmap([], _Offset, Result) ->
	list_to_binary(array:to_list(Result));
list_to_bitmap([Id|Tail], Offset, Result) when Id > Offset andalso Id =< Offset+64 ->
	Id2 = Id - Offset - 1,
	Index = Id2 div 8,
	BitNum = 7 - (Id2 rem 8),
	CurValue = array:get(Index, Result),
	NewValue = CurValue bor (1 bsl BitNum),
	list_to_bitmap(Tail, Offset, array:set(Index, NewValue, Result));
list_to_bitmap([_Id|Tail], Offset, Result) ->
	list_to_bitmap(Tail, Offset, Result).

bitmap_int_to_list(_Value, Offset, 64, Result) ->
	[Index+Offset || Index <- Result];
bitmap_int_to_list(Value, Offset, N, Result) ->
	case Value band (1 bsl N) of
		0 ->
			bitmap_int_to_list(Value, Offset, N+1, Result);
		_ ->
			bitmap_int_to_list(Value, Offset, N+1, [64-N|Result])
	end.

