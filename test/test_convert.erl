%% Author: carl
%% Created: 20 Mar 2011
%% Description: TODO: Add description to test_convert
-module(test_convert).

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



%%
%% Local Functions
%%
digit_to_ascii_hex_test() ->
	"0" = erl8583_convert:digit_to_ascii_hex(0),
	"9" = erl8583_convert:digit_to_ascii_hex(9),
	"A" = erl8583_convert:digit_to_ascii_hex(10),
	"F" = erl8583_convert:digit_to_ascii_hex(15),
	?assertError(_, erl8583_convert:digit_to_ascii_hex(16)).

ascii_hex_to_digit_test() ->
	0 = erl8583_convert:ascii_hex_to_digit("0"),
	9 = erl8583_convert:ascii_hex_to_digit("9"),
	10 = erl8583_convert:ascii_hex_to_digit("A"),
	15 = erl8583_convert:ascii_hex_to_digit("F"),
	10 = erl8583_convert:ascii_hex_to_digit("a"),
	15 = erl8583_convert:ascii_hex_to_digit("f"),
	?assertError(_, erl8583_convert:ascii_hex_to_digit("G")).

string_to_ascii_hex_test() ->
	"" = erl8583_convert:string_to_ascii_hex(""),
	"30" = erl8583_convert:string_to_ascii_hex("0"),
	"48656C6C6F" = erl8583_convert:string_to_ascii_hex("Hello").

ascii_hex_to_string_test() ->
	"" = erl8583_convert:ascii_hex_to_string(""),
	"Hello" = erl8583_convert:ascii_hex_to_string("48656C6c6F").

binary_to_ascii_hex_test() ->
	"00FFA5" = erl8583_convert:binary_to_ascii_hex(<<0, 255, 165>>).

binary_list_to_ascii_hex_test() ->
	"00FFA5" = erl8583_convert:binary_list_to_ascii_hex([0, 255, 165]).

ascii_hex_to_binary_test() ->
	<<0, 255, 165>> = erl8583_convert:ascii_hex_to_binary("00FFa5").

ascii_hex_to_binary_list_test() ->
	[0, 255, 165] = erl8583_convert:ascii_hex_to_binary_list("00FFa5").

integer_to_bcd_test() ->
	[1] = erl8583_convert:integer_to_bcd(1, 1),
	[1] = erl8583_convert:integer_to_bcd(1, 2),
	[0, 1] = erl8583_convert:integer_to_bcd(1, 3),
	[16] = erl8583_convert:integer_to_bcd(10, 2),
	[0, 16] = erl8583_convert:integer_to_bcd(10, 3),
	[9, 153] = erl8583_convert:integer_to_bcd(999, 3),
	[0,18,52,86,120] = erl8583_convert:integer_to_bcd(12345678, 10),
	?assertError(_, erl8583_convert:integer_to_bcd(1000, 3)).

ascii_hex_to_bcd_test() ->
	[48, 31] = erl8583_convert:ascii_hex_to_bcd("301", "F"),
	[64, 31] = erl8583_convert:ascii_hex_to_bcd("401", "F"),
	[48, 16] = erl8583_convert:ascii_hex_to_bcd("301", "0"),
	[18, 52] = erl8583_convert:ascii_hex_to_bcd("1234", "0").

bcd_to_integer_test() ->
	17 = erl8583_convert:bcd_to_integer([23]),
	1 = erl8583_convert:bcd_to_integer([1]),
	1 = erl8583_convert:bcd_to_integer([0, 1]),
	123 = erl8583_convert:bcd_to_integer([1, 35]),
	123456 = erl8583_convert:bcd_to_integer([18, 52, 86]).

bcd_to_ascii_hex_test() ->
	"1234" = erl8583_convert:bcd_to_ascii_hex([18, 52], 4, "F"),
	"123" = erl8583_convert:bcd_to_ascii_hex([18, 63], 3, "F"),
	"123" = erl8583_convert:bcd_to_ascii_hex([18, 48], 3, "0"),
	"401" = erl8583_convert:bcd_to_ascii_hex([64, 31], 3, "F").

track2_to_string_test() ->
	";1234123412341234=0305101193010877?" = erl8583_convert:track2_to_string([177, 35, 65, 35, 65, 35, 65, 35, 77, 3, 5, 16, 17, 147, 1, 8, 119, 240],  35).

string_to_track2_test() ->
	[177, 35, 65, 35, 65, 35, 65, 35, 77, 3, 5, 16, 17, 147, 1, 8, 119, 240] = erl8583_convert:string_to_track2(";1234123412341234=0305101193010877?").

strip_leading_spaces_test() ->
	"hello" = erl8583_convert:strip_trailing_spaces("hello"),
	"hello" =  erl8583_convert:strip_trailing_spaces("hello   "),
	"    hello" =  erl8583_convert:strip_trailing_spaces("    hello  ").

strip_leading_zeroes_test() ->
	"1234" = erl8583_convert:strip_leading_zeroes("1234"),
	"2345" = erl8583_convert:strip_leading_zeroes("0000000002345").

ascii_to_ebcdic_lower_case_chars_test() ->
	[129, 130, 131, 132, 133, 134, 135, 136, 137] = erl8583_convert:ascii_to_ebcdic("abcdefghi"),
	[145, 146, 147, 148, 149, 150, 151, 152, 153] = erl8583_convert:ascii_to_ebcdic("jklmnopqr"),
	[162, 163, 164, 165, 166, 167, 168, 169] = erl8583_convert:ascii_to_ebcdic("stuvwxyz").
	
ascii_to_ebcdic_upper_case_chars_test() ->
	[193, 194, 195, 196, 197, 198, 199, 200, 201] = erl8583_convert:ascii_to_ebcdic("ABCDEFGHI"),
	[209, 210, 211, 212, 213, 214, 215, 216, 217] = erl8583_convert:ascii_to_ebcdic("JKLMNOPQR"),
	[226, 227, 228, 229, 230, 231, 232, 233] = erl8583_convert:ascii_to_ebcdic("STUVWXYZ").

ascii_to_ebcdic_digits_test() ->
	[240, 241, 242, 243, 244, 245, 246, 247, 248, 249] = erl8583_convert:ascii_to_ebcdic("0123456789").

ascii_to_ebcdic_punctuation_1_test() ->
	[64] = erl8583_convert:ascii_to_ebcdic(" "),
	[75] = erl8583_convert:ascii_to_ebcdic("."),
	[76] = erl8583_convert:ascii_to_ebcdic("<"),
	[77] = erl8583_convert:ascii_to_ebcdic("("),
	[78] = erl8583_convert:ascii_to_ebcdic("+"),
	[79] = erl8583_convert:ascii_to_ebcdic("|"),
	[80] = erl8583_convert:ascii_to_ebcdic("&").

ascii_to_ebcdic_punctuation_2_test() ->
	[90] = erl8583_convert:ascii_to_ebcdic("!"),
	[91] = erl8583_convert:ascii_to_ebcdic("$"),
	[92] = erl8583_convert:ascii_to_ebcdic("*"),
	[93] = erl8583_convert:ascii_to_ebcdic(")"),
	[94] = erl8583_convert:ascii_to_ebcdic(";").

ascii_to_ebcdic_punctuation_3_test() ->
	[96] = erl8583_convert:ascii_to_ebcdic("-"),
	[97] = erl8583_convert:ascii_to_ebcdic("/"),
	[96] = erl8583_convert:ascii_to_ebcdic([45]).

ascii_to_ebcdic_punctuation_4_test() ->
	[107] = erl8583_convert:ascii_to_ebcdic(","),
	[108] = erl8583_convert:ascii_to_ebcdic("%"),
	[109] = erl8583_convert:ascii_to_ebcdic("_"),
	[110] = erl8583_convert:ascii_to_ebcdic(">"),
	[111] = erl8583_convert:ascii_to_ebcdic("?").

ascii_to_ebcdic_punctuation_5_test() ->
	[121] = erl8583_convert:ascii_to_ebcdic("`"),
	[122] = erl8583_convert:ascii_to_ebcdic(":"),
	[123] = erl8583_convert:ascii_to_ebcdic("#"),
	[124] = erl8583_convert:ascii_to_ebcdic("@"),
	[125] = erl8583_convert:ascii_to_ebcdic("'"),
	[126] = erl8583_convert:ascii_to_ebcdic("="),
	[127] = erl8583_convert:ascii_to_ebcdic("\"").

ascii_to_ebcdic_punctuation_6_test() ->
	[161] = erl8583_convert:ascii_to_ebcdic("~"),
	[176] = erl8583_convert:ascii_to_ebcdic("^"),
	[186] = erl8583_convert:ascii_to_ebcdic("["),
	[187] = erl8583_convert:ascii_to_ebcdic("]"),
	[192] = erl8583_convert:ascii_to_ebcdic("{"),
	[208] = erl8583_convert:ascii_to_ebcdic("}"),
	[224] = erl8583_convert:ascii_to_ebcdic("\\").

ebcdic_to_ascii_lower_case_chars_test() ->
	"abcdefghi" = erl8583_convert:ebcdic_to_ascii([129, 130, 131, 132, 133, 134, 135, 136, 137]),
	"jklmnopqr" = erl8583_convert:ebcdic_to_ascii([145, 146, 147, 148, 149, 150, 151, 152, 153]),
	"stuvwxyz" = erl8583_convert:ebcdic_to_ascii([162, 163, 164, 165, 166, 167, 168, 169]).

ebcdic_to_ascii_upper_case_chars_test() ->
	"ABCDEFGHI" = erl8583_convert:ebcdic_to_ascii([193, 194, 195, 196, 197, 198, 199, 200, 201]),
	"JKLMNOPQR" = erl8583_convert:ebcdic_to_ascii([209, 210, 211, 212, 213, 214, 215, 216, 217]),
	"STUVWXYZ" = erl8583_convert:ebcdic_to_ascii([226, 227, 228, 229, 230, 231, 232, 233]).

ebcdic_to_ascii_digits_test() ->
	"0123456789" = erl8583_convert:ebcdic_to_ascii([240, 241, 242, 243, 244, 245, 246, 247, 248, 249]).

ebcdic_to_ascii_punctuation_1_test() ->
	" " = erl8583_convert:ebcdic_to_ascii([64]),
	"." = erl8583_convert:ebcdic_to_ascii([75]),
	"<" = erl8583_convert:ebcdic_to_ascii([76]),
	"(" = erl8583_convert:ebcdic_to_ascii([77]),
	"+" = erl8583_convert:ebcdic_to_ascii([78]),
	"|" = erl8583_convert:ebcdic_to_ascii([79]),
	"&" = erl8583_convert:ebcdic_to_ascii([80]).

ebcdic_to_ascii_punctuation_2_test() ->
	"!$*);" = erl8583_convert:ebcdic_to_ascii([90, 91, 92, 93, 94]).

ebcdic_to_ascii_punctuation_3_test() ->
	"-/" ++ [45] = erl8583_convert:ebcdic_to_ascii([96, 97, 96]).

ebcdic_to_ascii_punctuation_4_test() ->
	",%_>?" = erl8583_convert:ebcdic_to_ascii([107, 108, 109, 110, 111]).

ebcdic_to_ascii_punctuation_5_test() ->
	"`:#@'=\"" = erl8583_convert:ebcdic_to_ascii([121, 122, 123, 124, 125, 126, 127]).

ebcdic_to_ascii_punctuation_6_test() ->
	"~^[]{}\\" = erl8583_convert:ebcdic_to_ascii([161, 176, 186, 187, 192, 208, 224]).

ascii_to_ebcdic_sanity_test() ->
	Ebcdic = [64] ++ lists:seq(75, 79) ++ [80] ++ lists:seq(90, 94) ++
					[96, 97] ++ lists:seq(107, 111) ++ lists:seq(121, 127) ++
					lists:seq(129, 137) ++ lists:seq(145, 153) ++ 
					lists:seq(161, 169) ++ [176, 186, 187] ++ 
					lists:seq(192, 201) ++ lists:seq(208, 217) ++ [224] ++
					lists:seq(226, 233) ++ lists:seq(240, 249),
	Ascii = erl8583_convert:ebcdic_to_ascii(Ebcdic),
	Ebcdic = erl8583_convert:ascii_to_ebcdic(Ascii).

list_to_bitmap_test() ->
	<<128, 0, 0, 0, 0, 0, 0, 0>> = erl8583_convert:list_to_bitmap([1], 0),
	<<128, 0, 0, 0, 0, 0, 0, 0>> = erl8583_convert:list_to_bitmap([65], 64),
	<<0, 0, 0, 0, 0, 0, 0, 1>> = erl8583_convert:list_to_bitmap([64], 0),
	<<128, 0, 0, 0, 0, 0, 0, 1>> = erl8583_convert:list_to_bitmap([1, 64], 0),
	<<128, 0, 0, 0, 0, 0, 0, 1>> = erl8583_convert:list_to_bitmap([1, 64, 1], 0),
	<<128, 0, 0, 0, 0, 0, 0, 1>> = erl8583_convert:list_to_bitmap([1, 1, 64, 64, 1], 0),
	<<128, 0, 0, 0, 0, 0, 0, 1>> = erl8583_convert:list_to_bitmap([1, 1, 64, 64, 1, 65, 0], 0).

bitmap_to_list_test() ->
	[1] = erl8583_convert:bitmap_to_list(<<128, 0, 0, 0, 0, 0, 0, 0>>, 0),
	[65] = erl8583_convert:bitmap_to_list(<<128, 0, 0, 0, 0, 0, 0, 0>>, 64),
	[1, 2, 64] = erl8583_convert:bitmap_to_list(<<192, 0, 0, 0, 0, 0, 0, 1>>, 0).
