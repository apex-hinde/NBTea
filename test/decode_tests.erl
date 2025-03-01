-module(decode_tests).
-include_lib("eunit/include/eunit.hrl").
get_data() ->
    {ok, Data} = file:read_file("test/bigtest.nbt"),
    zlib:gunzip(Data).



nested_compound_test() ->
    Data = <<10,0,11,104,101,108,108,111,32,119,111,114,108,100,10,0,11,104,101,108,108,111,32,119,111,114,108,100,1,0,4,110,97,109,101,1,0,1,0,4,110,97,109,101,1,0>>,
    ?assertEqual(nbt:decode(Data), [{tag_compound, "hello world", [{tag_compound, "hello world", [{tag_byte, "name", 1}]}, {tag_byte, "name", 1}]}]).

big_test() ->
    Data = get_data(),
    Result = bigtest_decoded:get_bigtest_decoded(),
    ?assertEqual(Result, nbt:decode(Data)).
