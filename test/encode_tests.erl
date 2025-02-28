-module(encode_tests).
-include_lib("eunit/include/eunit.hrl").

big_test() ->

    {ok, Result} = file:read_file("test/bigtest.nbt"),
    Result2 = zlib:gunzip(Result),
    ?assertEqual(Result2, encode:encode_nbt(decode:decode_nbt(Result2))).
small_test() ->
    Data = <<10,0,11,104,101,108,108,111,32,119,111,114,108,100,10,0,11,104,101,108,108,111,32,119,111,114,108,100,1,0,4,110,97,109,101,1,0,1,0,4,110,97,109,101,1,0>>,
    ?assertEqual(<<10,0,11,104,101,108,108,111,32,119,111,114,108,100,10,0,11,104,101,108,108,111,32,119,111,114,108,100,1,0,4,110,97,109,101,1,0,1,0,4,110,97,109,101,1,0>>, encode:encode_nbt(decode:decode_nbt(Data))).


