-module(bigtest_decoded).
-export([get_bigtest_decoded/0]).
get_bigtest_decoded() ->

    [{tag_compound,"Level",
        [{tag_long,"longTest",9223372036854775807},
        {tag_short,"shortTest",32767},
        {tag_string,"stringTest",
            [72,69,76,76,79,32,87,79,82,76,68,32,84,72,73,83,32,73,83,32,65,32,
            84,69,83,84,32,83,84,82,73,78,71,32,195,133,195,132,195,150,33]},
        {tag_float,"floatTest",0.4982314705848694},
        {tag_int,"intTest",2147483647},
        {tag_compound,"nested compound test",
            [{tag_compound,"ham",
                [{tag_string,"name","Hampus"},{tag_float,"value",0.75}]},
            {tag_compound,"egg",
                [{tag_string,"name","Eggbert"},{tag_float,"value",0.5}]}]},
        {tag_list,"listTest (long)",
            [{tag_long,11},
            {tag_long,12},
            {tag_long,13},
            {tag_long,14},
            {tag_long,15}]},
        {tag_list,"listTest (compound)",
            [{tag_compound,
                [{tag_string,"name","Compound tag #0"},
                    {tag_long,"created-on",1264099775885}]},
            {tag_compound,
                [{tag_string,"name","Compound tag #1"},
                    {tag_long,"created-on",1264099775885}]}]},
        {tag_byte,"byteTest",127},
        {tag_byte_array,
            "byteArrayTest (the first 1000 values of (n*n*255+n*7)%100, starting with n=0 (0, 62, 34, 16, 8, ...))",
            <<0,62,34,16,8,10,22,44,76,18,70,32,4,86,78,80,92,14,46,88,40,2,74,
                56,48,50,62,84,16,58,10,72,44,26,18,20,32,54,86,28,80,42,14,96,88,
                90,2,24,56,98,50,12,84,66,58,60,72,94,26,68,20,82,54,36,28,30,42,
                64,96,38,90,52,24,6,98,0,12,34,66,8,60,22,94,76,68,70,82,4,36,78,
                30,92,64,46,38,40,52,74,6,48,0,62,34,16,8,10,22,44,76,18,70,32,4,
                86,78,80,92,14,46,88,40,2,74,56,48,50,62,84,16,58,10,72,44,26,18,
                20,32,54,86,28,80,42,14,96,88,90,2,24,56,98,50,12,84,66,58,60,72,
                94,26,68,20,82,54,36,28,30,42,64,96,38,90,52,24,6,98,0,12,34,66,8,
                60,22,94,76,68,70,82,4,36,78,30,92,64,46,38,40,52,74,6,48,0,62,34,
                16,8,10,22,44,76,18,70,32,4,86,78,80,92,14,46,88,40,2,74,56,48,50,
                62,84,16,58,10,72,44,26,18,20,32,54,86,28,80,42,14,96,88,90,2,24,
                56,98,50,12,84,66,58,60,72,94,26,68,20,82,54,36,28,30,42,64,96,38,
                90,52,24,6,98,0,12,34,66,8,60,22,94,76,68,70,82,4,36,78,30,92,64,
                46,38,40,52,74,6,48,0,62,34,16,8,10,22,44,76,18,70,32,4,86,78,80,
                92,14,46,88,40,2,74,56,48,50,62,84,16,58,10,72,44,26,18,20,32,54,
                86,28,80,42,14,96,88,90,2,24,56,98,50,12,84,66,58,60,72,94,26,68,
                20,82,54,36,28,30,42,64,96,38,90,52,24,6,98,0,12,34,66,8,60,22,94,
                76,68,70,82,4,36,78,30,92,64,46,38,40,52,74,6,48,0,62,34,16,8,10,
                22,44,76,18,70,32,4,86,78,80,92,14,46,88,40,2,74,56,48,50,62,84,
                16,58,10,72,44,26,18,20,32,54,86,28,80,42,14,96,88,90,2,24,56,98,
                50,12,84,66,58,60,72,94,26,68,20,82,54,36,28,30,42,64,96,38,90,52,
                24,6,98,0,12,34,66,8,60,22,94,76,68,70,82,4,36,78,30,92,64,46,38,
                40,52,74,6,48,0,62,34,16,8,10,22,44,76,18,70,32,4,86,78,80,92,14,
                46,88,40,2,74,56,48,50,62,84,16,58,10,72,44,26,18,20,32,54,86,28,
                80,42,14,96,88,90,2,24,56,98,50,12,84,66,58,60,72,94,26,68,20,82,
                54,36,28,30,42,64,96,38,90,52,24,6,98,0,12,34,66,8,60,22,94,76,68,
                70,82,4,36,78,30,92,64,46,38,40,52,74,6,48,0,62,34,16,8,10,22,44,
                76,18,70,32,4,86,78,80,92,14,46,88,40,2,74,56,48,50,62,84,16,58,
                10,72,44,26,18,20,32,54,86,28,80,42,14,96,88,90,2,24,56,98,50,12,
                84,66,58,60,72,94,26,68,20,82,54,36,28,30,42,64,96,38,90,52,24,6,
                98,0,12,34,66,8,60,22,94,76,68,70,82,4,36,78,30,92,64,46,38,40,52,
                74,6,48,0,62,34,16,8,10,22,44,76,18,70,32,4,86,78,80,92,14,46,88,
                40,2,74,56,48,50,62,84,16,58,10,72,44,26,18,20,32,54,86,28,80,42,
                14,96,88,90,2,24,56,98,50,12,84,66,58,60,72,94,26,68,20,82,54,36,
                28,30,42,64,96,38,90,52,24,6,98,0,12,34,66,8,60,22,94,76,68,70,82,
                4,36,78,30,92,64,46,38,40,52,74,6,48,0,62,34,16,8,10,22,44,76,18,
                70,32,4,86,78,80,92,14,46,88,40,2,74,56,48,50,62,84,16,58,10,72,
                44,26,18,20,32,54,86,28,80,42,14,96,88,90,2,24,56,98,50,12,84,66,
                58,60,72,94,26,68,20,82,54,36,28,30,42,64,96,38,90,52,24,6,98,0,
                12,34,66,8,60,22,94,76,68,70,82,4,36,78,30,92,64,46,38,40,52,74,6,
                48,0,62,34,16,8,10,22,44,76,18,70,32,4,86,78,80,92,14,46,88,40,2,
                74,56,48,50,62,84,16,58,10,72,44,26,18,20,32,54,86,28,80,42,14,96,
                88,90,2,24,56,98,50,12,84,66,58,60,72,94,26,68,20,82,54,36,28,30,
                42,64,96,38,90,52,24,6,98,0,12,34,66,8,60,22,94,76,68,70,82,4,36,
                78,30,92,64,46,38,40,52,74,6,48>>},
        {tag_double,"doubleTest",0.4931287132182315}]}].