-module(common_tests).

-include_lib("eunit/include/eunit.hrl").



%% Common



can_get_invalid_map_test() ->
    Map = some_invalid_map,
    Data = nobody_cares,
    ?assertError({invalid_type, some_invalid_map}, jiffy_v:validate(Map, Data)),
    ?assertError({invalid_type, some_invalid_map}, jiffy_vm:validate(Map, Data)).



can_get_any_value_test() ->
    Map = jiffy_v:any(),
    Data = {complicated, {data, {structure}}},
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch({complicated, {data, {structure}}}, Result).



can_get_any_value_m_test() ->
    Map = jiffy_vm:any(),
    Data = {complicated, {data, {structure}}},
    {Errors, Result} = jiffy_vm:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch({complicated, {data, {structure}}}, Result).
