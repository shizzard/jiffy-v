-module(common_tests).

-include_lib("eunit/include/eunit.hrl").



%% Common



can_get_invalid_map_test() ->
    Map = some_invalid_map,
    Data = nobody_cares,
    ?assertError({invalid_type, some_invalid_map}, jiffy_v:validate(Map, Data)),
    ?assertError({invalid_type, some_invalid_map}, jiffy_vm:validate(Map, Data)).
