-module(composites_variant_m_tests).

-include_lib("eunit/include/eunit.hrl").



%% Composites - variant



can_get_invalid_variant_test() ->
    Map = jiffy_vm:variant([
        jiffy_vm:integer(), jiffy_vm:boolean()
    ]),
    Data = [1,2,3,4,5],
    {Errors, _Result} = jiffy_vm:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"INVALID_VARIANT">>, <<"">>, []}], Errors).



can_get_valid_variant_test() ->
    Map = jiffy_vm:variant([
        jiffy_vm:integer(), jiffy_vm:boolean()
    ]),
    Data = true,
    {Errors, Result} = jiffy_vm:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(Data, Result).
