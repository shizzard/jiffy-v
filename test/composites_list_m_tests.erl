-module(composites_list_m_tests).

-include_lib("eunit/include/eunit.hrl").



%% Composites - list



can_get_invalid_list_test() ->
    Map = jiffy_vm:list([
        jiffy_vm:integer()
    ]),
    Data = {not_a_list},
    {Errors, _Result} = jiffy_vm:validate(Map, Data),
    ?assertMatch([_], Errors),
    ?assertEqual(1, length(Errors)),
    ?assertEqual([{<<"INVALID_LIST">>, <<"">>, []}], Errors).



can_get_typed_list_test() ->
    Map = jiffy_vm:list([
        jiffy_vm:integer()
    ]),
    Data = [1,2,3,4,5],
    {Errors, Result} = jiffy_vm:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertEqual(Data, Result).



can_get_multityped_list_test() ->
    Map = jiffy_vm:list([
        jiffy_vm:integer(), jiffy_vm:float()
    ]),
    Data = [1.0,2.0,3.0,4.0,5.0],
    {Errors, Result} = jiffy_vm:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertEqual(Data, Result).



can_get_invalid_list_on_types_mismatch_test() ->
    Map = jiffy_vm:list([
        jiffy_vm:integer(), jiffy_vm:boolean()
    ]),
    Data = [1.0,2.0,3,4,5],
    {Errors, Result} = jiffy_vm:validate(Map, Data),
    io:format("~p", [Result]),
    ?assertEqual(1, length(Errors)),
    ?assertEqual([{<<"INVALID_LIST">>, <<"">>, []}], Errors).



can_get_result_on_empty_list_test() ->
    Map = jiffy_vm:list([
        jiffy_vm:integer(), jiffy_vm:float()
    ]),
    Data = [],
    {Errors, Result} = jiffy_vm:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertEqual(Data, Result).

