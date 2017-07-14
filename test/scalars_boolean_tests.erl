-module(scalars_boolean_tests).

-include_lib("eunit/include/eunit.hrl").



%% Scalars - boolean



can_get_invalid_boolean_test() ->
    Map = jiffy_v:boolean(),
    Data = <<"not_boolean">>,
    {Errors, _Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"INVALID_BOOLEAN">>, <<>>, []}], Errors).



can_get_valid_boolean_test() ->
    Map = jiffy_v:boolean(),
    Data = false,
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(Data, Result).



can_get_custom_boolean_error_code_test() ->
    Fun = fun
        (validate, [], false) ->
            {error, <<"CUSTOM_ERROR_CODE">>}
    end,
    Map = jiffy_v:boolean(Fun),
    Data = false,
    {Errors, _Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"CUSTOM_ERROR_CODE">>, <<>>, []}], Errors).



can_fix_invalid_boolean_error_code_test() ->
    Fun = fun
        (fix, [], 1) ->
            {ok, true}
    end,
    Map = jiffy_v:boolean(Fun),
    Data = 1,
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(true, Result).
