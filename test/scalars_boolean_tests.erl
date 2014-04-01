-module(scalars_boolean_tests).

-include_lib("eunit/include/eunit.hrl").



%% Scalars - boolean



can_get_invalid_boolean_test() ->
    Map = {boolean},
    Data = <<"not_boolean">>,
    {Errors, _Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"INVALID_BOOLEAN">>, <<>>, []}], Errors).



can_get_valid_boolean_test() ->
    Map = {boolean},
    Data = false,
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(Data, Result).



can_get_custom_boolean_error_code_test() ->
    Map = {boolean},
    Data = false,
    Fun = fun
        (validate, [], false) ->
            {error, <<"CUSTOM_ERROR_CODE">>};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, _Result} = jiffy_v:validate(Map, Data, Fun),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"CUSTOM_ERROR_CODE">>, <<>>, []}], Errors).



can_fix_invalid_boolean_error_code_test() ->
    Map = {boolean},
    Data = 1,
    Fun = fun
        (fix, [], 1) ->
            {ok, true};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, Result} = jiffy_v:validate(Map, Data, Fun),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(true, Result).