-module(composites_variant_tests).

-include_lib("eunit/include/eunit.hrl").



%% Composites - variant



can_get_invalid_variant_test() ->
    Map = jiffy_v:variant([{integer}, {boolean}]),
    Data = [1,2,3,4,5],
    {Errors, _Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"INVALID_VARIANT">>, <<"">>, []}], Errors).



can_get_valid_variant_test() ->
    Map = jiffy_v:variant([{integer}, {boolean}]),
    Data = true,
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(Data, Result).



can_get_custom_variant_error_code_test() ->
    Map = jiffy_v:variant([{integer}, {boolean}]),
    Data = 3,
    Fun = fun
        (validate, [], Value) when Value == 3 ->
            {error, <<"CUSTOM_ERROR_CODE">>};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, _Result} = jiffy_v:validate(Map, Data, Fun),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"CUSTOM_ERROR_CODE">>, <<>>, []}], Errors).



can_fix_invalid_variant_test() ->
    Map = jiffy_v:variant([{integer}, {boolean}]),
    Data = 3.14,
    Fun = fun
        (fix, [], _Value) ->
            {ok, false};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, Result} = jiffy_v:validate(Map, Data, Fun),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(false, Result).
