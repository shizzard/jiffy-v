-module(scalars_integer_tests).

-include_lib("eunit/include/eunit.hrl").



%% Scalars - integer



can_get_invalid_integer_test() ->
    Map = jiffy_v:integer(),
    Data = true,
    {Errors, _Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"INVALID_INTEGER">>, <<>>, []}], Errors).



can_get_valid_integer_test() ->
    Map = jiffy_v:integer(),
    Data = 10,
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(Data, Result).



can_get_custom_integer_error_code_test() ->
    Fun = fun
        (validate, [], 3) ->
            {error, <<"CUSTOM_ERROR_CODE">>}
    end,
    Map = jiffy_v:integer(Fun),
    Data = 3,
    {Errors, _Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"CUSTOM_ERROR_CODE">>, <<>>, []}], Errors).



can_fix_invalid_integer_test() ->
    Fun = fun
        (fix, [], _Value) ->
            {ok, 0}
    end,
    Map = jiffy_v:integer(Fun),
    Data = false,
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch(0, Result).
