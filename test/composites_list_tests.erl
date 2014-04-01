-module(composites_list_tests).

-include_lib("eunit/include/eunit.hrl").



%% Composites - list



can_get_invalid_list_test() ->
    Map = {list, [{integer}]},
    Data = {not_a_list},
    {Errors, _Result} = jiffy_v:validate(Map, Data),
    ?assertMatch([_], Errors),
    ?assertEqual(1, length(Errors)),
    ?assertEqual([{<<"INVALID_LIST">>, <<"">>, []}], Errors).



can_get_typed_list_test() ->
    Map = {list, [{integer}]},
    Data = [1,2,3,4,5],
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertEqual(Data, Result).



can_get_multityped_list_test() ->
    Map = {list, [{integer}, {float}]},
    Data = [1.0,2.0,3.0,4.0,5.0],
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertEqual(Data, Result).



can_get_invalid_list_on_types_mismatch_test() ->
    Map = {list, [{integer}, {boolean}]},
    Data = [1.0,2.0,3,4,5],
    {Errors, Result} = jiffy_v:validate(Map, Data),
    io:format("~p", [Result]),
    ?assertEqual(1, length(Errors)),
    ?assertEqual([{<<"INVALID_LIST">>, <<"">>, []}], Errors).



can_get_result_on_empty_list_test() ->
    Map = {list, [{integer}, {float}]},
    Data = [],
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertEqual(Data, Result).



can_get_custom_list_error_code_test() ->
    Map = {list, [{integer}]},
    Data = [1,2,3,4,5],
    Fun = fun
        (validate, [_], Value) when Value > 4 ->
            {error, <<"CUSTOM_ERROR_CODE">>};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, _Result} = jiffy_v:validate(Map, Data, Fun),
    ?assertEqual(1, length(Errors)),
    ?assertMatch([{<<"CUSTOM_ERROR_CODE">>, <<"4">>, [<<"4">>]}], Errors).



can_fix_invalid_list_test() ->
    Map = {list, [{integer}]},
    Data = [true,2,3,false,5],
    Fun = fun
        (fix, [_], true) ->
            {ok, 1};
        (fix, [_], false) ->
            {ok, 0};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, Result} = jiffy_v:validate(Map, Data, Fun),
    ?assertEqual(0, length(Errors)),
    ?assertMatch([1,2,3,0,5], Result).
