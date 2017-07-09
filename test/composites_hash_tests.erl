-module(composites_hash_tests).

-include_lib("eunit/include/eunit.hrl").



%% Composites - hash



can_get_invalid_hash_error_test() ->
    Map = jiffy_v:hash([
        jiffy_v:hashfield(<<"foo">>, required, jiffy_v:integer())
    ]),
    Data = not_hash,
    {Errors, _Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertEqual([{<<"INVALID_HASH">>, <<"">>, []}], Errors).



can_get_mapped_hash_value_test() ->
    Map = jiffy_v:hash([
        jiffy_v:hashfield(<<"foo">>, required, jiffy_v:integer())
    ]),
    Data = {[
        {<<"foo">>, 1}
    ]},
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch({ok, 1}, test_helper:get(Result, <<"foo">>)).



cannot_get_unmapped_hash_field_test() ->
    Map = jiffy_v:hash([
        jiffy_v:hashfield(<<"foo">>, required, jiffy_v:integer())
    ]),
    Data = {[
        {<<"foo">>, 1},
        {<<"bar">>, 2}
    ]},
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch({error, undefined}, test_helper:get(Result, <<"bar">>)).



can_get_result_with_undefined_hash_optional_field_test() ->
    Map = jiffy_v:hash([
        jiffy_v:hashfield(<<"foo">>, required, jiffy_v:integer()),
        jiffy_v:hashfield(<<"bar">>, optional, jiffy_v:integer())
    ]),
    Data = {[
        {<<"foo">>, 1}
    ]},
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(0, length(Errors)),
    ?assertMatch({error, undefined}, test_helper:get(Result, <<"bar">>)),
    ?assertMatch({ok, 1}, test_helper:get(Result, <<"foo">>)).



can_get_undefined_field_error_test() ->
    Map = jiffy_v:hash([
        jiffy_v:hashfield(<<"foo">>, required, jiffy_v:integer())
    ]),
    Data = {[
        {<<"bar">>, 1}
    ]},
    {Errors, Result} = jiffy_v:validate(Map, Data),
    ?assertEqual(1, length(Errors)),
    ?assertMatch({error, undefined}, test_helper:get(Result, <<"foo">>)),
    ?assertMatch([{<<"UNDEFINED_FIELD">>, <<"foo">>, [<<"foo">>]}], Errors).



can_pass_custom_hash_error_code_test() ->
    Map = jiffy_v:hash([
        jiffy_v:hashfield(<<"foo">>, required, jiffy_v:integer())
    ]),
    Data = {[
        {<<"foo">>, -1}
    ]},
    Fun = fun
        (validate, [<<"foo">>], Value) when Value < 0 ->
            {error, <<"CUSTOM_ERROR_CODE">>};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, Result} = jiffy_v:validate(Map, Data, Fun),
    ?assertEqual(1, length(Errors)),
    ?assertMatch({error, undefined}, test_helper:get(Result, <<"foo">>)),
    ?assertMatch([{<<"CUSTOM_ERROR_CODE">>, <<"foo">>, [<<"foo">>]}], Errors).



can_fix_undefined_hash_field_test() ->
    Map = jiffy_v:hash([
        jiffy_v:hashfield(<<"foo">>, required, jiffy_v:integer())
    ]),
    Data = {[
        {<<"baz">>, 3.14}
    ]},
    Fun = fun
        (fix, [<<"foo">>], undefined) ->
            {ok, 15};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, Result} = jiffy_v:validate(Map, Data, Fun),
    ?assertEqual(0, length(Errors)),
    ?assertMatch({ok, 15}, test_helper:get(Result, <<"foo">>)).



can_fix_invalid_hash_field_test() ->
    Map = jiffy_v:hash([
        jiffy_v:hashfield(<<"foo">>, required, jiffy_v:integer())
    ]),
    Data = {[
        {<<"foo">>, false}
    ]},
    Fun = fun
        (fix, [<<"foo">>], _Value) ->
            {ok, 0};
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    {Errors, Result} = jiffy_v:validate(Map, Data, Fun),
    ?assertEqual(0, length(Errors)),
    ?assertMatch({ok, 0}, test_helper:get(Result, <<"foo">>)).
