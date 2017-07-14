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
