-module(jiffy_v).

-define(UNDEFINED_FIELD,    <<"UNDEFINED_FIELD">>).
-define(INVALID_HASH,       <<"INVALID_HASH">>).
-define(INVALID_STRING,     <<"INVALID_STRING">>).
-define(INVALID_ENUM,       <<"INVALID_ENUM">>).
-define(INVALID_FLOAT,      <<"INVALID_FLOAT">>).
-define(INVALID_INTEGER,    <<"INVALID_INTEGER">>).
-define(INVALID_BOOLEAN,    <<"INVALID_BOOLEAN">>).
-define(INVALID_NULL,       <<"INVALID_NULL">>).
-define(INVALID_LIST,       <<"INVALID_LIST">>).
-define(INVALID_VARIANT,    <<"INVALID_VARIANT">>).

-export([validate/2, validate/3]).

-type field_type() :: {hash, Fields :: list()}
                    | {list, Variants :: list()}
                    | {enum, Variants :: list()}
                    | {atom()}.

-spec validate(Map :: field_type(), Data :: any()) -> any().
validate(Map, Data) ->
    Fun = fun
        (validate, _, _) ->
            {ok, valid};
        (fix, _, _) ->
            {error, invalid}
    end,
    validate(Map, Data, Fun).
-spec validate(Map :: field_type(), Data :: any(), Fun :: fun()) -> {any(), any()}.
validate(Map, Data, Fun) when is_function(Fun, 3)  ->
    case handle(Map, Data, [], Fun, []) of
        {ok, Errors, Result} ->
            {Errors, Result};
        {error, Errors, Result} ->
            {Errors, Result}
    end.

-spec handle(Type :: field_type(),
             Data0 :: list() | {Data0 :: list()},
             Errors0 :: any(),
             Validator :: fun((_, _, _) -> any()),
             Stack0 :: list()
            ) -> {ok, any(), any()} | {error, nonempty_maybe_improper_list(), any()}.
handle({hash, Fields}, {Data0}, Errors0, Validator, Stack0) when is_list(Data0) ->
    {_Data1, Result, Errors1, Validator, _Stack1} = lists:foldl(
        fun iterate_hash/2, {Data0, {[]}, Errors0, Validator, Stack0}, Fields),
    val(Validator, Result, Errors1, Stack0);
handle({hash, _Fields}, Data0, Errors0, Validator, Stack0) ->
    fix(Validator, Data0, ?INVALID_HASH, Errors0, Stack0);

handle({list, [Variant | Variants]}, Data0, Errors0, Validator, Stack0) when is_list(Data0) ->
    Result = lists:foldl(fun
        (Elem, {I0, Acc, E0, V, S0}) ->
            case handle(Variant, Elem, E0, V, [i_to_b(I0) | S0]) of
                {ok, E1, R1} ->
                    {I0 + 1, [R1 | Acc], E1, V, S0};
                {custom_error, E1, _R1} ->
                    {I0 + 1, Acc, E1, V, S0};
                {error, _E1, _R1} ->
                    failure
            end;
        (_Elem, failure) ->
            failure
    end, {0, [], Errors0, Validator, Stack0}, Data0),
    case Result of
        failure ->
            handle({list, Variants}, Data0, Errors0, Validator, Stack0);
        {_Index, Result1, Errors1, Validator, _Stack1} ->
            val(Validator, lists:reverse(Result1), Errors1, Stack0)
    end;
handle({list, []}, Data0, Errors0, Validator, Stack0) when is_list(Data0) ->
    fix(Validator, Data0, ?INVALID_LIST, Errors0, Stack0);
handle({list, _Variants}, Data0, Errors0, Validator, Stack0) ->
    fix(Validator, Data0, ?INVALID_LIST, Errors0, Stack0);

handle({enum, Variants}, Data0, Errors0, Validator, Stack0) ->
    case lists:member(Data0, Variants) of
        true ->
            val(Validator, Data0, Errors0, Stack0);
        false ->
            fix(Validator, Data0, ?INVALID_ENUM, Errors0, Stack0)
    end;

handle({variant, [Variant | Variants]}, Data0, Errors0, Validator, Stack0) ->
    case handle(Variant, Data0, Errors0, Validator, Stack0) of
        {error, _E1, _R1} ->
            handle({variant, Variants}, Data0, Errors0, Validator, Stack0);
        Res -> Res
    end;
handle({variant, _Variants}, Data0, Errors0, Validator, Stack0) ->
    fix(Validator, Data0, ?INVALID_VARIANT, Errors0, Stack0);

handle({string}, Data0, Errors0, Validator, Stack0) when is_binary(Data0) ->
    val(Validator, Data0, Errors0, Stack0);
handle({string}, Data0, Errors0, Validator, Stack0) ->
    fix(Validator, Data0, ?INVALID_STRING, Errors0, Stack0);

handle({integer}, Data0, Errors0, Validator, Stack0) when is_integer(Data0) ->
    val(Validator, Data0, Errors0, Stack0);
handle({integer}, Data0, Errors0, Validator, Stack0) ->
    fix(Validator, Data0, ?INVALID_INTEGER, Errors0, Stack0);

handle({float}, Data0, Errors0, Validator, Stack0) when is_float(Data0); is_integer(Data0) ->
    val(Validator, Data0, Errors0, Stack0);
handle({float}, Data0, Errors0, Validator, Stack0) ->
    fix(Validator, Data0, ?INVALID_FLOAT, Errors0, Stack0);

handle({any}, Data0, Errors0, Validator, Stack0) ->
    val(Validator, Data0, Errors0, Stack0);

handle({boolean}, Data0, Errors0, Validator, Stack0) when is_boolean(Data0) ->
    val(Validator, Data0, Errors0, Stack0);
handle({boolean}, Data0, Errors0, Validator, Stack0) ->
    fix(Validator, Data0, ?INVALID_BOOLEAN, Errors0, Stack0);

handle({null}, null, Errors0, Validator, Stack0) ->
    val(Validator, null, Errors0, Stack0);
handle({null}, Data0, Errors0, Validator, Stack0) ->
    fix(Validator, Data0, ?INVALID_NULL, Errors0, Stack0);

handle(Type, Data, Errors, _Validator, _Stack) ->
    erlang:error({invalid_type, Type}),
    {ok, Errors, Data}.

-spec iterate_hash({FName :: atom(), Obligatorness :: required | optional, Type :: field_type()},
                   {D0 :: any(),
                    {R0 :: list()},
                    E0 :: any(),
                    V :: any(),
                    S0 :: list()
                   }) -> {any(), {list()}, any(), any(), any()}.
iterate_hash({FName, Obligatoriness, Type}, {D0, {R0}, E0, V, S0}) ->
    case proplists:get_value(FName, D0) of
        %% required field is unset, we're trying to fix it
        undefined when required == Obligatoriness ->
            case fix(V, undefined, ?UNDEFINED_FIELD, E0, [FName | S0]) of
                {ok, E1, R1} ->
                    {D0, {R0++[{FName, R1}]}, E1, V, S0};
                {error, E1, _R1} ->
                    {D0, {R0}, E1, V, S0};
                {custom_error, E1, _R1} ->
                    {D0, {R0}, E1, V, S0}
            end;
        undefined ->
            {D0, {R0}, E0, V, S0};
        Value ->
            case handle(Type, Value, E0, V, [FName | S0]) of
                {ok, E1, R1} ->
                    {D0, {R0++[{FName, R1}]}, E1, V, S0};
                {error, E1, _R1} ->
                    {D0, {R0}, E1, V, S0};
                {custom_error, E1, _R1} ->
                    {D0, {R0}, E1, V, S0}
            end
    end;
iterate_hash(Any, {D0, R0, E0, V, S0}) ->
    error_logger:error_msg("Map definition error: invalid type: ~p", [Any]),
    {D0, R0, E0, V, S0}.

-spec path_by_stack(Stack :: list()) -> binary().
path_by_stack(Stack) ->
    lists:foldl(fun
        (Elem, <<>>) ->
            Elem;
        (Elem, Acc) ->
            <<Acc/binary, <<".">>/binary, Elem/binary>>
    end, <<>>, lists:reverse(Stack)).

-spec i_to_b(Int :: integer()) -> binary().
i_to_b(Int) when is_integer(Int) ->
    list_to_binary(integer_to_list(Int)).

-spec val(Validator :: fun((_, _, _) -> any()),
          Data :: any(),
          Errors :: any(),
          Stack :: list()
         ) -> {ok, any(), any()} | {error, nonempty_maybe_improper_list(), any()}.
val(Validator, Data, Errors, Stack) ->
    case Validator(validate, lists:reverse(Stack), Data) of
        {ok, valid} ->
            {ok, Errors, Data};
        {ok, NewVal} ->
            {ok, Errors, NewVal};
        {error, Code} ->
            {custom_error, [{Code, path_by_stack(Stack), lists:reverse(Stack)} | Errors], Data}
    end.

-spec fix(Validator :: fun((_, _, _) -> any()),
          Data :: any(),
          Code :: binary(),
          Errors :: any(),
          Stack :: list()
         ) -> {ok, any(), any()} | {error, nonempty_maybe_improper_list(), any()}.
fix(Validator, Data, Code, Errors, Stack) ->
    case Validator(fix, lists:reverse(Stack), Data) of
        {ok, NewVal} ->
            {ok, Errors, NewVal};
        {error, invalid} ->
            {error, [{Code, path_by_stack(Stack), lists:reverse(Stack)} | Errors], Data};
        {error, NewCode} ->
            {custom_error, [{NewCode, path_by_stack(Stack), lists:reverse(Stack)} | Errors], Data}
    end.
