Jiffy-V
=======

This module is a validation utility for Jiffy-like (https://github.com/davisp/jiffy) JSON-to-term notation.

Typing
------

We have a number of types in JSON, such as integers, floats, strings, etc. In addition to this types we have defined so-called typed lists: a lists with elements of defined type.

Jiffy-V can handle the following types:

 - `integer`
 - `float`
 - `boolean`
 - `string`
 - `null`
 - `any` ('any' type matches any erlang term)
 - `hash` (or associative array, key-value array)
 - typed `list`
 - `enum` (a number of predefined values)

The last three types are composite; this means, that this types can be root types of your message map. But something tells us that in most cases root type of your message map will be `hash`.

Typing and maps
---------------

First of all we need to define message map. Map is an erlang term, describing your message format. 

All of elementary types are desribed as tupled atom: `{integer}`, `{boolean}`, etc.

Typed list is described as a tuple `{list, [type1, type2, ...]}`. **NB**: list of types means, that all element of list must be typed with one type, e.g. `{list, [{string}, {integer}]}` means "list of strings or list of integers", not "list of strings or integers".

Enum is described as a tuple `{enum, [value1, value2, ...]}`. 

Hash is described as a tuple `{hash, [field1, field2, ...]}`. Each of the hash fields is described as a tuple `{field_name, required|optional, type}`.

Following map shows most of the mapping variants:

```erlang
{hash, [
    {<<"hash">>, required, {hash, [
        {<<"subfield1">>, optional, {integer}},
        {<<"subfield2">>, required, {enum, [3.14, 2.71]}}
    ]}},
    {<<"list">>, optional, {list, [{float}, {boolean}]}},
    {<<"enum">>, required, {enum, [<<"CONSTANT1">>, <<"CONSTANT2">>]}},
    {<<"boolean">>, required, {bool}},
    {<<"integer">>, required, {integer}},
    {<<"float">>, reuqired, {float}},
    {<<"string">>, optional, {string}},
    {<<"null">>, optional, {null}},
    {<<"any">>, required, {any}}
]}
```



Using this you can build more complicated maps. For example, 3d-array of integers:

```erlang
%% 3D array of integers
{list, [
    {list, [
        {list, [{integer}]}
    ]}
]}
```



Usage
-----

After describing the map you can start validation routines. Just call `jiffy_v:validate(Map, Data)`. Result of this call is a tuple `{Errors, Result}`, so you can use this snippet:

```erlang
case jiffy_v:validate(Map, Data) of 
    {[], Result} ->
        %% safe data processing
    {Errors, _Result} ->
        %% handling errors
end
```



`Errors` tuple element is a list of error tuples. Each error is `{ErrorCode, FieldPathBinary, FieldPath}` tuple; `ErrorCode` is one of predefined binary strings describing the error occured (this description can be overwritten); `FieldPathBinary` is string like `<<"field.0.anotherfield.5.errorfield">>` (very similar to XPath); `FieldPath` is a list like `[<<"field">>,<<"0">>,<<"anotherfield">>,<<"5">>,<<"errorfield">>]` (splitted XPath).

`Result` is Jiffy-like term, containing only valid and mapped values; e.g. if you are not describing field `foo` in your root hash, this field will never pass to result term, even this field passes with decoded JSON to validator.

Validation and fixing
---------------------

You can pass custom validation function to Jiffy-V to provide more flexible validation, based not only on data types. 

Jiffy-V calls this function in two cases: 

First. Field is validated successfully; function called as `Function(validate, [path, to, field], Value)`. Function must return one of the following tuples:

 - `{ok, valid}`; in this case current field value will be passed to result term.
 - `{ok, NewValue}`; in this case `NewValue` will be passed to result term as a value of current field.
 - `{error, Code}`; in this case Jiffy-V will generate an error with `Code` as `ErrorCode`.

Second. Field is invalidated; function called as `Function(fix, [path, to, field], Value)`. Function must return one of the following tuples:

 - `{ok, NewValue}`; in this case current field will be "fixed" and `NewValue` will be passed to result term.
 - `{error, invalid}`; in this case Jiffy-V will generate standard error.
 - `{error, NewCode}`; in this case Jiffy-V will generate an error with `NewCode` as `ErrorCode`.

Here is a little example of validate function usage: what we want is to map enum values to logical values:

```erlang
validator(validate, [<<"enum">>], <<"SEX_MALE">>) ->
    {ok, 0};
validator(validate, [<<"enum">>], <<"SEX_FEMALE">>) ->
    {ok, 1};
validator(validate, [<<"enum">>], <<"SEX_ANY">>) ->
    {ok, 2};
validator(validate, _, _) ->
    {ok, valid};
validator(fix, _, _) ->
    {error, invalid}.
```



**NB**: do not forget to include list index to validator function pattern match:

```erlang
validator(validate, [<<"list">>, _, <<"foobar">>], Value) when Value == 3.14 ->
    {error, <<"PI_VALUE_IS_NOT_ALLOWED">>};
validator(validate, _, _) ->
    {ok, valid};
validator(fix, _, _) ->
    {error, invalid}.
```



Todo tasks
----------

1. Hash fields can be defined as multityped like lists, e.g. `{<<"hashfield">>, required, [{integer}, {string}]}`.
2. We can define `soft_list` type, which will be opposite to `strong_list` type, e.g. `{soft_list, [{string}, {integer}]}` will mean "list of strings or integers", not "list of strings or list of integers".
3. Sometimes it may be useful to return erroneous field value in error tuple: `{ErrorCode, FieldPath, ErroneousValue}`. **NB**: this can be aaplied only to elementary types, not composite ones.
4. It may be useful to add a little function to get data from decoded JSON term with XPath-style:

```erlang
get(undefined, _Keys) ->
    {error, undefined};
get(Value, Key) when not is_list(Key) ->
    get(Value, [Key]);
get(Value, []) ->
    {ok, Value};
get({Proplist}, [Key | Keys]) ->
    Struct = proplists:get_value(Key, Proplist),
    get(Struct, Keys);
get(_Value, Keys) ->
    get(undefined, Keys).
```