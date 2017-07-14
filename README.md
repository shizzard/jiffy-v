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
 - `variant` (any listed type)

The last four types are composite; this means, that this types can be root types of your message map. But something tells us that in most cases root type of your message map will be `hash`.

Typing and maps
---------------

First of all we need to define message map. Map is an erlang term, describing your message format.

With the latest `0.4.0` version you're able to utilize erlang maps structures. To use maps, simple use `jiffy_vm` module instead of `jiffy_v`.

Both `jiffy_v` and `jiffy_vm` modules provide helper functions to describe maps. These functions are: `integer/0`, `float/0`, `string/0`, `boolean/0`, `null/0`, `hashfield/3`, `hash/1`, `list/1`, `enum/1`, `variant/1`. All datatypes are self-describing, but you can look down below to get the idea.

Following map shows most of the mapping variants:

```erlang
jiffy_v:hash([
    jiffy_v:hashfield(<<"hash">>, required, jiffy_v:hash([
        jiffy_v:hashfield(<<"subfield1">>, optional, jiffy_v:integer()),
        jiffy_v:hashfield(<<"subfield2">>, required, jiffy_v:enum([3.14, 2.71]))
    ])),
    jiffy_v:hashfield(<<"list">>, optional, jiffy_v:list([jiffy_v:float(), jiffy_v:boolean()])),
    jiffy_v:hashfield(<<"enum">>, required, jiffy_v:enum([<<"CONSTANT1">>, <<"CONSTANT2">>])),
    jiffy_v:hashfield(<<"variant">>, required, jiffy_v:variant([jiffy_v:integer(), jiffy_v:string()])),
    jiffy_v:hashfield(<<"boolean">>, required, jiffy_v:boolean()),
    jiffy_v:hashfield(<<"integer">>, required, jiffy_v:integer()),
    jiffy_v:hashfield(<<"float">>, required, jiffy_v:float()),
    jiffy_v:hashfield(<<"string">>, optional, jiffy_v:string()),
    jiffy_v:hashfield(<<"null">>, optional, jiffy_v:null()),
    jiffy_v:hashfield(<<"any">>, required, jiffy_v:any())
])
```

And the same for erlang-map-based map:

```erlang
jiffy_vm:hash([
    jiffy_vm:hashfield(<<"hash">>, required, jiffy_vm:hash([
        jiffy_vm:hashfield(<<"subfield1">>, optional, jiffy_vm:integer()),
        jiffy_vm:hashfield(<<"subfield2">>, required, jiffy_vm:enum([3.14, 2.71]))
    ])),
    jiffy_vm:hashfield(<<"list">>, optional, jiffy_vm:list([jiffy_vm:float(), jiffy_vm:boolean()])),
    jiffy_vm:hashfield(<<"enum">>, required, jiffy_vm:enum([<<"CONSTANT1">>, <<"CONSTANT2">>])),
    jiffy_vm:hashfield(<<"variant">>, required, jiffy_vm:variant([jiffy_vm:integer(), jiffy_vm:string()])),
    jiffy_vm:hashfield(<<"boolean">>, required, jiffy_vm:boolean()),
    jiffy_vm:hashfield(<<"integer">>, required, jiffy_vm:integer()),
    jiffy_vm:hashfield(<<"float">>, required, jiffy_vm:float()),
    jiffy_vm:hashfield(<<"string">>, optional, jiffy_vm:string()),
    jiffy_vm:hashfield(<<"null">>, optional, jiffy_vm:null()),
    jiffy_vm:hashfield(<<"any">>, required, jiffy_vm:any())
])
```



Using this you can build more complicated maps. For example, 3d-array of integers:

```erlang
%% 3D array of integers
jiffy_vm:list([
    jiffy_vm:list([
        jiffy_vm:list([{integer}])
    ])
])
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

In newer versions (0.5.0+) `Validator` function is to be passed for each term (see `/2` map constructor functions in `jiffy_v.erl` and `jiffy_vm.erl`). You can also see some examples in `test/*_tests.erl` files.

Todo tasks
----------

1. We can define `soft_list` type, which will be opposite to `strong_list` type, e.g. `{soft_list, [{string}, {integer}]}` will mean "list of strings or integers", not "list of strings or list of integers".
2. Sometimes it may be useful to return erroneous field value in error tuple: `{ErrorCode, FieldPath, ErroneousValue}`. **NB**: this can be applied only to elementary types, not composite ones.
3. It may be useful to add a little function to get data from decoded JSON term with XPath-style (to be used with `jiffy_v` module):

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
