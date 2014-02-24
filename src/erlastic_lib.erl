-module(erlastic_lib).

-export([to_b/1]).
-export([to_json/1]).
-export([from_json/1]).

-define(PRIMITIVE(Value), 
    is_binary(Value);
    is_float(Value);
    is_integer(Value);
    is_boolean(Value);
    Value == null).

%% ===================================================================
%% Public
%% ===================================================================

to_b(Val) when is_integer(Val) ->
    integer_to_binary(Val);
to_b(Val) when is_atom(Val) ->
    list_to_binary(atom_to_list(Val)).

to_json(Term) ->
    jiffy:encode(make_io(Term,[], true),[force_utf8]).

from_json(Bin) ->
    format(jiffy:decode(Bin), []).

%% ===================================================================
%% Private
%% ===================================================================

make_io([], Acc, false) ->
    {lists:reverse(Acc)};
make_io([], Acc, true)->
    lists:reverse(Acc);
make_io([{Key, {}} | Rest], Acc, _Type) ->
    make_io(Rest, [{Key, {[]}} | Acc], false);
make_io([{Key, Value} | Rest], Acc, _Type) when is_list(Value) ->
    make_io(Rest, [{Key, make_io(Value,[], true)} | Acc], false);
make_io([{Key, Value} | Rest], Acc, _Type) when ?PRIMITIVE(Value) ->
    make_io(Rest, [{Key, Value} | Acc], false);
make_io([{Key, undefined} | Rest], Acc, _Type) ->
    make_io(Rest, [{Key, null} | Acc], false);
make_io([{Key, Value} | Rest], Acc, _Type) ->
    make_io(Rest, [{Key, to_str(Value)} | Acc], false);
make_io([Value | Rest], Acc, Type) when is_list(Value) ->
    make_io(Rest, [make_io(Value, [], true) | Acc], Type);
make_io([KV | Rest], Acc, Type) ->
    make_io(Rest, [KV | Acc], Type).

format([], Acc) ->
    lists:reverse(Acc);
format([{Key, Value} | Rest], Acc) ->
    format(Rest, [{Key, format(Value, [])} | Acc]);
format([{Value} | Rest], Acc) ->
    format(Rest, [format(Value, []) | Acc]);
format([Value | Rest], Acc) when is_list(Value)->
    format(Rest, [format(Value, []) | Acc]);
format([Value | Rest], Acc) ->
    format(Rest, [Value | Acc]);
format({Value}, Acc) ->
    format(Value, Acc);
format(Value, _Acc) ->
    Value.

to_str(Term) ->
    oneline(io_lib:format("~p",[Term])).

oneline(Str) ->
    replace(Str, "\n\s*", "").

replace(Str, Old, New) ->
    re:replace(Str, Old, New, [global,{return,binary}]).
