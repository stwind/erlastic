-module(erlastic).

-export([new/3]).
-export([index_doc/4]).
-export([index_doc/5]).
-export([get_doc/4]).
-export([get_doc/5]).
-export([delete_doc/4]).
-export([delete_doc/5]).
-export([update_doc/5]).
-export([update_doc/6]).

-type vals() :: list({binary(), binary()}).

-record(req, {
          path = <<>> :: binary(),
          method = get :: atom(),
          body = [] :: vals(),
          qs = [] :: vals(),
          opts = [] :: list(term())
         }).

%% ===================================================================
%% Public
%% ===================================================================

new(Host, Port, Opts) ->
    erlastic_client:new(Host, Port, Opts).

index_doc(Client, Index, Type, Doc) ->
    index_doc(Client, Index, Type, Doc, []).

index_doc(Client, Index, Type, Doc, Opts) ->
    Req = #req{path = path({Index, Type}), method = post, body = Doc, opts = Opts}, 
    send(Client, parse_opt(index_doc, Opts, Req)).

get_doc(Client, Index, Type, Id) ->
    get_doc(Client, Index, Type, Id, []).

get_doc(Client, Index, Type, Id, Opts) ->
    Req = #req{path = path({Index, Type, Id}), method = get, opts = Opts}, 
    send(Client, parse_opt(get_doc, Opts, Req)).

delete_doc(Client, Index, Type, Id) ->
    delete_doc(Client, Index, Type, Id, []).

delete_doc(Client, Index, Type, Id, Opts) ->
    Req = #req{path = path({Index, Type, Id}), method = delete, opts = Opts}, 
    send(Client, parse_opt(delete_doc, Opts, Req)).

update_doc(Client, Index, Type, Id, Body) ->
    update_doc(Client, Index, Type, Id, Body, []).

update_doc(Client, Index, Type, Id, Body, Opts) ->
    Req = #req{path = path({Index, Type, Id},<<"/_update">>), body = Body,
               method = post, opts = Opts}, 
    send(Client, parse_opt(update_doc, Opts, Req)).

%% ===================================================================
%% Private
%% ===================================================================

prefix(Client) ->
    Host = erlastic_client:host(Client),
    Port = erlastic_client:port(Client),
    <<"http://",Host/binary,":",(erlastic_lib:to_b(Port))/binary>>.

url(Client, #req{path = Path, qs = KVs}) ->
    iolist_to_binary([prefix(Client), Path, 
                      [<<"?">> || KVs /= []], hackney_url:qs(KVs)]).

path({Index, Type}) ->
    <<"/",Index/binary,"/",Type/binary>>;
path({Index, Type, Id}) ->
    <<"/",Index/binary,"/",Type/binary,"/",Id/binary>>.

path(Prefix, Suffix) ->
    <<(path(Prefix))/binary,Suffix/binary>>.

send(Client, #req{method = Method, opts = Opts, body = Body} = Req) ->
    ConnectOpts = proplists:get_value(connect_opts, Opts, []),
    Url = url(Client, Req),
    Res = hackney:request(Method, Url, [], encode_body(Body), ConnectOpts),
    case Res of
        {ok, Status, _, Ref} when Status >= 200, Status < 300 ->
            {ok, json_body(Ref)};
        {ok, 400, _, Ref} ->
            {error, raw_body(Ref)};
        {ok, 404, _, Ref} ->
            {error, {not_found, json_body(Ref)}};
        {ok, Status, _, Ref} ->
            {error, {internal_error, Status, json_body(Ref)}};
        {error, Reason} ->
            {error, Reason}
    end.

encode_body(Body) when is_binary(Body) ->
    Body;
encode_body(Body) when is_list(Body) ->
    erlastic_lib:to_json(Body).

parse_opt(_, [], Req) -> Req;

parse_opt(index_doc, [{id, Id} | Rest], #req{path = Path} = Req) ->
    parse_opt(index_doc, Rest, Req#req{path = <<Path/binary,"/",Id/binary>>, method = put});

parse_opt(get_doc, [{source, true} | Rest], #req{path = Path} = Req) ->
    parse_opt(get_doc, Rest, Req#req{path = <<Path/binary,"/_source">>});

parse_opt(Type, [{Key, Val} | Rest], Req) ->
    parse_opt(Type, Rest, common_qs(Key, Val, Req)).

json_body(Ref) ->
    {ok, ResBody} = hackney:body(Ref),
    erlastic_lib:from_json(ResBody).

raw_body(Ref) ->
    {ok, Body} = hackney:body(Ref),
    Body.

common_qs(version, Val, Req) ->
    to_qs(<<"version">>, Val, Req);
common_qs(fields, Vals, Req) ->
    to_qs(<<"fields">>, hackney_bstr:join(Vals, <<",">>), Req);
common_qs(routing, Val, Req) ->
    to_qs(<<"routing">>, Val, Req);
common_qs(timeout, Val, Req) ->
    to_qs(<<"timeout">>, Val, Req);
common_qs(timestamp, Val, Req) ->
    to_qs(<<"timestamp">>, Val, Req);
common_qs(op_type, Val, Req) ->
    to_qs(<<"op_type">>, Val, Req);
common_qs(ttl, Val, Req) ->
    to_qs(<<"ttl">>, Val, Req);
common_qs(parent, Val, Req) ->
    to_qs(<<"parent">>, Val, Req);
common_qs(source_include, Val, Req) ->
    to_qs(<<"source_include">>, Val, Req);
common_qs(source_exclude, Val, Req) ->
    to_qs(<<"source_exclude">>, Val, Req);
common_qs(refresh, Val, Req) ->
    to_qs(<<"refresh">>, Val, Req);
common_qs(replication, Val, Req) ->
    to_qs(<<"replication">>, Val, Req);
common_qs(consistency, Val, Req) ->
    to_qs(<<"consistency">>, Val, Req);
common_qs(retry_on_conflict, Val, Req) ->
    to_qs(<<"retry_on_conflict">>, Val, Req);
common_qs(_, _, Req) ->
    Req.

to_qs(Key, Val, #req{qs = Qs} = Req) ->
    Req#req{qs = [{Key, Val} | Qs]}.
