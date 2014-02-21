-module(erlastic_client).

-export([new/3]).
-export([host/1]).
-export([port/1]).

-record(client, {
          host = <<"127.0.0.1">> :: binary(),
          port = 9200
         }).

%% ===================================================================
%% Public
%% ===================================================================

new(Host, Port, _Opts) ->
    #client{host = Host, port = Port}.

host(#client{host = Host}) ->
    Host.

port(#client{port = Port}) ->
    Port.
