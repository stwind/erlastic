#!/bin/sh
ERL=erl
NODE_NAME=erlastic@127.0.0.1
CONFIG=priv/app.config
LIBS_DIR=deps

exec erl -pa ebin \
    -boot start_sasl \
    -setcookie $COOKIE \
    -name $NODE_NAME \
    -env ERL_LIBS $LIBS_DIR \
    -config $CONFIG \
    -eval "application:ensure_all_started(erlastic)" \
    -s sync go
