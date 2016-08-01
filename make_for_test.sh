#!/bin/bash

mkdir -p ebin
erl -make
erlc -o ebin -DTEST -I include src/client/public_transport/public_transport.erl debug_pt.erl
