#!/usr/bin/env bash

rm -rf .eunit/Mnesia.nonode@nohost
mkdir -p .eunit/Mnesia.nonode@nohost
escript test/init
