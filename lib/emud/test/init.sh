#!/usr/bin/env bash

rm -rf .test/Mnesia.nonode@nohost
mkdir -p .test/Mnesia.nonode@nohost
escript test/init
