﻿CC?=cc
ANTLR?=antlr-3.4
ERL_ROOT?=/usr/local/Cellar/erlang/R15B01/lib/erlang/erts-5.9.1
EMUD_LIBS=../:../../debs
CCARGS=-undefined dynamic_lookup -l antlr3c -I ${ERL_ROOT}/include

.PHONY: test

build: priv/emud_cmd_parser.so
	ERL_LIBS=$(EMUD_LIBS) ../../rebar compile

priv/emud_cmd_parser.so: c_src/MudParser.c c_src/emud_cmd_parser.c
	${CC} ${CCARGS} -o priv/emud_cmd_parser.so c_src/*.c 

c_src/MudParser.c: c_src/Mud.g
	antlr-3.4 c_src/Mud.g

shell: build
	erl -pa ebin

test: build
	ERL_LIBS=$(EMUD_LIBS) ../../rebar eunit

clean:
	rm c_src/Mud*.c
	rm c_src/Mud*.h
	rm Mud.tokens
	rm priv/emud_cmd_parser.so
	../../rebar clean
