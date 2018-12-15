#!/usr/bin/env bash

if [ -n "$1" ]; then
    echo "$1";
    PROFILE="$1";
else
  PROFILE="local";
fi;

PWD=$(pwd)
START_SCRIPT="$PWD/start.sh"
ERLANG_DEPS_PATH="${PWD}/deps"

if [ -z "$2" ]; then
    USER_ARGS="";
else
    USER_ARGS="$2";
fi

if [ -d "${PWD}/_build/${PROFILE}" ]; then
    ERLANG_DEPS_PATH="${PWD}/_build/${PROFILE}/lib";
fi;

if [ -f "${START_SCRIPT}" ]; then
    eval=$(egrep -o '^exec .+$' "${START_SCRIPT}" | sed 's/exec //g; s/${1}/local/g');
    eval "$eval $args";
else
    ERLANG_START_ARGUMENTS="-pa ebin/"
    ERLANG_START_ARGUMENTS="$ERLANG_START_ARGUMENTS -pa ${ERLANG_DEPS_PATH}/*/ebin";
    if [ -d "_checkouts" ]; then
        ERLANG_START_ARGUMENTS="$ERLANG_START_ARGUMENTS -pa ${PWD}/_checkouts/*/ebin";
        echo "Adding checkouts to path!";
    fi;

    ERLANG_CMD="erl -args_file ${PWD}/env/${PROFILE}.vmargs -config ${PWD}/env/${PROFILE}.config $ERLANG_START_ARGUMENTS $USER_ARGS"
    exec env ERL_LIBS=".:${ERLANG_DEPS_PATH}" ${ERLANG_CMD}
fi;
