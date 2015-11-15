#!/bin/sh

TMP=`mktemp`

echo "-module(user_default)." | tee user_default.erl > $TMP

echo "-compile([export_all])." | tee -a user_default.erl > $TMP

find ../program/ -name "*.hrl" | grep -v global2.hrl | awk -F/ '{print "-include(\""$NF"\")."}' | tee -a user_default.erl > $TMP

erlc -I ../program/include/ -I ../framework/include/ -I ../framework/include/third/ +debug_info user_default.erl 

mv user_default.beam ../program/app/

rm user_default.erl $TMP
