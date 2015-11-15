#!/bin/sh
PROJ="net_encrypt"

OUT="../../../lib/"

INC="-I ../erl5.10/include/"

FLAG="-fPIC -shared"

rm -rf *.o

gcc $FLAG $INC -o $OUT$PROJ.so *.c

rm -rf *.o