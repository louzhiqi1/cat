#! /bin/bash
ROOT=`cd $(dirname $0)/..; pwd`
#echo ${ROOT}
echo -n "代码行数:"
find . -name "*.[he]rl" | xargs grep -h '^[^%]' | wc -l
