#! /bin/bash
# 从client-res/config/erl拷贝数据
ROOT=`cd $(dirname $0)/../..; pwd`

WORK_DIR=`mktemp -d`

usage() {
    echo "用法:"
    echo "$0 语言"
    echo "语言类型: zh(中文), en(英文)"
}


if [ $# -eq 0 ]; then
    echo "请指明语言!"
    usage
    exit 1
fi
DATA_LANG=$1

# svn路径
case ${DATA_LANG} in
    "zh") ;;
    "en") ;;
    *) echo "非法的语言类型!"; exit 1;;
esac
SVN_URL=http://192.168.19.211/svn/client-res/trunk/config/erl_${DATA_LANG}
DATA_DIR=${ROOT}/data_${DATA_LANG}
echo "svn url:${SVN_URL}"
echo "data dir:${DATA_DIR}"

# checkout代码
checkout() {
    if !(cd $WORK_DIR && svn co $SVN_URL .); then
        echo "获取svn出错!"
        exit 1
    fi
    if !(cd $WORK_DIR && cp *.erl ${DATA_DIR}); then
        echo "拷贝erl代码失败"
        exit 1
    fi
    if !(cd $WORK_DIR && cp ecode.hrl ${ROOT}/include/ecode_${DATA_LANG}.hrl); then
        echo "拷贝hrl代码失败"
        exit 1
    fi
}

# 清理
clear_temp() {
    rm -rf ${WORK_DIR}
}

trap clear_temp SIGINT SIGTERM EXIT

# checkout代码
echo "svn路径:$SVN_URL"
checkout
