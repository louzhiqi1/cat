#! /bin/bash
# litaocheng
# 代码根目录
ROOT=`cd $(dirname $0)/../../; pwd`
#echo $ROOT
WORK_DIR=$ROOT
CASE_SENSITIVE=false

usage() {
    echo "修改代码中文本信息"
    echo "比如将斗魂世界修改为斗破天下"
    echo "执行: $0 from to [选项]"
    echo "选项"
    echo " -d   - 指定目录(默认为代码根目录下所有文件)"
    echo " -i   - 大小写敏感"
}

if [ $# -lt 2 ]; then
    usage
    exit 1
fi

FROM=$1
TO=$2
shift 2
# 解析参数
while [ $# -ne 0 ] ; do
    PARAM=$1
    #echo "$PARAM"
    shift
    case $PARAM in
        -d) 
            WORK_DIR=$1
            shift;;
        -i) 
            CASE_SENSITIVE=true
            ;;
        *)
            usage
            exit 1;;
    esac
done

cd ${WORK_DIR}
L=$(grep -ri "${FROM}" . | cut -d':' -f 1 | grep -v '.beam' | grep -v '.exe' | grep -v '.svn' | grep -v '$0' | uniq)

if [ "$L" = "" -o "$L" = " " ]; then
    echo "没有文件需要替换"
    exit 0
fi

echo "首先备份将要修改的文件..."
tar czf chang_name_`date +%Y-%H%M`.tar.gz ${L}

echo "替换文件..."
for FILE in ${L}; do
    if [ "$CASE_SENSITIVE" = "false" ]; then
        FROM_LOW=`echo $FROM | tr '[A-Z]' '[a-z]'`
        TO_LOW=`echo $TO | tr '[A-Z]' '[a-z]'`
        FROM_UP=`echo $FROM | tr '[a-z]' '[A-Z]'`
        TO_UP=`echo $TO | tr '[a-z]' '[A-Z]'`
        sed -i -e "s/${FROM_LOW}/${TO_LOW}/g" \
           -e "s/${FROM_UP}/${TO_UP}/g" ${FILE}
    else
        sed -i -e "s/${FROM}/${TO}/g" ${FILE}
    fi
    if [ $? -eq 0 ]; then
        echo "文件${FILE}替换成功!"
    else
        echo "文件${FILE}名字修改失败!"
        exit 1
    fi
done
