#! /bin/bash
# 使用mysqlhotcopy备份线上数据库
# 生成文件dbname_hotcopy_yyyymmddhh.tar.gz
ROOT=`cd $(dirname $0)/..; pwd`
BACKUP_DIR=$ROOT/sql/backup/hotcopy
DB_USER=root
DB_PASS=`cat /data/save/mysql_root 2> /dev/null`
[ -z "$DB_PASS" ] && DB_PASS=root
SERVER="local"
DB_NAME=mxqy_1
TABLE_LIST=

# 用法
usage () {
    echo "备份线上数据库"
    echo "用法:"
    echo ""
    echo " -u User    - 数据库用户"
    echo " -p Pass    - 数据库密码"
    echo " -d Database- 数据库名"
    echo ""
}

# 获取数据库中的表名
get_table_names() {
    LIST=""
    SQL="USE INFORMATION_SCHEMA; SELECT TABLE_NAME from TABLES where TABLE_SCHEMA='${DB_NAME}';"
    echo $SQL | mysql -s -n -u ${DB_USER} -p${DB_PASS} | awk '{print $0}'
}

# 处理参数
if [ $# -eq 0 ]; then
    usage;
    exit 0
fi 

while [ $# -ne 0 ] ; do
    PARAM=$1
    shift
    case $PARAM in
        --)break ;;
        -u) DB_USER=$1; shift ;;
        -p) DB_PASS=$1; shift ;;
        -d) DB_NAME=$1; shift ;;
        --help|-h) usage; exit 0;;
        *) usage; exit 0;;
    esac
done

# 生成work_dir
TAR_FILE=${DB_NAME}_hotcopy_`date +'%Y%m%d%H'`
WORK_DIR=$BACKUP_DIR/${TAR_FILE}
if ! mkdir -p ${WORK_DIR} 2> /dev/null; then
    echo "创建hotcopy备份目录:${WORK_DIR}失败，检查权限"
    exit 1
fi

# 获取表
TABLE_LIST=`get_table_names`
echo "数据库${DB_NAME}中表:${TABLE_LIST}"

# 备份表
for TABLE in ${TABLE_LIST}; do
    echo "mysqlhotcopy -u ${DB_USER} -p ${DB_PASS} --addtodest ${DB_NAME}./${TABLE}/ ${WORK_DIR}"
    if !(mysqlhotcopy -u ${DB_USER} -p ${DB_PASS} --addtodest ${DB_NAME}./${TABLE}/ ${WORK_DIR}); then
        echo "备份表${DB_NAME}到目录${WORK_DIR}失败"
        exit 1
    fi
    sleep 20
done

# 打包数据
if !(cd ${BACKUP_DIR} && tar czf ${TAR_FILE}.tar.gz ${TAR_FILE} \
    && rm -rf ${TAR_FILE}); then
    echo "打包备份数据失败!"
    exit 1
fi
echo "备份成功:${TAR_FILE}.tar.gz"
