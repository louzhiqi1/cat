#! /bin/bash
# 游戏数据库备份和恢复
ROOT=`cd $(dirname $0)/..; pwd`
BACKUP_DIR=$ROOT/sql/backup
DB_NAME=
DB_USER=root
DB_PASS=`cat /data/save/mysql_root 2> /dev/null`
[ -z "$DB_PASS" ] && DB_PASS=root
SERVER="local"
SHOW_CONFIRM=true

if ! mkdir -p ${BACKUP_DIR} 2> /dev/null; then
    echo "创建备份目录失败，检查权限"
    exit 1
fi

# 用法
usage () {
    echo "备份数据库"
    echo "用法:"
    echo "$0 Action"
    echo ""
    echo " Action     - 操作:backup备份;import导入"
    echo " -u User    - 数据库用户"
    echo " -p Pass    - 数据库密码"
    echo " -d Database- 数据库名"
    echo " -f DumpFile - 导入的数据库文件"
    echo " -r DbDir   - 导入的数据库目录"
    echo " -q         - 不需要提示"
    echo ""
}

# 错误
error() {
    echo $1
    exit 1
}

# 备份本地数据库
backup() {
    echo "备份$DB_NAME 到 ${BACKUP_DIR}/${DB_BACKUP} ... "
    if ! mysqldump -u ${DB_USER} -p${DB_PASS} ${DB_NAME} > ${BACKUP_DIR}/${DB_BACKUP}; then
        echo "失败"
        exit 1
    fi
}

# 导入数据库(单文件)
import_file() {
    DB_FILE=$1
    DB_FILENAME=`basename ${DB_FILE}`
    if [ -z "$DB_NAME" ]; then
        DB_NAME=${DB_FILENAME%%_back*}
    fi
    echo "导入$DB_FILE到$DB_NAME ... "
    mysql -u ${DB_USER} -p"${DB_PASS}" << EOF
drop database if exists ${DB_NAME};
create database ${DB_NAME};
use ${DB_NAME};
source ${DB_FILE};
EOF
    if [ $? -ne 0 ]; then
        error "导入数据库失败"
    fi  
}


# 导入数据库(目录)
import_dir() {
    if [ -z "$DB_NAME" ]; then
        error "请指定数据库名称! -d "
    fi
    echo "导入$DB_DIR到$DB_NAME ... "
    mysql -u ${DB_USER} -p"${DB_PASS}" << EOF
drop database if exists ${DB_NAME};
create database ${DB_NAME};
use ${DB_NAME};
source ${DB_DIR}/db_struc.sql;
EOF
    if [ $? -ne 0 ]; then
        error "导入创建数据库失败"
    fi  

    echo "导入数据库 ..."
    if ! mysqlimport --local -uroot -p"${DB_PASS}" ${DB_NAME} `find ${DB_DIR} -name "*.txt"`; then
        error "导入数据失败!"
    fi
}

# 显示数据库中的行数
show_rows_num () {
    echo "当前数据库${DB_NAME}中表的行数: "
    SQL="USE INFORMATION_SCHEMA; SELECT TABLE_NAME, TABLE_ROWS from TABLES where TABLE_SCHEMA='${DB_NAME}';"
    echo $SQL | mysql -u ${DB_USER} -p${DB_PASS} |
    while read LINE; do
        echo $LINE | awk '{printf "%-25s%s\n", $1, $2}'
    done
}

# 处理参数
if [ $# -eq 0 ]; then
    usage;
    exit 0
fi 

ACTION=$1;
shift;
while [ $# -ne 0 ] ; do
    PARAM=$1
    shift
    case $PARAM in
        --)break ;;
        -u) DB_USER=$1; shift ;;
        -p) DB_PASS=$1; shift ;;
        -d) DB_NAME=$1; shift ;;
        -f) DB_FILE=$1; shift;;
        -r) DB_DIR=$1; shift;;
        -q) SHOW_CONFIRM=false;;
        --help|-h) usage; exit 0;;
        *) usage; exit 0;;
    esac
done

if [ "$ACTION" = "backup" ]; then
    show_rows_num
fi
DB_BACKUP=${DB_NAME}_`date +'back_%Y%m%d%H%M'`.dump

if [ "${SHOW_CONFIRM}" = "true" ]; then
    echo -n "确定要执行${ACTION}操作么?(Yes/No): " 
    read SELECT
    if [ "$SELECT" != "Yes" ]; then
        exit 0
    fi
fi

if [ "$ACTION" = "backup" ]; then
    backup
elif [ "$ACTION" = "import" ]; then
    if [ -n "$DB_FILE" ] && [ -f $DB_FILE ]; then
        import_file $DB_FILE
    elif [ -n "$DB_DIR" ] && [ -d $DB_DIR ]; then
        import_dir $DB_DIR
    fi
fi
