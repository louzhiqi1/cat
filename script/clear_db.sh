#! /bin/bash
# 游戏清档和备份,请慎重使用!
# 1,连接目标服务器，首先进行备份
# 2,删除数据
# 3,删除rutime数据
# 4,删除log数据
ROOT=`cd $(dirname $0)/..; pwd`
BACKUP_DIR=$ROOT/sql/backup
DB_USER=root
DB_PASS=`cat /data/save/mysql_root 2> /dev/null`
[ -z "$DB_PASS" ] && DB_PASS=root
RUNTIME_DIR=${ROOT}/runtime
TODAY_STR=`date +'%Y_%m_%d_%H_%M'`
DB_DONT_BACKUP=false

if ! mkdir -p ${BACKUP_DIR} 2> /dev/null; then
    echo "创建备份目录失败，检查权限"
    exit 1
fi

# 用法
usage () {
    echo "游戏清档,清理${DB}和相关的runtime文件(会首先备份)"
    echo "清理mxqy_Agent_ServerId和mxqy_admin_Agent_ServerId两个数据库"
    echo "用法:"
    echo "./clear_db.sh"
    echo ""
    echo " -a Agent     - 代理名称(默认game.conf中的platform)"
    echo " -i ServerId  - 服务器id(默认game.conf中的server_id)"
    echo " -p Pass      - root密码(默认/data/save/mysql_root)"
    echo " -n           - 清档前不备份数据库"
    echo " -da         - 删除t_log_admin表"
    echo " -dc         - 删除card表"
    echo " -dn         - 删除notice表"
    echo ""
}

# 生成删除sql语句
gen_sql() {
    SQL=
    for TAB in $@
    do
        #SQL="${SQL}TRUNCATE TABLE ${TAB};"
        SQL="${SQL}DELETE FROM \`${TAB}\`;"
    done
    echo ${SQL}
}

# 生成数据库的所有表
get_tables() {
    DB=$1
    SQL="SHOW TABLES"
    declare RESULT=`echo $SQL | mysql -n -s -u ${DB_USER} -p${DB_PASS} ${DB}`
    echo $RESULT
}

# 获取game.conf中某个配置
game_conf_value() {
    KEY=$1
    if [ -n "$KEY" ]; then
        echo `cat ${ROOT}/config/game.conf | grep "${KEY}" | tail -n 1 | sed -e "s/\s*{${KEY},\s*\(.*\)}.*/\1/"`
    fi  
}

# 清理数据库
# 参数1为数据库名字
# 参数2为过滤的数据表列表
clear_db() {
    DB=$1
    shift
    TABLE_SKIP=$@
    echo "${DB}跳过表:${TABLE_SKIP}"
    if [ ${DB_DONT_BACKUP} != "true" ]; then
        DST=${BACKUP_DIR}/${DB}_${TODAY_STR}.dump
        echo "备份$DB 到 ${DST} "
        if ! mysqldump -u ${DB_USER} -p${DB_PASS} $DB > ${DST}; then
            echo "失败"
            exit 1
        fi
    fi
    TABLE_ALL=$(get_tables ${DB})

    TABLE_DELETE=
    for T in ${TABLE_ALL}; do
        TABLE_SKIP_2=${TABLE_SKIP/${T}/}
        if [ "${TABLE_SKIP}" = "${TABLE_SKIP_2}" ]; then
            TABLE_DELETE="${TABLE_DELETE} ${T}"
        fi
    done
    echo "${DB}删除表:${TABLE_DELETE}"

    SQL=$(gen_sql ${TABLE_DELETE})
    #echo "sql is ${SQL}"
    if ! echo ${SQL} | mysql -u ${DB_USER} -p${DB_PASS} ${DB}; then
        echo "删除数据库失败!"
    fi
    show_rows_num $DB
    echo "${DB}清理成功!"
    echo ""
    echo ""
}

# 显示数据库中的行数
show_rows_num () {
    DB=$1
    echo "======================================="
    echo " 当前数据库${DB}中表的行数 "
    echo "======================================="
    SQL="USE INFORMATION_SCHEMA; SELECT TABLE_NAME, TABLE_ROWS from TABLES where TABLE_SCHEMA='${DB}';"
    echo $SQL | mysql -u ${DB_USER} -p${DB_PASS} |
    while read LINE; do
        echo $LINE | awk '{printf "%-25s%s\n", $1, $2}'
    done
}

# 处理参数
while [ $# -ne 0 ] ; do
    PARAM=$1
    shift
    case $PARAM in
        --)break ;;
        -a) AGENT=$1; shift;;
        -i) SERVER_ID=$1;shift;;
        -p) DB_PASS=$1; shift ;;
        -n) DB_DONT_BACKUP=true;;
        -da) DELETE_ADMIN=true;;
        -dc) DELETE_CARD=true;;
        -dn) DELETE_NOTICE=true;;
        --help|-h) usage; exit 0;;
        *) usage; exit 0;;
    esac
done

if [ -z ${AGENT} ] || [ -z ${SERVER_ID} ]; then
    AGENT=$(game_conf_value platform)
    SERVER_ID=$(game_conf_value server_id)
    AGENT=${AGENT//\"/}
fi

if ${ROOT}/gamectl status > /dev/null 2>&1; then
   echo "游戏仍在运行中，清档时请首先停止服务器!"
   exit 1
fi

DB_GAME=mxqy_${AGENT}_${SERVER_ID}
DB_ADMIN=mxqy_admin_${AGENT}_${SERVER_ID}
echo -n "确定要清理清除数据库:${DB_GAME}和${DB_ADMIN}吗?(Yes/No): " 
read SELECT
if [ "$SELECT" != "Yes" ]; then
    exit 0
fi

# 清除数据库
DB_TABLE_SKIP=
if [ "$DELETE_CARD" != "true" ]; then
    DB_TABLE_SKIP=" card"
fi
if [ "$DELETE_NOTICE" != "true" ]; then
    DB_TABLE_SKIP=$DB_TABLE_SKIP" notice"
fi
clear_db $DB_GAME $DB_TABLE_SKIP

DB_TABLE_SKIP=
if [ "$DELETE_ADMIN" != "true" ]; then
    DB_TABLE_SKIP=" t_log_admin"
fi
clear_db $DB_ADMIN $DB_TABLE_SKIP

# 3
echo "清理runtime文件 ..."
rm -rf $RUNTIME_DIR/*

# 4
LOG_DIR=/data/logs/mxqy/${AGENT}s${SERVER_ID}/wait
echo "清理log文件:${LOG_DIR} ..."
rm -rf $LOG_DIR/*
