#! /bin/bash
# 更新数据库
# 1,确定游戏已经停止
# 2,备份数据库
# 3,分析mxqy.sql,生成update/_sql_*.erl
# 4,根据时间顺序执行update/_db_*.erl和_sql_*.erl
# 5,更新完成，执行清理
ROOT=`cd $(dirname $0)/..; pwd`
WORK_DIR=`mktemp -d`
CONF_DIR=$ROOT/config
UPDATE_DIR=${ROOT}/sql/update
BACKUP_DB_SCRIPT=${ROOT}/script/backup_db.sh
SQL_FILE=${ROOT}/sql/mxqy.sql
GEN_SQL_ESCRIPT=${ROOT}/sql/gen_sql.escript
UPDATE_TABLE_ESCRIPT=${ROOT}/sql/update_table.escript

DUMP_DIR=${ROOT}/script/mysql
DB_NAME=
DB_USER=root
DB_PASS=`cat /data/save/mysql_root 2> /dev/null`
DB_EXIST=true
[ -z "$DB_PASS" ] && DB_PASS=root

#---------
# 选项
#---------
# 数据库版本
DB_VERSION=
# 数据库是否备份
DB_BACKUP=true
# 显示确认
SHOW_CONFIRM=true

# 用法
usage () {
    echo "更新数据库"
    echo "用法:"
    echo "$0.sh"
    echo ""
    echo " -u       - 数据库用户名(默认root)"
    echo " -p       - 数据库密码(首先读取mysql_root随后root)"
    echo " -d       - 数据库名(默认从config/game.conf获得)"
    echo " -r       - 数据库起始版本(首先从db_version表读取，如果为空则使用此版本,格式20121112)"
    echo " -n       - 不要进行备份"
    echo " -q       - 不弹出确认"
}

# 显示确认信息
show_confirm() {
    echo "$1? (Yes/No)"
    read CONFIRM
    if [ "$CONFIRM" != "Yes" ]; then
        exit 0
    fi  
}

# 打印分割线
show_line() {
    echo "==========================================================="
}

# 出错
error() {
    echo "错误:$1"
    exit $2
}

# 获取game.conf中某个配置
game_conf_value() {
    KEY=$1
    if [ -n "$KEY" ]; then
        echo `cat $CONF_DIR/game.conf | grep "${KEY}" | tail -n 1 | sed -e "s/\s*{${KEY},\s*\(.*\)}.*/\1/"`
    fi  
}

# 获取数据库名
get_db_name() {
    #{db_game, {"localhost", 3306, "root", "root", "mxqy_1", 8}}.
    PLATFORM=$(game_conf_value platform)
    SERVER_ID=$(game_conf_value server_id)
    echo mxqy_${PLATFORM//\"/}_${SERVER_ID}
}

# 从数据库获取当前版本
is_db_exist() {
    EXIST=`echo "use information_schema;select 1 from schemata where schema_name = '${DB_NAME}';" | mysql -u root -p"${DB_PASS}"`
    if [ -n "${EXIST}" ]; then
        DB_EXIST=true
    else
        DB_EXIST=false
    fi
}

# 从数据库获取当前版本
get_db_version() {
    VERSION=`echo "select max(version) from ${DB_NAME}.db_version;" | mysql -s -n -u root -p"${DB_PASS}"`
    if [ "${VERSION}" = "NULL" ]; then
        echo ""
    else
        echo ${VERSION}
    fi
}

# 从文件中获取匹配某一个字符的行数
get_first_line_num () {
    FILE=$1
    STR=$2
    echo `grep -n -m 1 "${STR}" ${FILE} | cut -d':' -f 1`
}

#-----------
# 处理参数
#-----------
while getopts "u:p:d:r:nq?" opt_name; do
    case "$opt_name" in
        u) DB_USER=$OPTARG;;
        p) DB_PASS=$OPTARG;;
        d) DB_NAME=$OPTARG;;
        r) DB_VERSION=$OPTARG;;
        n) DB_BACKUP=false;;
        q) SHOW_CONFIRM=false;;
        ?) usage; exit 1;;
    esac
done

[ -z "$DB_NAME" ] && DB_NAME=$(get_db_name)

# 检查参数
show_line
echo " 数据库用户名:${DB_USER} 数据库密码:${DB_PASS} 数据库:${DB_NAME}"
show_line
if [ -z "$DB_USER" ] || [ -z "$DB_PASS" ] || [ -z "$DB_NAME" ]; then
    echo "请检查数据库参数是否正确!"
    exit 1
fi

is_db_exist
if [ "$DB_EXIST" = "false" ]; then
    echo "数据库${DB_NAME}不存在！"
    exit 1
fi

# 检查版本是否正确
DB_VERSION_NOW=$(get_db_version)
show_line
echo " 数据库的当前版本:${DB_VERSION_NOW}"
show_line

if [ -n "$DB_VERSION_NOW" ]; then
    if [ -n "$DB_VERSION" ]; then
        [ "$DB_VERSION_NOW" != "$DB_VERSION" ] && \
        error "数据库中版本:${DB_VERSION_NOW}与参数:${DB_VERSION}不匹配" 1
    else
        DB_VERSION=$DB_VERSION_NOW
    fi
else 
    if [ -z "$DB_VERSION" ]; then
        error "数据库版本为空!" 1
    fi
fi

if !([ ${#DB_VERSION} -eq 8 ] && date -d "${DB_VERSION}" > /dev/null 2>&1); then
    error "数据库版本号不合法:${DB_VERSION}" 1
fi

# 显示确认信息
if [ "$SHOW_CONFIRM" = true ]; then
    show_confirm "你确定要更新数据库${DB_NAME}"
fi

#----------------
# 1
#----------------
if ${ROOT}/gamectl status > /dev/null 2>&1; then
    error "游戏仍在运行中，不可以更新数据库!" 1
fi

#----------------
# 2
#----------------
if [ "$DB_BACKUP" = "false" ] ;then
    if [ "$SHOW_CONFIRM" = "true" ]; then
        show_confirm "你确定不备份数据库${DB_NAME}"
    fi
else
    if !(${BACKUP_DB_SCRIPT} backup -u ${DB_USER} -p ${DB_PASS} -d ${DB_NAME} -q); then
        error "备份数据库失败！退出操作!" 2
    fi
fi

#----------------
# 3
#----------------
show_line
echo " 工作路径为:${WORK_DIR}"
show_line
if !(${GEN_SQL_ESCRIPT} ${WORK_DIR} ${SQL_FILE} ${DB_VERSION}); then
    echo "分析${SQL_FILE}失败!"
    exit 3
fi

#----------------
# 4
#----------------
if !(find ${UPDATE_DIR} -type f -name "*.erl" -print0 | xargs -0 -I FILE cp FILE ${WORK_DIR}); then
    error "拷贝更新脚本到工作目录失败!" 4
fi

if !(${UPDATE_TABLE_ESCRIPT} ${ROOT} ${WORK_DIR} ${DB_USER} ${DB_PASS} ${DB_NAME} ${DB_VERSION}); then
    error "通过escript更新数据库${DB_NAME}出错" 4
fi
exit 0
