#! /bin/bash
# 初始化数据库环境.
# 创建用户,创建游戏数据库
# by litaocheng
ROOT=`cd $(dirname $0)/..; pwd`
CONFDIR=$ROOT/config
DB=mxqy
DB_SQL_FILE=mxqy.sql
DB_USER=root
DB_PASS=
DB_EXIST=false
BACKUP_DIR=${ROOT}/sql/backup
BACKUP_FIRST="yes"
SERVER_ID=
DB_PASS=`cat /data/save/mysql_root 2> /dev/null`
[ -z "$DB_PASS" ] && DB_PASS=root

# 用法
usage() {
    echo "初始化游戏数据库环境"
    echo "用法: $0 [选项]"
    echo ""
    echo "选项"
    echo " -n           - 初始化前不对原数据库进行备份"
    echo " -t           - 平台标识(默认从game.conf读取)"
    echo " -i           - 服务器标识(默认从game.conf读取)"
    echo " -p           - root密码"
    echo " -?           - 打印本信息"
    echo ""
}

# 错误
error() {
    echo $1
    exit 1
}

# 获取game.conf中某个配置
game_conf_value() {
    KEY=$1
    if [ -n "$KEY" ]; then
        echo `cat $CONFDIR/game.conf | grep "${KEY}" | tail -n 1 | sed -e "s/\s*{${KEY},\s*\(.*\)}.*/\1/"`
    fi
}

# 备份原数据
backup() {
    echo "备份数据库${DB}到${BACKUP_DIR}..."
    if ! mkdir -p ${BACKUP_DIR} 2> /dev/null; then
        error "创建备份目录${BACKUP_DIR}失败，检查权限"
    fi
    if [ "$DB_EXIST" != "true" ]; then
        echo "数据库${DB}不存在!"
    else
        BACKUP_FILE=${DB}_`date +'%Y_%m_%d_%H_%M'`.bak
        if ! mysqldump --single-transaction --quick -u root -p"${DB_PASS}" $DB > ${BACKUP_DIR}/${BACKUP_FILE} \
            2> /dev/null; then
            error "备份数据库$DB 到 ${BACKUP_DIR}失败"
        fi
    fi
}

# 检测某个数据库是否存在
# 返回:0存在,1不存在
is_database_exist() {
    EXIST=`echo "use information_schema;select 1 from schemata where schema_name = '${DB}';" | mysql -u root -p"${DB_PASS}"`
    if [ "$EXIST" != "" ]; then
        return 0
    else
        return 1
    fi
}

# 删除数据库和用户
delete_database() {
    if [ "$DB_EXIST" = "true" ]; then
        # 删除数据库
        if ! mysqladmin -u root -p"${DB_PASS}" -f drop ${DB}; then
            error "删除数据库$DB失败"
        fi
    fi

    # 删除用户
    if [ "$DB_USER" != "root" ]; then
        if ! echo "DROP USER ${DB_USER};" | mysql -u root -p"${DB_PASS}" 2> /dev/null; then
           echo "数据库中用户${DB_USER}不存在!"
        fi
    fi
}

# 创建用户
create_user() {
    mysql -u root -p"${DB_PASS}" << EOF
SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";
## 创建数据库
CREATE DATABASE IF NOT EXISTS ${DB};
EOF
    if [ $? -ne 0 ]; then
        error "创建数据库${DB}失败"
    fi

    if [ "$DB_USER" != "root" ]; then
        echo "创建用户..."
        mysql -u root -p"${DB_PASS}" << EOF
#创建用户
CREATE USER '${DB_USER}'@'%' IDENTIFIED BY '';

#将数据库的所有操作权限赋给用户
GRANT ALL PRIVILEGES ON ${DB}.* TO '${DB_USER}'@'%';

#修改密码
SET PASSWORD FOR ${DB_USER}=password ('root');
FLUSH PRIVILEGES;
EOF
        if [ $? -ne 0 ]; then
            error "创建用户${DB_USER}失败"
        fi
    fi
}

# 建立数据库结构
create_db() {
    echo "创建数据库..."
    EMPTY_DUMP=${ROOT}/sql/${DB_SQL_FILE}
    if [ ! -f ${EMPTY_DUMP} ]; then
        error "${EMPTY_DUMP}文件不存在!"
    fi
    cd `dirname $EMPTY_DUMP` &&
    mysql -u root -p"${DB_PASS}" << EOF
use ${DB};
source ${DB_SQL_FILE};
EOF
    if [ $? -ne 0 ]; then
        error "创建数据库失败"
    fi
}

PLATFORM=$(game_conf_value platform)
SERVER_ID=$(game_conf_value server_id)
PLATFORM=${PLATFORM#\"}
PLATFORM=${PLATFORM%\"}

# 处理参数
#echo "$@"
while getopts "t:ni:p:?h" opt_name; do
    case "$opt_name" in
        n) BACKUP_FIRST="no";;
        t) PLATFORM="$OPTARG";;
        i) SERVER_ID="$OPTARG";;
        p) DB_PASS="$OPTARG";;
        h) usage; exit 0;;
        ?) usage; exit 0;;
    esac
done

if [ -z "$SERVER_ID" ]; then
    usage
    exit 0
fi
DB_SQL_FILE=${DB}.sql
DB=${DB}_${PLATFORM}_${SERVER_ID}

if is_database_exist; then
    DB_EXIST=true
fi

if [ "$BACKUP_FIRST" = "no" ]; then
    echo "=================================================================="
    echo "===========================警告警告==============================="
    echo "===========================警告警告==============================="
    echo "=================================================================="
    echo "=======未备份的情况下(-n选项),将要对游戏数据库进行初始化!!!======="
    echo "=======================请输入 yes 表示确认===================="
    echo "=================================================================="
    read CONFIRM
    if [ "$CONFIRM" != "yes" ]; then
        exit 0
    fi
else
    backup
fi
delete_database
create_user 
create_db
echo "初始化数据库成功!"
