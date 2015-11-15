/**
* 冒险契约数据库
* 设计到修改字段时，请在文件尾部追加，格式要严格相符
* 格式如下:
* --
* -- 添加xx字段
* -- author
* -- 2012xxxx
* --
*ALTER table `xxx` add `new_field` int(10) COMMENT '新字段";
*/

--
-- 添加表，设置数据库版本
-- litao
-- 20121001
--
CREATE TABLE `db_version` (
    `version` varchar(32) NOT NULL COMMENT '版本',
    PRIMARY KEY ( `version` )
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COMMENT="当前数据库版本，更新数据库时使用";

--
-- 添加表gm_ctrl用来进行封禁操作
--
CREATE TABLE `gm_ctrl` (
      `id` int(11) NOT NULL COMMENT '唯一id',
      `ctrl_type` tinyint(1) NOT NULL default '0' COMMENT '控制类型:1.禁IP,2.禁登录,3.禁言,10防沉迷状态',
      `begin_time` int(11) NOT NULL default '0' COMMENT '开始时间',
      `end_time` int(11) NOT NULL default '0' COMMENT '结束时间',
      `target` char(18) NOT NULL default '' COMMENT '禁IP存IP,禁角色存ID',
      PRIMARY KEY  (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COMMENT="gm控制表";


-- 全局数据表
-- DROP TABLE IF EXISTS `global_data`;
CREATE TABLE `global_data` (
      `key` int(11) unsigned NOT NULL COMMENT 'key',
      `val` blob COMMENT 'val',
      PRIMARY KEY (`key`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8  COMMENT="全局数据表";

-- 关系表
CREATE TABLE IF NOT EXISTS `relation` (
  `idA` int(11) unsigned NOT NULL DEFAULT '0' COMMENT 'id A',
  `idB` int(11) unsigned NOT NULL DEFAULT '0' COMMENT 'id A',
  `type` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT '玩家间关系类型:1好友，2仇人，3黑名单',
  `data` blob COMMENT '数据',
  PRIMARY KEY (`idA`,`idB`,`type`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COMMENT='玩家关系';