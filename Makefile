PROJECT = cat
include erlang.mk

DEPS = ranch cowboy lager protobuffs
dep_cowboy = git https://github.com/ninenines/cowboy master
dep_protobuffs = https://github.com/basho/erlang_protobuffs
LOCAL_DEPS = crypto
ERLC_OPTS = -Werror +debug_info +warn_obsolete_guard {parse_transform, lager_transform} 



