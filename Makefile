PROJECT = cat
include erlang.mk

LOCAL_DEPS = crypto
ERLC_OPTS = -Werror +debug_info +warn_obsolete_guard {parse_transform, lager_transform} 
