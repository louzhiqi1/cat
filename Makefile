PROJECT = cat
include erlang.mk

DEPS = ranch
LOCAL_DEPS = crypto
ERLC_OPTS = -Werror +debug_info +warn_obsolete_guard


