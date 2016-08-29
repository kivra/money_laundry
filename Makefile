PROJECT = money_laundry

# Options ##############################################################
COMPILE_FIRST = money_format
COVER         = 1
EUNIT_OPTS    = [verbose]
ERLC_OPTS    ?=  -Werror +debug_info +warn_export_all +warn_export_vars \
                 +warn_shadow_vars +warn_obsolete_guard -DS2_USE_LAGER

# Test Dependencies ####################################################
TEST_DEPS = meck proper

# Standard targets #####################################################
include erlang.mk

# eof
