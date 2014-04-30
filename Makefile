PROJECT = money_laundry

# Options
ERLC_OPTS = +debug_info +nowarn_shadow_vars +warnings_as_errors
TEST_DEPS = meck

# Dependencies
dep_meck = git://github.com/kivra/stdlib2.git 0.8.1

# Standard targets
include erlang.mk

.PHONY: eunit
eunit:
	erl -noshell -pa ebin -eval 'eunit:test("ebin", [verbose])' -s init stop

# eof
