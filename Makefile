.PHONY: test

default:
	rebar3 compile

test:
	rebar3 compile && WITH_COVERAGE=true ETEST_BUILD_DIR="_build/default/lib/esonic" _build/default/lib/etest/bin/etest-runner
