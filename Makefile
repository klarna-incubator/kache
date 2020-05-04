REBAR := rebar3
ELVIS := elvis

export REBAR_PROFILE ?= default

.PHONY: get-deps
get-deps:
	$(REBAR) get-deps
	$(REBAR) as test get-deps

.PHONY: eunit
eunit:
	$(REBAR) eunit

.PHONY: cover
cover:
	$(REBAR) cover

.PHONY: covertool
covertool:
	$(REBAR) covertool generate

.PHONY: xref
xref:
	$(REBAR) xref

.PHONY: dialyzer
dialyzer:
	$(REBAR) dialyzer

.PHONY: lint
lint:
	$(ELVIS) rock -V

.PHONY: clean
clean:
	$(REBAR) clean

.PHONY: distclean
distclean:
	rm -rf _build
