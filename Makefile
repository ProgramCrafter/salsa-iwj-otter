#!/bin/sh

SHELL=/bin/bash

CARGO ?= cargo

default: debug

ifneq (,$(NAILINGCARGO_MANIFEST_DIR))
vpath % $(NAILINGCARGO_MANIFEST_DIR)
CARGO_FLAGS += $(addprefix --manifest-path=,$(NAILINGCARGO_MANIFEST_DIR)/Cargo.toml)
CARGO_FLAGS += $(NAILINGCARGO_CARGO_OPTIONS)
endif

foo:
	printenv | sort

debug: target/debug/server templates/script.js

.PHONY: target/debug/server

target/debug/server:
	$(CARGO) build $(CARGO_FLAGS)

templates/script.js: templates/script.ts tsconfig.json
	mkdir -p templates
	test -f templates || ln -s $< templates/
	tsc --outfile $@.tmp 2>&1 \
	| perl -pe 's/\((\d+),(\d+)\):/:$$1:$$2:/'; \
	test "$${PIPESTATUS[*]}" = "0 0"
	mv -f $@.tmp $@

clean:
	rm -f templates/script.js
	rm -rf target
