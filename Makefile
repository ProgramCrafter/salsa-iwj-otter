#!/bin/sh
#
# make -j8
# make -j8 release

SHELL=/bin/bash

CARGO ?= cargo
CARGO_TARGET_DIR ?= target

ifneq (,../Cargo.nail)
CARGO = nailing-cargo
CARGO_TARGET_DIR = ../Build/target
endif

default: debug

debug release:: %: $(CARGO_TARGET_DIR)/%/server templates/script.js
	@echo Built $@.

.PHONY: $(CARGO_TARGET_DIR)/%/server

$(CARGO_TARGET_DIR)/debug/server:
	$(CARGO) build

$(CARGO_TARGET_DIR)/release/server:
	$(CARGO) build --release

templates/script.js: templates/script.ts tsconfig.json
	tsc --outfile $@.tmp 2>&1 \
	| perl -pe 's/\((\d+),(\d+)\):/:$$1:$$2:/'; \
	test "$${PIPESTATUS[*]}" = "0 0"
	mv -f $@.tmp $@

clean:
	rm -f templates/script.js
	rm -rf target
