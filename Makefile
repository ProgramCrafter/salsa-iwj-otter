#!/bin/sh
# Copyright 2020 Ian Jackson
# SPDX-License-Identifier: AGPL-3.0-or-later
# There is NO WARRANTY.

# make -j8
# make -j8 release

SHELL=/bin/bash

default: debug

CARGO ?= cargo
CARGO_TARGET_DIR ?= target

ifneq (,$(wildcard(../Cargo.nail)))
CARGO = nailing-cargo
CARGO_TARGET_DIR = ../Build/$(notdir $(PWD))/target
BUNDLE_SOURCES_DIR = ../bundle-sources
BUNDLE_SOURCES = ../Build/bundle-sources/target/debug/bundle-rust-sources

ifneq (,$(wildcard $(BUNDLE_SOURCES_DIR)))
$(BUNDLE_SOURCES):
	cd ../bundle-sources && $(CARGO) build
.PHONY: $(BUNDLE_SOURCES)
endif
endif

debug release:: %: $(CARGO_TARGET_DIR)/%/server templates/script.js extra-%
	@echo Built $@.
.PHONY: $(CARGO_TARGET_DIR)/debug/server
.PHONY: $(CARGO_TARGET_DIR)/release/server

extra-debug:
extra-release: bundled-sources

bundled-sources: $(CARGO_TARGET_DIR)/bundled-sources
$(CARGO_TARGET_DIR)/bundled-sources: $(BUNDLE_SOURCES)
	nailing-cargo --- $(abspath $(BUNDLE_SOURCES)) --output $(abspath $@)
	@echo Bundled sources.
.PHONY: bundle-sources $(CARGO_TARGET_DIR)/bundled-sources

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
