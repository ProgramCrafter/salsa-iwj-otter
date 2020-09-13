#!/bin/sh
# Copyright 2020 Ian Jackson
# SPDX-License-Identifier: AGPL-3.0-or-later
# There is NO WARRANTY.

# make -j8
# make -j8 release

SHELL=/bin/bash

default: debug libraries

CARGO ?= cargo
CARGO_TARGET_DIR ?= target
USVG ?= usvg

ifneq (,$(wildcard(../Cargo.nail)))
NAILING_CARGO = nailing-cargo
CARGO = $(NAILING_CARGO)
CARGO_TARGET_DIR = ../Build/$(notdir $(PWD))/target

BUNDLE_SOURCES_DIR = ../bundle-sources
BUNDLE_SOURCES = ../Build/bundle-sources/target/debug/bundle-rust-sources

USVG_BINARY = ../resvg/target/release/usvg
USVG = $(NAILING_CARGO) --- $(USVG_BINARY)
# To build usvg
# zealot:resvg$ nailing-cargo build -p usvg --release

ifneq (,$(wildcard $(BUNDLE_SOURCES_DIR)))
$(BUNDLE_SOURCES):
	cd ../bundle-sources && $(CARGO) build
.PHONY: $(BUNDLE_SOURCES)
endif
endif

include $(wildcard library/*/files.make)

LIBRARY_PROCESS_SVG = ./usvg-processor $@ $^ '$(USVG)'

libraries: $(LIBRARY_FILES)

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

DEPLOY_ARCH=x86_64-unknown-linux-musl
DEPLOY_RELEASE=debug
DEPLOY_TARGET_DIR=$(CARGO_TARGET_DIR)/$(addsuffix /,$(DEPLOY_ARCH))$(DEPLOY_RELEASE)

deploy-build: $(DEPLOY_TARGET_DIR)/server
ifneq (,$(DEPLOY_ARCH))
$(DEPLOY_TARGET_DIR)/server:
	$(CARGO) -T$(DEPLOY_ARCH) build $(addprefix --,$(filter-out debug,$(DEPLOY_RELEASE)))
	@echo Built $@.
.PHONY: $(DEPLOY_TARGET_DIR)/server
endif

PROGRAMS=daemon-otter otter

DEPLOY_BASE=Otter@login.chiark.greenend.org.uk:/volatile/Otter

deploy: deploy-build bundled-sources
	rsync -zv --progress $(addprefix $(DEPLOY_TARGET_DIR)/,$(PROGRAMS)) $(DEPLOY_BASE)/bin/
	rsync -rv --progress $(CARGO_TARGET_DIR)/bundled-sources/. $(DEPLOY_BASE)/bundled-sources

clean:
	rm -f templates/script.js
	rm -rf target
