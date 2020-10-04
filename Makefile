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
USVG ?= usvg
USVG_OPTIONS = "--sans-serif-family=DejaVu Sans"

ifneq (,$(wildcard(../Cargo.nail)))
NAILING_CARGO = nailing-cargo
CARGO = $(NAILING_CARGO)
CARGO_TARGET_DIR = ../Build/$(notdir $(PWD))/target

BUNDLE_SOURCES_DIR = ../bundle-sources
BUNDLE_SOURCES = ../Build/bundle-sources/target/debug/bundle-rust-sources

USVG_BINARY = ../resvg/target/release/usvg
USVG = $(NAILING_CARGO) --just-run -q --- $(USVG_BINARY)
# To build usvg
# zealot:resvg$ nailing-cargo build -p usvg --release

ifneq (,$(wildcard $(BUNDLE_SOURCES_DIR)))
$(BUNDLE_SOURCES):
	cd ../bundle-sources && $(CARGO) build
.PHONY: $(BUNDLE_SOURCES)
endif
endif

include $(wildcard library/*/files.make)

USVG_PROCESSOR = usvg-processor
LIBRARY_PROCESS_SVG = ./$(USVG_PROCESSOR) $@ $(wordlist 1,2,$^) '$(USVG) $(USVG_OPTIONS)'
$(LIBRARY_FILES): $(USVG_PROCESSOR) Makefile

TS_SRCS= script bigfloat
TS_SRC_FILES= $(addprefix templates/,$(addsuffix .ts,$(TS_SRCS)))

LITFILES= LICENCE AGPLv3
TXTFILES= CC-BY-SA-3.0 CC-BY-SA-4.0

FILEASSETS = $(addprefix templates/,$(LITFILES) $(TXTFILES))

assets: templates/script.js libraries $(FILEASSETS)

$(addprefix templates/,$(LITFILES)): templates/%: %;
	cp $< $@.new && mv -f $@.new $@

$(addprefix templates/,$(TXTFILES)): templates/%: %.txt
	cp $< $@.new && mv -f $@.new $@

libraries: $(LIBRARY_FILES)

debug release:: %: cargo-% assets extra-%
cargo-debug cargo-release:: cargo-%: $(CARGO_TARGET_DIR)/%/server
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

templates/script.js: $(TS_SRC_FILES) tsconfig.json
	tsc --outfile $@.tmp $(TS_SRC_FILES) 2>&1 \
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
	rm -f templates/script.js library/*/*.usvg
	rm -rf target
