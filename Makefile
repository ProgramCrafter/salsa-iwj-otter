#!/bin/sh
# Copyright 2020 Ian Jackson
# SPDX-License-Identifier: AGPL-3.0-or-later
# There is NO WARRANTY.

# make -j8
# make -j8 release

SHELL=/bin/bash

default: debug

#---------- funky macros etc. ----------

cr = $(addprefix --,$(filter-out debug,$1))
rsrcs = $(shell \
    find $1 \( -name Cargo.toml -o -name Cargo.lock -o -name \*.rs \) )

#---------- programs and config variables ----------

CARGO ?= cargo
CARGO_TARGET_DIR ?= target

USVG ?= usvg
USVG_OPTIONS = "--sans-serif-family=DejaVu Sans"

WASM_PACK ?= wasm-pack
WASM_PACK_OPTIONS = --cargo-path=/bin/echo

BUNDLE_SOURCES ?= bundle-rust-sources

DEPLOY_ARCH=x86_64-unknown-linux-musl
DEPLOY_RELEASE=debug
DEPLOY_TARGET_DIR=$(CARGO_TARGET_DIR)/$(addsuffix /,$(DEPLOY_ARCH))$(DEPLOY_RELEASE)

#---------- nailing-cargo ----------

ifneq (,$(wildcard(../Cargo.nail)))

NAILING_CARGO = nailing-cargo
CARGO = $(NAILING_CARGO)
BUILD_SUBDIR ?= ../Build
CARGO_TARGET_DIR = $(BUILD_SUBDIR)/$(notdir $(PWD))/target

BUNDLE_SOURCES_CMD ?= $(NAILING_CARGO) --- $(BUNDLE_SOURCES)
USVG_CMD ?= $(NAILING_CARGO) --just-run -q --- $(USVG)

endif # Cargo.nail

BUILD_SUBDIR ?= ../Build
BUNDLE_SOURCES_CMD ?= $(BUNDLE_SOURCES)
USVG_CMD ?= =$(USVG)

#---------- local programs ----------

define lp
$(if $(wildcard $(BUILD_SUBDIR)/$2),
$(shell echo >&2 'Makefile: lp: Using program $4 from $(BUILD_SUBDIR)/$2')
$1 := $(abspath $(BUILD_SUBDIR)/$2/target/$3/$4)
$(abspath $(BUILD_SUBDIR)/$2/target/$3/$4):; cd ../$2 && $$(CARGO) build $(call cr,$3)
)
endef

$(eval $(call lp,BUNDLE_SOURCES,bundle-sources,debug,bundle-rust-sources))
$(eval $(call lp,USVG,resvg,release,usvg))
$(eval $(call lp,WASM_PACK,wasm-pack,debug,wasm-pack))

#---------- variables defining bits of source etc. ----------

PROGRAMS=daemon-otter otter

FILEASSETS = $(addprefix templates/,$(LITFILES) $(TXTFILES))

WASM := wasm32-unknown-unknown

#---------- toplevel aggregate targets ----------

check: cargo/check js-check
	@echo 'All tests passed.'

debug release:: %: cargo/% assets extra-%

assets: templates/script.js libraries $(FILEASSETS)

extra-debug:
extra-release: bundled-sources

#---------- cargo ----------

DR=debug release
CARGOES=$(foreach t,/ /wasm-,$(addprefix $t,check $(DR)))
rstamp:=@mkdir -p cargo; touch $@

$(addprefix cargo/,$(DR)):: cargo/%: $(call rsrcs,. ! -path './wasm/*')
	$(CARGO) build $(call cr,$*)
	$(rstamp)

cargo/check: $(call $(csrcs,.))
	$(CARGO) test
	$(rstamp)

$(addprefix cargo/wasm-,$(DR)):: cargo/wasm-%: $(call rsrcs, zcoord wasm)
	: $@ out of date $?
	$(CARGO) -TWASM build -p otter-wasm $(call cr,$*)
	$(rstamp)

cargo/deploy-build: $(call rsrcs,.)
	$(CARGO) -T$(DEPLOY_ARCH) build $(call cr,$(DEPLOY_RELEASE))
	$(rstamp)

#---------- wasm ----------

WASM_ASSETS := $(addprefix otter_wasm,.js _bg.wasm)
WASM_OUTPUTS := $(addprefix otter_wasm,.d.ts)

$(WASM_ASSETS) $(WASM_OUTPUTS): wasm-pack
.PHONY: wasm-pack
wasm-pack: cargo/wasm-release

real/wasm-pack: 

wasm-pack:
	$(MAKE) real/$@

#---------- bundle-sources ----------

bundled-sources: $(CARGO_TARGET_DIR)/bundled-sources
$(CARGO_TARGET_DIR)/bundled-sources: $(BUNDLE_SOURCES)
	$(BUNDLE_SOURCES_CMD) --output $(abspath $@)
	@echo Bundled sources.
.PHONY: bundle-sources $(CARGO_TARGET_DIR)/bundled-sources

#---------- svg processing ----------

include $(wildcard library/*/files.make)

USVG_PROCESSOR = usvg-processor
LIBRARY_PROCESS_SVG = ./$(USVG_PROCESSOR) $@ $(wordlist 1,2,$^) '$(USVG_CMD) $(USVG_OPTIONS)'
$(LIBRARY_FILES): $(USVG_PROCESSOR) $(USVG_BINARY) Makefile

# actual command for each of $(LIBRARY_FILES) is in one of the files.make

#---------- typescript ----------

TS_SRCS= script bigfloat
TS_SRC_FILES= $(addprefix templates/,$(addsuffix .ts,$(TS_SRCS)))

LITFILES= LICENCE AGPLv3
TXTFILES= CC-BY-SA-3.0 CC-BY-SA-4.0

templates/%.js: tsc-wrap tsconfig.json
	./tsc-wrap $@ tsconfig.json $(filter %.ts,$^)

templates/script.js: $(TS_SRC_FILES)
templates/bigfloat-tests.js: templates/bigfloat.ts \
	templates/bigfloat-tests.ts templates/bigfloat-tests-auto.ts

templates/bigfloat-tests-auto.ts: extract-bf-tests src/bigfloat.rs
	./$^ >$@.tmp && mv -f $@.tmp $@

js-check: templates/bigfloat-tests.js
	nodejs <$<
	@echo 'nodejs check $< ok'

#---------- other templates ----------

$(addprefix templates/,$(LITFILES)): templates/%: %;
	cp $< $@.new && mv -f $@.new $@

$(addprefix templates/,$(TXTFILES)): templates/%: %.txt
	cp $< $@.new && mv -f $@.new $@

libraries: $(LIBRARY_FILES)

#---------- deployment ----------

DEPLOY_BASE=Otter@login.chiark.greenend.org.uk:/volatile/Otter

deploy: cargo/deploy-build bundled-sources
	rsync -zv --progress $(addprefix $(DEPLOY_TARGET_DIR)/,$(PROGRAMS)) $(DEPLOY_BASE)/bin/
	rsync -rv --progress $(CARGO_TARGET_DIR)/bundled-sources/. $(DEPLOY_BASE)/bundled-sources

#---------- clean ----------

clean:
	rm -f templates/script.js library/*/*.usvg
	rm -rf target
