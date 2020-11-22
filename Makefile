#!/bin/sh
# Copyright 2020 Ian Jackson
# SPDX-License-Identifier: AGPL-3.0-or-later
# There is NO WARRANTY.

# make -j8
# make -j8 release

SHELL=/bin/bash

default: all check
all: debug
everything: debug doc release check bundled-sources

#---------- funky macros etc. ----------

cr = $(addprefix --,$(filter-out debug,$1))
rsrcs = $(shell \
    find $1 \( -name Cargo.toml -o -name Cargo.lock -o -name \*.rs \) )
stamp=@mkdir -p stamp; touch $@

#---------- programs and config variables ----------

CARGO ?= cargo
TARGET_DIR ?= target

USVG ?= usvg
USVG_OPTIONS = "--sans-serif-family=DejaVu Sans"

WASM_PACK ?= wasm-pack
WASM_PACK_OPTIONS = --cargo-path=/bin/echo

BUNDLE_SOURCES ?= bundle-rust-sources

DEPLOY_ARCH=x86_64-unknown-linux-musl
DEPLOY_RELEASE=debug
DEPLOY_TARGET_DIR=$(TARGET_DIR)/$(addsuffix /,$(DEPLOY_ARCH))$(DEPLOY_RELEASE)

#---------- nailing-cargo ----------

ifneq (,$(wildcard(../Cargo.nail)))

NAILING_CARGO = nailing-cargo
CARGO = $(NAILING_CARGO)
BUILD_SUBDIR ?= ../Build
TARGET_DIR = $(BUILD_SUBDIR)/$(notdir $(PWD))/target

NAILING_CARGO_JUST_RUN ?= $(NAILING_CARGO) --just-run -q ---
BUNDLE_SOURCES_CMD ?= $(NAILING_CARGO) --- $(BUNDLE_SOURCES)
USVG_CMD ?= $(NAILING_CARGO_JUST_RUN) $(USVG)
WASM_PACK_CMD ?= $(NAILING_CARGO) --linkfarm=git --- $(WASM_PACK)

endif # Cargo.nail

BUILD_SUBDIR ?= ../Build
BUNDLE_SOURCES_CMD ?= $(BUNDLE_SOURCES)
USVG_CMD ?= $(USVG)
WASM_PACK_CMD ?= $(WASM_PACK)

WASM_PACKED=$(TARGET_DIR)/packed-wasm

#---------- local programs ----------

define lp
$(if $(wildcard $(BUILD_SUBDIR)/$2),
$(shell echo >&2 'Makefile: lp: Using program $4 from $(BUILD_SUBDIR)/$2')
$1 := $(abspath $(BUILD_SUBDIR)/$2/target/$3/$4)
$(abspath $(BUILD_SUBDIR)/$2/target/$3/$4):; cd ../$2 && $$(CARGO) build $(call cr,$3)
)
bundled-sources:: bundled-sources/$2
endef

$(eval $(call lp,BUNDLE_SOURCES,bundle-sources,debug,bundle-rust-sources))
$(eval $(call lp,USVG,resvg,release,usvg))
$(eval $(call lp,WASM_PACK,wasm-pack,debug,wasm-pack))

#---------- variables defining bits of source etc. ----------

PROGRAMS=daemon-otter otter

WASM_ASSETS := $(addprefix otter_wasm,.js _bg.wasm)
WASM_OUTPUTS := $(addprefix otter_wasm,.d.ts)

TS_SRCS= script
TS_SRC_FILES= \
	$(addprefix templates/,$(addsuffix .ts,$(TS_SRCS))) \
	webassembly-types/webassembly.d.ts \
	templates/otter_wasm.ns.d.ts

LITFILES= LICENCE AGPLv3
TXTFILES= CC-BY-SA-3.0 CC-BY-SA-4.0

FILEASSETS = $(addprefix templates/, libre script.js \
			$(LITFILES) $(TXTFILES)) \
		$(wildcard templates/*.tera)

WASM := wasm32-unknown-unknown

#---------- toplevel aggregate targets ----------

check: stamp/cargo.check
	@echo 'All tests passed.'

doc: cargo-doc

debug release:: %: stamp/cargo.% assets libraries extra-%

cargo: cargo-debug cargo-wasm-release

cargo-debug cargo-release cargo-check cargo-doc \
cargo-wasm-debug cargo-wasm-release:: \
cargo-%: stamp/cargo.%

cargo-wasm: cargo-wasm-release

wasm-pack: stamp/wasm-pack

assets: js stamp/wasm-pack $(FILEASSETS)

js: templates/script.js

extra-debug:
extra-release: bundled-sources

#---------- cargo ----------

DR=debug release
CARGOES=$(foreach t, wasm-,$(addprefix $t,check $(DR)))

$(addprefix stamp/cargo.,$(DR)):: \
stamp/cargo.%: $(call rsrcs,. ! -path './wasm/*')
	$(CARGO) build $(call cr,$*)
	$(stamp)

stamp/cargo.check: $(call rsrcs,.)
	$(CARGO) test --workspace
	$(stamp)

stamp/cargo.doc: $(call rsrcs,.)
	$(CARGO) doc --workspace
	$(stamp)

$(addprefix stamp/cargo.wasm-,$(DR)):: \
stamp/cargo.wasm-%: $(call rsrcs, zcoord wasm Cargo.*)
	$(CARGO) -TWASM build -p otter-wasm $(call cr,$*)
	$(stamp)

stamp/cargo.deploy-build: $(call rsrcs,.)
	$(CARGO) -T$(DEPLOY_ARCH) build $(call cr,$(DEPLOY_RELEASE))
	$(stamp)

#---------- wasm ----------

$(addprefix $(WASM_PACKED)/,$(WASM_ASSETS) $(WASM_OUTPUTS)): stamp/wasm-pack
stamp/wasm-pack: stamp/cargo.wasm-release
	$(WASM_PACK_CMD) $(WASM_PACK_OPTIONS) build \
		--out-dir=../target/packed-wasm wasm -t no-modules --release
	$(stamp)

# There is some kind of bug in wasm-pack's version of wasm-opt, which
# can be avoided by running --dev instead (but also then using the
# debug rust built).  IME this happened when trying to return a u64,
# so maybe bigints are affected?  (Which I don't want to use anyway.)
# See
#    https://github.com/WebAssembly/binaryen/issues/3006

#---------- bundle-sources ----------

bundled-sources:: bundled-sources/otter

bundled-sources/%: $(BUNDLE_SOURCES)
	set -e; d=$(abspath $(TARGET_DIR)/bundled-sources); \
	$(NAILING_CARGO_JUST_RUN) mkdir -p $$d; \
	$(if $(filter-out otter,$*), cd ../$*;) \
	$(BUNDLE_SOURCES_CMD) --output $$d/$*

.PHONY: bundled-sources/%

bundled-sources::
	@echo Bundled sources.

#---------- svg processing ----------

include $(wildcard library/*/files.make)

USVG_PROCESSOR = usvg-processor
LIBRARY_PROCESS_SVG = ./$(USVG_PROCESSOR) $@ $(wordlist 1,2,$^) '$(USVG_CMD) $(USVG_OPTIONS)'
$(LIBRARY_FILES): $(USVG_PROCESSOR) $(USVG_BINARY) Makefile

# actual command for each of $(LIBRARY_FILES) is in one of the files.make

#---------- typescript ----------

templates/%.js: tsc-wrap tsconfig.json
	./tsc-wrap $@ tsconfig.json $(filter %.ts,$^)

templates/script.js: $(TS_SRC_FILES)
#templates/bigfloat-tests.js: templates/bigfloat.ts \
#	templates/bigfloat-tests.ts templates/bigfloat-tests-auto.ts

#templates/bigfloat-tests-auto.ts: extract-bf-tests src/bigfloat.rs
#	./$^ >$@.tmp && mv -f $@.tmp $@

#js-check: templates/bigfloat-tests.js
#	nodejs <$<
#	@echo 'nodejs check $< ok'

templates/otter_wasm.ns.d.ts: $(WASM_PACKED)/otter_wasm.d.ts \
				stamp/wasm-pack Makefile
	set -e; exec >$@.tmp; 				\
	echo 'declare namespace wasm_bindgen {'; 	\
	sed 's/^export default function init/export function init/' <$<; \
	echo '}'
	mv -v $@.tmp $@

#---------- other templates ----------

$(addprefix templates/,$(LITFILES)): templates/%: %
	cp $< $@.new && mv -f $@.new $@

$(addprefix templates/,$(TXTFILES)): templates/%: %.txt
	cp $< $@.new && mv -f $@.new $@

libraries: $(LIBRARY_FILES)

#---------- deployment ----------

DEPLOY_USER=ian@login.chiark.greenend.org.uk
DEPLOY_BASE=$(DEPLOY_USER):/volatile/Otter
DEPLOY_FINISH=/home/Otter/etc/deploy-finish

deploy: stamp/cargo.deploy-build bundled-sources assets libraries
	rsync -zv --progress $(addprefix $(DEPLOY_TARGET_DIR)/,$(PROGRAMS)) $(DEPLOY_BASE)/bin/
	rsync -rv --progress $(TARGET_DIR)/bundled-sources/. $(DEPLOY_BASE)/bundled-sources
	rsync -r README.md $(DEPLOY_BASE)/.
	rsync -r --delete --exclude=\*~ library specs $(DEPLOY_BASE)/.
	rsync -r $(FILEASSETS) $(addprefix $(WASM_PACKED)/, $(WASM_ASSETS)) \
		$(DEPLOY_BASE)/assets/
	ssh -o BatchMode=true $(DEPLOY_USER) $(DEPLOY_FINISH)

#$(DEPLOY_BASE)/bundled-sources

#---------- clean ----------

clean:
	rm -f templates/script.js library/*/*.usvg
	rm -rf target
