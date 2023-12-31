# Copyright 2020-2021 Ian Jackson and contributors to Otter
# SPDX-License-Identifier: AGPL-3.0-or-later
# There is NO WARRANTY.

# make -j8
# make -j8 release
# make -j8 shapelib

SHELL=/bin/bash
src=.

default: all check
all: debug
full-check: all check cargo-syntaxcheck-release shapelib doc-sphinx
full-check: for-deploy release
everything: debug doc release check bundled-sources

shapelib: templates/shapelib.html docs/html/index.html
	@echo 'Shape library preview and docs can now be found here:'
	@for f in $^; do echo '  file://$(PWD)/'$$f; done

MAKEFILE_DEP ?= Makefile
MAKEFILE_FIND_X ?=
# ^ set this to "x" to debug the $rsrcs rune

#---------- funky macros etc. ----------

cr = $(addprefix --,$(filter-out debug,$1))
rsrcs = $(shell $(foreach x,$(MAKEFILE_FIND_X),set -$x;)\
    find -H $1 \( -name Cargo.toml -o -name Cargo.lock -o -name Cargo.lock.example -o -name \*.rs \) \! -path '*/build/*' )
stamp=@mkdir -p stamp; touch $@

BUNDLED_SOURCES_LIT = README.md LICENCE
BUNDLED_SOURCES_FILES = index.html $(BUNDLED_SOURCES_LIT)
BUNDLED_SOURCES_LINKS += $(BUNDLED_SOURCES_LIT) otter/
BUNDLED_SOURCES += $(BUNDLED_SOURCES_FILES)

#---------- programs and config variables ----------

TARGET_DIR ?= target

WASM_BINDGEN = $(TARGET_DIR)/debug/wasm-bindgen
WASM_BINDGEN_OPTIONS =						\
	--remove-name-section --remove-producers-section	\
	--typescript

BUNDLE_SOURCES ?= bundle-rust-sources

SPHINXBUILD   ?= sphinx-build

ifndef INKSCAPE_EXTENSIONS
INKSCAPE ?= inkscape
INKSCAPE_SDD_QUIETEN ?= 2>/dev/null
# inkscape 0.92.4: --extension-directory works, no --system-data-directory
# inkscape 1.0.2: --system-data-directory, no --extension-directory
INKSCAPE_EXTENSIONS := $(shell set -e; unset DISPLAY; { sdd=$$( $(INKSCAPE) --system-data-directory $(INKSCAPE_SDD_QUIETEN) ) && echo "$$sdd/extensions"; } || $(INKSCAPE) --extension-directory )
endif
RECOLOUR_SVG ?= ./run-inkscape-extension $(INKSCAPE_EXTENSIONS)/color_replace.py

DEPLOY_ARCH=x86_64-unknown-linux-musl
DEPLOY_RELEASE=release
DEPLOY_TARGET_DIR=$(TARGET_DIR)/$(addsuffix /,$(DEPLOY_ARCH))$(DEPLOY_RELEASE)
DEPLOYED_BRANCH=deployed
PUBLISHED_BRANCH=published

RUST_CLIPPY_OPTIONS ?= $(shell perl -pe 's/\#.*//; s/\n/ /' clippy-options)
RUST_CLIPPY ?= clippy
RUST_CLIPPY_CMD := clippy $(RUST_CLIPPY_OPTIONS)

#---------- nailing-cargo ----------

ifneq (,$(wildcard ../Cargo.nail))

NAILING_CARGO ?= nailing-cargo
CARGO_CMD ?= $(NAILING_CARGO)
BUILD_SUBDIR ?= ../Build
TARGET_DIR = $(BUILD_SUBDIR)/$(notdir $(PWD))/target

NAILING_CARGO_JUST_RUN ?= $(NAILING_CARGO) --just-run -q ---
BUNDLE_SOURCES_CMD ?= $(NAILING_CARGO) --- $(BUNDLE_SOURCES)
USVG_CMD ?= $(NAILING_CARGO_JUST_RUN) $(USVG)
WASM_BINDGEN_CLI_CARGO_OPTS ?= --subcommand-props=!manifest-path

CARGO_DOCUMENT_PRIVATE_ITEMS ?= --document-private-items
CARGO_DOC_OPTS += $(CARGO_DOCUMENT_PRIVATE_ITEMS)

clean-nailing:
	$(NAILING_CARGO_JUST_RUN) \
 sh -c 'cd "$1"; find -mindepth 1 -maxdepth 1 -print0 | xargs -0r rm -rf --' \
		$(abspath $(BUILD_SUBDIR)/$(notdir $(PWD)))

else
CARGO_CMD ?= cargo
clean-nailing:
endif # Cargo.nail

BUILD_SUBDIR ?= ../Build
BUNDLE_SOURCES_CMD ?= $(BUNDLE_SOURCES)
USVG_CMD ?= $(USVG)

WASM_PACKED=$(TARGET_DIR)/packed-wasm

#---------- local programs ----------

define lp
stamp/cargo.$2: $(call rsrcs, ! -name \*.rs)
	$$(CARGO) build $(call cr,$3) -p $2
	$$(stamp)
$1 := $(abspath $(TARGET_DIR)/$3/$4)
endef

$(eval $(call lp,BUNDLE_SOURCES,bundle-sources,debug,bundle-rust-sources))
$(eval $(call lp,USVG,usvg,release,usvg))

#---------- variables defining bits of source etc. ----------

PROGRAMS=daemon-otter otter

WASM_ASSETS := $(addprefix otter_wasm,.js _bg.wasm)
WASM_OUTPUTS := $(addprefix otter_wasm,.d.ts)

TS_SRCS= script
TS_SRC_FILES= \
	$(addprefix templates/,$(addsuffix .ts,$(TS_SRCS))) \
	webassembly-types/webassembly.d.ts \
	$(WASM_PACKED)/otter_wasm.d.ts

LITFILES= LICENCE AGPLv3
TXTFILES= CC-BY-SA-3.0 CC-BY-SA-4.0

NWTEMPLATEASSETS = nwtemplates/die-overlay.tera
FILEASSETS =	$(addprefix templates/, libre shapelib.html script.js \
			$(LITFILES) $(TXTFILES)) \
		$(wildcard templates/*.tera) \
		$(NWTEMPLATEASSETS)

WASM := wasm32-unknown-unknown
# ^ todo: Is this still right after
#     Use correct ABI for wasm32 by default
#     https://github.com/rust-lang/rust/pull/79998
# ?  But maybe it doesn't matter since we're very conservative and
# only pass JsValue and a few strings across the WASM ABI.

RUST_NIGHTLY_VERSION ?= +nightly
CARGO = $(CARGO_CMD) $(RUST_VERSION)

#---------- toplevel aggregate targets ----------

check: stamp/cargo.debug-check at wdt jstest
	@echo 'Tests passed.'

full-check: stamp/cargo.release-check
full-check: stamp/cargo.release-miri stamp/cargo.debug-miri

full-check:
	@echo 'Full tests passed.'

doc: cargo-doc doc-sphinx examples

debug release:: %: stamp/cargo.% assets libraries extra-%

cargo: cargo-debug cargo-wasm-release

cargo-debug cargo-release cargo-check cargo-doc \
cargo-wasm-debug cargo-wasm-release:: \
cargo-%: stamp/cargo.%

EXAMPLE_BUNDLES = test-bundle big-bundle
EXAMPLE_BUNDLE_FILES = $(foreach f, $(EXAMPLE_BUNDLES), examples/$f.zip)

examples: $(EXAMPLE_BUNDLE_FILES)
.PHONY: examples

cargo-wasm: cargo-wasm-release

wasm: stamp/wasm-bindgen

assets: js stamp/wasm-bindgen $(FILEASSETS)

js: templates/script.js

extra-debug:
extra-release: bundled-sources

cargo-syntaxcheck: cargo-syntaxcheck-host cargo-syntaxcheck-wasm
cargo-syntaxcheck-host:
	$(CARGO) check --workspace
cargo-syntaxcheck-wasm:
	$(CARGO) check --target $(WASM) -p otter-wasm --release
cargo-syntaxcheck-release:
	$(CARGO) check --workspace --release

cargo-clippy: cargo-clippy-host cargo-clippy-wasm
cargo-clippy-host: clippy-options
	$(CARGO) $(RUST_CLIPPY) --workspace $(RUST_CLIPPY_OPTIONS)
cargo-clippy-wasm: clippy-options
	$(CARGO) $(RUST_CLIPPY) --target $(WASM) -p otter-wasm $(RUST_CLIPPY_OPTIONS)

#---------- cargo ----------

DR=debug release
CARGOES=$(foreach t, wasm-,$(addprefix $t,check $(DR)))

$(addprefix stamp/cargo.,$(DR)):: \
stamp/cargo.%: $(call rsrcs,. ! -path './wasm/*')
	$(CARGO) build --workspace $(call cr,$*) -p otter -p otter-daemon -p otter-cli
	$(NAILING_CARGO_JUST_RUN) \
	ln -sf otter $(abspath $(TARGET_DIR))/$*/otter-ssh-proxy
	$(stamp)

$(TARGET_DIR)/debug/%: $(call rsrcs, ! -path './wasm/*')
	$(CARGO) build --workspace -p otter-cli
	$(NAILING_CARGO_JUST_RUN) touch $(abspath $@)

stamp/cargo.wasm-bindgen: $(call rsrcs, ! -name \*.rs)
	$(CARGO_CMD) $(WASM_BINDGEN_CLI_CARGO_OPTS) $(RUST_VERSION) \
		build --target-dir=target \
		--manifest-path=$(abspath wasm/Cargo.toml) -p wasm-bindgen-cli
	$(stamp)

stamp/cargo.%-check: $(call rsrcs,.)
	$(CARGO) test --workspace $(call cr,$*)
	$(stamp)

stamp/cargo.%-miri: $(call rsrcs,.)
	$(CARGO_CMD) $(RUST_NIGHTLY_VERSION) \
		miri test --workspace $(call cr,$*)
	$(stamp)

stamp/cargo-at.debug: $(call rsrcs,.)
	$(CARGO) build --workspace $(call cr,$*) -p otter-api-tests
	$(stamp)

stamp/cargo-wdt.debug: $(call rsrcs,.)
	$(CARGO) build --workspace $(call cr,$*) -p otter-webdriver-tests
	$(stamp)

stamp/cargo-jstest.debug: $(call rsrcs,.)
	$(CARGO) build --workspace $(call cr,$*) -p otter-nodejs-tests
	$(stamp)

stamp/cargo.doc: $(call rsrcs,.)
	set -o pipefail -e; \
	$(CARGO) doc $(CARGO_DOC_OPTS) --workspace 2>&1 \
		|egrep -vf .cargo-doc-suppress-errors
	$(stamp)

$(addprefix stamp/cargo.wasm-,$(DR)):: \
stamp/cargo.wasm-%: $(call rsrcs, base wasm Cargo.*)
	$(CARGO) build --target $(WASM) -p otter-wasm $(call cr,$*)
	$(stamp)

stamp/cargo.deploy-build: $(call rsrcs,.)
	$(CARGO) build --target $(DEPLOY_ARCH) $(call cr,$(DEPLOY_RELEASE)) -p otter -p otter-cli -p otter-daemon -p usvg
	$(NAILING_CARGO_JUST_RUN) \
	ln -sf otter $(abspath $(TARGET_DIR)/$(DEPLOY_ARCH))/$(DEPLOY_RELEASE)/otter-ssh-proxy
	$(stamp)

#---------- sphnix ----------

doc-sphinx:	docs/html/index.html \
	$(foreach f, $(EXAMPLE_BUNDLES), docs/html/examples/$f.zip) \
	$(addprefix docs/html/examples/, $(notdir $(wildcard specs/*.toml)))
	@echo 'Documentation can now be found here:'
	@echo '  file://$(PWD)/$<'

docs/html/index.html: docs/conf.py $(wildcard docs/*.md docs/*.rst docs/*.png)
	$(SPHINXBUILD) -M html docs docs $(SPHINXOPTS)

docs/html/examples/%.zip: examples/%.zip
	mkdir -p docs/html/examples
	rm -f $@ && ln $< $@

docs/html/examples/%.toml: specs/%.toml
	mkdir -p docs/html/examples
	rm -f $@ && ln $< $@

#---------- jstest ----------

JSTESTS= basic lower

.PHONY: jstest
jstest jstests: $(foreach t,$(JSTESTS),stamp/$t.jstest)

stamp/%.jstest: jstest/run1 jstest/%.nodejs templates/script.js \
		stamp/wasm-bindgen-jstest stamp/cargo-jstest.debug
	$(NAILING_CARGO_JUST_RUN) $(abspath $(filter-out stamp/%,$^))
	$(stamp)

#---------- wasm ----------

$(addprefix $(WASM_PACKED)/,$(WASM_ASSETS) $(WASM_OUTPUTS)): stamp/wasm-bindgen
stamp/wasm-bindgen: stamp/cargo.wasm-bindgen stamp/cargo.wasm-release
	$(NAILING_CARGO_JUST_RUN) $(abspath $(WASM_BINDGEN)) \
		$(WASM_BINDGEN_OPTIONS) --no-modules \
		--out-dir target/packed-wasm \
		target/$(WASM)/release/otter_wasm.wasm
	$(stamp)

stamp/wasm-bindgen-jstest: stamp/cargo.wasm-bindgen stamp/cargo.wasm-release
	$(NAILING_CARGO_JUST_RUN) $(abspath $(WASM_BINDGEN)) \
		$(WASM_BINDGEN_OPTIONS) --nodejs \
		--out-dir target/jstest \
		target/$(WASM)/release/otter_wasm.wasm
	$(stamp)

#---------- bundle-sources ----------

BUNDLED_SOURCES_DIRS += otter

bundled-sources:: $(addprefix bundled-sources/, $(BUNDLED_SOURCES_DIRS))

TARGET_BUNDLED=$(TARGET_DIR)/bundled-sources

$(TARGET_BUNDLED):
	$(NAILING_CARGO_JUST_RUN) mkdir -p $(abspath $@)

$(addprefix bundled-sources/, $(BUNDLED_SOURCES_DIRS)): \
bundled-sources/%: stamp/cargo.bundle-sources $(TARGET_BUNDLED)
	set -e; d=$(abspath $(TARGET_BUNDLED)); \
	$(if $(filter-out otter,$*), cd ../$*;) \
	$(BUNDLE_SOURCES_CMD) --output $$d/$*

bundled-sources:: $(addprefix $(TARGET_BUNDLED)/, $(BUNDLED_SOURCES_FILES))

$(addprefix $(TARGET_BUNDLED)/, $(BUNDLED_SOURCES_LIT)): \
$(TARGET_BUNDLED)/%: % $(TARGET_BUNDLED)
	$(NAILING_CARGO_JUST_RUN) cp $(abspath $(src))/$< $(abspath $@)

$(TARGET_BUNDLED)/index.html: bundled-sources-make-index \
		$(MAKEFILE_DEP) $(TARGET_BUNDLED)
	$(NAILING_CARGO_JUST_RUN) sh -ec ' 			\
		cd $(abspath $(src)); mkdir -p $(dir $@);	\
		./$< >$@.tmp $(BUNDLED_SOURCES_LINKS);		\
		mv -f $@.tmp $@;				\
	'

bundled-sources::
	@echo Bundled sources.

#---------- svg processing ----------

LIBRARIES ?= $(basename $(wildcard library/*.toml))
USVG_DEP = stamp/cargo.usvg src/USVG_DEFAULT_ARGS.txt

include $(addsuffix /files.make, $(LIBRARIES))

USVG_PROCESSOR = usvg-processor
LIBRARY_PROCESS_SVG = ./$(USVG_PROCESSOR) $@ $(wordlist 1,2,$^) '$(USVG_CMD) $(USVG_OPTIONS)'
$(LIBRARY_FILES): $(USVG_PROCESSOR) $(USVG_DEP) $(MAKEFILE_DEP)

# actual command for each of $(LIBRARY_FILES) is in one of the files.make

library/%/files.make: media-scraper library/%.toml
	./$< --offline library/$*.toml

#---------- typescript ----------

templates/%.js: tsc-wrap tsconfig.json
	./tsc-wrap $@ tsconfig.json $(filter %.ts,$^)

templates/script.js: $(TS_SRC_FILES) stamp/wasm-bindgen

#---------- other templates ----------

$(addprefix templates/,$(LITFILES)): templates/%: %
	cp $< $@.new && mv -f $@.new $@

$(addprefix templates/,$(TXTFILES)): templates/%: %.txt
	cp $< $@.new && mv -f $@.new $@

libraries: $(LIBRARY_FILES)

templates/shapelib.html: $(TARGET_DIR)/debug/otterlib $(LIBRARY_FILES) \
			$(NWTEMPLATEASSETS)
	$(NAILING_CARGO_JUST_RUN) $(abspath $<) \
	--nwtemplates $(abspath nwtemplates) \
	--libs '$(addprefix $(PWD)/, $(addsuffix .toml, $(LIBRARIES)))' \
		preview >$@.tmp && mv -f $@.tmp $@

nwtemplates/die-overlay.tera: dice/overlay-template-extractor dice/die.svg
	./$< <dice/die.svg >$@.tmp && mv -f $@.tmp $@

library/edited/die-image-d6-_c.svg: dice/extract-image-only dice/die.svg
	./$< <dice/die.svg >$@.tmp && mv -f $@.tmp $@

#---------- examples ----------

EXAMPLE_BUNDLE_INPUT_DEPS := $(shell					\
	cd examples/test-bundle/ && find \! \( -name '*~' -o -name '.*' \) \
)

examples/%.zip: $(MAKEFILE_DEP)
	set -e; rm -f $@.tmp; cd examples/$*/; \
	zip $(ZIPFLAGS) -DX ../$(notdir $@).tmp $(ZIP_INPUTS)
	mv -f $@.tmp $@

examples/test-bundle.zip: ZIP_INPUTS=$(EXAMPLE_BUNDLE_INPUT_DEPS)
examples/test-bundle.zip: \
 $(addprefix examples/test-bundle/, $(EXAMPLE_BUNDLE_INPUT_DEPS))

examples/big-bundle.zip: examples/big-bundle/otter.toml
examples/big-bundle.zip: ZIPFLAGS+= -0 -r -q
examples/big-bundle.zip: ZIP_INPUTS=.
examples/big-bundle/otter.toml: $(LIBRARY_FILES) $(MAKEFILE_DEP)
	rm -rf examples/big-bundle
	mkdir examples/big-bundle examples/big-bundle/library
	@set -e; echo 'MKDIR for $@'; \
	for l in $(LIBRARIES); do \
		mkdir examples/big-bundle/$$l; \
		perl -p \
 -e 'BEGIN { print "# -- AUTOGENERATED FROM COPY IN OTTER SOURCE --\n" }' \
 -e 'if (m/^\[scraper]/..0) { unless (m/^\[(?!scraper)/..0) { s/^/\#/ } }' \
 -e 's/(?<=\s)\w\S*(?=\s)/-/ if m/^files = """/..m/^"""|^:/;' \
			<$$l.toml >examples/big-bundle/$$l.toml; done
	@set -e; echo 'LN for $@'; \
	for e in $(LIBRARY_FILE_INPUTS); do \
		ln $${e#*:} examples/big-bundle/$${e%%.usvg:*}.svg; done;
	@set -e; echo 'MV for $@'; \
	cd examples/big-bundle/library/; for x in '' .toml; do \
		mv wikimedia$$x duped-example$$x; done
	@set -e; mkdir examples/big-bundle/specs/; \
	perl -pe <specs/demo.game.toml \
		>examples/big-bundle/specs/Modded-spec.game.toml \
		's/chess-b-/chess-purple-/; s/chess-w-/chess-yellow-/'
	@set -e; echo 'ECHO for $@'; \
	echo 'title = "Autogenerated test bundle - do not distribute"' \
		>$@.tmp
	mv -f $@.tmp $@

#---------- webdriver tests (wdt) ----------

AT_TESTS := $(basename $(notdir $(wildcard apitest/at-*.rs)))
WDT_TESTS := $(basename $(notdir $(wildcard wdriver/wdt-*.rs)))

WDT_LANDSCAPE_TESTS = wdt-altergame

at:	$(foreach f, $(AT_TESTS), stamp/$f.check)

wdt:	$(foreach f, $(WDT_TESTS), stamp/$f.check) \
	$(foreach f, $(WDT_LANDSCAPE_TESTS), stamp/$f.lcheck) \

RUNTEST_DEPS =	apitest/run1 stamp/cargo.debug $(FILEASSETS) \
		$(wildcard specs/*.toml) examples/test-bundle.zip \
		$(wildcard libraries/*.toml) $(LIBRARY_FILES)

AT_DEPS =	$(filter-out templates/script.js, $(RUNTEST_DEPS)) \
		examples/big-bundle.zip \
		stamp/cargo-at.debug

WDT_DEPS =	$(RUNTEST_DEPS) wdriver/firefox-wrapper \
		stamp/cargo-wdt.debug

AT_WDT_RUN = $(NAILING_CARGO_JUST_RUN) $(abspath $<)

AT_RUN = $(AT_WDT_RUN) apitest --test=$(basename $(notdir $@))
WDT_RUN = $(AT_WDT_RUN) wdriver --test=$(basename $(notdir $@))

for-at:			$(AT_DEPS)
stamp/at-%.check:	$(AT_DEPS)
	$(AT_RUN)
	$(stamp)

for-wdt:		$(WDT_DEPS)
stamp/wdt-%.check:	$(WDT_DEPS)
	$(WDT_RUN)
	$(stamp)

stamp/wdt-%.lcheck:	$(WDT_DEPS)
	$(WDT_RUN) --as-if=lwdt-$* --layout=Landscape
	$(stamp)

#---------- docs publication ----------

PUBLISH_VERSION=unreleased
PUBLISH_USER=ianmdlvl@login.chiark.greenend.org.uk
PUBLISH_DOC_SPHINX_BASE=public-html/otter
PUBLISH_DOC_SPHINX_TAIL=$(PUBLISH_VERSION)/docs
PUBLISH_DOC_SPHINX=$(PUBLISH_USER):$(PUBLISH_DOC_SPHINX_BASE)/$(PUBLISH_DOC_SPHINX_TAIL)

publish: doc-sphinx
	ssh $(PUBLISH_USER) 'cd $(PUBLISH_DOC_SPHINX_BASE) && mkdir -p $(PUBLISH_DOC_SPHINX_TAIL)'
	rsync -r --delete-delay docs/html/. $(PUBLISH_DOC_SPHINX)/.
	git branch -f $(PUBLISHED_BRANCH)

publish-make-current:
	ssh $(PUBLISH_USER) 'set -e; cd $(PUBLISH_DOC_SPHINX_BASE); rm -f current.tmp; ln -s $(PUBLISH_VERSION) current.tmp; mv -T current.tmp current'

#---------- deployment ----------

DEPLOY_USER=ian@login.chiark.greenend.org.uk
DEPLOY_BASE=$(DEPLOY_USER):/volatile/Otter
DEPLOY_FINISH=/home/Otter/etc/deploy-finish

for-deploy: stamp/cargo.deploy-build

deploy-copy: for-deploy bundled-sources assets libraries
	rsync -zvl --progress $(addprefix $(DEPLOY_TARGET_DIR)/,$(PROGRAMS) otter-ssh-proxy) $(DEPLOY_BASE)/bin/
	rsync -zv --progress $(DEPLOY_TARGET_DIR)/usvg $(DEPLOY_BASE)/libexec/
	rsync -rv --progress $(TARGET_DIR)/bundled-sources/. $(DEPLOY_BASE)/bundled-sources
	rsync -r README.md $(DEPLOY_BASE)/.
	rsync -r --delete --exclude=\*~ library specs $(DEPLOY_BASE)/.
	rsync -r $(FILEASSETS) $(addprefix $(WASM_PACKED)/, $(WASM_ASSETS)) \
		$(DEPLOY_BASE)/assets/
	rsync -r nwtemplates/*.tera $(DEPLOY_BASE)/nwtemplates/

deploy: deploy-copy
	ssh -o BatchMode=true $(DEPLOY_USER) $(DEPLOY_FINISH)
	git branch -f $(DEPLOYED_BRANCH)
	-git push origin main
	-git push chiark main

#$(DEPLOY_BASE)/bundled-sources

#---------- clean ----------

clean-for-retest:
	rm -f templates/script.js library/*/*.usvg stamp/*
	rm -rf $(LIBRARY_CLEAN)
	rm -rf examples/big-bundle examples/big-bundle.zip
	rm -f examples/test-bundle.zip
	find * \( -name '*~' -o -name '*.tmp' \) -print0 | xargs -0r rm --

clean: clean-nailing clean-for-retest
	rm -rf target
	$(NAILING_CARGO_JUST_RUN) rm -rf target
