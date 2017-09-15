<<<<<<< HEAD
=======
-include ./Makefile.config

OCAMLMAKEFILE = ./OCamlMakefile

PACKS=str deriving.syntax deriving.syntax.classes deriving.runtime lwt lwt.syntax lwt.unix cgi base64 cohttp cohttp.lwt unix websocket websocket.lwt websocket.cohttp ANSITerminal linenoise
export OCAMLFLAGS=-syntax camlp4o

PATH := $(PATH):deriving

ifneq ($(shell ocamlfind query sqlite3),)
   DB_CODE += lite3_database.ml
   PACKS   += sqlite3
endif

ifneq ($(shell ocamlfind query mysql),)
   DB_CODE += mysql_database.ml
   PACKS   += mysql
endif

ifneq ($(shell ocamlfind query postgresql),)
   DB_CODE += pg_database.ml
   PACKS   += postgresql
   THREADS = yes
endif

ifdef PROF
OCAMLOPT := ocamlopt -p -inline 0
endif

#OCAMLYACC := menhir --infer --comment --explain --dump --log-grammar 1 --log-code 1 --log-automaton 2 --graph
OCAMLYACC := ocamlyacc -v

OCAMLFLAGS=-dtypes -w Ae-44-45-60 -g -cclib -lunix
#OCAMLDOCFLAGS=-pp deriving

# additional files to clean
TRASH=*.tmp *.output *.cache

# Other people's code.
OPC = unionfind.ml unionfind.mli \
      getopt.ml getopt.mli PP.ml

SOURCES = $(OPC)                                \
          multipart.ml                          \
          notfound.ml                           \
          utility.ml                            \
	  processTypes.mli processTypes.ml      \
          env.mli env.ml                        \
          settings.mli settings.ml              \
          basicsettings.ml                      \
	  parseSettings.ml \
          debug.mli debug.ml                    \
          performance.mli performance.ml        \
          graph.ml                              \
          types.mli types.ml                    \
          constant.ml                           \
          sourceCode.ml                         \
          regex.ml                              \
          sugartypes.ml                         \
          parser.mly                            \
          lexer.mli lexer.mll                   \
          typeUtils.mli typeUtils.ml            \
          errors.mli errors.ml                  \
          instantiate.mli instantiate.ml        \
          generalise.mli generalise.ml          \
          typevarcheck.mli typevarcheck.ml      \
          unify.mli unify.ml                    \
          var.ml                                \
          ir.mli ir.ml                          \
          tables.ml                             \
          closures.ml                           \
          parse.mli parse.ml                    \
          sugarTraversals.mli  sugarTraversals.ml       \
	  moduleUtils.mli moduleUtils.ml \
          resolvePositions.mli resolvePositions.ml       \
	  chaser.mli chaser.ml \
	  desugarModules.mli desugarModules.ml \
          desugarDatatypes.mli desugarDatatypes.ml      \
          defaultAliases.ml                     \
          requestData.mli requestData.ml        \
          value.mli value.ml                    \
          eventHandlers.mli eventHandlers.ml    \
          xmlParser.mly xmlLexer.mll            \
          parseXml.mli parseXml.ml              \
          refineBindings.mli refineBindings.ml           \
          desugarLAttributes.mli desugarLAttributes.ml   \
          transformSugar.mli transformSugar.ml           \
          fixTypeAbstractions.mli fixTypeAbstractions.ml \
          desugarPages.mli desugarPages.ml               \
          desugarFormlets.mli desugarFormlets.ml         \
          desugarRegexes.mli desugarRegexes.ml           \
          desugarFors.mli desugarFors.ml                 \
          desugarDbs.mli desugarDbs.ml                   \
          desugarFuns.mli desugarFuns.ml                 \
          desugarProcesses.mli desugarProcesses.ml       \
          desugarInners.mli desugarInners.ml             \
	  desugarCP.mli desugarCP.ml                     \
	  desugarHandlers.mli desugarHandlers.ml         \
          typeSugar.mli typeSugar.ml                     \
          checkXmlQuasiquotes.ml                \
          experimentalExtensions.ml \
          frontend.ml                           \
          dumpTypes.ml                          \
          compilePatterns.ml                    \
					websocketMessages.ml \
          jsonparse.mly                         \
          jsonlex.mll                           \
          js.ml                                 \
          json.mli json.ml                      \
          proc.mli proc.ml                      \
					resolveJsonState.mli resolveJsonState.ml \
          database.mli database.ml              \
          linksregex.ml                         \
	  lib.mli lib.ml                        \
          sugartoir.mli sugartoir.ml            \
          loader.mli loader.ml                  \
          $(DB_CODE)                            \
          irtojs.mli irtojs.ml                  \
          query.mli query.ml                              \
          queryshredding.ml                     \
          webserver_types.mli webserver_types.ml \
          webserver.mli                         \
	  evalir.ml                             \
          buildTables.ml                        \
          webif.mli webif.ml                    \
	  webserver.ml                          \
	  jsEmit.ml jscomp.ml                   \
          links.ml                              \

# TODO: get these working again
>>>>>>> A separate JS ir. Adapted higher-order continuation structure.
#
# Makefile for the Links Programming Language.
#

# This file contains instructions for building a bleeding-edge version
# of Links in release or development mode with and without database
# support.

# LATEST STABLE RELEASE
# =====================
# To install the latest stable release of Links please see the INSTALL
# file.

# To build the bleeding-edge version of Links please refer to the
# instructions below.

# DEVELOPMENT
# ===========
# By default the build rule `all' attempts to build the latest
# development version of Links with database support, thus the
# following command suffices to build everything
# $ make all
# For legacy reasons the rule `nc' (native compile) is an alias of
# `all'.
# NOTE: The rule `all' may skip the building the database drivers if
# the necessary system prerequisites are absent.

# The following command builds Links in development mode without
# database support
# $ make no-db

# RELEASE
# =======
# To build Links in release mode use the following command
# $ make all-release
# This rule builds every artefact which is part of a release. As such,
# it will include all the database drivers. Currently, there is no
# support for building Links without database support in release mode.


# Project root and build directory
ROOT:=$(shell dirname $(shell readlink -fn $(firstword $(MAKEFILE_LIST))))
BUILD_DIR:=$(ROOT)/_build

# The build command and some standard build system flags
BUILD=dune build
SOURCES=links
DB_SOURCES=links-postgresql,links-sqlite3
# Note: this relies on lazy expansion of `SOURCES'.
COMMON_FLAGS=--only-packages $(SOURCES) --build-dir=$(BUILD_DIR)
DEV_FLAGS=$(COMMON_FLAGS) --profile=development
REL_FLAGS=$(COMMON_FLAGS) --profile=release

# Build rules.

# The default is to build everything in development mode.
.DEFAULT_GOAL:= all
.PHONY: all
all: build-dev-all create-startup-script

# Builds everything in release mode.
.PHONY: all-release
all-release: build-release-all create-startup-script

# Legacy rule to remain backwards compatible with the OCamlMakefile
# interface.
.PHONY: nc
nc: all

# Builds core Links (without database support) in development mode.
.PHONY: no-db
no-db:	build-dev-nodb create-startup-script

# Creates a thin shell script, which executes the built Links
# executable in a pre-configured environment.
.PHONY: create-startup-script
create-startup-script:
	@echo "#!/usr/bin/env sh" > links
	@echo "LINKS_LIB=\"$(BUILD_DIR)/default/lib\" $(BUILD_DIR)/default/bin/links.exe \"\$$@\"" >> links
	@chmod +x links
	ln -fs links linx

# Invokes `dune' to build everything in development mode.
.PHONY: build-dev-all
build-dev-all: dune dune-project include-db-sources
	$(BUILD) $(DEV_FLAGS) @install

# Invokes `dune' to build only core Links in development mode.
.PHONY: build-dev-nodb
build-dev-nodb: dune dune-project
	$(BUILD) $(DEV_FLAGS) @install

# Invokes `dune' to build everything in release mode.
.PHONY: build-release-all
build-release-all: dune dune-project include-db-sources
	$(BUILD) $(REL_FLAGS) @install


# This rule updates the contents of the `SOURCES' variable to include
# the database sources.
.PHONY: include-db-sources
include-db-sources:
	$(eval SOURCES:=$(SOURCES),$(DB_SOURCES))

# Runs the test suite. We reset the variables CAMLRUNPARAM and
# OCAMLRUNPARAM, because some of the tests are sensitive to the
# printing of stack traces.
.PHONY: tests
tests: links
	$(eval OCAMLRUNPARAM="")
	$(eval CAMLRUNPARAM="")
	./run-tests

# Cleans the project directory.
.PHONY: clean
clean:
	dune clean
	rm -rf *.install
	rm -rf links linx

# Applies some ad-hoc checks on the source code.
.PHONY: rule-check
rule-check: tools/rule-check
	@echo "Applying rule check"
	@tools/rule-check

# The below machinery is used to prepare a release using topkg.
REPO=../opam-repository
PACKAGES=$(REPO)/packages

pkg-%:
	topkg opam pkg -n $*
	mkdir -p $(PACKAGES)/$*
	cp -r $(BUILD_DIR)/$*.* $(PACKAGES)/$*/
	rm -f $(PACKAGES)/$*/$*.opam
	cd $(PACKAGES) && git add $*

PKGS=$(basename $(wildcard *.opam))
opam-pkg:
	$(MAKE) $(PKGS:%=pkg-%)
