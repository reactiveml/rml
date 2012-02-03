#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#   Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt  #
#                                                                       #
#   Copyright 2007 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id: Makefile 10286 2010-04-20 15:38:58Z doligez $

.PHONY: all byte native profile debug ppcache doc

include ../../../config

INSTALL_BIN=$(BINDIR)
INSTALL_LIB=$(LIBDIR)/rmlbuild


ifndef BUILDDIR
BUILDDIR := "_build"
endif

ifndef OCAMLBUILDCMD
OCAMLBUILDCMD := ./boot/ocamlbuild -classic-display
endif

ifdef O
OCAMLBUILD_OPTIONS := $(OCAMLBUILD_OPTIONS) $(O)
endif

ifeq ($(wildcard ./ocamlbuild_Myocamlbuil*_config.ml),./ocamlbuild_Myocamlbuild_config.ml)
ifeq ($(wildcard ./boot/oc*build),./boot/ocamlbuild)
OCAMLBUILD=INSTALL_LIB=$(INSTALL_LIB) INSTALL_BIN=$(INSTALL_BIN) $(OCAMLBUILDCMD) -build-dir $(BUILDDIR) -no-links $(OCAMLBUILD_OPTIONS)
LIBS=ocamlbuildlib ocamlbuildlightlib
PROGRAMS=ocamlbuild ocamlbuildlight
BYTE=$(LIBS:=.cma) $(PROGRAMS:=.byte)
NATIVE=$(LIBS:=.cmxa) $(PROGRAMS:=.native)

all:
	$(OCAMLBUILD) $(BYTE)
ifeq ($(TARGET), opt)
	$(OCAMLBUILD) $(NATIVE)
endif
byte:
	$(OCAMLBUILD) $(BYTE)
opt: native
native:
	$(OCAMLBUILD) $(NATIVE)
profile:
	$(OCAMLBUILD) $(LIBS:=.p.cmxa) $(PROGRAMS:=.p.native)
debug:
	$(OCAMLBUILD) $(LIBS:=.d.cma) $(PROGRAMS:=.d.byte)
ppcache:
	$(OCAMLBUILD) ppcache.byte ppcache.native
doc:
	$(OCAMLBUILD) ocamlbuild.docdir/index.html
	ln -s -f $(BUILDDIR)/ocamlbuild.docdir doc
else
all byte native: ocamlbuild.byte.start
	mkdir -p boot
	cp ocamlbuild.byte.start boot/ocamlbuild
	$(MAKE) $(MFLAGS) $(MAKECMDGOALS)
	cp $(BUILDDIR)/ocamlbuild.native boot/ocamlbuild
	$(MAKE) $(MFLAGS) $(MAKECMDGOALS) OCAMLBUILD_OPTIONS="-nothing-should-be-rebuilt -verbose -1"
endif
else
all byte native:
	@echo "Please copy the myocamlbuild_config.ml of the OCaml source distribution"
	@echo "  as ocamlbuild_Myocamlbuild_config.ml"
	@echo
	@echo "$$ cp ../myocamlbuild_config.ml ocamlbuild_Myocamlbuild_config.ml"
endif

ocamlbuild.byte.start:
	./start.sh

promote:
	cp $(BUILDDIR)/ocamlbuild.native boot/ocamlbuild

clean:
	rm -rf $(BUILDDIR)

distclean: clean
	rm -rf _log _start ocamlbuild.byte.start boot/ocamlbuild

install: all install-prepare install-opt install-byte
ifeq ($(TARGET), opt)
	cp $(BUILDDIR)/rmlbuild.native $(INSTALL_BIN)/rmlbuild
	cp $(BUILDDIR)/rmlbuild.byte $(INSTALL_BIN)
else
	cp $(BUILDDIR)/rmlbuild.byte   $(INSTALL_BIN)/rmlbuild
endif

install-prepare:
	mkdir -p $(INSTALL_BIN)
	mkdir -p $(INSTALL_LIB)

install-opt: install-byte
	cp $(BUILDDIR)/ocamlbuild.native $(BUILDDIR)/rmlbuild.native
	install -m 644 \
	        $(BUILDDIR)/ocamlbuildlib.cmxa \
	        $(BUILDDIR)/ocamlbuildlib.a \
	        $(BUILDDIR)/ocamlbuild_unix_plugin.cmx \
	        $(BUILDDIR)/ocamlbuild_unix_plugin.o \
	        $(BUILDDIR)/ocamlbuild_executor.cmx \
	        $(BUILDDIR)/ocamlbuild_executor.o \
	        $(BUILDDIR)/ocamlbuild_pack.cmx \
	        $(BUILDDIR)/ocamlbuild_pack.o \
	        $(BUILDDIR)/ocamlbuild.cmi \
	        $(BUILDDIR)/ocamlbuild_plugin.cmi \
	        $(BUILDDIR)/ocamlbuild.cmx \
	        $(BUILDDIR)/ocamlbuild.o \
		$(INSTALL_LIB)
	-ranlib $(INSTALL_LIB)/ocamlbuildlib.a

install-byte:
	cp $(BUILDDIR)/ocamlbuild.byte $(BUILDDIR)/rmlbuild.byte
	install -m 644 \
	        $(BUILDDIR)/ocamlbuildlib.cma \
	        $(BUILDDIR)/ocamlbuild_unix_plugin.cmo \
	        $(BUILDDIR)/ocamlbuild_unix_plugin.cmi \
	        $(BUILDDIR)/ocamlbuild_executor.cmi \
	        $(BUILDDIR)/ocamlbuild_executor.cmo \
	        $(BUILDDIR)/ocamlbuild_pack.cmi \
	        $(BUILDDIR)/ocamlbuild_pack.cmo \
	        $(BUILDDIR)/ocamlbuild.cmi \
	        $(BUILDDIR)/ocamlbuild_plugin.cmi \
	        $(BUILDDIR)/ocamlbuild.cmo \
		$(INSTALL_LIB)
