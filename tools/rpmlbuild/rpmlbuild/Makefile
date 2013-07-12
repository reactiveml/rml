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
#include ../../../configure-tools/Makefile.common

INSTALL_BIN=$(INSTALL_BINDIR)
INSTALL_LIB=$(RPMLBUILD_LIBDIR)


ifndef BUILDDIR
BUILDDIR := "_build"
endif

ifndef OCAMLBUILDCMD
OCAMLBUILDCMD := ocamlbuild -classic-display
endif

OCAMLBUILD=INSTALL_LIB=$(INSTALL_LIB) INSTALL_BIN=$(INSTALL_BIN) $(OCAMLBUILDCMD) -build-dir $(BUILDDIR) -no-links $(OCAMLBUILD_OPTIONS)
LIBS=ocamlbuildlib
PROGRAMS=ocamlbuild
BYTE=$(LIBS:=.cma) $(PROGRAMS:=.byte)
NATIVE=$(LIBS:=.cmxa) $(PROGRAMS:=.native)

all:
	$(OCAMLBUILD) $(BYTE)
ifeq ($(TARGET), native)
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

promote:
	cp $(BUILDDIR)/ocamlbuild.native boot/ocamlbuild

clean:
	rm -rf $(BUILDDIR)
	rm -rf _log _start

realclean: clean
	rm -rf ocamlbuild_Myocamlbuild_config.ml

install: all install-prepare install-opt install-byte
ifeq ($(TARGET), native)
	cp $(BUILDDIR)/rpmlbuild.native $(INSTALL_BIN)/rpmlbuild
	cp $(BUILDDIR)/rpmlbuild.byte $(INSTALL_BIN)
else
	cp $(BUILDDIR)/rpmlbuild.byte   $(INSTALL_BIN)/rpmlbuild
endif

install-prepare:
	mkdir -p $(INSTALL_BIN)
	mkdir -p $(INSTALL_LIB)

install-opt: install-byte
ifeq ($(TARGET), native)
	cp $(BUILDDIR)/ocamlbuild.native $(BUILDDIR)/rpmlbuild.native
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
endif

install-byte:
	cp $(BUILDDIR)/ocamlbuild.byte $(BUILDDIR)/rpmlbuild.byte
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
