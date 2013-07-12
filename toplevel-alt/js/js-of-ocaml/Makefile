
all: check_lwt compiler library runtime

include Makefile.conf
-include Makefile.local

.PHONY: compiler library runtime examples check_lwt doc

compiler:
	$(MAKE) -C compiler
library:
	$(MAKE) -C lib
runtime:
	$(MAKE) -C runtime
toplevel: compiler library runtime
	$(MAKE) -C toplevel
examples: compiler library runtime
	$(MAKE) -C examples
tests: compiler library
	$(MAKE) -C tests
doc: library
	$(MAKE) -C doc

LWTERROR="Js_of_ocaml requires Lwt version 2.3.0 at least.  Please upgrade."
check_lwt:
	@if ocamlfind query lwt -l | ocaml tools/check_version.ml 2.3.0; then \
	  echo $(LWTERROR); exit 1; \
	fi

include Makefile.filelist

VERSION := $(shell head -n 1 VERSION)
install:
	ocamlfind install -patch-version ${VERSION} $(LIBRARY) lib/META $(INTF) $(IMPL) $(OTHERS) $(DOC)
	install -d -m 755 $(BINDIR)
	install $(BIN) $(BINDIR)

uninstall:
	ocamlfind remove $(LIBRARY)
	rm -f $(BINDIR)/$(COMPILER)

reinstall: uninstall install

depend:
	$(MAKE) -C compiler depend
	$(MAKE) -C lib depend

clean:
	$(MAKE) -C compiler clean
	$(MAKE) -C lib clean
	$(MAKE) -C runtime clean
ifeq ($(wildcard tests),tests)
	$(MAKE) -C tests clean
endif

realclean: clean
	find . -name "*~" -print | xargs rm -f

dist:
	rm -rf /tmp/js_of_ocaml-${VERSION} &&\
        cd /tmp &&\
	darcs get http://ocsigen.org/darcs/js_of_ocaml/ js_of_ocaml-${VERSION} &&\
	tar zcvf js_of_ocaml-${VERSION}.tar.gz js_of_ocaml-${VERSION} --exclude benchmarks --exclude _darcs --exclude tests