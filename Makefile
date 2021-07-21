# Makefile for ReactiveML
# Taken from Lucid-synchrone
# Organization : SPI team, LIP6 laboratory, University Paris 6

include configure-tools/version
include config

all: config-stamp
	(cd compiler; touch .depend; $(MAKE) depend; $(MAKE) $(TARGET))
	(cd stdlib; $(MAKE) all)
	(cd interpreter; touch .depend; $(MAKE) depend; $(MAKE) all)
	(cd toplevel; $(MAKE) all)
	(cd tools; $(MAKE) $(TARGET))

config-stamp:
	./configure
	touch $@

toplevel: FORCE
	(cd toplevel; $(MAKE) all)

toplevel-install:
	(cd toplevel; $(MAKE) install)


opt: TARGET := opt
opt: all

byte: TARGET := byte
byte: all

.PHONY: install

install:
	(cd compiler; $(MAKE) install)
	(cd stdlib; $(MAKE) install)
	(cd interpreter; $(MAKE) install)
	(cd man; $(MAKE) install)
	(cd emacs; $(MAKE) install)
	(cd toplevel; $(MAKE) install)
	(cd tools; $(MAKE) install)

checkinstall: config
	checkinstall -D --deldoc=yes --deldesc=yes --nodoc -y --install=no

uninstall:
	(cd compiler; $(MAKE) uninstall)
	(cd stdlib; $(MAKE) uninstall)
	(cd interpreter; $(MAKE) uninstall)
	(cd man; $(MAKE) uninstall)
	(cd emacs; $(MAKE) uninstall)
	(cd toplevel; $(MAKE) uninstall)
	(cd tools; $(MAKE) uninstall)


### BEGIN Patch from Serge Leblanc

install.findlib:
	@echo "Install ReactiveML interpreter in ocamlfind hierarchy."
	@(echo "version = \"$(VERSION)\"" > ./META)
	@(cat ./configure-tools/META.in >> ./META)
	- [ ! -e ./rmllib.a ] && \
		ln -s ./interpreter/rmllib.a ./rmllib.a
	- [ ! -e ./rmllib.cma ] && \
		ln -s ./interpreter/rmllib.cma ./rmllib.cma
	- [ ! -e ./rmllib.cmxa ] && \
		ln -s ./interpreter/rmllib.cmxa ./rmllib.cmxa
	- [ -r ./META -a -r ./rmllib.a -a -r ./rmllib.cma -a -r ./rmllib.cmxa ] && \
		ocamlfind install rmlc ./META ./rmllib.a ./rmllib.cma ./rmllib.cmxa
	- rm -rf rmllib.a rmllib.cma rmllib.cmxa

uninstall.findlib:
	ocamlfind remove rmlc

### END


doc: dvi
dvi:
	(cd doc; $(MAKE) dvi)
html:
	(cd doc; $(MAKE) html)

wc:
	(cd compiler;$(MAKE) wc)
	(cd interpreter; $(MAKE) wc)
	(cd toplevel; $(MAKE) wc)
	(cd tools; $(MAKE) wc)

clean:
	(cd compiler;$(MAKE) clean)
	(cd stdlib; $(MAKE) clean)
	(cd interpreter; $(MAKE) clean)
	(cd toplevel; $(MAKE) clean)
	(cd tools; $(MAKE) clean)
	(cd man; $(MAKE) clean)
	(cd doc; $(MAKE) clean)
	(cd patch; $(MAKE) clean)
	(cd examples; $(MAKE) clean)

realclean: clean-distrib
	(cd compiler;$(MAKE) realclean)
	(cd stdlib; $(MAKE) realclean)
	(cd interpreter; $(MAKE) realclean)
	(cd toplevel; $(MAKE) realclean)
	(cd tools; $(MAKE) realclean)
	(cd man; $(MAKE) realclean)
	(cd doc; $(MAKE) realclean)
	(cd patch; $(MAKE) realclean)
	(cd examples; $(MAKE) realclean)
	rm -rf META
	rm -rf config config-stamp Makefile.common distrib/rml/rmlc.in distrib/rml/Makefile *~
	touch config
	rm -rf configure-tools/rmlbuild.config

cleanall: realclean

# Making distribution
DATE=`date "+%Y-%m-%d"`

public-distrib:
	touch config
	$(MAKE) realclean
	mkdir -p distrib/rml-$(VERSION)-$(DATE)
	cp -r compiler interpreter stdlib toplevel tools emacs doc man examples \
		distrib/rml-$(VERSION)-$(DATE)
	cp -r configure configure-tools  patch Makefile CHANGES INSTALL LICENSE \
		distrib/rml-$(VERSION)-$(DATE)
	mkdir -p distrib/rml-$(VERSION)-$(DATE)/distrib
	cp -r distrib/rml distrib/Makefile.byte distrib/Makefile.opt \
		distrib/rmlc.in.byte distrib/rmlc.in.opt \
		distrib/rml-$(VERSION)-$(DATE)/distrib
	(cd distrib/rml-$(VERSION)-$(DATE)/patch; \
	 $(MAKE) public-distrib)
	(cd distrib; \
	 tar --exclude=CVS --exclude=.svn --exclude=.git -zcvf rml-$(VERSION)-$(DATE).tar.gz rml-$(VERSION)-$(DATE); \
	 rm -rf rml-$(VERSION)-$(DATE); \
	 mv rml-$(VERSION)-$(DATE).tar.gz ..)


source-distrib:
	touch config
	$(MAKE) realclean
	mkdir -p distrib/rml-$(VERSION)-$(DATE)
	cp -r compiler interpreter stdlib toplevel tools emacs doc man examples \
		distrib/rml-$(VERSION)-$(DATE)
	cp -r configure configure-tools patch Makefile CHANGES INSTALL LICENSE \
		distrib/rml-$(VERSION)-$(DATE)
	mkdir -p distrib/rml-$(VERSION)-$(DATE)/distrib
	cp -r distrib/rml distrib/Makefile.byte distrib/Makefile.opt \
		distrib/rmlc.in.byte distrib/rmlc.in.opt \
		distrib/rml-$(VERSION)-$(DATE)/distrib
	(cd distrib; \
	 tar --exclude=CVS --exclude=.svn --exclude=.git -zcvf rml-$(VERSION)-$(DATE).tar.gz rml-$(VERSION)-$(DATE); \
	 rm -rf rml-$(VERSION)-$(DATE); \
	 mv rml-$(VERSION)-$(DATE).tar.gz ..)

binary-distrib: binary-distrib.opt

binary-distrib.opt: clean-distrib
	touch config
	$(MAKE) realclean
	./configure
	(cd compiler; touch .depend; $(MAKE) depend; $(MAKE) opt)
	(cd stdlib; $(MAKE) all)
	(cd interpreter; $(MAKE) all)
	(cd toplevel; $(MAKE) all)
	(cd tools; $(MAKE) opt)
	(cd distrib/rml/; \
	 mkdir bin lib lib/rml; \
	 cp ../../compiler/rmlc.opt bin/rmlc.opt ; \
	 cp ../../toplevel/rmltop bin/rmltop ; \
	 cp ../../stdlib/*.rzi ../../stdlib/*.rmli lib/rml ; \
	 cp ../../interpreter/*.cma ../../interpreter/*.cmxa ../../interpreter/*.a ../../interpreter/*.cmi lib/rml ; \
	 cp ../../toplevel/*.cmo ../../toplevel/*.cmi lib/rml ; \
	 cp ../../tools/rmldep/rmldep.opt bin/rmldep ; \
	 cp -r ../../emacs . ; \
	 cp ../Makefile.opt Makefile; \
	 cp ../rmlc.in.opt rmlc.in; \
         cd ..; \
	 tar --exclude=CVS --exclude=.svn --exclude=.git -zcvf rml-`../compiler/rmlc -version`.opt.tar.gz rml; \
         mv rml-`../compiler/rmlc -version`.opt.tar.gz ..)

binary-distrib.byte: clean-distrib
	touch config
	$(MAKE) realclean
	./configure
	(cd compiler; touch .depend; $(MAKE) depend; $(MAKE) byte)
	(cd stdlib; $(MAKE) all)
	(cd interpreter; $(MAKE) all)
	(cd toplevel; $(MAKE) all)
	(cd tools; $(MAKE) all)
	(cd distrib/rml/ ; \
	 mkdir bin lib lib/rml ; \
	 cp ../../compiler/rmlc.byte bin/rmlc.byte ; \
	 cp ../../toplevel/rmltop bin/rmltop ; \
	 cp ../../stdlib/*.rzi ../../stdlib/*.rmli lib/rml ; \
	 cp ../../interpreter/*.cma ../../interpreter/*.cmxa ../../interpreter/*.a ../../interpreter/*.cmi lib/rml ; \
	 cp ../../toplevel/*.cmo ../../toplevel/*.cmi lib/rml ; \
	 cp ../../tools/rmldep/rmldep.byte bin/rmldep ; \
	 cp -r ../../emacs . ; \
	 cp ../Makefile.byte Makefile ; \
	 cp ../rmlc.in.byte rmlc.in ; \
         cd ..; tar --exclude=CVS --exclude=.svn --exclude=.git -zcvf rml-`../compiler/rmlc -version`.byte.tar.gz rml ; \
         mv rml-`../compiler/rmlc -version`.byte.tar.gz ..)

clean-distrib:
	rm -rf distrib/rml/bin \
		distrib/rml/lib \
		distrib/rml/emacs \
		distrib/rml/man
	- rm -f rml-`./compiler/rmlc -version`.*.tar.gz
	rm -f rml-$(VERSION)-????-??-??.tar.gz

FORCE:
