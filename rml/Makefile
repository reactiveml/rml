# Makefile for Reactive-ML
# Taken from Lucid-synchrone
# Organization : SPI team, LIP6 laboratory, University Paris 6

include configure-tools/version
-include config

all: config-stamp
	(cd compiler; touch .depend; make depend; make $(TARGET))
	(cd stdlib; make all)
	(cd interpreter; touch .depend; make depend; make all)
	(cd toplevel; make all)
	(cd tools; make $(TARGET))

config-stamp:
	./configure
	touch $@

toplevel: FORCE
	(cd toplevel; make all)

toplevel-install:
	(cd toplevel; make install)


opt: TARGET := opt
opt: all

byte: TARGET := byte
byte: all

install:
	(cd compiler; make install)
	(cd stdlib; make install)
	(cd interpreter; make install)
	(cd man; make install)
	(cd emacs; make install)
	(cd toplevel; make install)
	(cd tools; make install)

checkinstall: config
	checkinstall -D --deldoc=yes --deldesc=yes --nodoc -y --install=no

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
	(cd doc; make dvi)
html:
	(cd doc; make html)

wc:
	(cd compiler;make wc)
	(cd interpreter; make wc)
	(cd toplevel; make wc)
	(cd tools; make wc)

clean:
	(cd compiler;make clean)
	(cd stdlib; make clean)
	(cd interpreter; make clean)
	(cd toplevel; make clean)
	(cd tools; make clean)
	(cd man; make clean)
	(cd doc; make clean)
	(cd patch; make clean)
	(cd examples; make clean)

realclean: clean-distrib
	(cd compiler;make realclean)
	(cd stdlib; make realclean)
	(cd interpreter; make realclean)
	(cd toplevel; make realclean)
	(cd tools; make realclean)
	(cd man; make realclean)
	(cd doc; make realclean)
	(cd patch; make realclean)
	(cd examples; make realclean)
	rm -rf META
	rm -rf config config-stamp Makefile.common distrib/rml/rmlc.in distrib/rml/Makefile *~
	rm -rf configure-tools/rmlbuild.config

cleanall: realclean

# Making distribution
DATE=`date "+%Y-%m-%d"`

public-distrib:
	touch config
	make realclean
	mkdir -p distrib/rml-$(VERSION)-$(DATE)
	cp -r compiler interpreter stdlib toplevel tools emacs doc man examples \
		distrib/rml-$(VERSION)-$(DATE)
	cp -r configure configure-tools  patch Makefile INSTALL LICENSE \
		distrib/rml-$(VERSION)-$(DATE)
	mkdir -p distrib/rml-$(VERSION)-$(DATE)/distrib
	cp -r distrib/rml distrib/Makefile.byte distrib/Makefile.opt \
		distrib/rmlc.in.byte distrib/rmlc.in.opt \
		distrib/rml-$(VERSION)-$(DATE)/distrib
	(cd distrib/rml-$(VERSION)-$(DATE)/patch; \
	 make public-distrib)
	(cd distrib; \
	 tar --exclude=CVS --exclude=.svn -zcvf rml-$(VERSION)-$(DATE).tar.gz rml-$(VERSION)-$(DATE); \
	 rm -rf rml-$(VERSION)-$(DATE); \
	 mv rml-$(VERSION)-$(DATE).tar.gz ..)


source-distrib:
	touch config
	make realclean
	mkdir -p distrib/rml-$(VERSION)-$(DATE)
	cp -r compiler interpreter stdlib toplevel tools emacs doc man examples \
		distrib/rml-$(VERSION)-$(DATE)
	cp -r configure configure-tools patch Makefile INSTALL LICENSE \
		distrib/rml-$(VERSION)-$(DATE)
	mkdir -p distrib/rml-$(VERSION)-$(DATE)/distrib
	cp -r distrib/rml distrib/Makefile.byte distrib/Makefile.opt \
		distrib/rmlc.in.byte distrib/rmlc.in.opt \
		distrib/rml-$(VERSION)-$(DATE)/distrib
	(cd distrib; \
	 tar --exclude=CVS --exclude=.svn -zcvf rml-$(VERSION)-$(DATE).tar.gz rml-$(VERSION)-$(DATE); \
	 rm -rf rml-$(VERSION)-$(DATE); \
	 mv rml-$(VERSION)-$(DATE).tar.gz ..)

binary-distrib: binary-distrib.opt

binary-distrib.opt: clean-distrib
	touch config
	make realclean
	./configure
	(cd compiler; touch .depend; make depend; make opt)
	(cd stdlib; make all)
	(cd interpreter; make all)
	(cd toplevel; make all)
	(cd tools; make opt)
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
	 tar --exclude=CVS --exclude=.svn -zcvf rml-`../compiler/rmlc -version`.opt.tar.gz rml; \
         mv rml-`../compiler/rmlc -version`.opt.tar.gz ..)

binary-distrib.byte: clean-distrib
	touch config
	make realclean
	./configure
	(cd compiler; touch .depend; make depend; make byte)
	(cd stdlib; make all)
	(cd interpreter; make all)
	(cd toplevel; make all)
	(cd tools; make all)
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
         cd ..; tar --exclude=CVS --exclude=.svn -zcvf rml-`../compiler/rmlc -version`.byte.tar.gz rml ; \
         mv rml-`../compiler/rmlc -version`.byte.tar.gz ..)

clean-distrib:
	rm -rf distrib/rml/bin \
		distrib/rml/lib \
		distrib/rml/emacs \
		distrib/rml/man
	- rm -f rml-`./compiler/rmlc -version`.*.tar.gz
	rm -f rml-$(VERSION)-????-??-??.tar.gz

FORCE:
