# Makefile for Reactive-ML
# Taken from Lucid-synchrone
# Organization : SPI team, LIP6 laboratory, University Paris 6 

TARGET=byte

all:
	(cd compiler; touch .depend; make depend; make $(TARGET))
	(cd stdlib; make all)
	(cd interpreter; touch .depend; make depend; make all)


opt: TARGET := opt
opt: all

byte: TARGET := byte
byte: all

install: install.byte

install.byte:
	(cd compiler; make install.byte)
	(cd stdlib; make install)
	(cd interpreter; make install)
	(cd man; make install)
	(cd emacs; make install)

install.opt:
	(cd compiler; make install.opt)
	(cd stdlib; make install)
	(cd interpreter; make install)
	(cd man; make install)
	(cd emacs; make install)

doc: dvi
dvi:
	(cd doc; make dvi)
html:
	(cd doc; make html)

clean:
	(cd compiler;make clean)
	(cd stdlib; make clean)
	(cd interpreter; make clean)
	(cd man; make clean)
	(cd doc; make clean)

realclean: clean-distrib
	(cd compiler;make realclean)
	(cd stdlib; make realclean)
	(cd interpreter; make realclean)
	(cd man; make realclean)
	(cd doc; make realclean)
	rm -rf config.cache config.status config.log config


# Making binary distribution 
DATE=`date -I`
source-distrib:
	touch config
	make realclean
	mkdir -p distrib/rml-v1.02-$(DATE)
	cp -r compiler interpreter stdlib emacs examples doc man \
		distrib/rml-v1.02-$(DATE)
	cp configure configure.in config.in Makefile install-sh INSTALL \
		distrib/rml-v1.02-$(DATE)
	mkdir -p distrib/rml-v1.02-$(DATE)/distrib
	cp -r distrib/rml distrib/rml-v1.02-$(DATE)/distrib
	(cd distrib; \
	 tar --exclude=CVS -czvf rml-v1.02-$(DATE).tar.gz rml-v1.02-$(DATE); \
	 rm -rf rml-v1.02-$(DATE); \
	 mv rml-v1.02-$(DATE).tar.gz ..)

binary-distrib: binary-distrib.opt

binary-distrib.opt: clean-distrib
	touch config
	make realclean
	./configure 
	(cd compiler; touch .depend; make depend; make opt)
	(cd stdlib; make all)
	(cd interpreter; make all)
	(cd distrib/rml/; \
	 mkdir bin lib lib/rml; \
	 cp ../../compiler/rmlc.opt bin/rmlc.opt ; \
	 cp ../../stdlib/*.{rzi,rmli} lib/rml; \
	 cp ../../interpreter/{*.{cma,cmxa,a,cmi}} lib/rml; \
	 cp -r ../../emacs .; \
	 cp ../Makefile.opt Makefile; \
	 cp ../rmlc.in.opt rmlc.in; \
         cd ..; \
	 tar --exclude=CVS -zcvf rml-`../compiler/rmlc -version`.opt.tar.gz rml; \
         mv rml-`../compiler/rmlc -version`.opt.tar.gz ..)

binary-distrib.byte: clean-distrib
	touch config
	make realclean
	./configure 
	(cd compiler; touch .depend; make depend; make byte)
	(cd stdlib; make all)
	(cd interpreter; make all)
	(cd distrib/rml/; \
	 mkdir bin lib lib/rml; \
	 cp ../../compiler/rmlc.byte bin/rmlc.byte ; \
	 cp ../../stdlib/*.{rzi,rmli} lib/rml; \
	 cp ../../interpreter/{*.{cma,cmxa,a,cmi}} lib/rml; \
	 cp -r ../../emacs .; \
	 cp ../Makefile.byte Makefile; \
	 cp ../rmlc.in.byte rmlc.in; \
         cd ..; tar --exclude=CVS -zcvf rml-`../compiler/rmlc -version`.byte.tar.gz rml; \
         mv rml-`../compiler/rmlc -version`.byte.tar.gz ..)

clean-distrib: 
	rm -rf distrib/rml/bin \
		distrib/rml/lib \
		distrib/rml/emacs \
		distrib/rml/man \
	rm -f rml-`./compiler/rmlc -version`.*.tar.gz
	rm -f rml-v1.02-????-??-??.tar.gz



