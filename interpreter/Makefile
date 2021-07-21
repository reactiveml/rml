include ../config
include ../configure-tools/Makefile.common

all:
	$(MAKE) -f Makefile-lco all
	$(MAKE) -f Makefile-lco_n all
	$(MAKE) -f Makefile-lco_class all
	$(MAKE) -f Makefile-lco_rewrite all
	$(MAKE) -f Makefile-lk all

install:
	$(MAKE) -f Makefile-lco install
	$(MAKE) -f Makefile-lco_n install
	$(MAKE) -f Makefile-lco_class install
	$(MAKE) -f Makefile-lco_rewrite install
	$(MAKE) -f Makefile-lk install

uninstall:
	$(MAKE) -f Makefile-lco uninstall
	$(MAKE) -f Makefile-lco_n uninstall
	$(MAKE) -f Makefile-lco_class uninstall
	$(MAKE) -f Makefile-lco_rewrite uninstall
	$(MAKE) -f Makefile-lk uninstall

clean:
	$(MAKE) -f Makefile-lco clean
	$(MAKE) -f Makefile-lco_n clean
	$(MAKE) -f Makefile-lco_class clean
	$(MAKE) -f Makefile-lco_rewrite clean
	$(MAKE) -f Makefile-lk clean

realclean:
	rm -f *~
	$(MAKE) -f Makefile-lco realclean
	$(MAKE) -f Makefile-lco_n realclean
	$(MAKE) -f Makefile-lco_class realclean
	$(MAKE) -f Makefile-lco_rewrite realclean
	$(MAKE) -f Makefile-lk realclean
cleanall: realclean


# utilities
depend:
	$(OCAMLDEP) $(OCAMLDEPFLAGS) $(INCLUDES) *.mli *.ml > .depend

wc:
	wc -l *.ml *.mli

.PHONY: all install uninstall clean realclean .depend wc
