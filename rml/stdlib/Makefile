include ../config
include ../configure-tools/Makefile.common

all:
	(cd lco; $(MAKE) all)
	(cd lco_n; $(MAKE) all)
	(cd lco_class; $(MAKE) all)
	(cd lco_rewrite; $(MAKE) all)
	(cd lk; $(MAKE) all)

install:
	(cd lco; $(MAKE) install)
	(cd lco_n; $(MAKE) install)
	(cd lco_class; $(MAKE) install)
	(cd lco_rewrite; $(MAKE) install)
	(cd lk; $(MAKE) install)

uninstall:
	(cd lco; $(MAKE) uninstall)
	(cd lco_n; $(MAKE) uninstall)
	(cd lco_class; $(MAKE) uninstall)
	(cd lco_rewrite; $(MAKE) uninstall)
	(cd lk; $(MAKE) uninstall)

clean:
	(cd lco; $(MAKE) clean)
	(cd lco_n; $(MAKE) clean)
	(cd lco_class; $(MAKE) clean)
	(cd lco_rewrite; $(MAKE) clean)
	(cd lk; $(MAKE) clean)

realclean:
	rm -f *~
	(cd lco; $(MAKE) realclean)
	(cd lco_n; $(MAKE) realclean)
	(cd lco_class; $(MAKE) realclean)
	(cd lco_rewrite; $(MAKE) realclean)
	(cd lk; $(MAKE) realclean)
cleanall: realclean
