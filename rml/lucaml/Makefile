include ../config

INSTALLFILE= \

LUCAMLDIR=$(LIBDIR)/lucaml

all: $(INSTALLFILE)

# install: $(INSTALLFILE)
# 	$(INSTALL) -d $(LIBDIR)
# 	$(INSTALL) -d $(LUCAMLDIR)
# 	$(INSTALL) -m 644 $(INSTALLFILE) $(LUCAMLDIR)

install: 
	$(INSTALL) -d $(LIBDIR)
	$(INSTALL) -d $(LUCAMLDIR)
	$(INSTALL) -m 644 * $(LUCAMLDIR)

uninstall:
	(cd $(LUCAMLDIR); rm -f $(INSTALLFILE) )


# cleaning:
clean:

realclean: clean
	rm -f  *~

.PHONY: all install uninstall clean realclean
