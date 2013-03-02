# files to install
FILES=	INSTALL \
	rml.el \
	rml-font.el  \
	rml-process-acc.el  \
	rml-xemacs.el \
	rml-emacs.el \
	rml-help.el \
	rml-types.el

include ../config

all:

install:
ifneq ($(EMACSDIR), "")
	$(INSTALL) -d $(EMACSDIR)
	$(INSTALL) $(FILES) $(EMACSDIR)
endif

uninstall:
ifneq ($(EMACSDIR), "")
	(cd $(EMACSDIR); rm -f $(FILES))
endif

clean:

realclean:
	rm -rf *~
