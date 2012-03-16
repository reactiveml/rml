.PHONY: all clean install

all:
	(cd rpmldep/; $(MAKE))
	(cd rpmlbuild/; $(MAKE))

install:
	(cd rpmldep/; $(MAKE) install)
	(cd rpmlbuild/; $(MAKE) install)

clean:
	(cd rpmldep/; $(MAKE) clean)
	(cd rpmlbuild/; $(MAKE) clean)
