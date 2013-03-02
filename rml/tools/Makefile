DIRECTORIES=rmldep rmlbuild

TARGET=byte

all:
	(for d in $(DIRECTORIES); \
	 do (cd $$d ; $(MAKE) $(TARGET)) ; \
	 done)

opt: TARGET := opt
opt: all

byte: TARGET := byte
byte: all


install:
	(for d in $(DIRECTORIES); \
	 do (cd $$d ; $(MAKE) install) ; \
	 done)

uninstall:
	(for d in $(DIRECTORIES); \
	 do (cd $$d ; $(MAKE) uninstall) ; \
	 done)


# cleaning:
clean:
	(for d in $(DIRECTORIES); \
	 do (cd $$d ; $(MAKE) clean) ; \
	 done)

realclean: clean
	(for d in $(DIRECTORIES); \
	 do (cd $$d ; $(MAKE) realclean) ; \
	 done)
	rm -rf *~

cleanall: realclean

# utilities
wc:
	(for d in $(DIRECTORIES); \
	 do (cd $$d ; $(MAKE) wc) ; \
	 done)
