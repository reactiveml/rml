DIRECTORIES=rmldep rmlbuild

TARGET=byte

all:
	(for d in $(DIRECTORIES); \
	 do (cd $$d ; make $(TARGET)) ; \
	 done)

opt: TARGET := opt
opt: all

byte: TARGET := byte
byte: all


install:
	(for d in $(DIRECTORIES); \
	 do (cd $$d ; make install) ; \
	 done)


# cleaning:
clean:
	(for d in $(DIRECTORIES); \
	 do (cd $$d ; make clean) ; \
	 done)

realclean: clean
	(for d in $(DIRECTORIES); \
	 do (cd $$d ; make realclean) ; \
	 done)
	rm -rf *~

cleanall: realclean

# utilities
wc:
	(for d in $(DIRECTORIES); \
	 do (cd $$d ; make wc) ; \
	 done)
