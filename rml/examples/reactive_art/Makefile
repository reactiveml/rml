RMLC=rmlc
RMLBUILD=rmlbuild -classic-display -tags lco_n
FLAGS=-lib graphics -no-links
TARGET=native

.PHONY: clean FORCE

all: rogervilder_carres_noir_et_blanc.rml.$(TARGET) ala_rogervilder.rml.$(TARGET)


%.rml.native: FORCE
	rm -f $@
	$(RMLBUILD) $(FLAGS) $@
	cp _build/$@ .

clean: 
	rm -rf _build
	rm -f *.byte *.native


realclean: clean

cleanall: clean
