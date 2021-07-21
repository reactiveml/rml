RMLC=rmlc
RMLBUILD=rmlbuild -classic-display -tags lco_n
FLAGS=-lib graphics -no-links
TARGET=native

.PHONY: clean FORCE

all: 	rogervilder_carres_noir_et_blanc.rml.$(TARGET) \
	ala_rogervilder.rml.$(TARGET)


%.rml.$(TARGET): FORCE
	rm -f $@
	$(RMLBUILD) $(FLAGS) $@
	cp _build/$@ .

clean:
	rm -rf _build

realclean: clean
	rm -f *.byte *.native

cleanall: realclean
