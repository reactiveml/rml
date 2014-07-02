RMLC=rmlc
RMLBUILD=rmlbuild -classic-display -tags lco_n
FLAGS=-lib graphics -no-links
TARGET=native

all:    rogervilder_carres_noir_et_blanc.rml.$(TARGET) \
	ala_rogervilder.rml.$(TARGET)

rogervilder_carres_noir_et_blanc.rml.$(TARGET):
	$(RMLBUILD) $(FLAGS) $@
	cp _build/$@ .

ala_rogervilder.rml.$(TARGET):
	$(RMLBUILD) $(FLAGS) $@
	cp _build/$@ .

clean:
	rmlbuild -clean

realclean: clean
	rm -rf rogervilder_carres_noir_et_blanc.rml.$(TARGET) \
	       ala_rogervilder.rml.$(TARGET)
cleanall: realclean
