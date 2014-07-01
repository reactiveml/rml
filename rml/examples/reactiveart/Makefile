RMLC=rmlc
RMLBUILD=rmlbuild -classic-display
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

cleanall: clean
	rm -rf rogervilder_carres_noir_et_blanc.rml.$(TARGET) \
	       ala_rogervilder.rml.$(TARGET)
