RMLC=rmlc -runtime Lco_rewrite_proba
RMLLIB=$(shell $(RMLC) -where)

OCAMLDEP=ocamldep
OCAMLC=ocamlc -g
OCAMLOPT=ocamlopt

NAME=slam_instantaneous

all: opt

byte: $(NAME)
opt: $(NAME).opt

$(NAME): $(NAME).ml array_misc.ml
	$(OCAMLC) -rectypes -o $(NAME) -I $(RMLLIB) \
		unix.cma rmllib.cma graphics.cma \
		array_misc.ml $(NAME).ml

$(NAME).opt: $(NAME).ml array_misc.ml
	$(OCAMLOPT) -rectypes -o $(NAME).opt -I $(RMLLIB) \
		unix.cmxa rmllib.cmxa graphics.cmxa \
		array_misc.ml $(NAME).ml

$(NAME).ml: array_misc.rzi $(NAME).rml
	$(RMLC) -dtypes $(NAME).rml

%.rzi: %.rmli
	$(RMLC) -c $<

clean:
	-rm -f *annot
	-rm -f *.rzi
	-rm -f *.cm* *.o
	-rm -f $(NAME).ml

cleanall: clean
	-rm -f $(NAME) $(NAME).opt
	-rm -f *~
