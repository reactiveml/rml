OCAMLC = ocamlc
OCAMLOPT = ocamlopt
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc
RMLC = rmlc
INTERPRETERDIR=`$(RMLC) -where`
CAMLIMAGESDIR=-I +camlimages
# CAMLIMAGESDIR=-I $(HOME)/Ocaml/camlimages-2.2/ps \
# -I $(HOME)/Ocaml/camlimages-2.2/bmp \
# -I $(HOME)/Ocaml/camlimages-2.2/gif \
# -I $(HOME)/Ocaml/camlimages-2.2/png \
# -I $(HOME)/Ocaml/camlimages-2.2/ppm \
# -I $(HOME)/Ocaml/camlimages-2.2/xpm \
# -I $(HOME)/Ocaml/camlimages-2.2/jpeg \
# -I $(HOME)/Ocaml/camlimages-2.2/tiff \
# -I $(HOME)/Ocaml/camlimages-2.2/corelib \
# -I $(HOME)/Ocaml/camlimages-2.2/graphics \
# -I $(HOME)/Ocaml/camlimages-2.2/lablgtk \
# -I $(HOME)/Ocaml/camlimages-2.2/xvthumb \
# -I $(HOME)/Ocaml/camlimages-2.2/freetype2 \
# -I $(HOME)/Ocaml/camlimages-2.2/lablgtk2 \


SAMPLING = 0.02

RMLCOPTS = -runtime ctrl_tree

MAINFILE=main.rml

OBJS = misc.cmo \
       moteur.cmo \
       convenient.cmo \
       fantome.cmo \
       affichage.cmo \
       pacman.cmo

#       convenient.cmo 
# pour savoir quel fichier contient la procedure principale :
MAINPROC= systeme 


MLINTER = pacman.ml moteur.ml main.ml fantome.ml affichage.ml


# Executable :
EXEC = pacman
# Librairie de linkage :
LIBS = unix.cma graphics.cma $(CAMLIMAGESDIR) ci_core.cma ci_graphics.cma ci_bmp.cma $(INTERPRETERDIR)/$(INTERPRETER).cma

all: $(EXEC)


main.cmo:main.ml pacman.cmi

moteur.ml:moteur.rml misc.rzi

pacman.ml:pacman.rml fantome.cmo affichage.cmo convenient.rzi

affichage.ml: affichage.rml

fantome.ml:fantome.rml convenient.rzi

misc.cmo:misc.ml misc.cmi
	$(OCAMLC) -c $(CAMLIMAGESDIR) $<

convenient.cmo:convenient.ml convenient.cmi

misc.cmi:misc.mli

#convenient.rzi:convenient.mli

#convenient.mli:convenient.ml
#	$(OCAMLOPT) -c -i $< >convenient.mli


#cat moteur.rml | grep "open " | cut -c 5-


#Ne pas toucher apres cette ligne !!!!!

INTERPRETER=rml_interpreter


#Genere les 'dependances' opt :

LIBSOPT= $(patsubst %.cma,%.cmxa,$(patsubst %.cmo,%.cmx, $(LIBS)))
MAINOBJOPT =$(patsubst %.cmo,%.cmx,$(MAINOBJ)) 
OBJSOPT= $(patsubst %.cmo,%.cmx,$(OBJS))
EXECOPT= $(EXEC).opt


MAINOBJ=$(patsubst %.rml,%.cmo,$(MAINFILE))


opt: $(EXECOPT)

$(EXEC): $(OBJS) $(MAINOBJ)
	$(OCAMLC) -o $(EXEC) $(LIBS) $(OBJS) $(MAINOBJ)


$(EXECOPT): $(OBJSOPT) $(MAINOBJOPT)
	$(OCAMLOPT) -o $(EXECOPT) $(LIBSOPT) $(OBJSOPT) $(MAINOBJOPT) -cclib -lunix 


$(patsubst %.rml,%.ml,$(MAINFILE)): $(MAINFILE)
	$(RMLC) $(RMLCOPTS) -sampling $(SAMPLING) -s $(MAINPROC) $<


# Clean tout :
clean:
	rm -f *.cmi *.cmo *.cmx *.o *.rzi
	rm -f $(MLINTER)
	rm -f *.rzi

cleanall realclean : clean
	rm -f $(EXEC) $(EXECOPT)


# Regles implicites :

.PHONY: all opt clean cleanall realclean
.SUFFIXES: .ml .mli .cmo .cmi .cmx .rml .rzi


# Regle rml:

# memo :  dans les commandes, $* remplace % !!

%.rzi:%.mli 
	$(RMLC) $(RMLCOPTS) $<

#%.rzi: %.mli
#	$(RMLC) $(RMLCOPTS) $<

%.rzi: %.rml
	$(RMLC) $(RMLCOPTS) $<

%.cmo:%.ml
	$(OCAMLC) -c -I $(INTERPRETERDIR) $*.ml

%.ml:%.rml
	$(RMLC) $(RMLCOPTS) $<

%.cmi:%.mli
	$(OCAMLC) -c $<

#%.cmi:%.ml
#	$(OCAMLC) -c $<
               

#%.cmx:%.ml
#	$(OCAMLOPT) -c $<
