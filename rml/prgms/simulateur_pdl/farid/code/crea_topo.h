#ifndef CREA_TOPO_H
#define CREA_TOPO_H

#include "structure.h"

void pos_noeud(noeud *node, int taille);
noeud * crea_noeud(int i);
noeud *crea_topo(int nb_noeud, int taille_topo, MYSQL *mysql, noeud **tab_topologie);

#endif
