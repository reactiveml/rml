#ifndef SEARCH_H
#define SEARCH_H

#include "structure.h"

struct packet_PDL info_dest(noeud *node, int id_dest, MYSQL *mysql, int PDL);
noeud * rech_ancre(noeud **tab_topologie, noeud *ancre_actuel, struct packet_PDL *packet, int alpha, int portee, MYSQL *mysql, int nb_noeud, int PDL, struct tab_resultats *tab_result);

#endif
