#ifndef ROUTAGE_H
#define ROUTAGE_H
#include "structure.h"
#include "search.h"

int reach_dest(noeud * node, struct packet_PDL *packet);
int calc_proba_ins(noeud *node, int id_dest, int proba_insertion, int portee, MYSQL *mysql);
noeud * routage_geo(noeud * node, struct packet_PDL *packet, MYSQL *mysql, noeud **tab_topologie, int portee, int alpha, int GREASE, int PDL, int nb_noeud, int proba_insertion, struct tab_resultats *tab_result);

#endif
