#ifndef ENVOI_H
#define ENVOI_H

#include "structure.h"
#include "routage.h"
#include "search.h"

void envoi_packet(noeud **tab_topologie, noeud *source, int id_dest, struct packet_PDL *packet, int alpha, int portee, MYSQL *mysql, int GREASE, int PDL, int nb_noeud, int proba_insertion, struct tab_resultats *tab_result);

#endif
