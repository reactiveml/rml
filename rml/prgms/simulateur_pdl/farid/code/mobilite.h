#ifndef MOBILITE_H
#define MOBILITE_H

#include "generateur.h"
#include "structure.h"
#include "impression_topo.h"

void update_pos_noeud_BDD(noeud *node, MYSQL *mysql);
void maj_tab_routage (noeud *node,  int portee, MYSQL *mysql, int nb_noeud);
void deplacer_noeud(noeud *node, int proba_mobilite, int taille, int waypoint);
void mobilite(noeud *topologie, int nb_iterations, int proba_mobilite, int taille, int portee, MYSQL *mysql, int nb_noeud, int waypoint);

#endif
