#ifndef STRUCTURE_H
#define STRUCTURE_H
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <mysql.h>
#include <unistd.h>
#include <math.h>
#include "generateur.h"
                                                                                
/****************************************************************/
/*              déclaration des structures                      */
/****************************************************************/

/* structure d'un noeud */

struct noeud {
	int num_noeud;
	int posX;
	int posY;
	struct noeud *next;
	struct noeud *prev;
	int last_direction;
	int posX_waypoint;//pour la mobilité waypoint
	int posY_waypoint;
	};
typedef struct noeud noeud;

struct packet_PDL {
	int id_source; //id de la source
	int id_dest; //id de la destination
	int posX_dest; //position de la destination
	int posY_dest;
	int age_pos_dest; //meilleur âge que connait le paquet
	int id_pdl; //id du noeud qui a inséré son info ds le paquet
	int posX_pdl; //position du noeud qui a inséré son info ds le paquet
	int posY_pdl;
};

struct tab_resultats {
	int nb_saut;
	int nb_saut_pdl;
	int nb_ancre;
	int nb_ancre_pdl;
	int nb_dest_non_trouvee;
	int nb_dest_non_trouvee_pdl;
	int nb_voisin;
	int nb_voisin_pdl;
	int rayon_rech;
	int rayon_rech_pdl;
	int age;
	int age_pdl;
	int nb_info_pdl_inserees;
	int nb_noeud_sans_info;
	int nb_noeud_sans_info_pdl;
	int overhead;
	int overhead_pdl;
};

#endif
