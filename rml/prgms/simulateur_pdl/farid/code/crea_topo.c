#include "crea_topo.h"



void pos_noeud(noeud *node, int taille){
	node->posX = gener_alea(taille)+1;
	node->posY = gener_alea(taille)+1;
	node->posX_waypoint = gener_alea(taille)+1;
        node->posY_waypoint = gener_alea(taille)+1;
};

noeud * crea_noeud(int i){
	noeud *aux;
	if ((aux = (noeud *) malloc (sizeof(noeud))) == NULL) {exit(1);}
	aux->num_noeud = i;
	aux->posX=0;
	aux->posY=0;
	aux->next=NULL;
	aux->prev=NULL;
	aux->last_direction = gener_alea(8)+1;
	return aux;
};

noeud *crea_topo(int nb_noeud, int taille_topo, MYSQL *mysql, noeud **tab_topologie){
	noeud *node, *topologie;
	char requete[80];
	int i, j;

	topologie=NULL;
	for (i=nb_noeud;i>=1;i--){
		//printf("création du noeud %d \n", i);
		node = crea_noeud(i);
		pos_noeud(node, taille_topo);
		node->next=topologie;
		//if (topologie) topologie->prev = node;
		topologie=node;
		//on commente ce qui suit car on a la table qui est déjà remplie
		for (j=1;j<=nb_noeud;j++){
			//sprintf(requete, "insert into routage values (%d, %d, %d, %d, %d, %d, %d, %d)", j, i, node->posX, node->posY, 0, node->posX, node->posY, 0);
			sprintf(requete, "insert into routage values (%d, %d, 0, 0, 10000, 0, 0, 10000)", j, i);
			if (mysql_query(mysql, requete)){
				printf("problème lors de la création de la table de routage du noeud %d avec entrée du noeud %d\n", j, i);
				exit(1);
			};
		};
		sprintf(requete, "insert into noeuds values (%d, %d, %d)", i, node->posX, node->posY);
		//sprintf(requete, "update noeuds set posX=%d, posY=%d where id=%d", node->posX, node->posY, i);
		if (mysql_query(mysql, requete)){
			printf("problème ds la mise à jour des coordonnées du noeud %d lors de sa création\n",i);
			exit(1);
		};
		tab_topologie[i-1]=node;
	};
	return topologie;
};
