#include "routage.h"

//rend 1 si le noeud relai est positionné au point ancre ou s'il est la destination
int reach_dest(noeud * node, struct packet_PDL *packet){
	return (((node->posX == packet->posX_dest) && (node->posY == packet->posY_dest)) || (node->num_noeud == packet->id_dest));
};

//fonction qui détermine si on doit insérer ou pas
int calc_proba_ins(noeud *node, int id_dest, int proba_insertion, int portee, MYSQL *mysql){
	MYSQL_ROW row;
	MYSQL_RES *result;
	char requete[512];
	char posX[10], posY[10];
	unsigned long *lengths;
	float distance;
	//la distance entre les noeuds (en nb de sauts)
	//on la calcule à partir des coordonnées du noeud
	//et des coordonnées connues de la destination
	//on a la distance cartésienne qu'on divise
	//par le rayon de couverture des noeuds
	if (proba_insertion) return (gener_alea(proba_insertion));
	else{
		sprintf(requete, "select posX_pdl, posY_pdl from routage where id = %d and id_noeud = %d", node->num_noeud, id_dest); 
		if (mysql_query(mysql,requete)){
			printf("pb lors de la seléction des coordonnées du noeud\n");
			exit(1);
		};
		
		result = mysql_store_result(mysql);
		if ((row = mysql_fetch_row(result))){ //on fait un if pour le cas où la requête renvoie 0 lignes
			lengths = mysql_fetch_lengths(result);
			sprintf(posX, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
			sprintf(posY, "%.*s", (int) lengths[1], row[1] ? row[1] : "NULL");
		};
		mysql_free_result(result);		
		distance = sqrt(pow((atoi(posX) - node->posX),2)+pow((atoi(posY) - node->posY),2));
		return (gener_alea(((int) (distance / portee))+1));
	};
};


noeud * routage_geo(noeud * node, struct packet_PDL *packet, MYSQL *mysql, noeud **tab_topologie, int portee, int alpha, int GREASE, int PDL, int nb_noeud, int proba_insertion, struct tab_resultats *tab_result){
	MYSQL_ROW row;
	MYSQL_RES *result;
	char requete[512];
	char Id[10], posX[10], posY[10];
	unsigned long *lengths;
	struct packet_PDL packet_aux;

	//printf("%d\t",node->num_noeud);
	if (reach_dest(node, packet)) return node;
	else{

		if (GREASE){
			//on regarde si le noeud en question n'a pas une meilleure info de la destination
			//auquel cas, on change la destination et l'âge qu'on a d'elle
			packet_aux = info_dest(node, packet->id_dest, mysql, PDL);
			if (!packet_aux.age_pos_dest){
				if (PDL) (tab_result->nb_saut_pdl)++;
				else (tab_result->nb_saut)++;
				return tab_topologie[(packet->id_dest)-1];
			};
			if (packet_aux.age_pos_dest <= packet->age_pos_dest/alpha){
				packet->age_pos_dest = packet_aux.age_pos_dest;
				packet->posX_dest = packet_aux.posX_dest;
				packet->posY_dest = packet_aux.posY_dest;
			};
		};

		if (PDL){
			//on regarde si le packet contient des infos (posX et posY != 0)
			//si oui, on met à jour sa table de routage avec un âge à 1
			if (packet->posX_pdl){
				//on incrémente l'overhead pdl
				(tab_result->overhead_pdl)++;
				if (info_dest(node, packet->id_pdl, mysql, PDL).age_pos_dest) 
					sprintf(requete, "update routage set posX_pdl=%d, posY_pdl=%d, age_pdl=%d where id=%d and id_noeud=%d", packet->posX_pdl, packet->posY_pdl, 1, node->num_noeud, packet->id_pdl);
				else 
					sprintf(requete, "update routage set posX_pdl=%d, posY_pdl=%d, age_pdl=%d where id=%d and id_noeud=%d", packet->posX_pdl, packet->posY_pdl, 0, node->num_noeud, packet->id_pdl);

				//printf("mise à jour avec PDL\n");
				if (mysql_query(mysql,requete)){
					printf("problème lors de la màj de la table de routage PDL\n");
					exit(1);
				};
			}
			else{	//on n'insère que si calc_proba_ins rend 0
				if(!calc_proba_ins(node, packet->id_dest, proba_insertion, portee, mysql)){
					//on incrémente le compteur de nombre d'info pdl ajoutées
					(tab_result->nb_info_pdl_inserees) ++;
					//on incrémente l'overhead pdl
					(tab_result->overhead_pdl)++;
					//récupération des infos concernant le noeud actuel
					sprintf(requete, "select posX, posY from noeuds where id = %d", node->num_noeud);
					if (mysql_query(mysql,requete)){
						printf("problème lors de la récupération des infos du noeud qui va insérer son info\n");
						exit(1);
					};
					result = mysql_store_result(mysql);
					if ((row = mysql_fetch_row(result))){ //on fait un if pour le cas où la requête renvoie 0 lignes
						lengths = mysql_fetch_lengths(result);
						sprintf(posX, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
						sprintf(posY, "%.*s", (int) lengths[1], row[1] ? row[1] : "NULL");
					};
					//insertion des infos du noeud actuel ds le packet
					packet->id_pdl = node->num_noeud;
					packet->posX_pdl = (atoi(posX));
					packet->posY_pdl = (atoi(posY));

					mysql_free_result(result);
				};
			};
		};
			
		
		
		//sélection des voisins plus proches de la destination que le noeud qui a le paquet en les ordonnant par distance les séparants de la destination
		//on les ordonne également par Id = id_dest. si la destination fait partie des voisins, elle sera sélectionnée
		//sinon, on prend un des voisins les plus proches de la destination (peu importe lequel)
		//le != est fait exprès pour que si la dest est atteinte, ce sera la 1e ligne
		sprintf(requete, " SELECT Id from noeuds where ((((posX-%d)*(posX-%d))+((posY-%d)*(posY-%d))) <= %d*%d) and  ((((posX-%d)*(posX-%d))+((posY-%d)*(posY-%d))) < (((%d-%d)*(%d-%d))+((%d-%d)*(%d-%d)))) order by (Id != '%d'), (((posX-%d)*(posX-%d))+((posY-%d)*(posY-%d))) asc", node->posX, node->posX, node->posY, node->posY, portee, portee, packet->posX_dest, packet->posX_dest, packet->posY_dest, packet->posY_dest, node->posX, packet->posX_dest, node->posX, packet->posX_dest, node->posY, packet->posY_dest, node->posY, packet->posY_dest, packet->id_dest, packet->posX_dest, packet->posX_dest, packet->posY_dest, packet->posY_dest);
		//sprintf(requete, " SELECT Id from noeuds where ((((posX-%d)*(posX-%d))+((posY-%d)*(posY-%d))) <= %d*%d) and  ((((posX-%d)*(posX-%d))+((posY-%d)*(posY-%d))) < (((%d-%d)*(%d-%d))+((%d-%d)*(%d-%d)))) and Id <= %d order by (Id != '%d'), (((posX-%d)*(posX-%d))+((posY-%d)*(posY-%d))) asc", node->posX, node->posX, node->posY, node->posY, portee, portee, packet->posX_dest, packet->posX_dest, packet->posY_dest, packet->posY_dest, node->posX, packet->posX_dest, node->posX, packet->posX_dest, node->posY, packet->posY_dest, node->posY, packet->posY_dest, nb_noeud, packet->id_dest, packet->posX_dest, packet->posX_dest, packet->posY_dest, packet->posY_dest);
		if (mysql_query(mysql,requete)){
			printf("problème lors de la sélection des voisins plus proche de la destination que le noeud qui a le paquet\n");
			exit(1);
		};

		result = mysql_store_result(mysql);
		if ((row = mysql_fetch_row(result))){ //on fait un if pour le cas où la requête renvoie 0 lignes
			lengths = mysql_fetch_lengths(result);
			sprintf(Id, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
			//if (atoi(Id) == packet.id_dest) printf("noeud destination atteint\n");
			//printf("routage passant par le noeud %s\n", Id);
			//on incrémente le nombre de sauts
			if (PDL) (tab_result->nb_saut_pdl)++;
			else (tab_result->nb_saut)++;
			mysql_free_result(result);
			return (routage_geo(tab_topologie[atoi(Id)-1], packet, mysql, tab_topologie, portee, alpha, GREASE, PDL, nb_noeud, proba_insertion, tab_result));
		}
		else{
			mysql_free_result(result);
			return node;
		};
	};
};

