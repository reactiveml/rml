#include "envoi.h"

void envoi_packet(noeud **tab_topologie, noeud *source, int id_dest, struct packet_PDL *packet, int alpha, int portee, MYSQL *mysql, int GREASE, int PDL, int nb_noeud, int proba_insertion, struct tab_resultats *tab_result){
	noeud *noeud_ancre, *aux;
	MYSQL_ROW row;
	MYSQL_RES *result;
	char requete[256];
	char age[10], posX[10], posY[10];
	unsigned long *lengths;
	int sortir =0;

	//on afficher les coordonnées de la source et de la destination
	printf("la source %d est en X= %d et Y= %d\n", source->num_noeud, source->posX, source->posY);
	printf("la dest %d est en  X= %d et Y= %d\n", id_dest, tab_topologie[id_dest-1]->posX, tab_topologie[id_dest-1]->posY);
	
	//on insère dans le paquet les id de la source et de la dest
	packet->id_source = source->num_noeud;
	packet->id_dest = id_dest;
	//...et on met les posX et posY à 0
	if (PDL){
		packet->posX_pdl = 0;
		packet->posY_pdl = 0;
	};

	//on donne à noeud ancre la valeur de source
	//pourqu'on fasse la requete de position avec la variable noeud_ancre
	//qui pointe sur la source si cette dernière a une info sur la destination
	//ou sur le noeud ancre trouvé (aux) sinon
	noeud_ancre = source;

	//on cherche l'âge connu par la source
	sprintf(requete, "select age from routage where Id ='%d' and Id_noeud ='%d'", source->num_noeud,  packet->id_dest);
	if (mysql_query(mysql,requete)){
		printf("problème lors du remplissage du paquet chez la source\n");
		exit(1);
	};

	result = mysql_store_result(mysql);
	row = mysql_fetch_row(result);
	lengths = mysql_fetch_lengths(result);
	sprintf(age, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
	
	//on vérifie que l'âge que connait la source n'est pas > 10000
	if (atoi(age) >= 10000){
		packet->age_pos_dest = atoi(age);
		//sinon, on cherche un noeud ancre
		if (!(aux = rech_ancre (tab_topologie, source, packet, alpha, portee, mysql, nb_noeud, PDL, tab_result))){
			//si on en trouve pas, c'est qu'on n'a aucune info concernant la destination
			if (PDL) (tab_result->nb_dest_non_trouvee_pdl)++;                                 
			else (tab_result->nb_dest_non_trouvee)++;
			printf("La source n'a pas d'info sur la destination et n'a pas trouvé d'ancre qui en a\n");                                 
			sortir = 1;
		}
		else{
			noeud_ancre = aux;
		};
	};

	//on insère l'âge et les coordonnées connus par le noeud ancre, qui est 
	//la source si l'âge est < 10000 sinon, un noeud qui a une meilleure info
	if (PDL) sprintf(requete, "select posX_pdl, posY_pdl, age_pdl from routage where Id ='%d' and Id_noeud ='%d'", noeud_ancre->num_noeud,  packet->id_dest);
	else sprintf(requete, "select posX, posY, age from routage where Id ='%d' and Id_noeud ='%d'", noeud_ancre->num_noeud,  packet->id_dest);
	if (mysql_query(mysql,requete)){
		printf("problème lors du remplissage du paquet chez la source\n");
		exit(1);
	};

			

	result = mysql_store_result(mysql);
	row = mysql_fetch_row(result);
	lengths = mysql_fetch_lengths(result);
	sprintf(posX, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
	sprintf(posY, "%.*s", (int) lengths[1], row[1] ? row[1] : "NULL");
	sprintf(age, "%.*s", (int) lengths[2], row[2] ? row[2] : "NULL");
	mysql_free_result(result);

	//on vérifie que l'âge qu'on a n'est pas 0 et que cet âge a été donné par la source
	//si c'est le cas, la dest est ds le voisinage de la source, sinon, on fait un routage
	//vers cette position
	//explication (parce que je me suis un peu perdu en relisant :))
	//si l'âge qu'on a est 0 et qu'il a été donné par la source, alors, la dest est ds le voisinage de la source
	//sinon, si l'âge est 0 mais donné par un noeud ancre, on route vers l'emplacement de ce noeud ancre
	if (!atoi(age) && (source == noeud_ancre)) printf("la destination est ds le voisinage de la source, destination trouvée en 1 saut et 0 ancre\n");
	else{
		//dans le cas où sortir =1, ie que la destination n'a pas d'info sur la dest
		//et qu'elle n'a pas trouvé de noeud ancre qui en a, on ne fait rien de ce qui suit
		if (!sortir){
			//remplissage du paquet
			printf("l'âge que connait la source est %s\n", age);
			packet->posX_dest = atoi(posX);
			packet->posY_dest = atoi(posY);
			packet->age_pos_dest = atoi(age);

			//envoi du paquet vers la destination (vers un noeud ancre)
			//printf("recherche du 1e noeud ancre\n");
			noeud_ancre = routage_geo(source, packet, mysql, tab_topologie, portee, alpha, GREASE, PDL, nb_noeud, proba_insertion, tab_result);
			if (PDL) (tab_result->nb_ancre_pdl)++;
			else (tab_result->nb_ancre)++;
		};

		//recherche de la destination
		while ((noeud_ancre->num_noeud != id_dest)&& (!sortir)){
			//printf("noeud ancre numéro %d\n", *nb_noeud_ancre);
			if (!(aux = rech_ancre (tab_topologie, noeud_ancre, packet, alpha, portee, mysql, nb_noeud, PDL, tab_result))){
				if (PDL) (tab_result->nb_dest_non_trouvee_pdl)++;
				else (tab_result->nb_dest_non_trouvee)++;
				printf("il n'y a pas d'ancre permettant de trouver la destination avec un tel alpha\n");
				//exit(1);
				sortir = 1;
			}
			else {
				printf("le noeud %d a une meilleure info sur la dest\n", aux->num_noeud);
				if (PDL) sprintf(requete, "select posX_pdl, posY_pdl, age_pdl from routage where Id ='%d' and Id_noeud ='%d'", aux->num_noeud,  packet->id_dest);
				else sprintf(requete, "select posX, posY, age from routage where Id ='%d' and Id_noeud ='%d'", aux->num_noeud,  packet->id_dest);
				if (mysql_query(mysql,requete)){
					printf("problème lors du remplissage du paquet chez un noeud intermédiaire\n");
					exit(1);
				};

				//printf("mise à jour des coordonnées de la destination dans le paquet\n");
				result = mysql_store_result(mysql);
				row = mysql_fetch_row(result);
				lengths = mysql_fetch_lengths(result);
				sprintf(posX, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
				sprintf(posY, "%.*s", (int) lengths[1], row[1] ? row[1] : "NULL");
				sprintf(age, "%.*s", (int) lengths[2], row[2] ? row[2] : "NULL");

				mysql_free_result(result);

				//remplissage du paquet
				packet->posX_dest = atoi(posX);
				packet->posY_dest = atoi(posY);
				packet->age_pos_dest = atoi(age);

				//envoi du paquet vers la destination connue par le noeud ancre (vers un noeud ancre)
				//printf("recherche du nouveau noeud ancre\n");
				noeud_ancre = routage_geo(noeud_ancre, packet, mysql, tab_topologie, portee, alpha, GREASE, PDL, nb_noeud, proba_insertion, tab_result);
				if (PDL) (tab_result->nb_ancre_pdl)++;
	                        else (tab_result->nb_ancre)++;

				if (!info_dest(noeud_ancre, packet->id_dest, mysql, PDL).age_pos_dest){
					if (noeud_ancre->num_noeud == id_dest){
						if (PDL) printf("destination=ancre, trouvée sur le chemin en %d sauts et %d noeuds ancres\n", tab_result->nb_saut_pdl, tab_result->nb_ancre_pdl);
						else printf("destination=ancre, trouvée sur le chemin en %d sauts et %d noeuds ancres\n", tab_result->nb_saut, tab_result->nb_ancre);
					}
					else {
						if (PDL){
							(tab_result->nb_saut_pdl)++;
							printf("la destination est ds le voisinage du noeud ancre, destination trouvée en %d sauts et %d noeuds ancres\n", tab_result->nb_saut_pdl, tab_result->nb_ancre_pdl);
						}
						else{
							(tab_result->nb_saut)++;
							printf("la destination est ds le voisinage du noeud ancre, destination trouvée en %d sauts et %d noeuds ancres\n", tab_result->nb_saut, tab_result->nb_ancre);
						};
					};
					//ds ce cas, on regarde si le champ pdl est rempli
					//si c'est le cas, on met à jour la table de routage du noeud destination
					if (PDL && packet->posX_pdl){
						printf("mise à jour avec PDL\n");
						sprintf(requete, "update routage set posX_pdl=%d, posY_pdl=%d, age_pdl=%d where id=%d and id_noeud=%d", packet->posX_pdl, packet->posY_pdl, 1, id_dest, packet->id_pdl);
						if (mysql_query(mysql,requete)){
							printf("problème lors de la màj de la table de routage PDL\n");
							exit(1);
						};
					};
					sortir =1;
				};

				//si la dest n'est pas ds le voisinage du noeud ancre et que 
				//l'info dans le paquet est = 0, c'est qu'on est dans le cas d'une dead-end
				if ((!packet->age_pos_dest) && (info_dest(noeud_ancre, packet->id_dest, mysql, PDL).age_pos_dest)){
					printf("dead-end. la position de la destination est connue mais elle n'est pas accessible avec cet algo de transert\n");
					if (PDL) (tab_result->nb_dest_non_trouvee_pdl)++;
					else (tab_result->nb_dest_non_trouvee)++;
					sortir = 1;
				};
			};
		};
	};
};


