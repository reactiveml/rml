#include "search.h"

//donne l'âge et la position que connait le noeud de la destination
//rend l'info ds une structure packet qui ne contient que l'âge et les positions
struct packet_PDL info_dest(noeud *node, int id_dest, MYSQL *mysql, int PDL){
	MYSQL_ROW row;
	MYSQL_RES *result;
	char requete[256];
	char age[10], posX[10], posY[10];
	unsigned long *lengths;
	struct packet_PDL packet_reponse;
	if (PDL) sprintf(requete, "select age_pdl, posX_pdl, posY_pdl from routage where Id ='%d' and Id_noeud ='%d'", node->num_noeud,  id_dest);
	else sprintf(requete, "select age, posX, posY from routage where Id ='%d' and Id_noeud ='%d'", node->num_noeud,  id_dest);
	if (mysql_query(mysql,requete)){
		printf("pb de sélection d'age\n");
		exit(1);
	};
	result = mysql_store_result(mysql);
	if ((row = mysql_fetch_row(result))){
		lengths = mysql_fetch_lengths(result);
		sprintf(age, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
		sprintf(posX, "%.*s", (int) lengths[1], row[1] ? row[1] : "NULL");
		sprintf(posY, "%.*s", (int) lengths[2], row[2] ? row[2] : "NULL");
	}
	else{
		printf("BIZARRE, le noeud %d ne connait pas la destinantion %d\n", node->num_noeud,  id_dest);
		exit(1);
	};
	packet_reponse.age_pos_dest = atoi(age);
	packet_reponse.posX_dest = atoi(posX);
	packet_reponse.posY_dest = atoi(posY);

	mysql_free_result(result);

	return packet_reponse;
};

noeud * rech_ancre(noeud **tab_topologie, noeud *ancre_actuel, struct packet_PDL *packet, int alpha, int portee, MYSQL *mysql, int nb_noeud, int PDL, struct tab_resultats *tab_result){
	MYSQL_ROW row, row_bis;
	MYSQL_RES *result1, *result2, *result3;
	char requete[512]; 
	char Id[10], age[10], posX[10], posY[10];
	int best_age = packet->age_pos_dest;
	int id_ancre=0;
	int ancre_trouve=0;
	int rayon_rech = 0;
	unsigned long *lengths;
	my_ulonglong nb_result=1;
	//avant de commencer, on efface le contenu des tables temp
	sprintf(requete, "delete from temp");
	if (mysql_query(mysql,requete)){
		printf("impossible d'effacer le contenu de temp2\n");
		exit(1);
	};
	sprintf(requete, "delete from temp2");
	if (mysql_query(mysql,requete)){
		printf("impossible d'effacer le contenu de temp2\n");
		exit(1);
	};

	//création de la table qui garde les id des noeuds déjà visités
	sprintf(requete, "insert into temp2 values (%d)", ancre_actuel->num_noeud);
	if (mysql_query(mysql,requete)){
		printf("problème lors de l'insertion ds la temp2 du noeud ancre actuel\n");
		exit(1);
	};

	

	//vérifier si le noeud en question actuel n'a pas une meilleure estimation que celle du paquet
	//sprintf(requete, "select age from routage where Id ='%d' and Id_noeud ='%d'", ancre_actuel->num_noeud,  packet.id_dest);
	//if (mysql_query(mysql,requete)){
		//printf("pb de sélection d'age du premier noeud\n");
		//exit(1);
	//};
	//result1 = mysql_store_result(mysql);
	//if ((row = mysql_fetch_row(result1))){
		//lengths = mysql_fetch_lengths(result1);
		//sprintf(age, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
	
	if ((info_dest(ancre_actuel, packet->id_dest, mysql, PDL)).age_pos_dest <= packet->age_pos_dest/(float) alpha){
		//printf("l'age trouve chez le noeud ancre convient, rayon de rech = 0\n");
		ancre_trouve = 1;
		//on droppe la table 2 si on sort là
		sprintf(requete, "delete from temp2");
		if (mysql_query(mysql,requete)){
			printf("impossible d'effacer le contenu de temp2\n");
			exit(1);
		};
		return ancre_actuel;
	};
	//}
	//else{
		//printf("BIZARRE, le noeud %d ne connait pas la destinantion %d\n", ancre_actuel->num_noeud,  packet.id_dest);
		//exit(1);
	//};

	//le noeud actuel n'est pas ancre, on le cherche alors
	sprintf(requete, "select Id, posX, posY from noeuds where Id = %d", ancre_actuel->num_noeud);
	if (mysql_query(mysql,requete)){
		printf("pb de sélection des voisins du premier noeud\n");
		exit(1);
	};
	result1 = mysql_store_result(mysql);

	while (!ancre_trouve && (nb_result)){

		//on incrémente le degré de voisinage 
		if (PDL) (tab_result->rayon_rech_pdl)++;
		else (tab_result->rayon_rech)++;

		//sélection des voisins de tous les noeuds se trouvant ds result1
		while ((row = mysql_fetch_row(result1))){
			//pour chq élément de result1 on...
			lengths = mysql_fetch_lengths(result1);
			//...récupère sa position
			sprintf(Id, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
			sprintf(posX, "%.*s", (int) lengths[1], row[1] ? row[1] : "NULL");
			sprintf(posY, "%.*s", (int) lengths[2], row[2] ? row[2] : "NULL");

			//...on cherche ses voisins et on les met ds temp
			sprintf(requete, "INSERT INTO temp select noeuds.Id, noeuds.posX, noeuds.posY from noeuds left join temp2 on noeuds.id=temp2.id where temp2.id is null and ((((noeuds.posX-%s)*(noeuds.posX-%s))+((noeuds.posY-%s)*(noeuds.posY-%s))) <= %d) and noeuds.id <= %d", posX, posX, posY, posY, (int) pow(portee, 2), nb_noeud);
			if (mysql_query(mysql,requete)){
				printf("problème lors de l'insertion des noeuds des voisins\n");
				exit(1);
			};

			//on insère ds la table temp2 les voisins du noeud en cours de traitement
			//afin de ne plus les resélectionner
			sprintf(requete, "INSERT INTO temp2 select noeuds.Id from noeuds left join temp2 on noeuds.id=temp2.id where temp2.id is null and ((((noeuds.posX-%s)*(noeuds.posX-%s))+((noeuds.posY-%s)*(noeuds.posY-%s))) <= %d) and noeuds.id <= %d", posX, posX, posY, posY, (int) pow(portee, 2), nb_noeud);
			if (mysql_query(mysql,requete)){
				printf("problème lors de l'insertion des id noeuds des voisins\n");
				exit(1);
			};
			
		};
		
		mysql_free_result(result1);
		
		//on sélectionne tous les voisins des voisins
		sprintf(requete, "select * from temp");
		if (mysql_query(mysql,requete)){
			printf("problème ds le select de la table temp\n");
			exit(1);
		};
		result2 = mysql_store_result(mysql);
		nb_result = mysql_num_rows(result2);

		//on incrémente l'overhead  de:
                //nb_voisin*(1+rayon_rech) (où nb_voisin = nb_result)
		if (PDL) (tab_result->overhead_pdl) += (((int) nb_result) * (1 + rayon_rech));
		else (tab_result->overhead) += (((int) nb_result) * (1 + rayon_rech));
		//on incrémente le rayon de recherche
		rayon_rech++;
		
		if (PDL) (tab_result->nb_voisin_pdl) += (int) nb_result;
		else (tab_result->nb_voisin) += (int) nb_result;
		while((row = mysql_fetch_row(result2))){
			//pour chaque voisin de ce degré 
			//on regarde si on n'a pas une meilleure info que celle
			//du voisin de degré précédent
			lengths = mysql_fetch_lengths(result2);
			sprintf(Id, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
			sprintf(posX, "%.*s", (int) lengths[1], row[1] ? row[1] : "NULL");
			sprintf(posY, "%.*s", (int) lengths[2], row[2] ? row[2] : "NULL");
			//on regarde si on n'est pas chez la destination, auquel cas l'age est de 0
			if (atoi(Id) == packet->id_dest){
				ancre_trouve = 1;
				//on efface le contenu de la table si on sort ici
				sprintf(requete, "delete from temp");
				if (mysql_query(mysql,requete)){
					printf("impossible d'effacer la table temp\n");
					exit(1);
				};
				//si on sort ici, on  remplit la BDD avec le rayon de recherche
				//et le nombre de voisins ds ce rayon
				//printf("somme des rayons de rech = %d avec somme de voisins = %d\n", (PDL) ? tab_result->rayon_rech_pdl : tab_result->rayon_rech, (PDL) ? tab_result->nb_voisin_pdl : tab_result->nb_voisin);
				mysql_free_result(result2);
				return tab_topologie[(packet->id_dest)-1];
			};
			//on regarde l'âge qu'a le voisin de la position de la destination
			if (PDL) sprintf(requete, "select age_pdl from routage where Id ='%s' and Id_noeud ='%d'", Id,  packet->id_dest);
			else sprintf(requete, "select age from routage where Id ='%s' and Id_noeud ='%d'", Id,  packet->id_dest);
			if (mysql_query(mysql,requete)){
				printf("problème lors de la sélection de l'age chez le voisin\n");
				exit(1);
			};
			result3 = mysql_store_result(mysql);
			row_bis = mysql_fetch_row(result3);
			lengths = mysql_fetch_lengths(result3);
			sprintf(age, "%.*s", (int) lengths[0], row_bis[0] ? row_bis[0] : "NULL");
			//comparaison des ages
			//printf("l'age trouvé est %s chez le noeud %s\n", age, Id);
			if (atoi(age) < packet->age_pos_dest/(float) alpha){
				//printf("age trouvé est %d age du packet est %d\n", atoi(age), packet.age_pos_dest);
				ancre_trouve = 1;
				//printf("une ancre trouvée à l'id %s \n", Id);
				if (atoi(age) < best_age){
					//printf("l'age est %d alors qu'en string est %s \n", atoi(age), age);
					best_age = atoi(age);
					//printf("l'ancre avec le meilleur age est %d alors qu'en string est %s \n", atoi(Id), Id);
					id_ancre = atoi(Id);
				};
			};
			mysql_free_result(result3);
		};

		mysql_free_result(result2);

		//on reseléctionne les voisins pour les gardes ds result1
		sprintf(requete, "select * from temp");
		if (mysql_query(mysql,requete)){
			printf("problème ds le select de la table temp\n");
			exit(1);
		};
		result1 = mysql_store_result(mysql);
		//nb_result = mysql_num_rows(result1);

		sprintf(requete, "delete from temp");
		if (mysql_query(mysql,requete)){
			printf("impossible d'effacer la table temp\n");
			exit(1);
		};
	};
	sprintf(requete, "delete from temp2");
	if (mysql_query(mysql,requete)){
		printf("impossible d'effacer la table temp2\n");
		exit(1);
	};

	mysql_free_result(result1);
	
	//printf("somme des rayons de rech = %d avec somme de voisins = %d\n", (PDL) ? tab_result->rayon_rech_pdl : tab_result->rayon_rech, (PDL) ? tab_result->nb_voisin_pdl : tab_result->nb_voisin);
	if (nb_result)	return tab_topologie[id_ancre-1];
	else{
		printf("plus de voisins chez qui chercher\n");
		return NULL;
	};
};
