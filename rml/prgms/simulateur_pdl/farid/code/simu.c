#include "simu.h"

MYSQL *mysql;
MYSQL_ROW row;
unsigned int num_fields, num_rows;
unsigned int i;
MYSQL_FIELD *fields, *field;
MYSQL_RES *result;

char *carg;
int cflag = 0;
int carg_int = 0;

int main(int argc, char *argv[]){
	static void usage();
	extern char * optarg;

	int portee, choix, nb_noeud, taille, duree_simu, vitess_noeud_max, vitesse_max, proba_mobilite, alpha, i, source, dest, PDL, proba_insertion, j, nb_msg, taille_msg, k, taille_msg_max, nb_dest_non_trouvee, nb_dest_non_trouvee_pdl, l, waypoint, nb_simu, m, densite;
	long int nb_entree_tab_routage;
	noeud *topologie, **tab_topologie, *aux;
	struct packet_PDL packet = {1, 2, 128, 231, 17, 1, 0, 0};
	FILE *contenu_BDD, *avec_info;
	FILE *rayon_voisin, *overhead, *chemin;
	unsigned long *lengths;
	MYSQL_ROW row;
	MYSQL_RES *result;
	char age[10], age_pdl[10], requete[256], nom_fichier[80], argument[80], id[10];
//	char Id[10], Id_noeud[10];
	struct tab_resultats tab_result = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};;

	//valeurs par défaut
	nb_noeud = 100;
	taille = 500;
	portee = 10;
	duree_simu = 10;
	vitess_noeud_max = 1;
	vitesse_max =1;
	proba_mobilite = 75; // en %
	alpha = 2;
	PDL = 1;
	proba_insertion = 0; //si = 0 alors on la calcule à chq fois
	nb_msg = 50; // par défaut, 50 % des noeuds communiquent pdt une itérations
	taille_msg_max = 5;
	l = 1;
	waypoint = 1;
	nb_simu = 50;

	while ((choix = getopt(argc , argv, "a:n:t:p:d:b:P:v:m:D:s:w:i:")) != -1)
		switch (choix) {
			case 'n': //définit le nombre de noeuds dans la topologie
				cflag++;
				carg = optarg;
				nb_noeud = atoi(carg);
				break;

			case 'D': //définit la densité par zone de couverture
				cflag++;
				carg = optarg;
				densite = atoi(carg);
				break;

			case 'a': //définit le coefficient de recherche d'age
				cflag++;
				carg = optarg;
				alpha = atoi(carg);
				break;

			case 't': //taille de la topologie
				cflag++;
				carg = optarg;
				taille = atoi(carg);
				break;

				//case 'e': //enregistrement dans un fichier
				//	enreg = '1';
				//	break;

			case 'p': //portee des noeuds
				cflag++;
				carg = optarg;
				portee = atoi(carg);
				break;
				
			case 'd': //durée de la simu
				cflag++;
				carg = optarg;
				duree_simu = atoi(carg);
				break;
				
			case 'b': //nb d'itérations par seconde
				cflag++;
				carg = optarg;
				vitess_noeud_max = atoi(carg);
				break;
				
			case 'P': //proba_mobilite
				cflag++;
				carg = optarg;
				proba_mobilite = atoi(carg);
				break;
				
			case 'v': //vitesse max de déplacement des noeuds
				cflag++;
				carg = optarg;
				vitesse_max = atoi(carg);
				break;
				
			case 's': //nb de simulations à faire
				cflag++;
				carg = optarg;
				nb_simu = atoi(carg);
				break;

			case 'm': //pourcentage de simulations
				cflag++;
				carg = optarg;
				nb_msg = atoi(carg);
				break;
				
			case 'i': //proba d'insertion (qd 0, on calcule à chq fois)
				cflag++;
				carg = optarg;
				proba_insertion = atoi(carg);
				break;
				
			case 'w': //waypoint (qd 0, route aléatoire forcée)
				cflag++;
				carg = optarg;
				waypoint = atoi(carg);
				break;
				
			default:
				printf("mauvais usage de simu.\n");
				break;
		};


	//nombre entrées table de routage = nb_noeud ^ 2
	nb_entree_tab_routage = pow(nb_noeud, 2);

	//calcul du rayon de couverture en fonction de la densité souhaitée
	portee = (int) sqrt(densite * pow(taille, 2) / (2 * 3.14 * nb_noeud));
	printf("le rayon de couverture est de %d\n", portee);

	//calcul du nombre de messages échangés à chq itérations
	//en fonction du pourcentage et du nombre de noeuds
	nb_msg = (int) (nb_noeud * nb_msg / 100);

	//on stocke le contenu de la BDD de routage pour connaitre comment ça évolue

		
	//nb_simu = le nombre de simulation qu'on veut effectuer
	//on le fixe par défaut à 50
	for (m=0; m<nb_simu; m++){

		//ouverture du fichier du chemin de 37
//		chemin= fopen ("resultats/chemin", "w");
		
		//création du nom de fichier avec_info
		strcpy(nom_fichier,"resultats/noeud_avec_info_nbnoeud_");
		sprintf(argument,"%.*d", 4, nb_noeud);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_taille_");
		sprintf(argument,"%.*d", 4, taille);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_densite_");
		sprintf(argument,"%.*d", 3, densite);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_mob_");
		sprintf(argument,"%.*d", 3, proba_mobilite);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_nbmsg_");
		sprintf(argument,"%.*d", 3, nb_msg);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_simu_");
		sprintf(argument,"%.*d", 2, m);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_waypoint_");
		sprintf(argument,"%.*d", 2, waypoint);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_insertion_");
		sprintf(argument,"%.*d", 2, proba_insertion);
		strcat(nom_fichier,argument);
		avec_info = fopen(nom_fichier,"w");
		
		//création du nom de fichier contenu_BDD
		strcpy(nom_fichier,"resultats/contenu_BDD_nbnoeud_");
		sprintf(argument,"%.*d", 4, nb_noeud);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_taille_");
		sprintf(argument,"%.*d", 4, taille);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_densite_");
		sprintf(argument,"%.*d", 3, densite);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_mob_");
		sprintf(argument,"%.*d", 3, proba_mobilite);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_nbmsg_");
		sprintf(argument,"%.*d", 3, nb_msg);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_simu_");
		sprintf(argument,"%.*d", 2, m);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_waypoint_");
		sprintf(argument,"%.*d", 2, waypoint);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_insertion_");
		sprintf(argument,"%.*d", 2, proba_insertion);
		strcat(nom_fichier,argument);
		contenu_BDD = fopen(nom_fichier, "w");

		printf("simu n° %d\n", m+1);

		//ouverture de la connexion avec la BDD
		mysql = mysql_init (NULL);
		mysql_real_connect(mysql, def_host_name, def_user_name, def_password, def_db_name, 0, NULL, 0);

		//mise à 0 des tables déjà existantes
		//	if(mysql_query(mysql,"update routage set posX=0, posY=0, age=0, age_pdl=0")) printf("merde, la table routage n'est pas remise à 0\n");
		//      else printf("table routage remise à 0\n");
		if(mysql_query(mysql,"delete from noeuds")){
			printf("merde, la table noeuds n'est pas vide\n");
			exit(1);
		}
		else printf("table noeuds effacée\n");
		if(mysql_query(mysql,"delete from routage")){
			printf("merde, la table routage n'est pas vide.\n");
			exit(1);
		}
		else printf("table routage effacée.\n");

		//création du tableau d'index de la topologie
		//tableau de pointeurs vers les noeuds pour un accès plus rapide aux noeuds
		printf("création du tableau d'indexation\n");
		if ((tab_topologie =(noeud **) malloc (nb_noeud*sizeof(noeud *))) == NULL) {exit(1);}

		//création de la topologie
		printf("création de la topologie\n");
		topologie = crea_topo(nb_noeud, taille, mysql, tab_topologie);
		//impr_topo(mysql, taille);

		//	fichier = fopen("resultats/chemin_parcouru","w");

		//on fait bouger les noeuds 50 fois avant de commencer
		/*	printf("on fait bouger la topo 20 fois avant de commencer les tests\n\n");
			for (i=0; i<50;i++){
			printf("déplacement avant test %d\n", i+1);
			mobilite(topologie, vitess_noeud_max, proba_mobilite, vitesse_max, taille, portee, mysql, nb_noeud, waypoint);
		//		fprintf(fichier, "%d\t%d\n", tab_topologie[1]->posX, tab_topologie[1]->posY);

		};*/

		//on met à jour les tables de routage
		aux = topologie;
		while(aux){
			maj_tab_routage(aux, portee, mysql, nb_noeud);
			aux=aux->next;
		};

		sprintf(requete, "select age, age_pdl from routage");
		if (mysql_query(mysql,requete)){
			printf("pb lors de la seléction des âges pour l'écriture ds le fichier\n");
			exit(1);
		};
		result = mysql_store_result(mysql);

		while ((row = mysql_fetch_row(result))){ 
			lengths = mysql_fetch_lengths(result);
			sprintf(age, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
			sprintf(age_pdl, "%.*s", (int) lengths[1], row[1] ? row[1] : "NULL");
			fprintf(contenu_BDD, "%d\t0\t%s\t%s\n", m, age, age_pdl);
		};
		mysql_free_result(result);

		//on imprime la topologie
		//impr_topo(mysql, taille, nb_noeud);


		//ouverture du fichier de résultats
		//fichier = fopen("resultats/result_courbe","w");
		

		taille_msg=1;
		nb_dest_non_trouvee = 0;
		nb_dest_non_trouvee_pdl = 0;

		tab_result.nb_info_pdl_inserees = 0;

		//création du nom de fichier contenu_BDD
		strcpy(nom_fichier,"resultats/rayon_voisin_nbnoeud_");
		sprintf(argument,"%.*d", 4, nb_noeud);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_taille_");
		sprintf(argument,"%.*d", 4, taille);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_densite_");
		sprintf(argument,"%.*d", 3, densite);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_mob_");
		sprintf(argument,"%.*d", 3, proba_mobilite);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_nbmsg_");
		sprintf(argument,"%.*d", 3, nb_msg);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_simu_");
		sprintf(argument,"%.*d", 2, m);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_waypoint_");
		sprintf(argument,"%.*d", 2, waypoint);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_insertion_");
		sprintf(argument,"%.*d", 2, proba_insertion);
		strcat(nom_fichier,argument);
		rayon_voisin = fopen(nom_fichier, "w");

		//on compte l'overhead sur toute la durée de la simulation
		strcpy(nom_fichier,"resultats/overhead_nbnoeud_");
		sprintf(argument,"%.*d", 4, nb_noeud);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_taille_");
		sprintf(argument,"%.*d", 4, taille);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_densite_");
		sprintf(argument,"%.*d", 3, densite);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_mob_");
		sprintf(argument,"%.*d", 3, proba_mobilite);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_nbmsg_");
		sprintf(argument,"%.*d", 3, nb_msg);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_simu_");
		sprintf(argument,"%.*d", 2, m);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_waypoint_");
		sprintf(argument,"%.*d", 2, waypoint);
		strcat(nom_fichier,argument);
		strcat(nom_fichier,"_insertion_");
		sprintf(argument,"%.*d", 2, proba_insertion);
		strcat(nom_fichier,argument);
		overhead = fopen(nom_fichier, "w");

		//on met l'overhead à 0
		tab_result.overhead = 0;
		tab_result.overhead_pdl = 0;
		
		for (i=0; i<duree_simu;i++){

			tab_result.nb_noeud_sans_info = 0;
			tab_result.nb_noeud_sans_info_pdl = 0;

			//mobilité
			//jusqu'à 3 par itération
			printf("mobilité n° %d\n", i+1);
			mobilite(topologie, vitess_noeud_max, proba_mobilite, taille, portee, mysql, nb_noeud, waypoint);
//			fprintf(chemin, "%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n", i, tab_topologie[1]->posX, tab_topologie[1]->posY, tab_topologie[2]->posX, tab_topologie[2]->posY, tab_topologie[3]->posX, tab_topologie[3]->posY, tab_topologie[4]->posX, tab_topologie[4]->posY);
			//impr_topo(mysql, taille, nb_noeud);

			//le nombre de paires source/dest par itération
			for (j=0;j<nb_msg;j++){
				printf("message n° %d\n", j+1);

				//désignation de la paire source dest
				source = gener_alea(nb_noeud);
				dest = gener_alea(nb_noeud);

				//la taille du message est aléatoire
				//on la met à 5 pour l'instant
				//taille_msg = gener_alea(taille_msg_max)+1;
				for (k=0;k<taille_msg;k++){
					tab_result.nb_saut_pdl = 0;
					tab_result.nb_ancre_pdl = 0;
					tab_result.nb_dest_non_trouvee_pdl = 0;
					tab_result.nb_voisin_pdl = 0;
					tab_result.rayon_rech_pdl = 0;

					tab_result.nb_saut = 0;
					tab_result.nb_ancre = 0;
					tab_result.nb_dest_non_trouvee = 0;
					tab_result.nb_voisin = 0;
					tab_result.rayon_rech = 0;

					//comparaison des âges 
					/*sprintf(requete, "select age, age_pdl from routage where id = '%d' and id_noeud = '%d'", source+1, dest+1);
					  if (mysql_query(mysql,requete)){
					  printf("pb de sélection des ages pour la comparaison\n");
					  exit(1);
					  };
					  result = mysql_store_result(mysql);
					  if ((row = mysql_fetch_row(result))){
					  lengths = mysql_fetch_lengths(result);
					  sprintf(age, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
					  sprintf(age_pdl, "%.*s", (int) lengths[1], row[1] ? row[1] : "NULL");
					  }
					  else{
					  printf("le noeud %d ne connait pas la destinantion %d (lors de la comparaison des âges\n", source+1,  dest+1);
					  exit(1);
					  };
					  tab_result.age = atoi(age);
					  tab_result.age_pdl = atoi(age_pdl);

					  mysql_free_result(result);*/


					//			printf("paquet n° %d\n", k+1);
//					envoi_packet(tab_topologie, tab_topologie[source], dest+1, &packet, alpha, portee, mysql, 0, 0, nb_noeud, proba_insertion, &tab_result);
					//printf("destination trouvée en %d sauts et %d noeuds ancre\n", nb_saut, nb_noeud_ancre);

					envoi_packet(tab_topologie, tab_topologie[source], dest+1, &packet, alpha, portee, mysql, 0, PDL, nb_noeud, proba_insertion, &tab_result);
					//printf("destination trouvée en %d sauts et %d noeuds ancre\n", nb_saut, nb_noeud_ancre);

					//on écrit les rés de la 1e simu
					/*				if (!tab_result.nb_dest_non_trouvee && !tab_result.nb_dest_non_trouvee_pdl){
									fprintf(fichier, "%d\t", l);
									fprintf(fichier, "%d\t", tab_result.nb_saut);
									fprintf(fichier, "%d\t", tab_result.nb_saut_pdl);
									fprintf(fichier, "%d\t", tab_result.age);
									fprintf(fichier, "%d\n", tab_result.age_pdl);
									l++;
									};

					//on incrémente le nb de destinations non trouvées
					nb_dest_non_trouvee += tab_result.nb_dest_non_trouvee;
					nb_dest_non_trouvee_pdl += tab_result.nb_dest_non_trouvee_pdl;*/
					fprintf(rayon_voisin, "%d\t%d\t%d\t%d\t%d\t%d\t%d\n", m, i, j, tab_result.rayon_rech, tab_result.nb_voisin, tab_result.rayon_rech_pdl, tab_result.nb_voisin_pdl);
					fprintf(overhead, "%d\t%d\t%d\t%d\t%d\n",  m, i, j, tab_result.overhead, tab_result.overhead_pdl);
				};

			};
			//on écrit de nouveau ds le fichier la table de routage
			sprintf(requete, "select age, age_pdl from routage");
			if (mysql_query(mysql,requete)){
				printf("pb lors de la seléction des âges pour l'écriture ds le fichier\n");
				exit(1);
			};
			result = mysql_store_result(mysql);

			while ((row = mysql_fetch_row(result))){
				lengths = mysql_fetch_lengths(result);
				sprintf(age, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
				sprintf(age_pdl, "%.*s", (int) lengths[1], row[1] ? row[1] : "NULL");
				if (atoi(age) < 10000) fprintf(contenu_BDD, "%d\t%d\t%s\t%s\n",m, i, age, age_pdl);
				else{
					tab_result.nb_noeud_sans_info++;
					if (atoi(age_pdl) >= 10000)  tab_result.nb_noeud_sans_info_pdl++;
				};
			};
			mysql_free_result(result);
			fprintf(avec_info, "%d\t%d\t%ld\t%ld\n", m, i, nb_entree_tab_routage - ((long int) tab_result.nb_noeud_sans_info),nb_entree_tab_routage - ((long int) tab_result.nb_noeud_sans_info_pdl));


		};
//		fclose(chemin);

/*		//on enregistre les positions des noeuds à la fin de la simu 
		//pour montrer qui connait la destination
		chemin_37 = fopen("resultats/positions_noeuds_pdl_ok", "w");
		sprintf(requete, "select id from routage where id_noeud = 37 and age_pdl < 1000");
		if (mysql_query(mysql,requete)){
			printf("pb lors de la seléction des voisins qui ne connaissent pas 37\n");
			exit(1);
		};
		result = mysql_store_result(mysql);

		while ((row = mysql_fetch_row(result))){
			lengths = mysql_fetch_lengths(result);
			sprintf(id, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
			fprintf(chemin_37, "%d\t%d\t%d\n", atoi(id), tab_topologie[(atoi(id))-1]->posX,  tab_topologie[(atoi(id))-1]->posY);
		};
		mysql_free_result(result);
		fclose(chemin_37);

		chemin_37 = fopen("resultats/positions_noeuds_pdl_no", "w");
		sprintf(requete, "select id from routage where id_noeud = 37 and age_pdl > 1000");
		if (mysql_query(mysql,requete)){
			printf("pb lors de la seléction des voisins qui ne connaissent pas 37\n");
			exit(1);
		};
		result = mysql_store_result(mysql);

		while ((row = mysql_fetch_row(result))){
			lengths = mysql_fetch_lengths(result);
			sprintf(id, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
			fprintf(chemin_37, "%d\t%d\t%d\n", atoi(id), tab_topologie[(atoi(id))-1]->posX,  tab_topologie[(atoi(id))-1]->posY);
		};
		mysql_free_result(result);
		fclose(chemin_37);
		
		chemin_37 = fopen("resultats/positions_noeuds_ler_ok", "w");
		sprintf(requete, "select id from routage where id_noeud = 37 and age < 1000");
		if (mysql_query(mysql,requete)){
			printf("pb lors de la seléction des voisins qui ne connaissent pas 37\n");
			exit(1);
		};
		result = mysql_store_result(mysql);

		while ((row = mysql_fetch_row(result))){
			lengths = mysql_fetch_lengths(result);
			sprintf(id, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
			fprintf(chemin_37, "%d\t%d\t%d\n", atoi(id), tab_topologie[(atoi(id))-1]->posX,  tab_topologie[(atoi(id))-1]->posY);
		};
		mysql_free_result(result);
		fclose(chemin_37);
			
		chemin_37 = fopen("resultats/positions_noeuds_ler_no", "w");
		sprintf(requete, "select id from routage where id_noeud = 37 and age > 1000");
		if (mysql_query(mysql,requete)){
			printf("pb lors de la seléction des voisins qui ne connaissent pas 37\n");
			exit(1);
		};
		result = mysql_store_result(mysql);

		while ((row = mysql_fetch_row(result))){
			lengths = mysql_fetch_lengths(result);
			sprintf(id, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
			fprintf(chemin_37, "%d\t%d\t%d\n", atoi(id), tab_topologie[(atoi(id))-1]->posX,  tab_topologie[(atoi(id))-1]->posY);
		};
		mysql_free_result(result);
		fclose(chemin);*/
		
		//fprintf(fichier, "\nnombre de dest non trouvée par LER est %d et par PDL est %d\n", nb_dest_non_trouvee, nb_dest_non_trouvee_pdl);
		//fprintf(fichier, "nombre de messages pdl insérés %d sur %d messages échangés\n", tab_result.nb_info_pdl_inserees, duree_simu*nb_msg*taille_msg);

		//enregistrement des contenus des tables de routage
/*		fichier = fopen("resultats/contenu_tab_routage","w");
		if(mysql_query(mysql,"select id, id_noeud, age, age_pdl from routage")) 
			printf("impossible de sélectionner les âges des tables de routage\n");
		else printf("sélection des âges ds les tables de routage \n");

		result = mysql_store_result(mysql);
		while ((row = mysql_fetch_row(result))){
			//pour chq élément de result1 on...
			lengths = mysql_fetch_lengths(result);
			//...récupère sa position
			sprintf(Id, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
			sprintf(Id_noeud, "%.*s", (int) lengths[1], row[1] ? row[1] : "NULL");
			sprintf(age, "%.*s", (int) lengths[2], row[2] ? row[2] : "NULL");
			sprintf(age_pdl, "%.*s", (int) lengths[3], row[3] ? row[3] : "NULL");
			fprintf(fichier, "%d\t%d\t%d\t%d\n", atoi(Id), atoi(Id_noeud), atoi(age), atoi(age_pdl));
		};

		mysql_free_result(result);*/

		fclose(rayon_voisin);
		fclose(overhead);
				

		//liberation de la mémoire
		printf("liberation de la mémoire\n");
		lib_mem(topologie);

		//fermeture de la connexion à la BDD
		mysql_close(mysql);

		fclose(contenu_BDD);

		fclose(avec_info);
	};

	return 0;
}
