#include "impression_topo.h"

void impr_topo(MYSQL *mysql, int taille, int nb_noeud){
	int i,j;
	MYSQL_ROW row;
	MYSQL_RES *result;
	char requete[256];
	char id[10];
	unsigned long *lengths;

	for (j=taille;j>=1;j--){
		for (i=1;i<=taille;i++){
			sprintf(requete, "select Id from noeuds where posX ='%d' and posY ='%d' and id <=%d", i, j, nb_noeud);
			if (mysql_query(mysql,requete)){
				printf("pb de sélection d'age\n");
				exit(1);
			};
			result = mysql_store_result(mysql);
			if ((row = mysql_fetch_row(result))){
				lengths = mysql_fetch_lengths(result);
				sprintf(id, "%.*s", (int) lengths[0], row[0] ? row[0] : "NULL");
				printf("%3d", atoi(id));
			}
			else{
				printf("...");
			};
			mysql_free_result(result);
		};
		printf(" %3d\n", j);
	};
	for (i=1;i<=taille;i++)	printf("%3d", i);
	printf("\n");
};
