#include"lib_mem.h"

void lib_mem (noeud *topologie) {
	noeud *aux, *tmp;

	aux = topologie;
	while (aux){
		tmp = aux;
		aux = aux->next;
		free (tmp);
	};
};
