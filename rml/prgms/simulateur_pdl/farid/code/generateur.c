#include "generateur.h"

int gener_alea(int taille) {
	//return ((int) (taille * (double) rand() / ((double) RAND_MAX + 1))+1);
	return ((int) (taille * (double) rand() / ((double) RAND_MAX + 1)));
};
