#include "generateur.h"

int gener_alea(int taille) {

	int fd;
	int alea;
	/* On essaie d'ouvrir l'un des deux fichiers UNIX... */
	/* qui "génèrent" des octets pseudo-aléatoires */
	if (((fd = open("/dev/urandom", O_RDONLY)) < 0) &&
			(fd = open("/dev/random", O_RDONLY) < 0))
	{
		/* Si échec, on génère de façon "traditionnelle" */
		alea = ((rand() ^ getpid()) << 5);
	}
	else {
		if (read(fd, &alea, sizeof(alea)) < 0) {
			alea = (rand() ^ getpid()) << 5;
		}
		else {
			alea = (unsigned int) alea % taille;
		}
		close(fd);
	}
	return alea;
};
