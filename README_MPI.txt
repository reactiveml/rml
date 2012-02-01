******** Installation de la version distribuée de RML ********

Prérequis:
- ocaml >= 3.12
- mpich2

Installation:
  - construction du compilateur rpmlc :
         cd compiler/
         ocamlbuild rpmlc.byte

  - construction de rpmldep :
         cd compiler/
         ocamlbuild rpmldep.byte

  - construction de la bibliothèque standard:
       * dans le fichier mpi/myocamlbuild_config.ml, mettre a jour la variable source_dir
       * construction:
           cd lib/
           ocamlbuild stdlib.otarget

  - construction de la bibliothèque MPI:
     * dans le fichier mpi/myocamlbuild.ml, mettre les bonnes vaeurs dans mpi_libdir et mpi_include, et changer le chemin dans mlmpi_libdir au chemin courant.
      * construction:
          cd mpi/
          ocamlbuild all.otarget

  - construction de rpmllib.cma:
       * dans le fichier mpi/myocamlbuild_config.ml, mettre a jour la variable source_dir
       * construction:
            cd interpreter/
             ocamlbuild rpmllib.cma


********  Construction des exemples **********************

Prérequis:
  - SDL (libsdl_gfx, libsdl_ttf)
  - ocamlsdl >= 0.8.0

Note for Macports users:
The ocamlsdl port has a bug. You need to change the file
'/opt/local/var/macports/sources/svn.macports.org/trunk/dports/devel/ocamlsdl/Portfile' (or
rsync... depending on your configuration) at line 20 from:

#patchfiles              patch-configure.diff patch-src-Makefile.diff

to:

patchfiles              patch-configure.diff

This should be done before building the package.


Construction:
   - construction de rmlsdl:
       * dans le fichier examples/rmlsdl/myocamlbuild.ml, mettre a jour sdl_dir.
       * construction:
            cd examples/rmlsdl
            ocamlbuild rmlsdl.cma mlsdl_server


  - construction de galaxy:
     * construction:
            cd examples/galaxy_mpi
            ocamlbuild planets_adapt.rml.byte

     * pour lancer l'exemple:
           mpiexec -n 1 ../rmlsdl/mlsdl_server.byte : -n 2 ./planets_adapt.rml.byte -min-rank 1
           -load-balancer robin
   On peut utiliser les load balancer suivants : local (tout dans le mem processus), robin (round
           robin), all_remote (tous les domaines d'horloges sont mis en distant)

  - construction de collision:
     * construction:
            cd examples/collision
            ocamlbuild collision.rml.byte
