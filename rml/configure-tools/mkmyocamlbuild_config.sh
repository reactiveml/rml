#!/bin/sh

#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#       Nicolas Pouillard, projet Gallium, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 2007 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id: mkmyocamlbuild_config.sh 10443 2010-05-20 09:44:25Z doligez $

sed -e "s@^PREFIX=.*@PREFIX=$1@" \
    -e "s@^LIBDIR=.*@LIBDIR=$2@" \
    -e "s@STUBLIBDIR=.*@STUBLIBDIR=$3/stublibs@" \
    $3/Makefile.config \
    > rmlbuild.config

sed \
    -e 's/^.*FLEXDIR.*$//g' \
    -e 's/^#ml \(.*\)/\1/' \
    -e 's/^\([^"][^"]*\("[^"]*"[^"]*\)*\)#.*$/\1/' \
    -e 's/^\(#.*\)$/(* \1 *)/' \
    -e 's/^\(.*\$([0-9]).*\)$/(* \1 *)/' \
    -e 's/^\([^(=]*\)=\([^"]*\)$/let <:lower<\1>> = "\2";;/' \
    -e 's/\$(AS)/as/g' \
    -e 's/\$(\([^)]*\))/"\^<:lower<\1>>\^"/g' \
    -e 's/""\^//g' \
    -e 's/\^""//g' \
    -e 's/^let <:lower<MAKE.*$//g' \
    -e 's/^let <:lower<DO.*$//g' \
    -e 's/"true"/true/g' \
    -e 's/"false"/false/g' \
    rmlbuild.config \
    | sed -f tolower.sed \
    | sed -f tolower.sed \
    | sed -f tolower.sed \
    | sed -f tolower.sed \
    | sed -f tolower.sed \
    | sed -f tolower.sed \
    > ../tools/rmlbuild/rmlbuild/ocamlbuild_Myocamlbuild_config.ml
