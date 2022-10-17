# grazing

Replacing NAG routines for GSL functions

Using debchange to create and manage the changelog,

$ debchange --create --newversion 0.0.1 --package grazing --changelog changelog

$ debchange -a --changelog changelog

GNU Scientific Library (GSL)
----------------------------

In Debian, the GSL library is distributed and installed through the development package libgsl-dev.

Environment variables
---------------------

The data files are defined through environmental variables. This supercedes the file names defined in grazing_file.icl.

In .bashrc,

GRAZ_DIR=[path to GRAZING data directory]

GRAZ_MASS_EXP=$GRAZ_DIR/massexp_2004.dat

GRAZ_MASS_NIX=$GRAZ_DIR/mtablex_2004.dat

GRAZ_FILE_BE23=$GRAZ_DIR/be23.dat

export GRAZ_MASS_EXP GRAZ_MASS_NIX GRAZ_FILE_BE23

I personally put the data files in /usr/local/share/grazing/
