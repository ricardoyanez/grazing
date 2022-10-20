
# GRAZING
[GRAZING](http://personalpages.to.infn.it/~nanni/grazing/): Replacing NAG routines for free or open-source alternatives

## NAG Documentation

GRAZING uses the Fortran NAG Library Mark 18. NAG does not maintain the documentation of too old versions. Fortunately, the documentation for Mark 18 can be found in this [site](https://www1.udel.edu/nag/ohufl18pd/LibDoc.html).

## Environment variables

The data files are defined through environmental variables. This supercedes the file names defined in `grazing_file.icl`.

In `.bashrc`

```
GRAZ_DIR=[path to GRAZING data directory]  
GRAZ_MASS_EXP=$GRAZ_DIR/massexp_2004.dat  
GRAZ_MASS_NIX=$GRAZ_DIR/mtablex_2004.dat  
GRAZ_FILE_BE23=$GRAZ_DIR/be23.dat  
export GRAZ_MASS_EXP GRAZ_MASS_NIX GRAZ_FILE_BE23  
```

I personally put the data files in `/usr/local/share/grazing/`.

## Calls to `C05ADF`

C05ADF locates a zero of a continuous function in a given interval by a combination of the methods of linear interpolation, extrapolation and bisection.
```Fortran
SUBROUTINE C05ADF(A,B,EPS,ETA,F,X,IFAIL)
INTEGER IFAIL
real A,B,EPS,ETA,F,X
EXTERNAL F
```

This function is replaced by the [Numerical Recipes](#numerical-recipes-in-fortran-77) ZBRENT function,
```Fortran
FUNCTION ZBRENT(FUNC,X1,X2,TOL)

```
which uses Brent's method to find the root of function `FUNC` known to lie between `X1` and `X2`. The root is refined until its accuracy is `TOL`.

Necessary changes to ZBRENT are distributed as a patch (`zbrent.for.patch`).

## Calls to `D02BBF`

NAG Library Documentation for Mark 18 does not have any information about `D02BBF`. It appears `D02BBF` has been replaced since Mark 17. This [web page](http://fy.chalmers.se/~frtbm/NAG/nagdoc_mk21/html/genint/fl_replace.html) contains advice. It literally says,

**D02BAF**

Withdrawn at Mark 18.  
Replaced by [D02PCF](http://fy.chalmers.se/~frtbm/NAG/nagdoc_mk21/pdf/D02/d02pcf.pdf) and associated D02P routines.

Old:  
```Fortran
      CALL D02BBF(X,XEND,N,Y,TOL,IRELAB,FCN,OUTPUT,W,IFAIL)
```
New:  
```Fortran
     CALL D02PVF(N,X,Y,XEND,TOL,THRES,2,'usualtask',.FALSE.,
    +            0.0D0,W,20*N,IFAIL)
     ... set XWANT ...
  10 CONTINUE
     CALL D02PCF(FCN,XWANT,X,Y,YP,YMAX,W,IFAIL)
     IF (XWANT.LT.XEND) THEN
       ... reset XWANT ...
       GO TO 10
     ENDIF


```
[D02PVF](http://fy.chalmers.se/~frtbm/NAG/nagdoc_mk21/pdf/D02/d02pvf.pdf) is a setup routine for `D02PCF`.

`D02PCF` solves an initial value problem for a first-order system of ordinary differential equations using Runge-Kutta methods".

Parameters deduced from the replacement routines:

`X` is the initial value of the independent variable.  
`XEND` is the final value of the independent variable.  
`N` is the number of ODEs in the system.  
`Y` is the initial values of the solutions at the initial value.  
`TOL` is a relative error tolerance.  
`IRELAB` is always 0 when called from GRAZING. Its usage is unknown.  
`FCN` is a user supplied external function and must evaluate f<sub>i</sub>.  
`OUTPUT` is a user supplied external function.  
`W` is an array of dimension (4,7).  
`IFAIL` on entry must be -1, 0 or 1. On exit is equal to 0 unless there is an error.




## RKSUITE - a suite of Runde-Kutta codes

[RKSUITE](https://netlib.sandia.gov/ode/rksuite/) is a suite of Runde-Kutta codes that is available free of charge to the scientific community. It has no discernable license. This is perhaps intentional. It will therefore use it to replace the D02PEF NAG routine without distributing it. The Makefile will download it from the site and compile it for use with GRAZING. It any changes are needed, they will be distributed as a patch. The documentation is also downloaded.

## Numerical Recipes in FORTRAN 77

Whomever has purchased a copy of *Numerical Recipes in Fortran 77: The Art of Scientific Computing* by William Press, Brian Flannery, Saul Teukolsky and William Vetterling is entitled to use the machine readable programs for personal use. Since distributing a copy is explicitly forbidden, I will distribute patches when a Numerical Recipe code is used. I will assume any interested person in GRAZING has a copy of this excellent book.

The codes can be found in [GitHub](https://github.com/wangvei/nrf77).

## GNU Scientific Library (GSL)

In Debian, the [GSL library](https://www.gnu.org/software/gsl/) is distributed and installed through the development package `libgsl-dev`.

## changelog

This project uses debchange to create and manage the changelog,
```
$ debchange --create --newversion 0.0.1 --package grazing --changelog changelog  
$ debchange -a --changelog changelog
```

