
# GRAZING
[GRAZING](http://personalpages.to.infn.it/~nanni/grazing/) calculates the outcome of collisions between two heavy nuclei using the [Grazing Model](http://dx.doi.org/10.1016/0375-9474(94)90430-8) of Aage Winther.

Replacing NAG routines for free or open-source alternatives.

## Installation

1. Clone repository, enter source directory and compile,

   ```  
   $ git clone https://github.com/ricardoyanez/grazing.git  
   $ cd grazing  
   $ make  
2. Edit `~/.bashrc` and add,
   ```  
   GRAZ_DIR=$HOME/grazing  
   GRAZ_MASS_EXP=$GRAZ_DIR/massexp_2004.dat  
   GRAZ_MASS_NIX=$GRAZ_DIR/mtablex_2004.dat  
   GRAZ_FILE_BE23=$GRAZ_DIR/be23.dat  
   export GRAZ_MASS_EXP GRAZ_MASS_NIX GRAZ_FILE_BE23
   ```  
   and source
   ```  
   $ source ~/.bashrc  
See section [About Data Files](#about-data-files) for more information about this step.

## About Data Files

GRAZING comes with three data files, `massexp_2004.dat`, `mtablex_2004.dat` and `be23.dat`. The location of these files is defined in `grazing_file.icl` and must be changed for every compilation of GRAZING.

A better way is to define the location of the files via shell environment variables. This supercedes the file names defined in `grazing_file.icl`.

In `.bashrc` add,

```
GRAZ_DIR=$HOME/grazing  
GRAZ_MASS_EXP=$GRAZ_DIR/massexp_2004.dat  
GRAZ_MASS_NIX=$GRAZ_DIR/mtablex_2004.dat  
GRAZ_FILE_BE23=$GRAZ_DIR/be23.dat  
export GRAZ_MASS_EXP GRAZ_MASS_NIX GRAZ_FILE_BE23  
```

`GRAZ_DIR` defines the path to the directory where the data files are located, for example, `$HOME/grazing/` (user) or `/usr/local/share/grazing/` (system-wide).

These changes to GRAZING are distributed as a patches.

## NAG Routine Calls by GRAZING

GRAZING uses the Fortran NAG Library Mark 18. NAG does not maintain the documentation of older versions. Fortunately, the documentation for Mark 18 can be found in this [site](https://www1.udel.edu/nag/ohufl18pd/LibDoc.html).

### C05ADF

Locates a zero of a continuous function in a given interval by a combination of the methods of linear interpolation, extrapolation and bisection.

```Fortran
SUBROUTINE C05ADF(A,B,EPS,ETA,F,X,IFAIL)
INTEGER IFAIL
REAL A,B,EPS,ETA,F,X
EXTERNAL F
```

The **C05ADF** routine is replaced by the [Numerical Recipes](#numerical-recipes-in-fortran-77) **ZBRENT** function,

```Fortran
FUNCTION ZBRENT(FUNC,X1,X2,TOL)
```

which uses Brent's method to find the root of function `FUNC` known to lie between `X1` and `X2`. The root is refined until its accuracy is `TOL`.

### D01AMF

Calculates an approximation to the integral of a function f(x) over an infinite or semi-infinite interval.

```Fortran
SUBROUTINE D01AMF(F,BOUND,INF,EPSABS,EPSREL,RESULT,ABSERR,W,LW,IW,LIW,IFAIL)
INTEGER INF, LW, IW(LIW), LIW, IFAIL
REAL F, BOUND, EPSABS, EPSREL, RESULT, ABSERR, W(LW)
EXTERNAL F
```

The **D01AMF** routine is replaced by [QUADPACK](#quadpack---numerical-integration) routine **DQAGI**.

### D01ASF

Calculates an approximation to the sine or the cosine transform of a function g over [a, ∞) for a user-specified value of ω.

```Fortran
 SUBROUTINE D01ASF(G,A,OMEGA,KEY,EPSABS,RESULT,ABSERR,
1 LIMLST,LST,ERLST,RSLST,IERLST,W,LW,IW,LIW,IFAIL)
 INTEGER KEY, LIMLST, LST, IERLST(LIMLST), LW, IW(LIW), LIW, IFAIL
 REAL G, A, OMEGA, EPSABS, RESULT, ABSERR, ERLST(LIMLST), RSLST(LIMLST), W(LW)
 EXTERNAL G
```
The **D01ASF** routine is replaced by [QUADPACK](#quadpack---numerical-integration) routine **QAWFE**.

### D01GAF

Integrates a function which is specified numerically at four or more points, over the whole of its specified range, using third-order finite-difference formulae with error estimates, according to a method due to Gill and Miller.

```Fortran
SUBROUTINE D01GAF(X,Y,N,ANS,ER,IFAIL)
INTEGER N, IFAIL
REAL X(N), Y(N), ANS, ER
```

The **D01GAF** routine is replaced by the [Gill-Miller Algorithm](#gill-miller-algorithm) routine **FOURPT**.

```FORTRAN
SUBROUTINE FOURPT(X,Y,N,ANS,ER,IFAIL)
INTEGER N,IFAIL
DOUBLE PRECISION X(N),Y(N),ANS,ER
```

### D02BBF

Solves an initial value problem for a first-order system of ordinary differential equations using Runge-Kutta methods.

```Fortran
CALL D02BBF(X,XEND,N,Y,TOL,IRELAB,FCN,OUTPUT,W,IFAIL)
```

The **D02BBF** routine is replaced by [RKSUITE](#rksuite---a-suite-of-runde-kutta-codes).

### E01BEF

Computes a monotonicity-preserving piecewise cubic Hermite interpolant to a set of data points.

```Fortran
SUBROUTINE E01BEF(N,X,F,D,IFAIL)
INTEGER N, IFAIL
REAL X(N), F(N), D(N)
```

The **E01BEF** routine is replaced by [PCHIP](#pchip) routine **DPCHIM**.

```Fortran
SUBROUTINE DPCHIM (N,X,F,D,INCFD,IERR)
INTEGER  N, IERR
DOUBLE PRECISION  X(N), F(INCFD,N), D(INCFD,N)
```

### E01BFF

Evaluates a piecewise cubic Hermite interpolant at a set of points.

```Fortran
SUBROUTINE E01BFF(N,X,F,D,M,PX,PF,IFAIL)
INTEGER N, M, IFAIL
REAL X(N), F(N), D(N), PX(M), PF(M)
```

The **E01BFF** routine is replaced by [PCHIP](#pchip) routine **DPCHFE**.

```Fortran
SUBROUTINE DPCHFE(N,X,F,D,INCFD,SKIP,NE,XE,FE,IERR)
INTEGER N, NE, IERR
DOUBLE PRECISION X(N), F(INCFD,N), D(INCFD,N), XE(NE), FE(NE)
LOGICAL SKIP
```

### E01BGF

Evaluates a piecewise cubic Hermite interpolant and its first derivative at a set of points.

```Fortran
SUBROUTINE E01BGF(N,X,F,D,M,PX,PF,PD,IFAIL)
INTEGER N, M, IFAIL
REAL X(N), F(N), D(N), PX(M), PF(M), PD(M)
```

The **E01BGF** routine is replaced by [PCHIP](#pchip) routine **DPCHFD**.

```Fortran
SUBROUTINE DPCHFD(N,X,F,D,INCFD,SKIP,NE,XE,FE,DE,IERR)
INTEGER N, NE, IERR
DOUBLE PRECISION X(N), F(INCFD,N), D(INCFD,N), XE(NE), FE(NE), DE(NE)
LOGICAL SKIP
```

### S14AAF

S14AAF returns the value of the Gamma function Γ(x), via the routine name.

```Fortran
REAL FUNCTION S14AAF(X, IFAIL)
INTEGER IFAIL
REAL X
```

The **S14AAF** routine is substituted with a C wrapper function that calls **tgamma()**.

### S14ABF

Returns a value for the logarithm of the Gamma function, ln Γ(x), via the routine name.

```Fortran
REAL FUNCTION S14ABF(X, IFAIL)
INTEGER IFAIL
REAL X
```

The **S14ABF** routine is substituted with a C wrapper function that calls **lgamma()**.

### S15ADF

Returns the value of the complementary error function, erfc x, via the routine name.

```Fortran
REAL FUNCTION S15ADF(X,IFAIL)
INTEGER IFAIL
REAL X
```

The **S15ADF** routine is substituted with a C wrapper function that calls **erfc()**.

### S18AEF

Returns the value of the modified Bessel Function I0(x), via the routine name.

```Fortran
REAL FUNCTION S18AEF(X, IFAIL)
INTEGER IFAIL
REAL X
```

The **S18AEF** routine is replaced by the [Numerical Recipes](#numerical-recipes-in-fortran-77) **BESSI0** function,

```Fortran
FUNCTION BESSI0(X)
REAL X
```

### S18AFF

Returns the value of the modified Bessel Function I1(x), via the routine name.

```Fortran
REAL FUNCTION S18AFF(X, IFAIL)
INTEGER IFAIL
REAL X
```

The **S18AFF** routine is replaced by the [Numerical Recipes](#numerical-recipes-in-fortran-77) **BESSI1** function,

```Fortran
FUNCTION BESSI1(X)
REAL X
```

### S18DEF

Returns a sequence of values for the modified Bessel functions I<sub>ν+n</sub>(z) for complex z, non-negative
ν and n = 0, 1, . . . , N − 1, with an option for exponential scaling.

```Fortran
SUBROUTINE S18DEF(FNU,Z,N,SCALE,CY,NZ,IFAIL)
INTEGER N, NZ, IFAIL
REAL FNU
COMPLEX Z, CY(N)
CHARACTER∗1 SCALE
```

The **S18DEF** routine is replaced by the modified Bessel function of [Amos](#amos---bessel-functions) routine **ZBESI**.

### X05BAF

Returns the amount of processor time used since an unspecified previous time, via the routine name.

```Fortran
REAL FUNCTION X05BAF()
```

The **X05BAF** routine is substituted with a C wrapper function that calls **clock()**.

## Numerical Recipes in FORTRAN 77

Whomever has purchased a copy of *Numerical Recipes in Fortran 77: The Art of Scientific Computing* by William Press, Brian Flannery, Saul Teukolsky and William Vetterling, is entitled to use the machine readable programs for personal use. Since distributing a copy is explicitly forbidden, the Numerical Recipes source codes will have to be placed in the `nrf77` directory. It will be assumed that any interested person in GRAZING has a personal copy of this excellent book. Any changes to the original codes are distributed as patches.

The codes can be found in [GitHub](https://github.com/wangvei/nrf77). Please refer to GitHub regarding any license issues.

## RKSUITE - a suite of Runde-Kutta codes

[RKSUITE](https://netlib.sandia.gov/ode/rksuite/) is a suite of Runde-Kutta codes that is available free of charge to the scientific community. It has no discernable license. The Makefile will download the suite from the forementioned site and compile it for use with GRAZING. Any changes to the original codes are distributed as patches.

## QUADPACK - numerical integration

[QUADPACK](https://nines.cs.kuleuven.be/software/QUADPACK/) is a set of Fortran 77 routines for integrating one-variable functions. The authors are Robert Piessens, Elise deDoncker-Kapenga, Christian Überhuber and David Kahaner. QUADPACK is licensed as Public Domain and its source code can be downloaded from [Netlib](https://netlib.org/quadpack/). The Makefile will download the necessary codes from Netlib and compile them for use with GRAZING.

<ins>REFERENCE</ins>

R. Piessens, E. De Doncker-Kapenga and C. W. Überhuber. QUADPACK: a subroutine package for automatic integration. Springer, ISBN: 3-540-12553-1. 1983. 

## Amos - Bessel Functions

The Amos routines for evaluating Bessel functions are distributed by [Netlib](https://netlib.org/amos/). Any changes to the original codes are distributed as patches.

<ins>REFERENCE</ins>

Amos, D. E. (1986) Algorithm 644: A portable package for Bessel functions of a complex argument and nonnegative order. *ACM Trans. Math. Software 12 265–273*.

## PCHIP

The PCHIP routines are part of the Slatec Library. They can be found in [Netlib](https://netlib.org/slatec/pchip/).

<ins>REFERENCE</ins>

Fritsch F. N. (1982) PCHIP final specifications Report UCID–30194 Lawrence Livermore National Laboratory.

## Gill-Miller Algorithm

The routine **FOURPT** is a FORTRAN port of the procedure by Gill and Miller written in ALGOL.

<ins>REFERENCE</ins>

Gill P. E. and Miller G. F. (1972) An algorithm for the integration of unequally spaced data, *Comput. J.* 15 80–83.

## changelog

This project uses debchange to create and manage the changelog,
```
$ debchange --create --newversion 0.0.1 --package grazing --changelog changelog  
$ debchange -a --changelog changelog
```

## Bug Reporting

Please report bugs to Ricardo Yanez &lt;ricardo.yanez@calel.org&gt;.

## License

The wrappers to substitute the NAG libraries in GRAZING were written by Ricardo Yanez &lt;ricardo.yanez@calel.org&gt; and licensed under the [GNU General Public License (GPL) version 2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html) or later.
