
# GRAZING - NAG Routine Replacements

The [GRAZING code](http://personalpages.to.infn.it/~nanni/grazing/) is based on the macroscopic model of grazing collisions proposed by [Aage Winther](http://dx.doi.org/10.1016/0375-9474(94)90430-8). Classical motion of colliding nuclei is combined with excitation of collective states and single-particle transfers along trajectories near and around the point of closest approach. The grazing model describes fairly well the mass and charge distributions, the angular and kinetic energy distributions of reaction products resulting from inelastic reactions in which a few nucleons are transferred. The GRAZING code is written in Fortran and uses the [NAG Library](https://www.nag.com/content/nag-library/) for various numerical calculations of the model.

This project is about replacing the NAG routines used by GRAZING for free or open-source alternatives.

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
   and source,
   ```  
   $ source ~/.bashrc  
See section [About Data Files](#about-data-files) for more information about this step.

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

The **C05ADF** routine is replaced by the [Numerical Recipes](#numerical-recipes-in-fortran-77) **ZBRENT** function.

### C05AVF

Attempts to locate an interval containing a simple zero of a continuous function using a binary search. It uses reverse communication for evaluating the function.

```Fortran
SUBROUTINE C05AVF(X,FX,H,BOUNDL,BOUNDU,Y,C,IND,IFAIL)
INTEGER IND,IFAIL
REAL X,FX,H,BOUNDL,BOUNDU,Y,C(11)
```

### D01AMF

Calculates an approximation to the integral of a function f(x) over an infinite or semi-infinite interval.

```Fortran
SUBROUTINE D01AMF(F,BOUND,INF,EPSABS,EPSREL,RESULT,ABSERR,W,LW,IW,LIW,IFAIL)
INTEGER INF,LW,IW(LIW),LIW,IFAIL
REAL F,BOUND,EPSABS,EPSREL,RESULT,ABSERR,W(LW)
EXTERNAL F
```

The **D01AMF** routine is replaced by [QUADPACK](#quadpack---numerical-integration) routine **DQAGI**.

### D01ASF

Calculates an approximation to the sine or the cosine transform of a function g over [a, ???) for a user-specified value of ??.

```Fortran
 SUBROUTINE D01ASF(G,A,OMEGA,KEY,EPSABS,RESULT,ABSERR,
1 LIMLST,LST,ERLST,RSLST,IERLST,W,LW,IW,LIW,IFAIL)
 INTEGER KEY,LIMLST,LST,IERLST(LIMLST),LW,IW(LIW),LIW,IFAIL
 REAL G,A,OMEGA,EPSABS,RESULT,ABSERR,ERLST(LIMLST),RSLST(LIMLST),W(LW)
 EXTERNAL G
```
The **D01ASF** routine is replaced by [QUADPACK](#quadpack---numerical-integration) routine **QAWFE**.

### D01GAF

Integrates a function which is specified numerically at four or more points, over the whole of its specified range, using third-order finite-difference formulae with error estimates, according to a method due to Gill and Miller.

```Fortran
SUBROUTINE D01GAF(X,Y,N,ANS,ER,IFAIL)
INTEGER N,IFAIL
REAL X(N),Y(N),ANS,ER
```

The **D01GAF** routine is replaced by the [Gill-Miller Algorithm](#gill-miller-algorithm) routine **FOURPT**.

```Fortran
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
INTEGER N,IFAIL
REAL X(N),F(N),D(N)
```

The **E01BEF** routine is replaced by the [PCHIP](#pchip---piecewise-cubic-hermite-interpolation-package) routine **DPCHIM**.

### E01BFF

Evaluates a piecewise cubic Hermite interpolant at a set of points.

```Fortran
SUBROUTINE E01BFF(N,X,F,D,M,PX,PF,IFAIL)
INTEGER N,M,IFAIL
REAL X(N),F(N),D(N),PX(M),PF(M)
```

The **E01BFF** routine is replaced by then [PCHIP](#pchip---piecewise-cubic-hermite-interpolation-package) routine **DPCHFE**.

### E01BGF

Evaluates a piecewise cubic Hermite interpolant and its first derivative at a set of points.

```Fortran
SUBROUTINE E01BGF(N,X,F,D,M,PX,PF,PD,IFAIL)
INTEGER N,M,IFAIL
REAL X(N),F(N),D(N),PX(M),PF(M),PD(M)
```

The **E01BGF** routine is replaced by the [PCHIP](#pchip---piecewise-cubic-hermite-interpolation-package) routine **DPCHFD**.

### S14AAF

S14AAF returns the value of the Gamma function ??(x), via the routine name.

```Fortran
REAL FUNCTION S14AAF(X,IFAIL)
INTEGER IFAIL
REAL X
```

The **S14AAF** routine is substituted with a C wrapper function that calls **tgamma()**.

### S14ABF

Returns a value for the logarithm of the Gamma function, ln ??(x), via the routine name.

```Fortran
REAL FUNCTION S14ABF(X,IFAIL)
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

The **S18AEF** routine is replaced by the [Numerical Recipes](#numerical-recipes-in-fortran-77) **BESSI0** function.

### S18AFF

Returns the value of the modified Bessel Function I1(x), via the routine name.

```Fortran
REAL FUNCTION S18AFF(X, IFAIL)
INTEGER IFAIL
REAL X
```

The **S18AFF** routine is replaced by the [Numerical Recipes](#numerical-recipes-in-fortran-77) **BESSI1** function.

### S18DEF

Returns a sequence of values for the modified Bessel functions I<sub>??+n</sub>(z) for complex z, non-negative
?? and n = 0, 1, . . . , N ??? 1, with an option for exponential scaling.

```Fortran
SUBROUTINE S18DEF(FNU,Z,N,SCALE,CY,NZ,IFAIL)
INTEGER N,NZ,IFAIL
REAL FNU
COMPLEX Z,CY(N)
CHARACTER???1 SCALE
```

The **S18DEF** routine is replaced by the modified Bessel function of [AMOS](#amos---bessel-functions) routine **ZBESI**.

### X05BAF

Returns the amount of processor time used since an unspecified previous time, via the routine name.

```Fortran
REAL FUNCTION X05BAF()
```

The **X05BAF** routine is substituted with a C wrapper function that calls **clock()**.

## AMOS - Bessel Functions

The AMOS routines for evaluating Bessel functions are distributed by [Netlib](https://netlib.org/amos/).

<ins>REFERENCE</ins>

Amos, D. E. (1986) Algorithm 644: A portable package for Bessel functions of a complex argument and nonnegative order. *ACM Trans. Math. Software 12 265???273*.

## Gill-Miller Algorithm

The routine **FOURPT** is a Fortran port of the procedure by Gill and Miller written in ALGOL.

<ins>REFERENCE</ins>

Gill P. E. and Miller G. F. (1972) An algorithm for the integration of unequally spaced data, *Comput. J.* 15 80???83.

## Numerical Recipes in Fortran 77

Whomever has purchased a copy of *Numerical Recipes in Fortran 77: The Art of Scientific Computing* by William Press, Brian Flannery, Saul Teukolsky and William Vetterling, is entitled to use the machine readable programs for personal use. Since distributing a copy is explicitly forbidden, the Numerical Recipes source codes will have to be placed in the `nrf77` directory. It will be assumed that any interested person in GRAZING has a personal copy of this excellent book.

## PCHIP - Piecewise Cubic Hermite Interpolation Package

The PCHIP routines are part of the Slatec Library. They can be found in [Netlib](https://netlib.org/slatec/pchip/).

<ins>REFERENCE</ins>

Fritsch F. N. (1982) PCHIP final specifications Report UCID???30194 Lawrence Livermore National Laboratory.

## QUADPACK - numerical integration

[QUADPACK](https://nines.cs.kuleuven.be/software/QUADPACK/) is a set of Fortran 77 routines for integrating one-variable functions. QUADPACK is licensed as Public Domain and its source code can be downloaded from [Netlib](https://netlib.org/quadpack/).

<ins>REFERENCE</ins>

R. Piessens, E. De Doncker-Kapenga and C. W. ??berhuber. QUADPACK: a subroutine package for automatic integration. Springer, ISBN: 3-540-12553-1. 1983. 

## RKSUITE - a suite of Runde-Kutta codes

[RKSUITE](https://netlib.sandia.gov/ode/rksuite/) is a suite of Runde-Kutta codes that is available free of charge to the scientific community. It has no discernable license.

## About Data Files

GRAZING comes with three data files, `massexp_2004.dat`, `mtablex_2004.dat` and `be23.dat`. The location of these files is defined in `grazing_file.icl` and must be changed for every compilation of GRAZING. To avoid this, the location of the files are instead defined via shell environment variables. This supersedes the file names defined in `grazing_file.icl`.

In `.bashrc` add,

```
GRAZ_DIR=$HOME/grazing  
GRAZ_MASS_EXP=$GRAZ_DIR/massexp_2004.dat  
GRAZ_MASS_NIX=$GRAZ_DIR/mtablex_2004.dat  
GRAZ_FILE_BE23=$GRAZ_DIR/be23.dat  
export GRAZ_MASS_EXP GRAZ_MASS_NIX GRAZ_FILE_BE23  
```

`GRAZ_DIR` defines the path to the directory where the data files are located, for example, `$HOME/grazing/` (user) or `/usr/local/share/grazing/` (system-wide).

## Bug Reporting

Please report bugs to Ricardo Yanez &lt;ricardo.yanez@calel.org&gt;.

## License

The routines and wrappers to substitute the NAG routines in GRAZING were written by Ricardo Yanez &lt;ricardo.yanez@calel.org&gt; and licensed under the [GNU General Public License (GPL) version 2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html) or later.
