
# GRAZING
Replacing NAG routines for free or open-source alternatives.

[GRAZING](http://personalpages.to.infn.it/~nanni/grazing/) calculates the outcome of collisions between two heavy nuclei using the [Grazing Model](http://dx.doi.org/10.1016/0375-9474(94)90430-8) of Aage Winther.

## INSTALLATION

1. Clone this GitHub repository,

   ```  
   $ git clone https://github.com/ricardoyanez/grazing.git  
2. Download the [GRAZING source](http://personalpages.to.infn.it/~nanni/grazing/grazing_jun2005.tar.gz) (full version).
3. Untar the source, enter the source directory and rename the original makefile,

   ```  
   $ tar -zxvf grazing_jun2005.tar.gz  
   $ cd grazing  
   $ mv makefile makefile.orig  
4. Put your personal copy of Numerical Recipes ZBRENT routine (`zbrent.f`) in directory `nrf77`,

   ```  
   $ cp -a [path-to-nummerical recipes]/zbrent.f nrf77/zbrent.f  
5. Compile the program,

   ```  
   $ make  
6. Define the environment were the GRAZING data is located. See section [About Data Files](#about-data-files).

## About Data Files

GRAZING comes with three data files, `massexp_2004.dat`, `mtablex_2004.dat` and `be23.dat`. The location of these files is defined in `grazing_file.icl` and must be changed for every compilation of GRAZING.

A better way is to define the location of the files via shell environment variables. This supercedes the file names defined in `grazing_file.icl`.

In `.bashrc`

```
GRAZ_DIR=$HOME/grazing  
GRAZ_MASS_EXP=$GRAZ_DIR/massexp_2004.dat  
GRAZ_MASS_NIX=$GRAZ_DIR/mtablex_2004.dat  
GRAZ_FILE_BE23=$GRAZ_DIR/be23.dat  
export GRAZ_MASS_EXP GRAZ_MASS_NIX GRAZ_FILE_BE23  
```

`GRAZ_DIR` defines the path to the directory where the data files are located, for example, `$HOME/grazing/` (user) or `/usr/local/share/grazing/` (system-wide).

Changes to GRAZING are distributed as a patches.

## NAG Routine Calls by GRAZING

#### Calls to `C05ADF`

`C05ADF` locates a zero of a continuous function in a given interval by a combination of the methods of linear interpolation, extrapolation and bisection.
```Fortran
SUBROUTINE C05ADF(A,B,EPS,ETA,F,X,IFAIL)
INTEGER IFAIL
real A,B,EPS,ETA,F,X
EXTERNAL F
```

The `C05ADF` routine is replaced by the [Numerical Recipes](#numerical-recipes-in-fortran-77) ZBRENT function,
```Fortran
FUNCTION ZBRENT(FUNC,X1,X2,TOL)

```
which uses Brent's method to find the root of function `FUNC` known to lie between `X1` and `X2`. The root is refined until its accuracy is `TOL`.

#### Calls to `D01AMF`

`D01AMF` calculates an approximation to the integral of a function f(x) over an infinite or semi-infinite interval.

```Fortran
SUBROUTINE D01AMF(F,BOUND,INF,EPSABS,EPSREL,RESULT,ABSERR,W,LW,IW,LIW,IFAIL)
INTEGER INF, LW, IW(LIW), LIW, IFAIL
real F, BOUND, EPSABS, EPSREL, RESULT, ABSERR, W(LW)
EXTERNAL F
```
The `D01AMF` routine is replaced by [QUADPACK](#quadpack---numerical-integration) routine `DQAGI`.

#### Calls to `D01ASF`

`D01ASF` calculates an approximation to the sine or the cosine transform of a function g over [a, ∞) for a user-specified value of ω.

```Fortran
 SUBROUTINE D01ASF(G,A,OMEGA,KEY,EPSABS,RESULT,ABSERR,
1 LIMLST,LST,ERLST,RSLST,IERLST,W,LW,IW,LIW,IFAIL)
 INTEGER KEY, LIMLST, LST, IERLST(LIMLST), LW, IW(LIW),
1 LIW, IFAIL
 real G, A, OMEGA, EPSABS, RESULT, ABSERR,
1 ERLST(LIMLST), RSLST(LIMLST), W(LW)
 EXTERNAL G
```
The `D01ASF` routine is replaced by [QUADPACK](#quadpack---numerical-integration) routine `QAWFE`.

#### Calls to `D02BBF`

`D02PCF` solves an initial value problem for a first-order system of ordinary differential equations using Runge-Kutta methods.

```Fortran
   CALL D02BBF(X,XEND,N,Y,TOL,IRELAB,FCN,OUTPUT,W,IFAIL)
```

The `D02BBF` routine is replaced by [RKSUITE](#rksuite---a-suite-of-runde-kutta-codes).

#### Calls to `E01BEF`

`E01BEF` computes a monotonicity-preserving piecewise cubic Hermite interpolant to a set of data points.

```Fortran
SUBROUTINE E01BEF(N,X,F,D,IFAIL)
INTEGER N, IFAIL
real X(N), F(N), D(N)
```

The `E01BEF` routine is replaced by [PCHIP](#pchip) routine DPCHIM.

#### Calls to `S14ABF`

`S14ABF` returns a value for the logarithm of the Gamma function, ln Γ(x), via the routine name.

```Fortran
REAL FUNCTION S14ABF(X, IFAIL)
INTEGER IFAIL
REAL X
```

The `S14ABF` routine is substituted with a C wrapper function that calls `lgamma()`.

#### Calls to `S15ADF`

`S15ADF` returns the value of the complementary error function, erfc x, via the routine name.

```Fortran
REAL FUNCTION S15ADF(X,IFAIL)
INTEGER IFAIL
REAL X
```

The `S15ADF` routine is substituted with a C wrapper function that calls `erfc()`.

#### Calls to `S18DEF`

`S18DEF` returns a sequence of values for the modified Bessel functions I<sub>ν+n</sub>(z) for complex z, non-negative
ν and n = 0, 1, . . . , N − 1, with an option for exponential scaling.

```Fortran
SUBROUTINE S18DEF(FNU,Z,N,SCALE,CY,NZ,IFAIL)
INTEGER N, NZ, IFAIL
real FNU
complex Z, CY(N)
CHARACTER∗1 SCALE
```

The `S18DEF` routine is replaced by the modified Bessel function of [Amos](#amos---bessel-functions).

## Numerical Recipes in FORTRAN 77

Whomever has purchased a copy of *Numerical Recipes in Fortran 77: The Art of Scientific Computing* by William Press, Brian Flannery, Saul Teukolsky and William Vetterling is entitled to use the machine readable programs for personal use. Since distributing a copy is explicitly forbidden, the Numerical Recipes source codes will have to be placed in the `nrf77` directory. It will be assumed that any interested person in GRAZING has a personal copy of this excellent book. Necessary changes to the original codes are distributed as patches.

The codes can be found in [GitHub](https://github.com/wangvei/nrf77). Please refer to GitHub regarding any license issues.

## RKSUITE - a suite of Runde-Kutta codes

[RKSUITE](https://netlib.sandia.gov/ode/rksuite/) is a suite of Runde-Kutta codes that is available free of charge to the scientific community. It has no discernable license. The Makefile will download the suite from the forementioned site and compile it for use with GRAZING. If any changes are needed, they will be distributed as a patch (`rksuite.f.patch`).

## QUADPACK - numerical integration

[QUADPACK](https://nines.cs.kuleuven.be/software/QUADPACK/) is a set of Fortran 77 routines for integrating one-variable functions. The authors are Robert Piessens, Elise deDoncker-Kapenga, Christian Überhuber and David Kahaner. QUADPACK is licensed as Public Domain and its source code can be downloaded from [Netlib](https://netlib.org/quadpack/). The Makefile will download the necessary codes from Netlib and compile them for use with GRAZING.

<ins>REFERENCE</ins>

R. Piessens, E. De Doncker-Kapenga and C. W. Überhuber. QUADPACK: a subroutine package for automatic integration. Springer, ISBN: 3-540-12553-1. 1983. 

## Amos - Bessel Functions

The Amos routines for evaluating Bessel functions are distributed by [Netlib](https://netlib.org/amos/). Necessary changes are distributed as a patch.

<ins>REFERENCE</ins>

Amos, D. E. (1986) Algorithm 644: A portable package for Bessel functions of a complex argument and nonnegative order. *ACM Trans. Math. Software 12 265–273*.

## PCHIP

The PCHIP routines are part of the Slatec Library. They can be found in [Netlib](https://netlib.org/slatec/pchip/).

<ins>REFERENCE</ins>

Fritsch F. N. (1982) PCHIP final specifications Report UCID–30194 Lawrence Livermore National Laboratory

## NAG Documentation

GRAZING uses the Fortran NAG Library Mark 18. NAG does not maintain the documentation of too old versions. Fortunately, the documentation for Mark 18 can be found in this [site](https://www1.udel.edu/nag/ohufl18pd/LibDoc.html).

## changelog

This project uses debchange to create and manage the changelog,
```
$ debchange --create --newversion 0.0.1 --package grazing --changelog changelog  
$ debchange -a --changelog changelog
```

## License

The wrappers to substitute the NAG libraries in GRAZING were written by Ricardo Yanez &lt;ricardo.yanez@calel.org&gt; and licensed under the [GNU General Public License (GPL) version 2](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html) or later.
