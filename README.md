# grazing

Replacing NAG routines for free or open-source alternatives

## changelog

This project uses debchange to create and manage the changelog,
```
$ debchange --create --newversion 0.0.1 --package grazing --changelog changelog  
$ debchange -a --changelog changelog
```

## GNU Scientific Library (GSL)

In Debian, the [GSL library](https://www.gnu.org/software/gsl/) is distributed and installed through the development package `libgsl-dev`.

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

## NAG Documentation

GRAZING uses the Fortran NAG Library Mark 18. NAG does not maintain the documentation of too old versions. Fortunately, the documentation for Mark 18 can be found in this [site](https://www1.udel.edu/nag/ohufl18pd/LibDoc.html)

## Calls to `C05ADF`

C05ADF locates a zero of a continuous function in a given interval by a combination of the methods of linear interpolation, extrapolation and bisection.
```Fortran
SUBROUTINE C05ADF(A, B, EPS, ETA, F, X, IFAIL)
INTEGER IFAIL
real A, B, EPS, ETA, F, X
EXTERNAL F
```

## Calls to `D02BBF`

NAG Library Documetation for Mark 18 does not have any information about `D02BBF`. It appears `D02BBF` has been replaced since Mark 17. This [web page](https://wwwafs.portici.enea.it/software/libnag/nagdoc_fl24/html/GENINT/replace.html) contains advice. It literally says,

**D02BAF**

Withdrawn at Mark 18.  
Replaced by D02PEF and associated D02P routines.

Old:  
```Fortran
CALL D02BBF(X,XEND,N,Y,TOL,IRELAB,FCN,OUTPUT,W,IFAIL)
```
New:  
```Fortran
THRES(1:N) = TOL
CALL D02PQF(N,X,XEND,Y,TOL,THRESH,  &
            -2,0.0D0,IWSAV,RWSAV,IFAIL)
CALL D02PEF(F2,N,XEND,X,Y,YP,YMAX,  &
            IUSER,RUSER,IWSAV,RWSAV,IFAIL)
```
`IWSAV` is an integer array of length 130 and `RWSAV` is a real array of length 350+32×`N`.

`IUSER` and `RUSER` are arrays available to allow you to pass information to the user defined routine `F2`.

The definition of `F2` can use the original routine `FCN` as follows:  
```Fortran
   SUBROUTINE F2(T,N,Y,YP,IUSER,RUSER)
!     .. Scalar Arguments ..
      Real (Kind=wp), Intent (In)      :: t
      Integer, Intent (In)             :: n
!     .. Array Arguments ..
      Real (Kind=wp), Intent (Inout)   :: ruser(1)
      Real (Kind=wp), Intent (In)      :: y(n)
      Real (Kind=wp), Intent (Out)     :: yp(n)
      Integer, Intent (Inout)          :: iuser(1)
!     .. Procedure Arguments ..
      External                         :: fcn
!     .. Executable Statements ..
      Continue

      Call fcn(t,y,yp)

      Return
    End Subroutine F2
```

The replacements `D02PQF` and `D02PEF` appeared starting from [Mark 24](https://www.nag.com/numeric/nl/nagdoc_24/nagdoc_fl24/html/d02/d02conts.html). `D02PQF` is a setup routine for `D02PEF`.

The documentation, Mark 24, says, "`D02PEF` solves an initial value problem for a first-order system of ordinary differential equations using Runge-Kutta methods."

## RKSUITE - a suite of Runde-Kutta codes

[RKSUITE](https://netlib.sandia.gov/ode/rksuite/) is a suite of Runde-Kutta codes that is available free of charge to the scientific community. It has no discernable license. This is perhaps intentional. It will therefore use it to replace the D02PEF NAG routine without distributing it. The Makefile will download it from the site and compile it for use with GRAZING. It any changes are needed, they will be distributed as a patch. The documentation is also downloaded.





