# grazing

Replacing NAG routines for GSL functions

Using debchange to create and manage the changelog,

$ debchange --create --newversion 0.0.1 --package grazing --changelog changelog

$ debchange -a --changelog changelog

## GNU Scientific Library (GSL)

In Debian, the GSL library is distributed and installed through the development package libgsl-dev.

## Environment variables

The data files are defined through environmental variables. This supercedes the file names defined in grazing_file.icl.

In .bashrc,

```
GRAZ_DIR=[path to GRAZING data directory]  
GRAZ_MASS_EXP=$GRAZ_DIR/massexp_2004.dat  
GRAZ_MASS_NIX=$GRAZ_DIR/mtablex_2004.dat  
GRAZ_FILE_BE23=$GRAZ_DIR/be23.dat  
export GRAZ_MASS_EXP GRAZ_MASS_NIX GRAZ_FILE_BE23  
```

I personally put the data files in /usr/local/share/grazing/

## Calls to D02BBF

NAG Library Documetation for Mark 18 does not have any information about D02BBF. It appears D02BBF has been replaced since Mark 17. This [web page](https://wwwafs.portici.enea.it/software/libnag/nagdoc_fl24/html/GENINT/replace.html) contains advice. It literally says,

**D02BAF**

Withdrawn at Mark 18.  
Replaced by D02PEF and associated D02P routines.

Old:  
```Fortran
CALL D02BBF(X,XEND,N,Y,TOL,IRELAB,FCN,OUTPUT,W,IFAIL)
```
New:  
```Fortran THRES(1:N) = TOL
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
