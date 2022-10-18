C
C     Copyright (c) 2022 Ricardo Yanez <ricardo.yanez@calel.org>
C
C     C wrapper to the NAG Fortran Library
C
C
C     This program is free software; you can redistribute it and/or modify
C     it under the terms of the GNU General Public License as published by
C     the Free Software Foundation; either version 2 of the License, or
C     (at your option) any later version.
C
C     This program is distributed in the hope that it will be useful,
C     but WITHOUT ANY WARRANTY; without even the implied warranty of
C     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C     GNU General Public License for more details.
C
C     You should have received a copy of the GNU General Public License
C     along with this program; if not, write to the Free Software
C     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
C
C
C     NAG X05BAF returns the amount of processor time used since an unspecified
C     previous time, via the routine name.
C
C     Calls the C wrapper function _X05BAF(double *elaps)
C
      real*8 function X05BAF()
      real*8 elaps
C      write(*,*) '*** call to X05BAF'
      call _X05BAF(elaps)
      X05BAF = elaps
      return
      end

      subroutine C05ADF(A,B,EPS,ETA,F,X,FAIL)
      write(*,*) '*** call to C05ADF'
      return
      end

      subroutine C05AVF(X,FX,H,BOUNDL,BOUNDU,Y,C,IND,IFAIL)
      write(*,*) '*** call to C05AVF'
      return
      end

      subroutine E01BEF(N,X,F,D,IFAIL)
      write(*,*) '*** call to E01BEF'
      return
      end

      subroutine E01BFF(N,X,F,D,M,PX,PF,IFAIL)
      write(*,*) '*** call to E01BFF'
      return
      end
 
      subroutine E01BGF(N,X,F,D,M,PX,PF,PD,IFAIL)
      write(*,*) '*** call to E01BGF'
      return
      end

      subroutine D01ASF(G,A,OMEGA,KEY,EPSABS,RESULT,ABSERR,
     &     LIMLST,LST,ERLST,RSLST,IERLST,W,LW,IW,LIW,IFAIL)
      write(*,*) '*** call to D01ASF'
      return
      end

      subroutine D01AMF(F,BOUND,INF,EPSABS,EPSREL,RESULT,ABSERR,
     &     W,LW,IW,LIW,IFAIL)
      write(*,*) '*** call to D01AMF'
      return
      end

      subroutine D01GAF(X,Y,N,ANS,ER,IFAIL)
      write(*,*) '*** call to D01GAF'
      return
      end

C     D02PQF is a setup routine which must be called prior to the first call
C     of either of the integration routines D02PEF and D02PFF.
      subroutine D02PQF(N,TSTART,TEND,YINIT,TOL,THRESH,METHOD,HSTART,
     &     IWSAV,RWSAV,IFAIL)
      write(*,*) '*** call to D02PQF'
      return
      end

C     D02PEF solves an initial value problem for a first-order system of
C     ordinary differential equations using Runge–Kutta methods.
      subroutine D02PEF (F,N,TWANT,TGOT,YGOT,YPGOT,YMAX,IUSER,RUSER,
     &     IWSAV,RWSAV,IFAIL)
      write(*,*) '*** call to D02PEF'
      return
      end


      subroutine S18DEF(FNU,Z,N,SCALE,CY,NZ,IFAIL)
      write(*,*) '*** call to S18DEF'
      return
      end
      real function S14AAF(X,IFAIL)
      write(*,*) '*** call to S14AaF'
      return
      end
      
      real function S14ABF(X,IFAIL)
      write(*,*) '*** call to S14ABF'
      return
      end

      real function S15ADF(X,IFAIL)
      write(*,*) '*** call to S15ADF'
      return
      end

      real function S18AEF(X,IFAIL)
      write(*,*) '*** call to S18AEF'
      return
      end

      real function S18AFF(X, IFAIL)
      write(*,*) '*** call to S18AFF'
      return
      end
