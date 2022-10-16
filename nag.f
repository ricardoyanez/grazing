      subroutine C05ADF(A,B,EPS,ETA,F,X,FAIL)
      write(*,*) '*** call to C05ADF'
      return
      end

      subroutine C05AVF(X,FX,H,BOUNDL,BOUNDU,Y,C,IND,IFAIL)
      write(*,*) '*** call to C05AVF'
      return
      end

      subroutine D02BBF(X,XEND,N,Y,TOL,IRELAB,FCN,OUTPUT,W,IFAIL)
      write(*,*) '*** call to D02BBF'
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
     1     LIMLST,LST,ERLST,RSLST,IERLST,W,LW,IW,LIW,IFAIL)
      write(*,*) '*** call to D01ASF'
      return
      end

      subroutine D01AMF(F,BOUND,INF,EPSABS,EPSREL,RESULT,ABSERR,
     1     W,LW,IW,LIW,IFAIL)
      write(*,*) '*** call to D01AMF'
      return
      end

      subroutine D01GAF(X,Y,N,ANS,ER,IFAIL)
      write(*,*) '*** call to D01GAF'
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

C     NAG X05BAF returns the amount of processor time used since an unspecified
C     previous time, via the routine name.
      real*8 function X05BAF()
      real*8 elaps
      write(*,*) '*** call to X05BAF'
      call nag_X05BAF(elaps)
      X05BAF = elaps
      return
      end
