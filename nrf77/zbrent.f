      REAL*8 FUNCTION ZBRENT(FUNC,X1,X2,TOL,IFAIL)
      IMPLICIT REAL*8(A-H,O-Z)
      EXTERNAL FUNC
      PARAMETER (ITMAX=100,EPS=3.D-8)
      A=X1
      B=X2
      FA=FUNC(A)
      FB=FUNC(B)
c$$$      IF(FB*FA.GT.0.) PAUSE 'Root must be bracketed for ZBRENT.'
      IF(FB*FA.GT.0.) THEN
        IFAIL=1
        RETURN
      ENDIF
      FC=FB
      DO 11 ITER=1,ITMAX
        IF(FB*FC.GT.0.) THEN
          C=A
          FC=FA
          D=B-A
          E=D
        ENDIF
        IF(DABS(FC).LT.DABS(FB)) THEN
          A=B
          B=C
          C=A
          FA=FB
          FB=FC
          FC=FA
        ENDIF
        TOL1=2.*EPS*DABS(B)+0.5*TOL
        XM=.5*(C-B)
        IF(DABS(XM).LE.TOL1 .OR. FB.EQ.0.)THEN
          ZBRENT=B
          IFAIL=0
          RETURN
        ENDIF
        IF(DABS(E).GE.TOL1 .AND. DABS(FA).GT.DABS(FB)) THEN
          S=FB/FA
          IF(A.EQ.C) THEN
            P=2.*XM*S
            Q=1.-S
          ELSE
            Q=FA/FC
            R=FB/FC
            P=S*(2.*XM*Q*(Q-R)-(B-A)*(R-1.))
            Q=(Q-1.)*(R-1.)*(S-1.)
          ENDIF
          IF(P.GT.0.) Q=-Q
          P=DABS(P)
          IF(2.*P .LT. MIN(3.*XM*Q-DABS(TOL1*Q),DABS(E*Q))) THEN
            E=D
            D=P/Q
          ELSE
            D=XM
            E=D
          ENDIF
        ELSE
          D=XM
          E=D
        ENDIF
        A=B
        FA=FB
        IF(DABS(D) .GT. TOL1) THEN
          B=B+D
        ELSE
          B=B+SIGN(TOL1,XM)
        ENDIF
        FB=FUNC(B)
11    CONTINUE
c$$$      PAUSE 'ZBRENT exceeding maximum iterations.'
      WRITE(*,*)'WARNING: ZBRENT exceeding maximum iterations.',B,FB
      IFAIL=0
      ZBRENT=B
      RETURN
      END
