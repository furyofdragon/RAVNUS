      SUBROUTINE LAELMF(A,B,IPVT,N,IA,X)
      DIMENSION A(IA,1),B(1),IPVT(1),X(1)
C1    DOUBLE PRECISION A,B,X,SUM
C     êÖòÖçàÖ LY=B Ñãü Y
      DO 5 I=1,N
5     X(I)=B(I)
      IW=0
      DO 20 I=1,N
      IP=IPVT(I)
      SUM=X(IP)
      X(IP)=X(I)
      IF(IW.EQ.0) GO TO 15
      IM1=I-1
      DO 10 J=IW,IM1
      SUM=SUM-A(I,J)*X(J)
10    CONTINUE
      GO TO 20
15    IF(SUM.NE.0) IW=I
20    X(I)=SUM
C     êÖòÖçàÖ UX=Y Ñãü X
      DO 30 IB=1,N
      I=N+1-IB
      IP1=I+1
      SUM=X(I)
      IF(IP1.GT.N) GO TO 30
      DO 25 J=IP1,N
      SUM=SUM-A(I,J)*X(J)
25    CONTINUE
30    X(I)=SUM/A(I,I)
      RETURN
      END
