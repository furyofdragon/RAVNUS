      SUBROUTINE DYDX(X,Y,U,A,B,C,D,G,N,IR,A1)
C.....*******************
C.....*????????? "RAVNUS" *
C.....*****06-JAN-92*****
      DIMENSION X(1),Y(1),U(1),A(1),B(1),C(1),D(1),
     *G(1),A1(N,1),IPUT(13),EQUIL(13)
      A(1)=0.
      B(1)=2.
      C(1)=1.
      D(1)=-3.*(Y(2)-Y(1))/(X(2)-X(1))
      A(N)=1.
      B(N)=2.
      C(N)=0.
      D(N)=-3.*(Y(N)-Y(N-1))/(X(N)-X(N-1))
      J=N-1
      DO 1 I=2,J
      H=X(I)-X(I-1)
      H1=X(I+1)-X(I)
      DY=Y(I)-Y(I-1)
      DY1=Y(I+1)-Y(I)
      A(I)=H
      B(I)=2.*(H+H1)
      C(I)=H1
1     D(I)=-3.*(H1/H*DY+H/H1*DY1)
      DO 2 I=1,N
      D(I)=D(I)*(-1)
      DO 2 J=1,N
      A1(I,J)=0.
      A1(I,I)=B(I)
      IF(I.NE.N) A1(I,I+1)=C(I)
      IF(I.NE.1) A1(I,I-1)=A(I)
2     CONTINUE
      CALL LADATF(A1,A1,N,N,0,D1,D2,IPUT,EQUIL,W,IER)
      CALL LAELMF(A1,D,IPUT,N,N,U)
C     WRITE(*,100) (U(I),I=1,N)
100   FORMAT(1X,11F6.3)
      RETURN
      END
