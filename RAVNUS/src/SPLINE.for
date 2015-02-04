      SUBROUTINE SPLINE(X,Y,U,N,A,B,C,D,G,KOD,IR)
C.....*********************
C.....*œ–Œ√–¿ÃÃ¿ "RAVNUS" *
C.....*****06-JAN-92*******
      DIMENSION X(1),Y(1),U(1),A(1),
     *          B(1),C(1),D(1),G(1),A1(13,13)
      IR=0
      IF(KOD.EQ.2) GO TO 2
      CALL DYDX(X,Y,U,A,B,C,D,G,N,IR,A1)
2     DO 3 I=2,N
      H=X(I)-X(I-1)
      A(I-1)=1./H/H*(U(I)+U(I-1)-2./H*(Y(I)-Y(I-1)))
      B(I-1)=3./H**3*((Y(I)-Y(I-1))*(X(I)+X(I-1)))-1./
     /       H/H*(U(I-1)*(X(I-1)+2.*X(I))+U(I)*(X(I)+
     +       2.*X(I-1)))
      C(I-1)=1./H/H*(-6./H*X(I-1)*X(I)*(Y(I)-Y(I-1))+
     +       U(I-1)*X(I)*(X(I)+2.*X(I-1))+U(I)*X(I-1)*
     *       (X(I-1)+2.*X(I)))
3     D(I-1)=1./H/H*(2./H*X(I-1)*X(I)*(X(I-1)*Y(I)-
     -       X(I)*Y(I-1))+Y(I-1)*X(I)*X(I)+Y(I)*
     *       X(I-1)**2-X(I-1)*X(I)*(U(I-1)*X(I)+U(I)*
     *       X(I-1)))
      RETURN
      END
