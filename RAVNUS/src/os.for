      SUBROUTINE OS(X,Y,J,
     1JOC,A,B,C,D)
C.....*********************
C.....*èêéÉêÄååÄ "RAVNUS" *
C.....*****06-JAN-92*******
      DIMENSION X(13),Y(13),U(14),
     1A(13),B(13),C(13),D(13),G(14),
     1X1(13),Y1(13),A1(13),B1(13),C1(13),D1(13)
      DO 1 I=1,JOC
      X1(I)=X(I)
1     Y1(I)=Y(I)
      CALL SPLINE(X1,Y1,U,JOC,A1,B1,C1,D1,G,0,IR)
      I1=JOC-1
      DO 2 I=1,I1
      A(I)=A1(I)
      B(I)=B1(I)
      C(I)=C1(I)
2     D(I)=D1(I)
      I1=J-JOC
      DO 3 I=1,I1
      X1(I)=X(JOC+I)
3     Y1(I)=Y(JOC+I)
      CALL SPLINE(X1,Y1,U,I1,A1,B1,C1,D1,G,0,IR)
      DO 4 I=1,I1
      I2=I+JOC
      A(I2)=A1(I)
      B(I2)=B1(I)
      C(I2)=C1(I)
4     D(I2)=D1(I)
      RETURN
      END
