      SUBROUTINE SPLN(AF,BF,CF,DF,ST)
C.....*********************
C.....*èêéÉêÄååÄ "RAVNUS" *
C.....*****06-JAN-92*******
C     COMMON /BG/ST(21,13)
      COMMON /T/J,T(13)/T1/JOC
     *       /SPL/V(13),S(13),XC(13),XF(13),R(13)
     *       /V/AV(13),BV(13),CV(13),DV(13)
     *       /YY/AS(13),BS(13),CS(13),DS(13)
     *       /YC/AXC(13),BXC(13),CXC(13),DXC(13)
     *       /YF/AXF(13),BXF(13),CXF(13),DXF(13)
     *       /P/AR(13),BR(13),CR(13),DR(13)
C     COMMON /H/AF(21,13),BF(21,13),CF(21,13),DF(21,13)
      DIMENSION AF(21,13),BF(21,13),CF(21,13),DF(21,13)
      DIMENSION ST(21,13)
      DIMENSION A(13),B(13),C(13),
     *          D(13),F(13),U(14),G(14)
C     PRINT 10,J,JOC,T,V,S,XC,XF,R
      IF(JOC) 4,4,5
4     CALL SPLINE(V,T,U,J,AV,BV,CV,DV,G,0,I)
      CALL SPLINE(T,S,U,J,AS,BS,CS,DS,G,0,I)
      CALL SPLINE(T,XC,U,J,AXC,BXC,CXC,DXC,G,0,I)
      CALL SPLINE(T,XF,U,J,AXF,BXF,CXF,DXF,G,0,I)
      CALL SPLINE(T,R,U,J,AR,BR,CR,DR,G,0,I)
      GO TO 9
5     CALL OS(V,T,J,JOC,AV,BV,CV,DV)
      CALL OS(T,S,J,JOC,AS,BS,CS,DS)
      CALL OS(T,XC,J,JOC,AXC,BXC,CXC,DXC)
      CALL OS(T,XF,J,JOC,AXF,BXF,CXF,DXF)
      CALL OS(T,R,J,JOC,AR,BR,CR,DR)
9     DO 1 I=1,21
      DO 2 K=1,J
2     F(K)=ST(I,K)
      IF(JOC)6,6,7
7     CALL OS(T,F,J,JOC,A,B,C,D)
      GO TO 8
6     CALL SPLINE(T,F,U,J,A,B,C,D,G,0,IR)
8     DO 3 M=1,J
      AF(I,M)=A(M)
      BF(I,M)=B(M)
      CF(I,M)=C(M)
3     DF(I,M)=D(M)
1     CONTINUE
C     PRINT 10,J,JOC,AV,BV,CV,DV
10    FORMAT(2I2,6G10.3/(8G10.3))
      RETURN
      END
