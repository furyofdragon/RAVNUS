       SUBROUTINE SP0P(RES,T,EL,ISP,ZY,KT,TS,SM)
C.....*********************
C.....*œ–Œ√–¿ÃÃ¿ "RAVNUS" *
C.....*****06-JAN-92*******
        COMMON /PL/V(11)/KF/DELTA,ALFA,BETA
        COMMON/TIP/KS/SPL/VT(13),S(13),X1(13),X2(13),
     *R(13)/T/J,TT(13)
      DIMENSION ZY(999),TS(100),SM(100)
      CHARACTER*20 RES
      OPEN(UNIT=6,FILE=RES,STATUS='OLD')
        EL2=EL/2.
1       CALL STATIK(T,EL,ZY,KT,TS,SM)
        IF(V(4).GT.1.E-6)GOTO10
      IF(KS.EQ.2)GO TO 6
        IF(ISP.NE.0) WRITE(6,4)T,V(1),V(4)
        RETURN
6     TT(J)=T
      VT(J)=V(1)
      S(J)=V(4)
      X1(J)=0.
      X2(J)=0.
      R(J)=0.
      RETURN
10      XF=V(5)/V(4)-EL2
        FI=V(7)-V(4)*(EL2+XF)**2
        IF(T.LT.1.E-3)GOTO2
        XC=V(2)/V(1)-EL2
        ZC=V(3)/V(1)
        RX=V(6)/V(1)
        RF=FI/V(1)
        GOTO3
2       XC=XF
        ZC=0.
      IF(KS.EQ.2)GO TO 7
      IF(ISP.NE.0)
     *       WRITE(6,40)T,V(1),V(4),XF,XC,ZC,ALFA,ALFA
      RETURN
7     TT(J)=T
      VT(J)=V(1)
      S(J)=V(4)
      X1(J)=XC
      X2(J)=XF
      R(J)=1.E6
      RETURN
3     IF(KS.EQ.2)GO TO 5
      IF(ISP.NE.0)
     *                 WRITE(6,4)T,V(1),V(4),XF,XC,ZC,
     *                           RX,RF,DELTA,ALFA,BETA
      RETURN
5     TT(J)=T
      VT(J)=V(1)
      S(J)=V(4)
      X1(J)=XC
      X2(J)=XF
      R(J)=RF
      RETURN
40    FORMAT(F9.3,F11.1,F10.1,F11.2,2F11.2,31X,3F8.3)
4     FORMAT(F9.3,F11.1,F10.1,F11.2,2F11.2,
     *       F14.2,F13.1,4X,3F8.3)
      END
