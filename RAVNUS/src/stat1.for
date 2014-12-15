        SUBROUTINE STAT1(T,EL,TET,ZY)
C.....*********************
C.....*èêéÉêÄååÄ "RAVNUS" *
C.....*****06-JAN-92*****
C       COMMON /FORMA/ZY(999)
        COMMON /ST/V(12)/BAT/B(50)
        DIMENSION ZY(999)
        DIMENSION VT(12)
        TE=TET/57.296
        SN=SIN(TE)
        CS=COS(TE)
        TJ=T
        DL0=EL/20.
        SC=100.
        DO 33 I=1,12
33      VT(I)=0.
        DO 23 IS=1,2
        J=0
        ZNS=SIGN(1.,SN)
        IF(ABS(SN).LT.1.E-6)ZNS=0.
        DO 1 I=1,7
1       V(I)=0.
        V(12)=0.
        X=EL
        R1=0.
        I=1
2       R=ZY(I)-SC
        IF(R.GE.100.)GOTO11
        IF(R.LT.-10.)GOTO13
        I=I+1
        J=J+1
        DL=DL0*(R-R1)
        X=X-DL
        X1=X
        IF(ZY(I).LT.100.)GOTO3
        I=I-2
        GOTO21
3       V1=0.
        V3=0.
        V4=0.
        V6=0.
        V12=0.
4       IF(ZY(I+2)-SC.GT.-10.)GOTO10
5       V(8)=ZY(I)*CS-ZY(I+1)*SN
        V(9)=ZY(I+1)*CS+ZY(I)*SN
        V(10)=ZY(I+2)*CS-ZY(I+3)*SN
        V(11)=ZY(I+3)*CS+ZY(I+2)*SN
        DZ=V(10)-V(8)
        ZN=SIGN(1.,DZ)
15      DZT=TJ-V(8)
        K=2
        IF(DZ.LT.0.)K=0
        IF((TJ-V(10))*DZT)7,17,6
6       IF(DZT)9,9,8
17      IF(TJ-V(10))9,77,14
77      IF(DZ)7,88,7
88      YWL=V(11)
        GOTO16
7       YWL=DZT*(V(11)-V(9))/DZ+V(9)
16      V(K+8)=TJ
        V(K+9)=YWL
        V4=YWL*ZN+V4
        V6=YWL*YWL/2.*ZN*ZNS+V6
14      DZ=V(10)-V(8)
8       V1=(V(9)+V(11))*DZ/2.+V1
        V3=((V(8)+V(10)*2.)*V(11)+(V(8)*2.+V(10))*
     *     V(9))*DZ/6.+V3
        V12=(V(9)*V(9)+V(9)*V(11)+V(11)*V(11))*DZ/6.*
     *      ZNS+V12
9       I=I+2
        GOTO4
10      IF(J.EQ.1)GOTO31
        T2=TJ-B(J)*CS
        T1=TJ-B(J-1)*CS
        IF(T1*T2)20,21,21
20      DX=DL/(T2-T1)*T2
        IF(T2.LT.0.)DX=DL-DX
        DDL=DX/DL
        X=X+DX/2.
        IF(T2.LT.0.)X=X+DL-DX/2.
        GOTO32
21      DDL=DL/DL0
        X=X+DL/2.
32      V(1)=(V1+V1K)/2.*DDL+V(1)
        V(2)=(V1+V1K)/2.*DDL*X+V(2)
        V(3)=(V3+V3K)/2.*DDL+V(3)
        V(12)=(V12+V12K)/2.*DDL+V(12)
        V44K=(V4+V4K)/2.
        V(4)=V44K*DDL+V(4)
        V(5)=V44K*DDL*X+V(5)
        V(6)=(V6+V6K)/2.*DDL+V(6)
        V(7)=V44K*X*X*DDL+V(7)
31      V1K=V1
        X=X1
        R1=R
        V3K=V3
        V6K=V6
        V12K=V12
30      V4K=V4
13      I=I+2
        GOTO2
11      DO 12 K=1,7
12      V(K)=V(K)*DL0*1.
        V(12)=V(12)*DL0
        DO 34 I=1,12
34      VT(I)=VT(I)+V(I)
23      SN=-SN
        DO 35 I=1,12
35      V(I)=VT(I)
        RETURN
        END
