        SUBROUTINE STATIK(T,EL,ZY,KT,TS,SM)
C     COMMON /FORMA/ZY(999)
      COMMON /PL/V(11)/BAT/B(50)/KF/DELTA,ALFA,BETA
C     COMMON /POV/KT,TS(100),SM(100)
      DIMENSION ZY(999),TS(100),SM(100)
      T2=1.E-6
      BWL=-1.E8
        ELWL=0.
        J=0
        TJ=T
        DL0=EL/20.
        SC=100.
        DO 1 I=1,7
1       V(I)=0.
      SP=0
        X=EL
        I=1
        R1=0.
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
      SP1=0.
4       IF(ZY(I+2)-SC.GT.-10.)GOTO10
5       V(8)=ZY(I)
        V(9)=ZY(I+1)
        V(10)=ZY(I+2)
        V(11)=ZY(I+3)
        DZ=V(10)-V(8)
15      DZT=TJ-V(8)
        K=2
        IF(DZ.LE.0.)K=0
        IF((TJ-V(10))*DZT)7,17,6
6       IF(DZT)9,9,8
17      IF(TJ-V(10))9,77,14
77    IF(DZT*(TJ-ZY(I+4)).GT.0.)GO TO 9
        IF(DZ)7,88,7
88      YWL=V(11)
        GOTO16
7       YWL=DZT*(V(11)-V(9))/DZ+V(9)
16      V(K+8)=TJ
        V(K+9)=YWL
        ZN=SIGN(1.,DZ)
        V4=YWL*ZN+V4
        V6=YWL**3/3.*ZN+V6
14      DZ=V(10)-V(8)
8       V1=(V(9)+V(11))*DZ/2.+V1
        V3=((V(8)+V(10)*2.)*V(11)+(V(8)*2.+V(10))*V(9))*DZ/6.+V3
      SP1=SQRT((V(9)-V(11))**2+DZ*DZ)+SP1
9       I=I+2
        GOTO4
10      IF(J.EQ.1)GOTO31
        T2=TJ-B(J)
        T1=TJ-B(J-1)
        IF(T2*T1)20,210,21
210     IF(T1)20,21,20
20      DX=DL/(T2-T1)*T2
        ELWL=ELWL+(X+DX)*SIGN(1.,-T1)
        IF(T2.LE.0.)DX=DL-DX
        DDL=DX/DL0
        X=X+DX/2.
        IF(T2.LT.0.)X=X+DL-DX
        GOTO32
21      DDL=DL/DL0
        X=X+DL/2.
        IF(T2.GE.0.AND.J.EQ.2)ELWL=X+DL/2.
32      V(1)=(V1+V1K)/2.*DDL+V(1)
      SP=(SPK+SP1)/2.*DDL+SP
        V(2)=(V1+V1K)/2.*DDL*X+V(2)
        V(3)=(V3+V3K)/2.*DDL+V(3)
        V44K=(V4+V4K)/2.
      BWL=AMAX1(BWL,V4)
      IF(V4.LT.1.E-6)GO TO 320
        IF(.NOT.(R.GE.10..AND.R1.LT.10.))GOTO320
        BETA=V1/V4/TJ
320   V44K=(V4+V4K)/2.
      IF(V44K.LT.1.E-6)GOTO31
      X=X-DL/2.+DL/3.*(2*V4K+V4)/(V4K+V4)
        V(4)=V44K*DDL+V(4)
        V(5)=V44K*DDL*X+V(5)
        V(6)=(V6+V6K)/2.*DDL+V(6)
      IF(V44K.LT.1.E-6)GO TO 31
        V(7)=V44K*X*X*DDL+V(7)
     *+DDL**3*DL0*DL0*(V4K*V4+4.*V4K*V4+V4*V4)/V44K/72.
31      V1K=V1
        T1=T2
        X=X1
        R1=R
        V3K=V3
        V6K=V6
      SPK=SP1
30    V4K=V4
13      I=I+2
        GOTO2
11      DO 12 K=1,7
12      V(K)=V(K)*DL0*2.
      SP=SP*DL0*2.
        ALFA=V(4)/BWL/ELWL/2.
        DELTA=V(1)/BWL/ELWL/TJ/2.
      IF(KT.GE.100)     RETURN
      KT=KT+1
      TS(KT)=TJ
      SM(KT)=SP
        RETURN
        END
