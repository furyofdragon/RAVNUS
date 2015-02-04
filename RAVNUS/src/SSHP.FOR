      SUBROUTINE SSHP(T,S,ZY)
C       COMMON /FORMA/ZY(999)
        DIMENSION ZY(999)
        DIMENSION S(21),V(4)
C     TYPE *,ZY
        J=0
        SC=100.
        I=1
    2 R=ZY(I)-SC
        IF(R.GE.20.1)GOTO11
        IF(R+0.01)13,3,3
3       R1=ZY(I+1)-SC
        IF(R1.GE.0.)GOTO10
        IF(AMOD(R,1.).LT..00001)GOTO130
        I=I+1
        GOTO13
130     I=I+1
        S1=0.
    4 IF(ZY(I+2)-SC)5,30,30
    5 V(1)=ZY(I)
        V(2)=ZY(I+1)
        V(3)=ZY(I+2)
        V(4)=ZY(I+3)
        DZ=V(3)-V(1)
        ZN=SIGN(1.,DZ)
   15 DZT=T-V(1)
        K=2
        IF(DZ.LE.0.)K=0
        IF((T-V(3))*DZT)7,17,6
    6 IF(DZT)9,9,8
   17 IF(T-V(3))9,77,14
   77 IF(DZ)7,88,7
   88 YWL=V(4)
        GOTO16
    7 YWL=DZT*(V(4)-V(2))/DZ+V(2)
   16 V(K+1)=T
        V(K+2)=YWL
   14 DZ=V(3)-V(1)
    8 S1=(V(2)+V(4))*DZ/2.+S1
    9 I=I+2
        GOTO4
30      N=INT(R+.01)
        N1=N
        S(N+1)=S1*2.
   13 I=I+1
        GOTO 2
10      N=INT(R+0.01)
        N0=N1+1
        DO 33 J=N0,N
33      S(J+1)=S1*2.
        I=I+1
        GOTO2
11	RETURN
        END
