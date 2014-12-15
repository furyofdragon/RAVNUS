      SUBROUTINE  UDS
C.....*********************
C.....*èêéÉêÄååÄ "RAVNUS" *
C.....*****19-JAN-90*******
      EXTERNAL FI
      COMMON /A5/E,F,Y,Z,XG,DT,S1,GAM
     *       /A6/P,V,W,S,Q,DL,N,DK,D
      DIMENSION E(23),F(23),Y(23),
     *          P(23),V(23),W(23),S(23)
      DL=Z/20
      D=DL/2
      N=23
      DK=D*D
      I=0
    4 I=I+1
      IF (I-N) 6,6,8
    6 Y(I)=Y(I)*2
      GO TO 4
    8 P(1)=0.0
      I=1
   10 I=I+1
      IF (I-N) 12,12,14
   12 T=E(I-1)/GAM
      P(I)=P(I-1)+2*T
      GO TO 10
   14 V(1)=0.0
      W(1)=0.0
      I=1
   16 I=I+1
      IF (I-N) 18,18,20
   18 V(I)=V(I-1)+F(I-1)+F(I)
      GO TO 16
20    R=V(2)
      DO 181 I=2,N
      V(I)=V(I)-R
181   W(I)=P(I)-V(I)
      V(23)=V(23)-F(22)
      W(23)=W(23)+F(22)
      M=12
      I=0
   22 I=I+1
      IF (I-N) 24,24,26
   24 M=M-1
      V(I)=M*Y(I)
      GO TO 22
   26 C=-W(23)
      C1=(W(1)-C)/2
      I=0
   28 I=1+I
      IF (I-N) 30,30,32
   30 C1=C1-W(I)
      GO TO 28
   32 S(1)=0.0
      P(1)=0.0
      I=1
   34 I=I+1
      IF (I-N) 36,36,38
   36 S(I)=Y(I)+Y(I-1)+S(I-1)
      P(I)=P(I-1)+V(I)+V(I-1)
      GO TO 34
38    R=S(2)
      R1=P(2)
      DO 360 I=2,N
      S(I)=S(I)-R
360   P(I)=P(I)-R1
      S(23)=S(23)-Y(22)
      P(23)=P(23)-V(22)
      A=S(23)
      B=P(23)
      A1=(S(1)+A)/(-2)
      I=0
   39 I=I+1
      IF (I-N) 40,40,42
   40 A1=A1+S(I)
      GO TO 39
   42 B1=-0.5*(B+P(1))
      I=0
   44 I=I+1
      IF (I-N) 46,46,48
   46 B1=B1+P(I)
      GO TO 44
   48 B=B*DL
      B1=B1*DL
   50 H=A*B1-B*A1
   52 DT=(C*B1-B*C1)/H
      Q=(A*C1-C*A1)/H
      S1=Q*Z/2
      RETURN
      END
