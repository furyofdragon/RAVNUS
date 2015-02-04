      SUBROUTINE RAWYS(RES,TK,TN,IS)
C.....*********************
C.....*èêéÉêÄååÄ "RAVNUS" *
C.....*****06-JAN-92*******
      COMMON /A5/E,F,Y,Z,XG,DT,SS,GAM
     *       /A6/P,V,W,S,Q,DL,N,DK,D
      DIMENSION E(23),F(23),Y(23),P(23),
     *          V(23),W(23),S(23),V1(23),S1(23)
      CHARACTER*20 RES
      SS=Q*DL
      RS1=0.
C      GAM=1.
      G=2.1E8
      I=0
   64 I=I+1
      IF (I-N) 66,66,68
   66 S(I)=S(I)*DT
      V(I)=P(I)*SS
      P(I)=W(I)+S(I)+V(I)
      GO TO 64
   68 S(1)=0.0
      I=1
   70 I=I+1
      IF (I-N) 72,72,74
   72 S(I)=S(I-1)+P(I-1)+P(I)
      GO TO 70
   74 I=0
   76 I=I+1
      IF (I-N) 78,78,80
   78 V(I)=D*P(I)*9.81*GAM
      S(I)=DK*S(I)*9.81*GAM
      V1(I)=V(I)
      S1(I)=S(I)
      GO TO 76
   80 CONTINUE
      XG=XG*G
      DO 81 I=1,23
      I1=I-1
      V1(I)=V1(I)-V1(23)*I1/22.
      V(I)=V(I)-V(23)*I1/22.
      S1(I)=S1(I)-S1(23)*I1/22.
  81  S(I)=S(I)-S(23)*I1/22.
  340 CALL INTN(S,P)
      CALL INTN(P,W)
      DO 13 I=2,22
13    W(I)=(W(I)-(I-2)/20.*FI(P))/XG*DL*DL
      A11=FI(Y)
      DO 15 I=2,22
   15 P(I)=((I-2)/20.-0.5)*Y(I)
      A12=FI(P)
      DO 17 I=2,22
   17 P(I)=P(I)*(I-2)/20.*Z
      A22=FI(P)
      DO 19 I=2,22
   19 P(I)=Y(I)*W(I)
      B1=FI(P)*(-1.)
      DO 21 I=2,22
   21 P(I)=P(I)*(I-2)/20.*Z
      B2=FI(P)*(-1.)
      DO 23 I=2,22
   23 P(I)=Y(I)*(I-2)/20.*Z
      A21=FI(P)
      RR=A11*A22-A21*A12
      ALFA=(B1*A22-A12*B2)/RR
      BETA=(A11*B2-A21*B1)/RR
      DO 25 I=2,22
   25 W(I)=W(I)+ALFA+BETA*((I-2)/20.-0.5)
   27 FORMAT(/59X,'íÄÅãàñÄ'/
     *6X,'êÄëèêÖÑÖãÖçàÖ éëÄÑäà à ÇçìíêÖççàï ',
     *'ìëàãàâ èé ÑãàçÖ äéêèìëÄ ëìÑçÄ'
     */25X,'ÇÄêàÄçí áÄÉêìáäà N',I2)
  150 FORMAT(4X,65(1H-)/4X,'≥ N  ≥',4X,'èEPEPEáõÇAûôÄü',5X,'≥',5X,
     *'àáÉàÅAûôàâ',8X,'≥',10X,'≥'/4X,'≥òè. ≥',4X,
     *'CàãA, KH',11X,'≥',5X,'MOMEHT, KHM',7X,
     *'≥  éëÄÑäÄ, ≥'/4X,'≥',4X,'≥',2(23(1H-),'≥'),4X,'M',5X,
     *'≥'/4X,'≥',4X,'≥',2(' ÅEá ìóETA ≥ C ìóETOM  ≥'),10X,
     *'≥'/4X,'≥',4X,'≥',2(' ÉàÅKOCTà  ≥ ÉàÅKOCTà  ≥'),
     *10X,'≥'/4X,'≥',63(1H-),'≥')
      DO 151 I=2,22
  151 P(I)=Y(I)*W(I)
      CALL INTN(P,E)
      DO 31 I=2,22
   31 V(I)=V1(I)-9.81*E(I)*DL*GAM
      CALL INTN(E,P)
      DO 33 I=2,22
33    S(I)=S1(I)-9.81*P(I)*DL*DL*GAM
      DO 84 I=1,23
      I1=I-1
      V1(I)=V1(I)-V1(23)*I1/22.
      V(I)=V(I)-V(23)*I1/22.
      S1(I)=S1(I)-S1(23)*I1/22.
  84  S(I)=S(I)-S(23)*I1/22.
      RS=ABS(S(1))
      DO 34 I=2,23
      IF(ABS(S(I)).GT.RS) RS=ABS(S(I))
34    CONTINUE
      RRS=ABS((RS-RS1)/RS)
      RS1=RS
      IF(RRS.GT.0.03) GOTO 340
      DO 333 I=1,23
 333  W(I)=W(I)+TN+(TK-TN)/22.*(I-1)
      WRITE(6,27) IS
      WRITE(6,150)
      DO 341 I=2,22
      I2=I-2
341   WRITE(6,82) I2,V1(I),V(I),S1(I),S(I),W(I)
   82 FORMAT(4X,'≥ ',I2,1X,'≥', 4(1X,F9.0,1X,'≥'),
     *1X,F8.3,1X,'≥')
      WRITE(6,83)
   83 FORMAT(4X,65(1H-)//)
      XG=XG/G
1111      RETURN
      END
