      FUNCTION Y(J,T,ZY)
C.....*********************
C.....*èêéÉêÄååÄ "RAVNUS" *
C.....*****06-JAN-92*******
C     COMMON /FORMA/ZY(999)
      DIMENSION ZY(999)
      SC=100.
      Y2=0.
      IS=0
      NS=J-1
      I1=1
      I=1
  3   R=ZY(I)-SC+1.E-6
      IF(R)1,2,2
1     I=I+I1
      GOTO 3
2     N=R
      IF(N.EQ.NS)GOTO15
      IF(N.LT.NS.AND.IS.EQ.1)GOTO7
      IF(N.LT.NS)GOTO1
12    IS=1
      I1=-1
      GOTO1
15    IF(ZY(I+1).GT.SC)GOTO12
7     I=I+1
11    IF(ZY(I+2)-SC)5,10,10
5     V8=ZY(I)
      V9=ZY(I+1)
      V10=ZY(I+2)
      V11=ZY(I+3)
      DZ=V10-V8
      DZT=T-V8
      IF((T-V10)*DZT)8,88,9
88    IF(DZ.NE.0.) GOTO 8
      Y2=V11
      GOTO 9
8     Y1=DZT*(V11-V9)/DZ+V9
      Y2=Y1*SIGN(1.,DZ)+Y2
9     I=I+2
      GOTO11
10    Y=Y2
      RETURN
      END
