      SUBROUTINE INTN(WN,WK)
C.....*********************
C.....*èêéÉêÄååÄ "RAVNUS" *
C.....*****06-JAN-92*******
      DIMENSION WN(23),WK(23)
      WK(1)=0.
      WK(2)=0.
      DO 1 I=3,22
      N=I-1
1     WK(I)=WK(N)+(WN(I)+WN(N))/2.
      WK(23)=WK(22)
      RETURN
      END
