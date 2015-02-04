      FUNCTION FI(WN)
C.....*********************
C.....*èêéÉêÄååÄ "RAVNUS" *
C.....*********************
      DIMENSION WN(23)
      FI=0.
      DO 1 I=2,22
1     FI=FI+WN(I)
      FI=FI-(WN(2)+WN(22))/2.
      RETURN
      END
