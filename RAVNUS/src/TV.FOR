      FUNCTION TV(X)
C.....*********************
C.....*èêéÉêÄååÄ "RAVNUS" *
C.....*****06-JAN-92*******
      COMMON /T/J,T(13)/V/AV(13),BV(13),
     1CV(13),DV(13)
     1/SPL/V(13),S(13),XC(13),XF(13),R(13)
      TV=CUB(X,V,AV,BV,CV,DV,J)
      RETURN
      END
