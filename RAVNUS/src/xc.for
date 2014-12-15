      FUNCTION XC(X)
C.....*********************
C.....*èêéÉêÄååÄ "RAVNUS" *
C.....*****06-JAN-92*******
      COMMON /T/J,T(13)/YC/AXC(13),BXC(13),
     1CXC(13),DXC(13)
      XC=CUB(X,T,AXC,BXC,CXC,DXC,J)
      RETURN
      END
