      FUNCTION XF(X)
C.....*********************
C.....*èêéÉêÄååÄ "RAVNUS" *
C.....*****16-NOV-88*******
      COMMON /T/J,T(13)/YF/AXF(13),BXF(13),
     1CXF(13),DXF(13)
      XF=CUB(X,T,AXF,BXF,CXF,DXF,J)
      RETURN
      END
