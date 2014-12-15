      FUNCTION RAD(X)
C.....*********************
C.....*Žƒ€ŒŒ€ "ravnus" *
C.....*****06-JAN-92*******
      COMMON /T/J,T(13)/P/AR(13),BR(13),
     1CR(13),DR(13)
      RAD=CUB(X,T,AR,BR,CR,DR,J)
      RETURN
      END
