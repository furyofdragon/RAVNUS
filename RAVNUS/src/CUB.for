      FUNCTION CUB(X,T,A,B,C,D,J)
C.....*******************
C.....*èêéÉêÄååÄ "RAVNUS" *
C.....*******************
      DIMENSION  T(1),A(1),B(1),C(1),D(1)
      DO 1 I=2,J
      K=I-1
      IF(X.LE.T(I)) GOTO 2
1     CONTINUE
2     CUB=A(K)*X**3+B(K)*X*X+C(K)*X+D(K)
      RETURN
      END
