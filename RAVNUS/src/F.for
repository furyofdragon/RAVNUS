      FUNCTION F(I,X,AF,BF,CF,DF)
C.....*********************
C.....*èêéÉêÄååÄ "RAVNUS" *
C.....*********************
      COMMON /T/J,T(13)
C     COMMON /H/AF(21,13),BF(21,13),CF(21,13),DF(21,13)
      DIMENSION AF(21,13),BF(21,13),CF(21,13),DF(21,13)
      DIMENSION A(13),B(13),C(13),D(13)
      DO 1 L=1,J
      A(L)=AF(I,L)
      B(L)=BF(I,L)
      C(L)=CF(I,L)
1     D(L)=DF(I,L)
      F=CUB(X,T,A,B,C,D,J)
      RETURN
      END
