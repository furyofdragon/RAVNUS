      SUBROUTINE VIVOD(RES,ZY)
      DIMENSION  ZY(999)
      DIMENSION Z(10),Y(10)
      CHARACTER*20 RES
      OPEN(UNIT=6,FILE=RES,STATUS='OLD')
      WRITE(6,1)
1     FORMAT(///T84,'íÄÅãàñÄ'/
     *T20,'à ç î é ê å Ä ñ à ü   é   î é ê å Ö',
     *'    ä é ê è ì ë Ä'/T19,54('-')//1X,96('-')/
     *' :  çéåÖê  :',9X,'ä é é ê Ñ à ç Ä í õ',4X,'í é ó Ö ä',
     *'    ò è Ä ç É é ì í é Ç , å',T86,':èêàåÖóÄçàü:'/
     *' :òèÄçÉéìíÄ:',T86,':',10X,':')
      I=1
10    WRITE(6,70)
      IP=1
70    FORMAT(' :',94('-'),':')
      WRITE(6,71) ZY(I)
71    FORMAT(' :',F8.2,' : Z :',T86,':',10X,':')
      IF(ZY(I).GT.200.)GOTO 100
      SHN=ZY(I)-100.
      IF(ZY(I+1).GT.90.)GOTO 90
      I=I+1
35    KZ=0
      KY=0
      J=I+18
      DO 3 I1=I,J,2
      IN=I1
      IF(ZY(I1).GT.90.)GOTO 5
      KZ=KZ+1
      Z(KZ)=ZY(I1)
      IF(ZY(I1+1).GT.90.)GOTO 5
      Y(KZ)=ZY(I1+1)
      KY=KZ
3     CONTINUE
      IN=IN+2
5     WRITE(6,30) (Z(IZ),IZ=1,KZ)
30    FORMAT('+',T16,10F7.3)
      IF(KZ.NE.KY) WRITE(6,32)
32    FORMAT('+',T87,'èêéèìôÖçÄ')
      WRITE(6,31)
31    FORMAT(' :  ',6X  ,' : Y :',T86,':',10X,':')
      IF(KY.EQ.0)GOTO 34
      IF(IP.EQ.1) WRITE(6,310) SHN
      IP=0
310   FORMAT('+',2X,'(',F6.2,')')
      WRITE(6,30) (Y(IY),IY=1,KY)
      IF(KZ.NE.KY)GOTO 34
      I=IN
      IF(ZY(IN).GT.90.)GOTO 10
      WRITE(6,36)
36    FORMAT(' :',9X,':',73('-'),':',10X,':'
     * /' :',9X,': Z :',T86,':',10X,':')
      GO TO 35
34    WRITE(6,33)
33    FORMAT('+',T87,'äééêÑàçÄíÄ')
      I=IN+1
      GO TO 10
90    WRITE(6,91) SHN
91    FORMAT('+',T87,'ñàãàçÑêàó.'/' : (',F6.2,'): Y :',
     *T86,': ÇëíÄÇäÄ  :')
      I=I+1
      GO TO 10
100   WRITE(6,101)
101   FORMAT('+',T89,'äéçÖñ'/' :',9X,': Y :',T86,':àçîéêåÄñàà:'/
     *1X,96('-'))
      RETURN
      END
