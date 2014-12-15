      SUBROUTINE BONGAP(RES,H,DT2,TOC,ZY,ST1)
C.....*********************
C.....*èêéÉêÄååÄ "RAVNUS" *
C.....*********************
C       COMMON /BG/ST1(21,13)
        COMMON /T/J,T2(13)/T1/JOC/TIP/KS
        DIMENSION S(21)
      DIMENSION TT(30),N1(11),N2(10)
      DIMENSION ST(21,30),ZY(999),ST1(21,13)
      CHARACTER*20 RES
      OPEN(UNIT=6,FILE=RES,STATUS='OLD')
      JOC=0
        DO 6 I=1,21
    6 S(I)=0.
        T=0.
        J=0
        DT1=H/97.
        DT=AMAX1(DT2,DT1)
        IT=0
        IF(TOC.EQ.0.)IT=1
        J=J+1
        TT(J)=T
        DO 10 I=1,21
   10 ST(I,J)=S(I)
    3 T=T+DT
        IF((H-T).LE.(0.25*DT).OR.T.GE.H)GO TO 5
        IF(T.GE.TOC.AND.IT.EQ.0)GOTO7
    2 CALL SSHP(T,S,ZY)
        J=J+1
        TT(J)=T
        DO 11 I=1,21
   11 ST(I,J)=S(I)
        GOTO3
    7 IT=1
        CALL SSHP(TOC,S,ZY)
        J=J+1
      JOC=J
        TT(J)=TOC
        DO 12 I=1,21
   12 ST(I,J)=S(I)
        IF((T-TOC).GT.0.001)GOTO2
        GOTO3
    5 J=J+1
        TT(J)=H
        CALL SSHP(H,S,ZY)
        DO 14 I=1,21
   14 ST(I,J)=S(I)
      IF(KS.NE.2)GO TO 15
      DO 16 I=1,21
      DO 16 I1=1,J
16    ST1(I,I1)=ST(I,I1)
      IF(JOC.EQ.0)RETURN
      DO 17 I1=JOC,J
      DO 17 I=1,21
17    ST1(I,I1+1)=ST(I,I1)
      J=J+1
      T2(JOC)=TOC
      T2(JOC+1)=TOC
      RETURN
15    WRITE(6,700)
      DO 1 KN=1,11
1     N1(KN)=KN-1
      DO 4 M=1,10
4     N2(M)=M+10
      WRITE(6,701) N1
      WRITE(6,702)
      WRITE(6,703) (TT(K),(ST(I,K),I=1,11),K=1,J)
      WRITE(6,702)
      WRITE(6,704)
      WRITE(6,701) N2
      WRITE(6,708)
      WRITE(6,705) (TT(K),(ST(I,K),I=12,21),K=1,J)
      WRITE(6,708)
        RETURN
700   FORMAT(///35X,'M  A  C  ò  T  A  Å     Å  O  ',
     *'H  Ü  A  H  A'/31X,47(1H-)/120(1H-)/
     *' :OCAÑKA,:',9X,'H  O  M  E  P  A     T  E  O',
     *'  P  E  T  à  ó  E  C  K  à  X     ò  è  A  ',
     *'H  É  O  ì  T  O  B',9X,':'/
     5' :   M   :',109(1H-),':'/' :',7X,':')
  701 FORMAT('+',9X,11(I5,4X,':'))
  702 FORMAT(1X,119(1H-))
  703 FORMAT(F8.3,11F10.4)
  704 FORMAT(//89X,'èPOÑOãÜEHàE TAÅã.'/1X,109(1H-)/
     1' :éëÄÑäÄ,:',4X,'ç  é  å  Ö  ê  Ä     í  Ö  é',
     *'  ê  Ö  í  à  ó  Ö  ë  ä  à  ï     ò  è  Ä  ',
     *'H  É  é  ì  í  é  Ç',
     34X,':'/' :   M   :',99(1H-),':'/' :',7X,':')
705     FORMAT(F8.3,10F10.4)
708     FORMAT(1X,109(1H-))
        END
