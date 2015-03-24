      SUBROUTINE SP(RES,EL,T,DT,TOC,ISP,ISM,ZY,KT,TS,SM)
      COMMON/TIP/KS/T/J,TT(13)
      DIMENSION TS(100),SM(100),ZY(999)
      CHARACTER(256) RES
      OPEN(UNIT=6, FILE=RES, STATUS='OLD')
      J=1
      KT=0
      IT=0
      IF(ISP.NE.0.AND.KS.NE.2) WRITE(6,94)
      IF(TOC.EQ.0)             IT=1
      IF(ISM+ISP.NE.0.OR.KS.EQ.2)
     *         CALL SP0P(RES,1.E-4,EL,ISP,ZY,KT,TS,SM)
      J=J+1
      T1=0.
   3  T1=T1+DT
      IF(T1.GE.T)               GOTO 9
      IF(T1.GE.TOC.AND.IT.EQ.0) GOTO 8
   2  IF(ISP+ISM.NE.0.OR.KS.EQ.2)
     *         CALL SP0P(RES,T1,EL,ISP,ZY,KT,TS,SM)
      J=J+1
      GOTO 3
   8  IT=1
      IF(ISP+ISM.NE.0.OR.KS.EQ.2)
     *         CALL SP0P(RES,TOC-0.001,EL,ISP,ZY,KT,TS,SM)
      J=J+1
      IF(ISP+ISM.NE.0.OR.KS.EQ.2)
     *         CALL SP0P(RES,TOC+0.001,EL,ISP,ZY,KT,TS,SM)
      J=J+1
      IF((T1-TOC).GT.0.001) GOTO 2
                            GOTO 3
   9  IF(ISP+ISM.NE.0.OR.KS.EQ.2)
     *         CALL SP0P(RES,T-0.001,EL,ISP,ZY,KT,TS,SM)
      IF(KS.EQ.2)       RETURN
   6  IF(ISP.NE.0)      WRITE(6,7)
      IF(ISM.EQ.0)      RETURN
      WRITE(6,20)
      WRITE(6,21)(TS(I),SM(I),I=1,KT)
      WRITE(6,22)
      RETURN
   7  FORMAT(1X,119(1H-))
  20  FORMAT(///////'   ����������� �������'/
     *'  ��������� �����������'/8X,'�� ������'//1X,
     *27(1H-)/' :������,:������� ���������:'/
     *' :   �   :�����������, �**2:'/1X,27(1H-))
  21  FORMAT(F8.3,F14.3)
  22  FORMAT(1X,27(1H-)//////)
  94  FORMAT(////31X,'�������� ���������� � ',
     *'��������� ������������'/1X,119(1H-)/1X,
     *':������ �,: �������. : �������  : ��������  ',
     *':�������� :��������� :���������������� ',
     *'�������,� : ������������ �������  :'/1X,
     *':    M    :  V, M**3 :����������:�.�. �����-',
     *':�.�. ��,�:�.�. ZC,M :',27(1H-),':',24(1H-),
     *':'/1X,':',9X,':',10X,':  S, M**2 :����� XF,M',
     *' :',9X,':',10X,': ����������  : ����������  ',
     *':  DELTA :  ALFA : BETA  :'/1X,119(1H-))
      END
