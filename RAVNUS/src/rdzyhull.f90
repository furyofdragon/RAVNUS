SUBROUTINE RDZY(EL, FILE, PROEKT, ZY, IR)
      real(4)            :: nstrok = 999
      real(4), DIMENSION :: ZY(nstrok)
      CHARACTER*64          FILE
      CHARACTER*64          PROEKT
10    WRITE (*,1)
1     FORMAT(/' “€†’…  ”€‰‹€ ‘ ”€–…‰  ”… “‘€:'/)
      READ  (*, '(a)', ERR = 10, END = 11) FILE
      OPEN  (11, FILE = FILE)
      WRITE (*,12)
12    FORMAT(//' ‚›‹…’‘ —’…… ”€‰‹€.'/)
      KT = 1
      IR = 0
      READ  (11, '(a)', ERR = 4) PROEKT
      READ  (11,     *, ERR = 4) EL
      DO 6 K = 1, nstrok
          READ (11, *, ERR = 4, END = 4) ZY(K)
          IF (ZY(K).LT.332.)  GOTO 35
          IF (ZY(K).GT.998.)  GOTO 8
          IF (MOD(KT,2).NE.0) GOTO 4
36        KT = 1
          GOTO 6
35        KT=KT+1
6     CONTINUE
8     CLOSE (11)
      GO TO 60
4     CONTINUE
      WRITE(*,'(a)')
      WRITE(*,'(a)') ' ‚€…! ‚ ”€–  ”… “‘€ €“†…› .'
      WRITE(*,'(a)') ' ……„“…’‘ ‚›‹’ ‡€„€—“ N 2 "…—€’ ”€–  ”… “‘€"'
      WRITE(*,'(a)') ' „‹ €€‹‡€  ‘‹…„“™…ƒ ‘€‚‹… .'
      WRITE(*,'(72a)') '-'
      WRITE(*,50)
50    FORMAT(' „‹ „‹†… €’› €†’… <RETURN> '/)
      READ(*,*)
      IR=2
      STOP
60    CALL XYZP(EL,ZY)
      RETURN
 11   IR=2
      RETURN
END SUBROUTINE RDZY





SUBROUTINE XYZP(EL,ZY)
      real(4) :: nstrok = 999
      DIMENSION ZY(nstrok)
      DL=EL/20.
      EL2=EL/2.
      I=1
      J=1
1     ZY(I)=(EL2-ZY(J))/DL+100.
2     J=J+1
      I=I+1
      IF(ZY(J).GT.332.AND.ZY(J).LT.998.)GO TO 3
      IF(ZY(J).GT.998.)GO TO 4
      ZY(I)=ZY(J)
      GO TO 2
3     J=J+1
      IF(ZY(J).LT.999.)GOTO 1
4     ZY(I)=ZY(J)
      RETURN
END SUBROUTINE XYZP



SUBROUTINE READ_EOF(n, nstrok)

     INTEGER, INTENT(in)  :: n      ! „ ρβαο οΆ­®¥ ®―¨α ­¨¥ Άε®¤οι¨ε Ά ―®¤―ΰ®£ΰ ¬¬γ ― ΰ ¬¥βΰ®Ά
     INTEGER, INTENT(out) :: nstrok ! "-" ¨αε®¤οι¨ε ¨§ ―®¤―ΰ®£ΰ ¬¬λ ― ΰ ¬¥βΰ®Ά

     INTEGER :: nstrok_counter
     CHARACTER(1) :: a

     ioer = 0
     nstrok_counter = 0
     DO WHILE (ioer.eq. 0)
     READ(n,*,iostat=ioer) a
     nstrok_counter = nstrok_counter + 1
     ENDDO
     REWIND(n)
     nstrok = nstrok_counter-1

     RETURN

END SUBROUTINE READ_EOF
