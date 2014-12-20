      SUBROUTINE RDZY(EL,FILE,PROEKT,ZY,IR)
      REAL(4)      ZY(999)
      CHARACTER*64 FILE
      CHARACTER*64 PROEKT
      REAL(4)      EL
10    WRITE(*,1)
1     FORMAT(/1X,'“Š€†ˆ’… ˆŒŸ ”€‰‹€ ‘ ˆ”ŽŒ€–ˆ…‰ Ž ”ŽŒ… ŠŽ“‘€: '/)
      READ(*,'(A)',ERR=10,END=11) FILE
      OPEN(11,FILE=FILE)
      KT=1
      IR=0
      READ(11,'(A)', ERR=4) PROEKT
      READ(11,    *, ERR=4) EL
      DO 6 K = 1, 999
          READ(11, *, ERR=4, END=4) ZY(K)
          IF(ZY(K).LT.332.)  GOTO 35
          IF(ZY(K).GT.998.)  GOTO 8
          IF(MOD(KT,2).NE.0) GOTO 4
          KT=1
          GOTO 6
35        KT=KT+1
6     CONTINUE
8     CLOSE(11)
      GO TO 60
4     CONTINUE
      WRITE(*,40)
40    FORMAT(' ‚ˆŒ€ˆ…! ‚ ˆ”ŽŒ€–ˆˆ Ž ”ŽŒ… ŠŽ“‘€ Ž€“†…›',
     *' Ž˜ˆŠˆ.'/' …ŠŽŒ…„“…’‘Ÿ ‚›Ž‹ˆ’œ ‡€„€—“ N 2 "…—€’œ ',
     *'ˆ”ŽŒ€–ˆˆ Ž ”ŽŒ… ŠŽ“‘€"'/' „‹Ÿ €€‹ˆ‡€ ˆ Ž‘‹…„“ž™…ƒŽ ',
     *'ˆ‘€‚‹…ˆŸ Ž˜ˆŽŠ.'/1X,72('-'))
      WRITE(*,50)
50    FORMAT(' „‹Ÿ Ž„Ž‹†…ˆŸ €Ž’› €†Œˆ’… <RETURN> '/)
      READ(*,*)
      IR=2
      STOP
60    CALL XYZP(EL,ZY)
      RETURN
 11   IR=2
      RETURN
      END
