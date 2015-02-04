      SUBROUTINE RDZY(EL, FILE, PROEKT, ZY, IR)
      IMPLICIT NONE
      REAL(4)        ZY(999)
      CHARACTER(256) FILE
      CHARACTER(256) PROEKT
      INTEGER(4)     KT, IR, K
      REAL(4)        EL
10    WRITE(*, '(A)') '“Š€†ˆ’… ˆŒŸ ”€‰‹€ ‘ ˆ”ŽŒ€–ˆ…‰ Ž ”ŽŒ… ŠŽ“‘€:'
      READ(*, '(A)', ERR = 10) FILE
      OPEN(11, FILE = FILE)
      WRITE(*, 12)
12    FORMAT(//' ‚›Ž‹Ÿ…’‘Ÿ —’…ˆ… ”€‰‹€.'/)
      KT = 1
      IR = 0
      READ(11, '(A)', ERR = 4) PROEKT
      READ(11, *, ERR = 4) EL
      DO K = 1, 999
          READ(11, *, ERR = 4, END = 4) ZY(K)
          IF(ZY(K).LT.332.) THEN
              KT = KT + 1
              cycle
          END IF
          IF(ZY(K).GT.998.) exit
          IF(MOD(KT,2).NE.0) GO TO 4
          KT = 1
      END DO
      CLOSE(11)
      CALL XYZP(EL, ZY)
      RETURN

4     CONTINUE
      WRITE(*, 40)
40    FORMAT(' ‚ˆŒ€ˆ…! ‚ ˆ”ŽŒ€–ˆˆ Ž ”ŽŒ… ŠŽ“‘€ Ž€“†…›',
     *' Ž˜ˆŠˆ.'/' …ŠŽŒ…„“…’‘Ÿ ‚›Ž‹ˆ’œ ‡€„€—“ N 2 "…—€’œ ',
     *'ˆ”ŽŒ€–ˆˆ Ž ”ŽŒ… ŠŽ“‘€"'/' „‹Ÿ €€‹ˆ‡€ ˆ Ž‘‹…„“ž™…ƒŽ ',
     *'ˆ‘€‚‹…ˆŸ Ž˜ˆŽŠ.'/1X,72('-'))
      WRITE(*, 50)
50    FORMAT(' „‹Ÿ Ž„Ž‹†…ˆŸ €Ž’› €†Œˆ’… <RETURN> ')
      READ(*,*)
      IR = 2
      STOP
      END
