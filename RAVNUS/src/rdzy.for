      SUBROUTINE RDZY(EL,FILE,PROEKT,ZY,IR)
      IMPLICIT NONE
      REAL(4)      ZY(999)
      CHARACTER*64 FILE
      CHARACTER*64 PROEKT
      REAL(4)      EL
      INTEGER(4)   IR
      INTEGER(4)   K
10    WRITE(*,'(A)') '“Š€†ˆ’… ˆŒŸ ”€‰‹€ ‘ ˆ”ŽŒ€–ˆ…‰ Ž ”ŽŒ… ŠŽ“‘€:'
      READ(*,'(A)', ERR=10, END=11) FILE
      OPEN(11,FILE=FILE)
      IR=0
      READ(11,'(A)', ERR=4) PROEKT
      READ(11,    *, ERR=4) EL
      DO K = 1, 999
!TO DO What if file length bigger then 999 ?
          READ(11, *, ERR=4, END=4) ZY(K)
          IF(ZY(K).GT.998.) exit
      END DO
      CLOSE(11)
      CALL XYZP(EL,ZY)
      RETURN

4     CONTINUE
      WRITE(*, '(A)') '‚ ˆ”ŽŒ€–ˆˆ Ž ”ŽŒ… ŠŽ“‘€ Ž€“†…› Ž˜ˆŠˆ.'
      WRITE(*, '(A,/)')' „‹Ÿ Ž„Ž‹†…ˆŸ €Ž’› €†Œˆ’… <RETURN>'
      READ(*,*)
11    IR=2
      STOP
      END
