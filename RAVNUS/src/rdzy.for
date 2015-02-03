      SUBROUTINE RDZY(EL,FILE,PROEKT,ZY,IR)
      IMPLICIT NONE
      REAL(4), ALLOCATABLE :: ZY(:)
      CHARACTER(256) FILE
      CHARACTER(256) PROEKT
      REAL(4)        EL
      INTEGER(4)     IR
      INTEGER(4)     K
      INTEGER(4)     nstrok
10    WRITE(*,'(A)') '“€†’…  ”€‰‹€ ‘ ”€–…‰  ”… “‘€:'
      READ(*,'(A)', ERR=10, END=11) FILE
      OPEN(11,FILE=FILE)
      IR=0
      READ(11,'(A)', ERR=4) PROEKT
      READ(11,    *, ERR=4) EL
      CALL READ_EOF(11, nstrok)
      allocate(ZY(nstrok))
      DO K = 1, nstrok
          READ(11, *, ERR=4, END=4) ZY(K)
          IF(ZY(K).GT.998.) exit
      END DO
      CLOSE(11)
      ! ¬®¤¨δ¨ª ζ¨ο δ®ΰ¬λ ε-ª®®ΰ¤¨­ β ª®ΰ―γα 
      CALL XYZP(nstrok, EL,ZY)
      RETURN

4     CONTINUE
      WRITE(*, '(A)')  '‚ ”€–  ”… “‘€ €“†…› .'
      WRITE(*, '(A,/)')'„‹ „‹†… €’› €†’… <RETURN>'
      READ(*,*)
11    IR=2
      STOP
      END
