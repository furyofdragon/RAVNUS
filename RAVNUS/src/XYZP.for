      SUBROUTINE XYZP(EL, ZY)
      IMPLICIT NONE
      REAL(4)    ZY(999)
      REAL(4)    DL, EL, EL2
      INTEGER(4) I, J
      DL  = EL/20.
      EL2 = EL/2.
      I = 1
      J = 1
1     ZY(I) = (EL2 - ZY(J))/DL + 100.
2     J = J + 1
      I = I + 1
      IF(ZY(J).GT.332.AND.ZY(J).LT.998.) GO TO 3
      IF(ZY(J).GT.998.) GO TO 4
      ZY(I) = ZY(J)
      GO TO 2
3     J = J + 1
      IF(ZY(J).LT.999.) GO TO 1
4     ZY(I) = ZY(J)
      RETURN
      END
