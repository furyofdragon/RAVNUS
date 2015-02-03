SUBROUTINE READ_EOF(n, nstrok)

implicit none

INTEGER, INTENT(in)  :: n      ! ������ �� ���ᠭ�� �室��� � ����ணࠬ�� ��ࠬ��஢
INTEGER, INTENT(out) :: nstrok ! "-" ��室��� �� ����ணࠬ�� ��ࠬ��஢

INTEGER(4)           :: nstrok_counter
INTEGER(4)           :: ioer
CHARACTER(1)         :: a

ioer = 0
nstrok_counter = 0
DO WHILE (ioer == 0)
    READ(n, *, iostat = ioer) a
    nstrok_counter = nstrok_counter + 1
END DO
REWIND(n)
nstrok=nstrok_counter-1

RETURN
END SUBROUTINE READ_EOF
