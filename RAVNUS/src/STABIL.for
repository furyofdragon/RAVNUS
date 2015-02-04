        SUBROUTINE STABIL(RES,D,EL,T,ZG,TETK,DTET,ZY)
C       COMMON /FORMA/ZY(999)
        DIMENSION ZY(999)
        COMMON /ST/V(12)
        CHARACTER*20 RES
        OPEN(UNIT=6,FILE=RES,STATUS='OLD')
        WRITE(6,94)
        TET=0.
        SLD=0.
1       CALL STAT1(T,EL,TET,ZY)
        IF(ABS(D-V(1)).LE.0.005*D)GOTO2
        IF(V(4).LT.1.E-6)GOTO8
        T=T+(D-V(1))/V(4)*0.75
        GOTO1
2       ELS=V(12)/D-ZG*SIN(TET/57.296)
        ELD=(SLD+ELS)*DTET*8.726646E-3
        SLD=SLD+2.*ELS
        WRITE(6,17) TET,ELS,ELD
        IF(TET.GE.TETK)GOTO6
        TET=TET+DTET
        GOTO1
6       WRITE(6,7)
        RETURN
8       WRITE(6,9) TET
        RETURN
7       FORMAT(1X,33(1H-))
9       FORMAT(1X,33(1H-)/F7.1,
     *       '- … ‚›Ž‹Ÿ…’‘Ÿ “€‚…ˆ… ‹€‚“—…‘’ˆ.'/
     *         1X,44(1H-))
17      FORMAT(F7.1,F10.3,F12.3)
94     FORMAT(///'  …‡“‹œ’€’› €‘—…’€ Ž‘’Ž‰—ˆ‚Ž‘’ˆ'//
     *1X,33(1H-)/' :“ƒ‹›  :  ‹…—ˆ Ž‘’Ž‰—ˆ‚Ž‘’ˆ,Œ  :'/
     2' :Š…€,:',24(1H-),':'/' :ƒ€„. :‘’€’ˆ—…‘ŠŽ‰:',
     3'„ˆ€Œˆ—…‘ŠŽ‰:'/1X,33(1H-))
        END
