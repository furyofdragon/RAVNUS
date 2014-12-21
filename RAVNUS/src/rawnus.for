      PROGRAM RAVNUS
      REAL NSH,KSH(50)
!     REAL*8 TIMH,TIMK
      COMMON /A5/GS,FS,YS,EL,GI,DT1,FI1,GAM
      DIMENSION FS(23),YS(23),GS(23)
      DIMENSION ZY(999),TSS(100),SM(100)
      DIMENSION ST(21,13)
      DIMENSION AF(21,13),BF(21,13),CF(21,13),DF(21,13)
      CHARACTER*64 FILE
      CHARACTER*4 PROEKT(15)
      CHARACTER*20 DAN,RES
      COMMON/TIP/KS
      COMMON/BAT/B(50)
      NZY=999
      WRITE(*,3000)
3000  FORMAT(
     *10X,'*************************************************'/
     *10X,'*    €’€…’ ƒ€€ “„””……’‚ ‘“„€,   *'/
     *10X,'*    €‘—…’€ ………‡›‚€™• ‘‹  ‡ƒ€™•    *'/
     *10X,'*    …’‚ € ’•‰ ‚„…                     *'/
     *10X,'*************************************************')
      KS=2
      IMB=0
      ISP=0
      ILD=0
      ISM=0

      ! ΆΆ®¤ ¨ ¬®¤¨δ¨ª ζ¨ο δ®ΰ¬λ ª®ΰ―γα 
      CALL RDZY(EL,FILE,PROEKT,ZY,IERR)

12002 WRITE(*, '(A)') '‚‚…„’… €€…’› “‘€  ‚„› ‚„…€'
12004 WRITE(*, '(A,$)') '€  ‚‹, ...........................'
      READ (*,*,ERR=12004) BK
12005 WRITE(*, '(A,$)') '‚›‘’€ ’€, ............................'
      READ (*,*,ERR=12005) H
12007 WRITE(*, '(A,$)') '‘€„€ ‘€, ...........................'
      READ (*,*,ERR=12007) TOC
22008 WRITE(*, '(A,$)') '…’ …– ……—ƒ ‘…—… ,**4...'
      READ (*,*,ERR=22008) GI
22007 WRITE(*, '(A,$)') '‹’‘’ ‚„›, ’/**3 ....................'
      READ (*,*,ERR=22007) GAM
2016  WRITE(*,'(A)') ' ”€‰‹€ „‹ …‡“‹’€’‚ €‘—…’€'
      READ (*,'(A)') RES
      OPEN(UNIT=6,FILE=RES,STATUS='NEW')  
      WRITE(*,'(A)') '‚‚…„’… —‘‹ ‚€€’‚ €ƒ“‡ NG'
      READ(*,*) NG
      WRITE(*,*) '†„’… „…’ €‘—…’'

      DO I=1,50
         B(I)=1.E20
      END DO
      J=0
      I=1
2     P=ZY(I)-100.
      IF(P.GE.100.) GOTO 9
      J=J+1
5     B(J)=AMIN1(B(J),ZY(I+1))
      IF(ZY(I+3).GE.90.) GOTO 4
      I=I+2
      GOTO 5
4     I=I+3
      IF(ZY(I+1).LT.100.) GOTO 2
      J=J+1
      B(J)=B(J-1)
      I=I+1
      GOTO2
9     CONTINUE
      CALL SP(RES,EL,H,H/10.,TOC,ISP,ISM,ZY,KT,TSS,SM)
      CALL BONGAP(RES,H,H/10.,TOC,ZY,ST)
      CALL SPLN(AF,BF,CF,DF,ST)
      IS=0
204   IS=IS+1
      WRITE(*,2023) IS
2023  FORMAT(/4X,'‚€€’ €ƒ“‡ N ',I2/
     *4X,'---------------------'/
     *1X,'‚‚…„’… €‘‘‚ €‘…„…‹…  '/
     *1X,'€ƒ“‡  €– , ')
      DO 2024 I5=1,22
      IH=I5-2
      GOTO 18
16    WRITE(*,14)
14    FORMAT(' O€. ‚’’… ‚‚„.',/)
18    WRITE(*,2025) IH,IH+1
2025  FORMAT(I3,1H-,I2,' ',$)
      READ(*,*,ERR=16)GS(I5)
2024  CONTINUE
2027  WRITE(*,2050)
      DO 2028 I5=1,22,2
      II5=I5+1
      II6=I5-2
      II7=I5-1
2028  WRITE(*,2051)I5,II6,II7,GS(I5),
     *II5,II7,I5,GS(II5)
      SGS=0.
      DO 2029 I5=1,22 
2029  SGS=SGS+GS(I5) 
      SSGS=0.
      DO 2030 I5=1,22
2030  SSGS=SSGS+GS(I5)*EL/40.*(22.-(2.*I5-1.))
      SSGS=SSGS/SGS
      WRITE(*,2052)
      READ(*,*) JJ
      IF(JJ.EQ.0) GOTO 2026
      WRITE(*,2053)
      READ(*,*) KK
      WRITE(*,2054)
      READ(*,*) GS(KK)
      GOTO 2027
2026  WRITE(*,*) '†„’… „…’ €‘—…’'
      WRITE(6,193) TEXT
      WRITE(6,22) EL,BK,H,TOC,GI,GAM
      WRITE(6,212) IS
      DO 2060 I=1,13,6
      K=I-2
      K1=K+1
      K2=K+2
      K3=K+3
      K4=K+4
      K5=K+5
      K6=K+6
2060  WRITE(6,206) K,K1,K1,K2,K2,K3,K3,K4,
     *K4,K5,K5,K6,(GS(I5),I5=I,I+5)
      K=I-2
      K1=K+1
      K2=K+2
      K3=K+3
      K4=K+4
      WRITE(6,2061) K,K1,K1,K2,
     *K2,K3,K3,K4,SGS,(GS(I5),I5=I,I+3),SSGS
      DO 90 I=1,22
90    GS(I)=GS(I)/9.81
      EG=2.1E8
C      GAM=1.
      GS(23)=0.
      R5=0.
      R6=0.
      DO 217 I5=1,11
      K5=23-I5
      K6=23-2*I5
217   R5=R5+(GS(I5)-GS(K5))*K6
      DO 214 I5=1,22
214   R6=R6+GS(I5)
      XG=EL/40.*R5/R6
      R61=R6/GAM
      TS=TV(R61)
      IF(TS.LE.H) GOTO 10
      WRITE(*,11)
      GOTO 1111
10    TN=TS+(EL/2.-XF(TS))*(XG-XC(TS))/RAD(TS)
      TK=TS-(EL/2.+XF(TS))*(XG-XC(TS))/RAD(TS)
      XWL=XF(TS)
      XCV=XC(TS)
      RM=RAD(TS)
      DO 12 I5=1,23
12    GS(I5)=GS(I5)/EL*20.
211   DO 208 I5=1,21
      I6=I5+1
      TT=TN+(TK-TN)/20.*(I5-1)
      IF(TT.GT.H) STOP 'O‘€„€ ‹… ‚›‘’› ’€'
      IF(TT.LT.0.) GOTO 209
      FS(I6)=F(I5,TT,AF,BF,CF,DF)
      YS(I6)=Y(I5,TT,ZY)
      GOTO 208
209   FS(I6)=0.
      YS(I6)=0.
208   CONTINUE
      FS(1)=0.
      FS(23)=0.
      YS(1)=0.
      YS(23)=0.
213   CALL UDS
      R7=ABS(DT1)+ABS(FI1)
      TN=TN-DT1-FI1
      TK=TK-DT1+FI1
      IF(R7.LE.0.03*TS) GOTO 210
      GOTO 211
210   CALL RAWYS(RES,TK,TN,IS)
      IF(IS.LT.NG) GOTO 204
1111  continue
      CLOSE(6)
      STOP 'K…– ƒ€› "RAVNUS"'
11    FORMAT(1H0,
     *'‘’€‚ : ‘…„ ‘€„€ ‹… ‚›‘’› ’€')
200   FORMAT(I2,F8.3)
205   FORMAT(11F7.3)
203   FORMAT(5X,'—‘‹ ‚€€’‚ €ƒ“‡ NG=',I2)
199   FORMAT(18X,'…’ …– .'/18X,
     *'‘…—. “‘€,M**4',F7.3)
206   FORMAT(1X,68(1H-)/1X,'³ …  . ³',6(1X,I2,
     *'-',I2,2X,1H³)/1X,'³------------³--------³--',
     *'------³--------³--------³--------³-------',
     *'-³'/1X,'³ Aƒ“‡€,³',6(F7.1,1X,1H³))
2061  FORMAT(1X,68(1H-)/1X,'³ …  . ³',4(1X,I2,
     *'-',I2,2X,1H³),2X,'D=',F7.0,1X,'KH',3X,1H³/1X,
     *'³------------³--------³--------³--------³--------³',
     *17X,1H³/1X,'³ Aƒ“‡€,³',4(F7.1,1X,1H³),2X,'XG='
     *,F7.2,1X,'M',3X,1H³/1X,68(1H-))
49    FORMAT(G10.3)
22    FORMAT(
     *18X,'„‹€ ‘“„€  ‚‹ ,                    ',F6.2/
     *18X,'€ “‘€  ‚‹ ,                 ',F6.2/
     *18X,'‚›‘’€ ’€,                           ',F6.2/
     *18X,'‘€„€ ‘€,M                          ',F6.2/
     *18X,'MOME’ …– ……—ƒ ‘…—… ,**4 ',F7.3/
     *18X,'‹’‘’ ‚„›, T/**3                   ',F6.3)
25    FORMAT(F10.3)
193   FORMAT(//18X,20A4)
212   FORMAT(60X,/29X,'€ƒ“‡€ € €–',12X,'’€‹–€'/
     *27X,'(‚€€’ €ƒ“‡ N',I2,1H))
2050  FORMAT(///1X,'N ‹-’€ ',2X,'€–',2X,'‚…‹—€',
     *          2X,'N ‹-’€ ',2X,'€–',2X,'‚…‹—€'/
     *          1X,'€‘‘‚€ ',10X,'€ƒ“‡,',
     *          1X,'€‘‘‚€ ',10X,'€ƒ“‡,'/
     *          1X,'€ƒ“‡',13X,'KH',
     *          5X,'€ƒ“‡',13X,'KH')
2051  FORMAT(2(6X,I2,2X,I2,'-',I2,4X,F7.1,2X))
2052  FORMAT(1X,'‚…’… €‚‹‘’ ‚‚„€.'/
     *1X,'…‘‹ …’ , ‚‚…„’… 0'/
     *1X,'‚ ’‚ ‘‹“—€… ‚‚…„’… 1')
2053  FORMAT(1X,'‚‚…„’… „‚›‰ … …‚… ',
     *'€€ƒ ‹……’€')
2054  FORMAT(1X,'‚‚…„’… ‡€‚ …‚… €€›‰ ‹……’')
      END
