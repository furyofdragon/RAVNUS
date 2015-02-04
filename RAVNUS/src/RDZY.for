      SUBROUTINE RDZY(EL, FILE, PROEKT, ZY, IR)
      DIMENSION ZY(999)
      CHARACTER(256) FILE
      CHARACTER(256) PROEKT
10    WRITE(*, 1)
1     FORMAT(/1X, 'сйюфхре яоежхтхйюжхч тюикю я хмтнплюжхеи ',
     :' н тнпле йнпосяю: ')
      READ(*, '(A)', ERR = 10, END = 11) FILE
      OPEN(11, FILE = FILE)
      WRITE(*, 12)
12    FORMAT(//' бшонкмъеряъ времхе тюикю.'/)
      KT = 1
      IR = 0
      READ(11, '(A)', ERR = 4) PROEKT
      READ(11, *, ERR = 4) EL
3     DO 6 K = 1, 999
      READ(11, *, ERR = 4, END = 4) ZY(K)
      IF(ZY(K).LT.332.) GO TO 35
      IF(ZY(K).GT.998.) GO TO 8
      IF(MOD(KT,2).NE.0) GO TO 4
36    KT = 1
      GO TO 6
35    KT = KT + 1
6     CONTINUE
8     CLOSE(11)
      GO TO 60
4     CONTINUE
      WRITE(*, 40)
40    FORMAT(' бмхлюмхе! б хмтнплюжхх н тнпле йнпосяю намюпсфемш',
     *' ньхайх.'/' пейнлемдсеряъ бшонкмхрэ гюдювс N 2 "оевюрэ ',
     *'хмтнплюжхх н тнпле йнпосяю"'/' дкъ юмюкхгю х онякедсчыецн ',
     *'хяопюбкемхъ ньханй.'/1X,72('-'))
      WRITE(*, 50)
50    FORMAT(' дкъ опнднкфемхъ пюанрш мюфлхре <RETURN> ')
      READ(*,*)
      IR = 2
100   STOP
60    CALL XYZP(EL, ZY)
      RETURN
 11   IR = 2
      RETURN
      END
