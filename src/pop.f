C======================================================================C
C                                                                      C
C     CLASSIFICATION ASCENDANTE HIERARCHIQUE                           C
C     --------------------------------------                           C
C                                                                      C
C     EN ENTREE :                                                      C
C                 N               : DIMENSION DE SNN ET DE Y           C
C                 SNN   ( N , N ) : MATRICE DES SIMILARITES SYMETRIQUE C
C                                   ( MODIFIEE , PUIS RESTAUREE )      C
C                                                                      C
C     EN SORTIE :                                                      C
C                 Y     ( N , N ) : PARTITION DES INDIVIDUS            C
C                 K               : NOMBRE DE CLASSES ( 1 A N )        C
C                 Z               : DEMI-COUT DE LA PARTITION          C
C                 BORNTH          : BORNE SUPERIEURE THEORIQUE DE Z    C
C                                                                      C
C     ATTENTION :                                                      C
C                 LA DIAGONALE DE Y CONTIENDRA LES NUMEROS DES CLASSES C
C                 LA DIAGONALE DE SNN N'INTERVIENT JAMAIS DANS LE COUT C
C                                                                      C
C======================================================================C
C
      SUBROUTINE PNKCAH ( N , SNN , Y , K , Z , BORNTH )
C
      IMPLICIT INTEGER ( A - Z )
C
C     REAL     SNN ( N , N ) , DSUP , DIJ , Z , BORNTH
      DOUBLE PRECISION SNN (   *   ) , DSUP , DIJ , Z , BORNTH
C
C     INTEGER  Y   ( N , N )
      INTEGER  Y   (   *   )
C
C
C     INITIALISATION DE LA PARTITION : 1 CLASSE PAR INDIVIDU
C     CHAQUE CLASSE EST NUMEROTEE DE 1 A N SUR LA DIAGONALE .
C     ------------------------------------------------------
C
      DO I = 1 , N
          DO J = 1 , I-1
              Y ( I + (J-1)*N ) = 0
          END DO
          Y ( I + (I-1)*N ) = I
      END DO
C
      K      = N
C
C
C     CLASSIFICATION ASCENDANTE HIERARCHIQUE ( COUT < (N*N*N-N)/6 )
C     REGROUPEMENT DES  2  CLASSES AYANT LA PLUS GRANDE SIMILARITE ;
C     ON S'ARRETE LORSQUE TOUTES LES SIMILARITES SNN(I,J) SONT < 0.
C     --------------------------------------------------------------
C
20    DSUP   = -1.
      I1     =  0
      I2     =  0
C
      DO I = 1 , N
C
C         ON N'EXAMINE QUE LES CLASSES "ACTIVES" : LES AUTRES
C         LIGNES CORRESPONDENT AUX CLASSES DEJA REGROUPEES
C
          IF ( Y(I+(I-1)*N) .GT. 0 ) THEN
C
              DO J = I+1 , N
C
C                 ON N'EXAMINE QUE LES CLASSES "ACTIVES" : LES AUTRES
C                 COLONNES CORRESPONDENT AUX CLASSES DEJA REGROUPEES
C
                  IF ( Y(J+(J-1)*N) .GT. 0 ) THEN
C
                      DIJ    = SNN ( I + (J-1)*N )
C
                      IF ( DIJ.GE.0.  .AND.  DIJ.GT.DSUP ) THEN
C                          ---------             --
C                         ON REGROUPE EGALEMENT SI DIJ = 0.
C
                          I1     = I
                          I2     = J
                          DSUP   = DIJ
C
                      ENDIF
C
                  ENDIF
C
              END DO
C
          ENDIF
C
      END DO
C
C
C     TOUTES LES SIMILARITES SONT NEGATIVES : FIN DE L'ALGORITHME
C     LES NUMEROS DES CLASSES SUR LA DIAGONALE SERONT CONSECUTIFS
C     ( AVEC CALCUL DU DEMI-COUT HORS DIAGONALE DE LA PARTITION )
C      ( MISE A JOUR DES TRIANGULAIRES SUPERIEURES DE Y ET SNN )
C     -----------------------------------------------------------
C
      IF ( I1 .EQ. 0 ) THEN
C
          DO I = 1 , N
              II     = I + (I-1)*N
              Y(II)  = IABS ( Y(II) )
          END DO
C
          K0     = 1
          DO NCL = 1 , N
              EFF    = 0
              DO I = 1 , N
                  II     = I + (I-1)*N
                  IF ( Y(II) .EQ. NCL ) THEN
                      Y(II)  = K0
                      EFF    = EFF + 1
                  ENDIF
              END DO
              IF ( EFF .NE. 0 ) K0     = K0 + 1
          END DO
C
          Z      = 0.
          BORNTH = 0.
          DO I = 1 , N
              DO J = 1 , I-1
                  IJ       = I + (J-1) * N
                  JI       = J + (I-1) * N
                  SNN (JI) = SNN (IJ)
                  Y   (JI) = Y   (IJ)
                  Z        = Z + Y(IJ) * SNN(IJ)
                  IF ( SNN(IJ) .GT. 0. ) BORNTH = BORNTH + SNN(IJ)
              END DO
          END DO
C
          RETURN
C
      ENDIF
C
C
C     I1 ET I2 ( I2 > I1 ) SONT LES PLUS PROCHES : ON LES REGROUPE
C     ------------------------------------------------------------
C     LE NOMBRE DE CLASSES DIMINUE DE 1 .
C
C     LE NUMERO DE LA CLASSE DE I1 EST AFFECTE EN NEGATIF A I2 ,
C     AINSI QU'AUX INDIVIDUS DE LA CLASSE DE I2 .
C
C     L'ELEMENT ( I1 , I2 ) DE LA MATRICE DE PARTITION VAUT 1 ,
C     AINSI QUE LES ELEMENTS ( E , I1 ) ET ( E , I2 ) TELS QUE
C     E  SOIT CLASSE , SOIT AVEC I1 , SOIT AVEC I2 .
C
C
      K                   =   K - 1
C
      NEWCLA              = - Y ( I1 + (I1-1)*N )
      ANCCLA              =   Y ( I2 + (I2-1)*N )
C
      DO I = 1 , N
          II     = I + (I-1)*N
          IF  (  Y(II) .EQ.  ANCCLA   .OR.
     .           Y(II) .EQ. -ANCCLA  )     Y(II) = NEWCLA
      END DO
C
      DO I = 1 , N
          II     = I + (I-1)*N
          IF  (  Y(II) .EQ.  NEWCLA   .OR.
     .           Y(II) .EQ. -NEWCLA  )     THEN
              DO J = 1 , I-1
                  JJ     = J + (J-1)*N
                  IF  (  Y(JJ) .EQ.  NEWCLA   .OR.
     .                   Y(JJ) .EQ. -NEWCLA  )     Y ( I + (J-1)*N ) = 1
              END DO
          ENDIF
      END DO
C
C
C     LES SIMILARITES DES AUTRES GROUPES AVEC I1 SONT RECALCULEES
C        DANS CE CAS PARTICULIER , LES SIMILARITES VERIFIENT :
C     SNN ( E , I1 U I2 )   =   SNN ( E , I1 )  +  SNN ( E , I2 )
C     -----------------------------------------------------------
C
      DO E = 1 , I1-1
          SNN ( E+(I1-1)*N ) = SNN ( E+(I1-1)*N ) + SNN ( E+(I2-1)*N )
      END DO
C
      DO E = I1+1 , I2-1
          SNN ( I1+(E-1)*N ) = SNN ( I1+(E-1)*N ) + SNN ( E+(I2-1)*N )
      END DO
C
      DO E = I2+1 , N
          SNN ( I1+(E-1)*N ) = SNN ( I1+(E-1)*N ) + SNN ( I2+(E-1)*N )
      END DO
C
C
C     ON RELANCE L'ALGORITHME
C     -----------------------
      GOTO 20
C
      END
C======================================================================C
C                                                                      C
C     ALGORITHME DE CLASSIFICATION : FAURE ET MALGRANGE BOOLEEN        C
C     ---------------------------------------------------------        C
C                                                                      C
C     EN ENTREE :                                                      C
C                 UECR         : UNITE D'ECRITURE DES RESULTATS        C
C                 FMBVR        : = .TRUE.  POUR LA SOLUTION EXACTE     C
C                                = .FALSE. POUR S'ARRETER A LA CAH     C
C                 TRIABS       : = .TRUE.  : TRI INITIAL VAL. ABSOLUE  C
C                                = .FALSE. : TRI INITIAL ALGEBRIQUE    C
C                 ALLSOL       : = .TRUE.  POUR TOUTES LES SOLUTIONS   C
C                                = .FALSE. POUR UNE SEULE SOLUTION     C
C                 N            : NOMBRE D'INDIVIDUS                    C
C                 COUTS  (N,N) : MATRICE DES COUTS ( SIGNES )          C
C                                                                      C
C     EN SORTIE :                                                      C
C                 YSAVE  (N,N) : SAUVEGARDE DE LA SOLUTION             C
C                 Y      (N,N) : MATRICE DE PARTITION FINALE           C
C                 RENUM  (N,N) : ADRESSE DES COUTS DES VARIABLES       C
C                 BORNTH       : MAJORANT DU COUT DES PARTITIONS       C
C                 NBCL0        : NOMBRE DE CLASSES INITIAL             C
C                 Z0           : COUT DE LA PARTITION INITIALE         C
C                 NBCL         : NOMBRE DE CLASSES FINAL               C
C                 Z            : COUT DE LA PARTITION FINALE           C
C                 NBEMP        : NOMBRE D'EMPILEMENTS                  C
C                 NBDEP        : NOMBRE DE DEPILEMENTS                 C
C                 NBSOL        : NOMBRE DE SOLUTIONS OPTIMALES         C
C                                SAUVEGARDEES APRES LA CAH             C
C                                                                      C
C     ATTENTION :                                                      C
C                 LA TRIANGULAIRE INFERIEURE DE RENUM CONTIENT LES     C
C                 ADRESSES DES COUTS DES M=N*(N-1)/2 VARIABLES DANS    C
C                 LA MATRICE DES COUTS                                 C
C                                                                      C
C                 LA TRIANGULAIRE SUPERIEURE DE RENUM CONTIENT LES     C
C                 ADRESSES RECIPROQUES DE CELLES DE LA TRIANGULAIRE    C
C                 INFERIEURE .                                         C
C                                                                      C
C                 LA TRIANGULAIRE INFERIEURE DE  Y  CONTIENT LES       C
C                 VALEURS  0  OU  1  CHOISIES POUR CHACUNE DES  M      C
C                 VARIABLES , OU  -1  SI LA VARIABLE N'EST PAS FIXEE . C
C                                                                      C
C                 LA TRIANGULAIRE SUPERIEURE DE  Y  CONTIENT L'        C
C                 ADRESSE DE LA VARIABLE PRECEDEMMENT FIXEE , OU  0    C
C                 POUR LA 1-ERE CHOISIE ; CE NUMERO EST NEGATIF SI     C
C                 LA VARIABLE EST CHOISIE PAR IMPLICATION .            C
C                                                                      C
C                 LA DIAGONALE DE Y CONTIENDRA LES NUMEROS DES CLASSES C
C                 LA DIAGONALE DE YSAVE AUSSI                          C
C                                                                      C
C                 LES ARGUMENTS COUTS ET YSAVE PEUVENT AVOIR LA MEME   C
C                 ADRESSE , A CONDITION QUE LES DECLARATIONS "REAL"    C
C                 ET "INTEGER" SUPPOSENT LE MEME NOMBRE DE MOTS :      C
C                 LES COUTS SONT DANS LA TRIANGULAIRE SUPERIEURE ,     C
C                 LA SAUVEGARDE DANS LA TRIANGULAIRE INFERIEURE .      C
C                                                                      C
C======================================================================C
C
C     FONCTION D'ADRESSAGE :
C     ----------------------------------------------------------------
C     $   ,         1 ,         2 ,       3   ,  . . . . . ,       N-1
C     1   ,         $ ,         N ,       N+1 ,            ,     2*N-3
C     2   ,         3 ,         $ ,                                  .
C     .                                                              .
C     .                                                              .
C     .                                          $         , N*(N-1)/2
C     N-1 ,     2*N-3 ,                          N*(N-1)/2 ,         $
C     ----------------------------------------------------------------
C     ADRSUP(I,J,N) = N*I - (I*(I+1))/2 + J - N
C     ADRINF(I,J,N) = N*J - (J*(J+1))/2 + I - N
C
C
C     ALGORITHME
C     ----------
C     ON ATTRIBUE PROGRESSIVEMENT LA VALEUR Y("K") = 1 , OU Y("K") = 0
C     POUR CHAQUE VARIABLE K , AFIN DE DEGRADER LE MOINS POSSIBLE LA
C     FONCTION ECONOMIQUE  Z  :  Y("K") = 1  SI SON COEFFICIENT DANS Z
C     EST POSITIF ( OU NUL ) ,  Y("K") = 0  SINON .
C     A CHAQUE ATTRIBUTION D'UNE VALEUR A LA VARIABLE K , ON EXAMINE
C     LES CONTRAINTES :
C     * SI LA VALEUR ATTRIBUEE EST REFUSEE , ON ATTRIBUE L'AUTRE VALEUR
C     * SI L'AUTRE VALEUR EST REFUSEE , ON REMET EN QUESTION LE DERNIER
C       CHOIX EFFECTUE .
C     * SI LA FONCTION ECONOMIQUE TOMBE EN DESSOUS DU COUT DE LA
C       SOLUTION SAUVEGARDEE , ON REMET EN QUESTION LE DERNIER CHOIX
C       EFFECTUE .
C     * SI ON EST AMENE A REMETTRE EN QUESTION TOUS LES CHOIX JUSQU'A
C       LA 1-ERE VARIABLE , ET QUE CELLE-CI EST ELLE MEME REFUSEE ,
C       L'ALGORITHME S'ARRETE : LA SOLUTION SAUVEGARDEE EST OPTIMALE .
C
C     ON STOCKE POUR CHAQUE VARIABLE , L'ADRESSE DU DERNIER CHOIX
C     EFFECTUE , AVEC UN SIGNE NEGATIF LORSQUE CELUI-CI A DEJA ETE
C     MODIFIE .
C
C======================================================================C
C
      SUBROUTINE pnkfmb ( FMBVR , TRIABS , ALLSOL , N ,
     ,                    COUTS , YSAVE , Y , RENUM ,
     ,                    BORNTH , NBCL0 , Z0 , NBCL , Z ,
     ,                    NBEMP , NBDEP , NBSOL, NAP )
C
      IMPLICIT INTEGER ( A - Z )
C
      INTEGER    YSAVE (N*N) , Y (N*N) , RENUM (N*N)
C
      DOUBLE PRECISION       COUTS (N*N) ,
     ,           BORNTH , Z0 , Z , DELTAZ , ZSAVE , ZNEW ,
     ,           ABS
C
      INTEGER    FMBVR , TRIABS , ALLSOL 
C
      LOGICAL REFUS , CINTEG
C      LOGICAL    FMBVR , TRIABS , ALLSOL , REFUS , CINTEG



C
C
C----------------------------------------------------------------------C
C
C
C     DETERMINATION DU TYPE DE COUTS : REELS OU ENTIERS
C     CINTEG N'EST UTILISE QU'A L'EDITION DES RESULTATS
C     -------------------------------------------------
      CINTEG = .TRUE.
      DO I = 1 , N*N
          ICOUTS = INT ( COUTS(I) )
          CINTEG = CINTEG  .AND.  COUTS(I) .EQ. REAL(ICOUTS)
      END DO
C
C
C     OBTENTION D'UNE PARTITION INITIALE PAR CAH
C     ------------------------------------------
      CALL PNKCAH ( N , COUTS , Y , NBCL0 , Z0 , BORNTH )
C
C
C     ARRET EVENTUEL
C     ---------------------------------------------------
C      write (*,*) 'UECR 4000-5002'
C
      IF ( FMBVR .EQ. 0) RETURN
C
      DO I = 1 , N
          DO J = 1 , I-1
              IJ         = I + (J-1)*N
              YSAVE (IJ) = Y (IJ)
          END DO
      END DO
      ZSAVE  = Z0
C
C
C     TRI QUADRATIQUE DES COUTS : ARITHMETIQUE OU ALGEBRIQUE
C     EX-AEQUOS : ON TESTE LES ADRESSES DES ELEMENTS
C     --------------------------------------------------------------
C     LA VARIABLE K (K-EME PLUS GRAND COUT) ASSOCIEE AU COUT (I1,J1)
C     AURA COMME ADRESSE (IK,JK) DANS LA TRIANGULAIRE INFERIEURE
C     --------------------------------------------------------------
C
      DO J1 = 1 , N
          DO I1 = 1 , J1-1
C
              I1J1        = I1 + (J1-1) * N
              J1I1        = J1 + (I1-1) * N
              RANG        = 1
C
              DO J2 = 1 , N
                  DO I2 = 1 , J2-1
                      I2J2   = I2 + (J2-1) * N
                      IF ( TRIABS .NE. 0 ) THEN
                          CDIF   = ABS(COUTS(I2J2)) - ABS(COUTS(I1J1))
                      ELSE
                          CDIF   =     COUTS(I2J2)  -     COUTS(I1J1)
                      ENDIF
C     IF ( CDIF ) 50 , 30 , 40
                      IF (CDIF < 0) THEN
                         GOTO 50
                      ELSE IF (CDIF == 0) THEN
                         GOTO 30
                      ELSE
                         GOTO 40
                      ENDIF
30                        IF ( I1J1 .GE. I2J2 ) GOTO 50
40                        RANG   = RANG + 1
50                    CONTINUE
C
                  END DO
              END DO
              JK     = 1
60                FINJ   = N*JK - (JK*(JK+1))/2
                  IF ( RANG .GT. FINJ ) THEN
                      JK     = JK + 1
                      GOTO 60
                  ENDIF
              IK     = RANG + N - FINJ
              IKJK        = IK + (JK-1)*N
              RENUM(IKJK) = I1J1
              RENUM(I1J1) = IKJK
C
          END DO
      END DO
C
C
C     INITIALISATIONS DIVERSES ; Z EST LE MAJORANT , SAUF POUR K=M
C     ------------------------------------------------------------
      M      = ( N * (N-1) ) / 2
C
      DO I = 1 , N
          DO J = 1 , I-1
              Y ( I + (J-1)*N ) = -1
              Y ( J + (I-1)*N ) =  0
          END DO
          Y ( I + (I-1)*N ) = - 1
      END DO
C
      NBEMP  = 0
      NBDEP  = 0
      NAP    = 0
      NBSOL  = 0
      KIJPRE = 1
      I      = 1
      J      = 1
      K      = 0
      Z      = BORNTH
      REFUS  = .FALSE.
C
C
C     EMPILEMENT DE L'ADRESSE SUIVANTE : (I,J)+1
C     ------------------------------------------
1000  IF ( K .GE. M ) GOTO 1500
C
      NBEMP  = NBEMP + 1
      I      = I + 1
      IF ( I .GT. N ) THEN
          J      = J + 1
          I      = J + 1
      ENDIF
      IJ     = I + (J-1)*N
      JI     = J + (I-1)*N
      K      = K     + 1
C
C
C     COUT ZNEW = Z +/- DELTAZ ASSOCIE A LA VARIABLE (I,J) , I > J
C     ------------------------------------------------------------
      DELTAZ = COUTS ( RENUM (IJ) )
      IF ( DELTAZ .GE. 0. ) THEN
          VAL01  = 1
      ELSE
          VAL01  = 0
      ENDIF
C
C
C     CONTROLE DE VALIDITE DU CHOIX INITIAL , PUIS DU CHOIX INVERSE
C       ON N'INSISTE PAS SI LE MAJORANT Z EST INFERIEUR A ZSAVE
C     SI  REFUS = .TRUE.   AVANT CONTROLE  ,  ON VIENT DE DEPILER .
C     -------------------------------------------------------------
      CALL PNKTSY ( N , I , J , VAL01 , Y , RENUM , NAP , REFUS )
      IF ( REFUS ) THEN
          VAL01  = 1 - VAL01
          KIJPRE = - KIJPRE
          CALL PNKTSY ( N , I , J , VAL01 , Y , RENUM , NAP , REFUS )
          IF ( REFUS ) GOTO 1200
      ENDIF
1100  ZNEW   = Z
      IF ( VAL01 .EQ. 0 ) THEN
          IF ( DELTAZ .GT. 0. ) ZNEW = Z - DELTAZ
      ELSE
          IF ( DELTAZ .LT. 0. ) ZNEW = Z + DELTAZ
      ENDIF
C      write (*,'(I13)') ALLSOL
      IF ( ALLSOL .EQ. 1 ) THEN
C         write (*,*) "ALLSOL 1"
          IF ( ZNEW .LT. ZSAVE ) GOTO 1200
      ELSE
C         write (*,*) "ALLSOL 0"
          IF ( ZNEW .LE. ZSAVE ) GOTO 1200
      ENDIF
C
C
C     ACCEPTATION DE LA K-EME VARIABLE : ECRITURE DE (I,J)
C     ----------------------------------------------------
      Y(IJ)  = VAL01
      Y(JI)  = KIJPRE
      KIJPRE = IJ
      Z      = ZNEW
      GOTO 1000
C
C
C     DEPILEMENT : ANNULATION DE (I,J)
C     --------------------------------
1200  NBDEP  = NBDEP + 1
      IF ( K .LE. 1 ) GOTO 2000
      K      = K     - 1
      I      = I     - 1
      IF ( I .LE. J ) THEN
          J      = J - 1
          I      =     N
      ENDIF
      IJ     = I + (J-1)*N
      JI     = J + (I-1)*N
      VAL01  = Y(IJ)
      KIJPRE = Y(JI)
      Y(IJ)  = - 1
      Y(JI)  =   0
      DELTAZ = COUTS ( RENUM (IJ) )
      IF ( VAL01 .EQ. 0 ) THEN
          IF ( DELTAZ .GT. 0. ) Z = Z + DELTAZ
      ELSE
          IF ( DELTAZ .LT. 0. ) Z = Z - DELTAZ
      ENDIF
      IF ( KIJPRE .LT. 0 ) GOTO 1200
C
C
C     ON ESSAIE L'AUTRE VALEUR
C     ------------------------
      VAL01  = 1 - VAL01
      KIJPRE = - KIJPRE
      CALL PNKTSY ( N , I , J , VAL01 , Y , RENUM , NAP , REFUS )
      IF ( REFUS ) GOTO 1200
      GOTO 1100
C
C
C     NOUVELLE SOLUTION
C     -----------------
1500  DO II = 1 , N
          DO JJ = 1 , II-1
              IJ           = II + (JJ-1)*N
              ISJS         = RENUM (IJ)
              JS           = ISJS / N
              IS           = ISJS - JS * N
              JS           = 1 + JS
              JSIS         = JS + (IS-1)*N
              YSAVE (JSIS) = Y (IJ)
          END DO
      END DO
      ZSAVE  = Z
      NBSOL  = NBSOL + 1
C
C
C     CALCUL DU NOMBRE DE CLASSES A PARTIR DE YSAVE
C     ---------------------------------------------
      DO IS = 1 , N
          YSAVE ( IS + (IS-1)*N ) = - 1
      END DO
      NBCL   = 0
      DO IS = 1 , N
          ISIS   = IS + (IS-1)*N
          IF ( YSAVE(ISIS) .LT. 0 ) THEN
              NBCL    = NBCL + 1
              DO JS = IS+1 , N
                  JSIS   = JS + (IS-1)*N
                  JSJS   = JS + (JS-1)*N
                  IF ( YSAVE(JSIS) .EQ. 1 ) YSAVE(JSJS) = NBCL
              END DO
              YSAVE(ISIS) = NBCL
          ENDIF
      END DO
C
C      write (*,*) 'UECR 6000-5002'
C
C     AU MIEUX , ON REEXAMINE K = M : SIMULATION DU REFUS DE K = M+1
C     --------------------------------------------------------------
      NBDEP  = NBDEP - 1
      K      = M + 1
C     I      = N
      J      = N
      GOTO 1200
C
C
C     RECUPERATION DE LA PARTITION OPTIMALE
C     -------------------------------------
2000  DO I = 1 , N
          DO J = 1 , I-1
              IJ     = I + (J-1)*N
              JI     = J + (I-1)*N
              Y (IJ) = YSAVE (IJ)
              Y (JI) = Y (IJ)
          END DO
      END DO
      Z      = ZSAVE
C
C
C     CALCUL DU NOMBRE DE CLASSES A PARTIR DE Y
C     -----------------------------------------
      NBCL   = 0
      DO I = 1 , N
          II     = I + (I-1)*N
          IF ( Y(II) .LT. 0 ) THEN
              NBCL    = NBCL + 1
              DO J = I+1 , N
                  IJ     = I + (J-1)*N
                  JJ     = J + (J-1)*N
                  IF ( Y(IJ) .EQ. 1 ) Y(JJ) = NBCL
              END DO
              Y(II)  = NBCL
          ENDIF
      END DO
C
C
C     ECRITURE DE LA PARTITION FINALE ET DES STATISTIQUES
C     ---------------------------------------------------
C      write (*,*) 'UECR 7000-5002'
C
      RETURN
C
      END
C======================================================================C
C                                                                      C
C     CONTROLE DE VALIDITE D'UNE NOUVELLE AFFECTATION DANS Y           C
C                        ( VOIR PNKFMB )                               C
C     ------------------------------------------------------           C
C                                                                      C
C     EN ENTREE :                                                      C
C                 N               : DIMENSION DE Y ET DE RENUM         C
C                 I               : INDICE DE LIGNE DU NOUVEL Y(I,J)   C
C                 J               : INDICE DE COLONNE DE Y(I,J)        C
C                 VAL01           : VALEUR PROPOSEE POUR Y(I,J)        C
C                 Y     ( N , N ) : MATRICE DE PARTITION ( DANS LA     C
C                                   TRIANGULAIRE INFERIEURE )          C
C                 RENUM ( N , N ) : MATRICE DES ADRESSES DES COUTS     C
C                 NAP             : NOMBRE D'APPELS                    C
C                                                                      C
C     EN SORTIE :                                                      C
C                 NAP             : NOMBRE D'APPELS + 1                C
C                 REFUS           : .TRUE.  SI ON REFUSE               C
C                                   .FALSE. SI ON ACCEPTE              C
C                                                                      C
C     ATTENTION :                                                      C
C                 LA TRIANGULAIRE INFERIEURE DE  RENUM  CONTIENT LE    C
C                 COUPLE (II,JJ) , ANCIENNE ADRESSE DE LA VARIABLE     C
C                 DANS LA TRIANGULAIRE SUPERIEURE , CORRESPONDANT      C
C                 AUX COUTS INITIAUX .                                 C
C                                                                      C
C                 LA TRIANGULAIRE SUPERIEURE DE  RENUM  CONTIENT LE    C
C                 COUPLE (I ,J ) , NOUVELLE ADRESSE DE LA VARIABLE     C
C                 DANS LA TRIANGULAIRE INFERIEURE , APRES CLASSEMENT   C
C                 PAR ORDRE DECROISSANT : VOIR CALREN .                C
C                                                                      C
C======================================================================C
C
      SUBROUTINE PNKTSY ( N , I , J , VAL01 , Y , RENUM , NAP , REFUS )
C
      IMPLICIT INTEGER ( A - Z )
C
      LOGICAL    REFUS
C
C     INTEGER    Y     ( N , N ) , RENUM ( N , N )
      INTEGER    Y     (   *   ) , RENUM (   *   )
C
C
      NAP    = NAP + 1
      REFUS  = .FALSE.
C
C
C     EXTRACTION DES INDICES II ET JJ > II , ASSOCIES A YIJ INITIAL
C     -------------------------------------------------------------
      JJII     = RENUM ( I + (J-1)*N )
      IIM1     = (JJII-1) / N
      II       = IIM1 + 1
      JJ       = JJII - N * IIM1
C     Y(II,JJ) = VAL01
C
C
C     BOUCLE SUR LES INDICES INITIAUX COHERENTS AVEC YIJ , YIK , YJK
C     --------------------------------------------------------------
      DO 100 KK = 1 , N
C
C     IF ( II - KK ) 10 , 100 , 20
         IF ((II - KK) < 0 ) THEN
           GOTO 10
        ELSE IF (( II - KK) == 0) THEN
           GOTO 100
        ELSE
           GOTO 20
        END IF
        
            
10        YIK    = Y ( RENUM ( II + (KK-1)*N ) )
          GOTO 30
20        YIK    = Y ( RENUM ( KK + (II-1)*N ) )
C 30       IF ( JJ - KK ) 40 , 100 , 50
 30       IF ( ( JJ - KK ) < 0 ) THEN
             GOTO 40
          ELSE IF  (( JJ - KK ) == 0 ) THEN
             GOTO 100
          ELSE
             GOTO 50
          ENDIF
          
40        YJK    = Y ( RENUM ( JJ + (KK-1)*N ) )
          GOTO 60
50        YJK    = Y ( RENUM ( KK + (JJ-1)*N ) )
C
C         CONTRAIREMENT A LA PROGRAMMATION LINEAIRE , ON TESTE
C         TROIS PAR TROIS LES CONTRAINTES  YIJ + YIK - YJK < 2
C         ----------------------------------------------------
C         REFUS  : YIJ , YIK , YJK  ( VALEURS POSSIBLES -1 , 0 , +1 )
C                   1     1     0
C                   1     0     1
C                   0     1     1
C
60        REFUS  =  VAL01+YIK+YJK  .EQ.  2
C
          IF ( REFUS ) RETURN
C
100       CONTINUE
C
      RETURN
C
      END
