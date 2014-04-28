CDECK  ID>, ALGPRE.
       SUBROUTINE ALGPRE(T,NT,VARLIS,NVAR,NNRES,USE,IENTRY,IFAIL)
*-----------------------------------------------------------------------
*   ALGPRE - Subroutine translating the string T into a series of state-
*            ments to be executed by ALGEXE.
*   VARIABLES : VARLIS      : List of acceptable parameter names.
*               NVAR        : Number of elements in the VARLIS array.
*               T           : The input string, it has NT elements.
*               S           : Is T where operators have been replaced by
*                             O, functions by F, constants and variables
*                             by R.
*               P           : Specifies which operation, function,
*                             register is meant by the code in S.
*               USE(I)      : .TRUE. if variable I is effectively used.
*               NNRES       : =NRES, number of results found in T.
*               CHAR, NEXT, STRING, AUX: Auxiliary.
*   (Last changed on 15/11/08.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER MXWIRE,MXSW,MXLIST,MXCHA,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR,
     -         MXPAIR,MXPART,MXFOUR,MXCLUS,
     -         MXLINE,MXEQUT,
     -         MXRECL,MXINCH,MXWORD,MXCHAR,MXNAME,MXLUN,
     -         MXINS,MXREG,MXARG,MXCONS,MXVAR,MXALGE,
     -         MXZERO,MXSTCK,MXFPNT,MXFPAR,MXWKLS,
     -         MXHLEV,MXHLRL,MXSUBT,
     -         MXDLVL,MXILVL,MXDLIN,
     -         MXHIST,MXFRAC,MXBANG,MXBTAB,
     -         MXEXG,MXIOG,MXCSG,
     -         MXORIA,
     -         MXMAT,MXEMAT,MXMDIM,
     -         MXSHOT,MXZPAR,
     -         MXMAP,MXEPS,MXWMAP,MXSOLI,MXSBUF,
     -         MXPLAN,MXPOIN,MXEDGE,
     -         MXMCA
       PARAMETER (MXWIRE=   300,MXSW  =   50)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXHIST=   200, MXCHA = MXLIST/2)
       PARAMETER (MXGRID=    50)
       PARAMETER (MXNAME=   200, MXLUN =    30)
       PARAMETER (MXCLUS=   500, MXPAIR=  2000, MXPART= 10000)
       PARAMETER (MXLINE=   150, MXEQUT=    50)
       PARAMETER (MXFOUR=    16)
       PARAMETER (MXRECL= 10000)
       PARAMETER (MXINCH=  2000, MXWORD=   200, MXCHAR=MXINCH)
       PARAMETER (MXINS =  1000, MXREG =   500, MXCONS=  -500,
     -            MXVAR =   500, MXALGE=   500, MXARG =   100)
       PARAMETER (MXMAT =   500, MXEMAT=200000, MXMDIM=   10)
       PARAMETER (MXZERO=MXWIRE)
       PARAMETER (MXSTCK=     5)
       PARAMETER (MXFPNT= 20000, MXFPAR=    10)
       PARAMETER (MXWKLS=    10)
       PARAMETER (MXHLEV=     9, MXSUBT=   200, MXHLRL=  860)
       PARAMETER (MXDLVL=    10, MXILVL=    20, MXDLIN= 2500)
       PARAMETER (MXFRAC=    13)
       PARAMETER (MXBANG=    20, MXBTAB=    25)
       PARAMETER (MXEXG =    50, MXIOG =    10, MXCSG =  200)
       PARAMETER (MXORIA=  1000)
       PARAMETER (MXSHOT=    10, MXZPAR=4*MXSHOT+2)
       PARAMETER (MXMAP =  5000,MXEPS =   10)
       PARAMETER (MXWMAP=     5)
       PARAMETER (MXSOLI=  1000)
       PARAMETER (MXPLAN= 50000, MXPOIN=100000,MXEDGE=100)
       PARAMETER (MXSBUF= 20000)
       PARAMETER (MXMCA = 50000)
*   The parameter MXNBMC must equal MXGNAM (sequence MAGBPARM) !
       INTEGER MXNBMC
       PARAMETER(MXNBMC=60)
       INTEGER INS(MXINS,4),ALGENT(MXALGE,10),MODREG(MXCONS:MXREG),
     -      ISYNCH,IINS0,ICONS0,ARGREF(MXARG,2),MODARG(MXARG),
     -      NREG,NCONS,NINS,NERR,NRES,NALGE,IENTRL,NAERR(100)
       REAL REG(MXCONS:MXREG),ARG(MXARG),EXPMAX
       PARAMETER(EXPMAX=40.0)
       LOGICAL EXEC(MXINS),LIGUND,LINUND
       COMMON /ALGDAT/ REG,ARG,MODARG,ARGREF,INS,MODREG,ALGENT,
     -      NREG,NCONS,NINS,NERR,NAERR,
     -      NRES,NALGE,IENTRL,ISYNCH,IINS0,ICONS0,EXEC,LIGUND,LINUND
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER CHAR,NEXT
       CHARACTER*9 MODFLG
       CHARACTER*10 VARLIS(MXVAR)
       CHARACTER*(*) T
       CHARACTER*(MXINCH) S
       INTEGER P(MXINCH),NBRACK,NINDEX,NT,NVAR,NNRES,IENTRY,IFAIL,
     -      I,J,II,IT,IS,IV,IC,IN,IR,IFAILR,IFAILS,IFAILC,IAUX,LASTOP,
     -      NDIM,IDIM,IDIM0,IARG,ISEND,JS,IIS,I1,I2,
     -      MINREG,MAXREG,ISTART,IEXEC,NPASS,NS
       REAL EPS,AUX
       LOGICAL OPER,LETTER,NUMBER,CHANGE,USE(MXVAR),REJECT,LOOP,PREC,
     -      PRECS,RNDUSE,USECON
*** Define some statement function to ease decoding.
       OPER  (CHAR)=INDEX('+-*/=#<>&|^~',CHAR).NE.0
       LETTER(CHAR)=INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ'//
     -      'abcdefghijklmnopqrstuvwxyz',CHAR).NE.0
       NUMBER(CHAR)=INDEX('.0123456789',CHAR).NE.0
       PREC(I,J)=(J.EQ.0).OR.(I.LE.9.AND.J.LE.9.AND.I.GE.J).OR.
     -      (I.GE.10.AND.I.LE.15.AND.J.GE.10.AND.J.LE.15.AND.I.GE.J).OR.
     -      (I.GE.16.AND.I.LE.17.AND.J.GE.16.AND.J.LE.17.AND.I.GE.J).OR.
     -      (I.LE.9.AND.J.GE.10).OR.(I.LE.15.AND.J.GE.16)
       PRECS(I,J)=(J.EQ.0).OR.
     -      (I.LE.9.AND.J.LE.9.AND.I.GE.J.AND.
     -           (I.NE.2.OR.J.NE.2).AND.(I.NE.4.OR.J.NE.4).AND.
     -           (I.NE.5.OR.J.NE.5)).OR.
     -      (I.GE.10.AND.I.LE.15.AND.J.GE.10.AND.J.LE.15.AND.I.GE.J).OR.
     -      (I.GE.16.AND.I.LE.17.AND.J.GE.16.AND.J.LE.17.AND.I.GE.J).OR.
     -      (I.LE.9.AND.J.GE.10).OR.(I.LE.15.AND.J.GE.16)
*** Define a few output formats.
1010   FORMAT(26X,'Constant ',I4,' = ',E15.7,', type=',I2)
1030   FORMAT(/,26X,I4,' Instructions are in use     (Max =',I5,')',
     -        /,26X,I4,' Registers are needed        (Max =',I5,')',
     -        /,26X,I4,' Constants have been defined (Max =',I5,')',
     -        /,26X,I4,' Results are obtained        (No maximum)')
1040   FORMAT(26X,'Variable ',I4,' = "',A10,'"')
1050   FORMAT(26X,'Variable ',I4,' = "',A10,'" (not used)')
1060   FORMAT(26X,'REG(',I3,')=',E15.7:';  REG(',I3,')=',E15.7)
*** Identify the subroutine.
       IF(LIDENT)PRINT *,' /// ROUTINE ALGPRE ///'
*** Check that NT does not exceed 80 characters.
       IF(NT.GT.LEN(T))THEN
            PRINT *,' ###### ALGPRE ERROR   : Input string length',
     -           ' specification inconsistent; rejected (program bug).'
            RETURN
       ENDIF
*** Preset the counter variables etc.
       CALL ALGGBC
       EPS=1.0E-5
       IFAIL=1
       REJECT=.FALSE.
       IT=0
       IS=1
       NBRACK=0
       NINDEX=0
       NRES=0
       NNRES=0
       S='$'
       DO 2 I=1,LEN(S)
       P(I)=0
2      CONTINUE
*** Assign an entry point to the instruction list.
       IENTRY=IENTRL+1
       IENTRL=IENTRL+1
       IINS0=NINS+1
       ICONS0=NCONS-1
*   Check storage, perform a garbage collect if necessary.
       IF(NALGE+1.GT.MXALGE)THEN
            CALL ALGGBC
            IF(NALGE+1.GT.MXALGE)THEN
                 PRINT *,' !!!!!! ALGPRE WARNING : Unable to allocate'//
     -                ' an entry point to the instruction list.'
                 PRINT *,'                         Increase MXALGE'//
     -                ' and recompile the program.'
                 IFAIL=1
                 IENTRY=-1
                 RETURN
            ENDIF
       ENDIF
       NALGE=NALGE+1
*   Initialise the entry point record.
       ALGENT(NALGE,1)=IENTRY
       ALGENT(NALGE,2)=1
       ALGENT(NALGE,3)=0
       ALGENT(NALGE,4)=0
       ALGENT(NALGE,5)=IINS0
       ALGENT(NALGE,6)=0
       ALGENT(NALGE,7)=NVAR
       ALGENT(NALGE,8)=ICONS0
       ALGENT(NALGE,9)=0
       ALGENT(NALGE,10)=0
*** Print the input expression if LDEBUG is on.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGPRE DEBUG   : Start of'',
     -      '' the translation.''//26X,''Input string (length'',I3,
     -      ''):''/26X,A)') NT,T(1:NT)
*** First translation step: operaters -> O, numbers -> R, funct -> F.
10     CONTINUE
       IT=IT+1
*   Check that it does not exceed NT.
       IF(IT.GT.NT)THEN
            IF(IS.GE.LEN(S))GOTO 3010
            IS=IS+1
            S(IS:IS)='$'
            IF(NBRACK.NE.0)THEN
                 PRINT *,' ###### ALGPRE ERROR   : Excess of opening'//
     -                ' brackets.'
                 REJECT=.TRUE.
            ENDIF
            GOTO 150
       ENDIF
*   Skip blanks.
       IF(T(IT:IT).EQ.' ')GOTO 10
*   Increment IS and check that IS < LEN(S).
       IS=IS+1
       IF(IS.GT.LEN(S))GOTO 3010
**  Identify operators.
       IF(OPER(T(IT:IT)))THEN
            S(IS:IS)='O'
            IF(T(IT:IT).EQ.'+')THEN
                 P(IS)=1
            ELSEIF(T(IT:IT).EQ.'-')THEN
                 P(IS)=2
            ELSEIF(T(IT:IT).EQ.'*')THEN
                 P(IS)=3
                 IF(IT.LT.NT)THEN
                      IF(T(IT+1:IT+1).EQ.'*')THEN
                           P(IS)=5
                           IT=IT+1
                      ENDIF
                 ENDIF
            ELSEIF(T(IT:IT).EQ.'/')THEN
                 P(IS)=4
            ELSEIF(T(IT:IT).EQ.'=')THEN
                 P(IS)=10
                 IF(IT.LT.NT)THEN
                      IF(T(IT+1:IT+1).EQ.'<')THEN
                           P(IS)=13
                           IT=IT+1
                      ELSEIF(T(IT+1:IT+1).EQ.'>')THEN
                           P(IS)=15
                           IT=IT+1
                      ENDIF
                 ENDIF
            ELSEIF(T(IT:IT).EQ.'#')THEN
                 P(IS)=11
            ELSEIF(T(IT:IT).EQ.'<')THEN
                 P(IS)=12
                 IF(IT.LT.NT)THEN
                      IF(T(IT+1:IT+1).EQ.'=')THEN
                           P(IS)=13
                           IT=IT+1
                      ELSEIF(T(IT+1:IT+1).EQ.'>')THEN
                           P(IS)=11
                           IT=IT+1
                      ENDIF
                 ENDIF
            ELSEIF(T(IT:IT).EQ.'>')THEN
                 P(IS)=14
                 IF(IT.LT.NT)THEN
                      IF(T(IT+1:IT+1).EQ.'=')THEN
                           P(IS)=15
                           IT=IT+1
                      ELSEIF(T(IT+1:IT+1).EQ.'<')THEN
                           P(IS)=11
                           IT=IT+1
                      ENDIF
                 ENDIF
            ELSEIF(T(IT:IT).EQ.'&')THEN
                 P(IS)=16
            ELSEIF(T(IT:IT).EQ.'|')THEN
                 P(IS)=17
            ELSEIF(T(IT:IT).EQ.'^'.OR.T(IT:IT).EQ.'~')THEN
                 P(IS)=18
            ENDIF
**  Identify variable and function names.
       ELSEIF(LETTER(T(IT:IT)))THEN
            IV=IT
20          CONTINUE
            IV=IV+1
            IF(IV.GT.NT)GOTO 30
            IF((.NOT.OPER(T(IV:IV))).AND.
     -           INDEX(' ([)],;',T(IV:IV)).EQ.0)GOTO 20
30          CONTINUE
            NEXT=','
            DO 40 IN=IV,NT
            IF(T(IN:IN).NE.' ')THEN
                 NEXT=T(IN:IN)
                 GOTO 50
            ENDIF
40          CONTINUE
50          CONTINUE
            CALL CLTOU(T(IT:IV-1))
            IF(OPER(NEXT).OR.INDEX(',)[];',NEXT).NE.0)THEN
                 S(IS:IS)='R'
                 DO 60 IR=1,NVAR
                 IF(T(IT:MIN(IT+LEN(VARLIS(IR))-1,IV-1)).EQ.
     -                VARLIS(IR))THEN
                      IF(IV-IT.GT.LEN(VARLIS(IR)))PRINT *,
     -                     ' !!!!!! ALGPRE WARNING : ',T(IT:IV-1),
     -                     ' is too long for a variable name; has'//
     -                     ' been matched with '//VARLIS(IR)
                      P(IS)=IR
                      GOTO 70
                 ENDIF
60               CONTINUE
                 IF(T(IT:IV-1).EQ.'PI')THEN
                      P(IS)=-3
                 ELSEIF(T(IT:IV-1).EQ.'FALSE')THEN
                      P(IS)=-4
                 ELSEIF(T(IT:IV-1).EQ.'TRUE')THEN
                      P(IS)=-5
                 ELSEIF(T(IT:IV-1).EQ.'NILL')THEN
                      P(IS)=-6
                 ELSEIF(T(IT:IV-1).EQ.'RND_UNIFORM')THEN
                      S(IS:IS)='G'
                      P(IS)=1
                 ELSEIF(T(IT:IV-1).EQ.'RND_GAUSS'.OR.
     -                T(IT:IV-1).EQ.'RND_NORMAL')THEN
                      S(IS:IS)='G'
                      P(IS)=2
                 ELSEIF(T(IT:IV-1).EQ.'RND_EXP'.OR.
     -                T(IT:IV-1).EQ.'RND_EXPONENTIAL')THEN
                      S(IS:IS)='G'
                      P(IS)=3
                 ELSEIF(T(IT:IV-1).EQ.'RND_POISSON')THEN
                      S(IS:IS)='G'
                      P(IS)=4
                 ELSEIF(T(IT:IV-1).EQ.'RND_LANDAU')THEN
                      S(IS:IS)='G'
                      P(IS)=5
                 ELSEIF(T(IT:IV-1).EQ.'RND_POLYA')THEN
                      S(IS:IS)='G'
                      P(IS)=6
                 ELSEIF(T(IT:IV-1).EQ.'RND_FUNCTION')THEN
                      S(IS:IS)='G'
                      P(IS)=7
                 ELSEIF(T(IT:IV-1).EQ.'RND_GAMMA')THEN
                      S(IS:IS)='G'
                      P(IS)=9
                 ELSEIF(T(IT:IV-1).EQ.'RND_LAPLACE')THEN
                      S(IS:IS)='G'
                      P(IS)=10
                 ELSE
                      PRINT *,' ###### ALGPRE ERROR   : ',T(IT:IV-1),
     -                     ' is not a valid parameter.'
                      REJECT=.TRUE.
                 ENDIF
70               CONTINUE
            ELSE
                 P(IS)=0
                 IF(T(IT:IV-1).EQ.'EXP')     P(IS)= 1
                 IF(T(IT:IV-1).EQ.'LOG')     P(IS)=-1
                 IF(T(IT:IV-1).EQ.'SIN')     P(IS)= 2
                 IF(T(IT:IV-1).EQ.'COS')     P(IS)= 3
                 IF(T(IT:IV-1).EQ.'TAN')     P(IS)= 4
                 IF(T(IT:IV-1).EQ.'ARCSIN')  P(IS)=-2
                 IF(T(IT:IV-1).EQ.'ARCCOS')  P(IS)=-3
                 IF(T(IT:IV-1).EQ.'ARCTAN')  P(IS)=-4
                 IF(T(IT:IV-1).EQ.'ABS')     P(IS)= 5
                 IF(T(IT:IV-1).EQ.'SQRT')    P(IS)=-5
                 IF(T(IT:IV-1).EQ.'SINH')    P(IS)= 7
                 IF(T(IT:IV-1).EQ.'COSH')    P(IS)= 8
                 IF(T(IT:IV-1).EQ.'TANH')    P(IS)= 9
                 IF(T(IT:IV-1).EQ.'ARCSINH') P(IS)=-7
                 IF(T(IT:IV-1).EQ.'ARCCOSH') P(IS)=-8
                 IF(T(IT:IV-1).EQ.'ARCTANH') P(IS)=-9
                 IF(T(IT:IV-1).EQ.'NOT')     P(IS)=10
                 IF(T(IT:IV-1).EQ.'ENTIER')  P(IS)=11
                 IF(T(IT:IV-1).EQ.'TRAILING')P(IS)=-11
                 IF(T(IT:IV-1).EQ.'STRING'  )P(IS)=12
                 IF(T(IT:IV-1).EQ.'NUMBER'  )P(IS)=-12
                 IF(T(IT:IV-1).EQ.'SUM'     )P(IS)=13
                 IF(T(IT:IV-1).EQ.'PRODUCT' )P(IS)=14
                 IF(T(IT:IV-1).EQ.'REFERENCE'.OR.
     -                T(IT:IV-1).EQ.'REF')P(IS)=15
                 IF(T(IT:IV-1).EQ.'REF_STRING')P(IS)=51
                 IF(T(IT:IV-1).EQ.'REF_HISTOGRAM'.OR.
     -                T(IT:IV-1).EQ.'REF_HIST')P(IS)=54
                 IF(T(IT:IV-1).EQ.'REF_MATRIX')P(IS)=55
                 IF(T(IT:IV-1).EQ.'GLOBAL'   )P(IS)=16
                 IF(T(IT:IV-1).EQ.'TYPE'     )P(IS)=17
                 IF(T(IT:IV-1).EQ.'LANDAU'   )P(IS)=18
                 IF(T(IT:IV-1).EQ.'MINIMUM'  )P(IS)=19
                 IF(T(IT:IV-1).EQ.'MAXIMUM'  )P(IS)=20
                 IF(T(IT:IV-1).EQ.'RND_UNIFORM')P(IS)=21
                 IF(T(IT:IV-1).EQ.'RND_EXP'.OR.
     -                T(IT:IV-1).EQ.'RND_EXPONENTIAL')P(IS)=23
                 IF(T(IT:IV-1).EQ.'RND_POISSON')P(IS)=24
                 IF(T(IT:IV-1).EQ.'RND_POLYA')P(IS)=26
                 IF(T(IT:IV-1).EQ.'RND_HISTOGRAM')P(IS)=28
                 IF(T(IT:IV-1).EQ.'RND_GAMMA')P(IS)=29
                 IF(T(IT:IV-1).EQ.'RND_LAPLACE')P(IS)=30
                 IF(T(IT:IV-1).EQ.'ROW'      )P(IS)=40
                 IF(T(IT:IV-1).EQ.'MEAN'     )P(IS)=41
                 IF(T(IT:IV-1).EQ.'RMS'      )P(IS)=42
                 IF(T(IT:IV-1).EQ.'SIZE'     )P(IS)=43
                 IF(T(IT:IV-1).EQ.'ZEROES'   )P(IS)=44
                 IF(T(IT:IV-1).EQ.'ONES'     )P(IS)=45
                 IF(T(IT:IV-1).EQ.'EXIST'.OR.
     -                T(IT:IV-1).EQ.'EXISTS' )P(IS)=46
                 IF(T(IT:IV-1).EQ.'GAMMA'    )P(IS)=47
                 IF(T(IT:IV-1).EQ.'LOG_GAMMA')P(IS)=48
                 IF(T(IT:IV-1).EQ.'REVERSE'  )P(IS)=49
                 IF(P(IS).EQ.0)THEN
                      PRINT *,' ###### ALGPRE ERROR   : ',T(IT:IV-1),
     -                     ' is not a valid function.'
                      REJECT=.TRUE.
                 ENDIF
                 S(IS:IS)='F'
            ENDIF
            IT=IV-1
**  Pick up strings.
       ELSEIF(T(IT:IT).EQ.'"'.OR.T(IT:IT).EQ.'`')THEN
            IC=IT
80          CONTINUE
            IC=IC+1
*   Make sure we did see the terminating quote.
            IF(IC.GT.NT)THEN
                 PRINT *,' !!!!!! ALGPRE WARNING : Strings should be'//
     -                ' terminated by a double quote; quote assumed.'
                 GOTO 90
            ELSEIF(T(IC:IC).EQ.T(IT:IT))THEN
                 GOTO 90
            ENDIF
            GOTO 80
90          CONTINUE
*   Assign the string pointer to the constant list.
            S(IS:IS)='R'
            NCONS=NCONS-1
            IF(NCONS.LT.MXCONS)GOTO 3020
*   If the string isn't empty, put it in the string buffer.
            IF(IC-1.GE.IT+1)THEN
                 CALL STRBUF('STORE',IAUX,T(IT+1:IC-1),IC-IT-1,IFAILS)
                 IF(IFAILS.NE.0)THEN
                      PRINT *,' !!!!!! ALGPRE WARNING : Unable to'//
     -                     ' store the string "',T(IT+1:IC-1),
     -                     '"; formula rejected.'
                      REJECT=.TRUE.
                      REG(NCONS)=0.0
                 ELSE
                      REG(NCONS)=REAL(IAUX)
                 ENDIF
*   A null string is stored as a blank string with length zero.
            ELSE
                 CALL STRBUF('STORE',IAUX,' ',0,IFAILS)
                 IF(IFAILS.NE.0)THEN
                      PRINT *,' !!!!!! ALGPRE WARNING : Unable to'//
     -                     ' store the null string; formula rejected.'
                      REJECT=.TRUE.
                      REG(NCONS)=0.0
                 ELSE
                      REG(NCONS)=REAL(IAUX)
                 ENDIF
            ENDIF
*   Keep track of the type of the variable.
            MODREG(NCONS)=1
            P(IS)=NCONS
*   Update string pointer.
            IT=IC
*   Identify numbers (constants) and assign them to a register.
       ELSEIF(NUMBER(T(IT:IT)))THEN
            IC=IT
100         CONTINUE
            IC=IC+1
            IF(IC.GT.NT)GOTO 110
            IF(NUMBER(T(IC:IC)))GOTO 100
            IF(T(IC:IC).EQ.'E')THEN
                 IC=IC+1
                 IF(IC.GT.NT)GOTO 110
                 IF(T(IC:IC).EQ.'+'.OR.T(IC:IC).EQ.'-')IC=IC+1
                 GOTO 100
            ENDIF
110         CONTINUE
            S(IS:IS)='R'
            CALL INPRRC(T(IT:IC-1),AUX,0.0,IFAILR)
            IF(IFAILR.NE.0)THEN
                 PRINT *,' ###### ALGPRE ERROR   : ',T(IT:IC-1),
     -                ' is not acceptable as a number.'
                 REJECT=.TRUE.
            ENDIF
*   See whether the number is already known globally or in this list.
            DO 120 II=0,NCONS,-1
            IF(MODREG(II).EQ.2.AND.(II.GE.-3.OR.II.LE.ICONS0).AND.
     -           ABS(REG(II)-AUX).LE.EPS*(ABS(REG(II))+ABS(AUX)))THEN
                 P(IS)=II
                 GOTO 130
            ENDIF
120         CONTINUE
*   If not known, add it to the list.
            NCONS=NCONS-1
            IF(NCONS.LT.MXCONS)GOTO 3020
            REG(NCONS)=AUX
            MODREG(NCONS)=2
            P(IS)=NCONS
130         CONTINUE
*   Update string pointer.
            IT=IC-1
*   Count brackets, reject if at any time < 0.
       ELSEIF(INDEX(')',T(IT:IT)).NE.0)THEN
            NBRACK=NBRACK-1
            S(IS:IS)=')'
            IF(NBRACK.LT.0)THEN
                 PRINT *,' ###### ALGPRE ERROR   : Excess of closing'//
     -                ' brackets.'
                 REJECT=.TRUE.
            ENDIF
       ELSEIF(INDEX('(',T(IT:IT)).NE.0)THEN
            NBRACK=NBRACK+1
            S(IS:IS)='('
*   Matrix indices, check that there is no nesting.
       ELSEIF(INDEX(']',T(IT:IT)).NE.0)THEN
            NINDEX=NINDEX-1
            S(IS:IS)=']'
            IF(NINDEX.LT.0)THEN
                 PRINT *,' ###### ALGPRE ERROR   : Incorrect array'//
     -                ' indexing.'
                 REJECT=.TRUE.
            ENDIF
       ELSEIF(INDEX('[',T(IT:IT)).NE.0)THEN
            NINDEX=NINDEX+1
            S(IS:IS)='['
C           IF(NINDEX.GT.1)THEN
C                PRINT *,' ###### ALGPRE ERROR   : Index nesting is'//
C    -                ' not permitted.'
C                REJECT=.TRUE.
C           ENDIF
       ELSEIF(INDEX(';',T(IT:IT)).NE.0)THEN
            S(IS:IS)=';'
            IF(NINDEX.NE.1)THEN
                 PRINT *,' ###### ALGPRE ERROR   : Semicolons can'//
     -                ' only be used in indexing expressions'
                 REJECT=.TRUE.
            ENDIF
       ELSEIF(INDEX(',',T(IT:IT)).NE.0.AND.NINDEX.EQ.1)THEN
            S(IS:IS)=','
*   Expression delimiter, check balance of brackets.
       ELSEIF(T(IT:IT).EQ.',')THEN
            S(IS:IS)='$'
            IF(NBRACK.NE.0)THEN
                 PRINT *,' ###### ALGPRE ERROR   : Excess of opening'//
     -                ' brackets in a sub expression.'
                 REJECT=.TRUE.
            ENDIF
            IF(NINDEX.NE.0)THEN
                 PRINT *,' ###### ALGPRE ERROR   : Index expression'//
     -                ' not ended before end of formula.'
                 REJECT=.TRUE.
            ENDIF
*   Invalid element.
       ELSE
            PRINT *,' !!!!!! ALGPRE WARNING : Invalid element "',
     -           T(IT:IT),'" ignored.'
            IS=IS-1
       ENDIF
*   End of loop.
       GOTO 10
150    CONTINUE
*   Store current string length
       NS=IS
*   Print the list if LDEBUG is on.
       IF(LDEBUG)WRITE(LUNOUT,'(/26X,''Code string:''/26X,A)') S(1:NS)
*   Replace $-, (-, O- and F- by functions (-6), $+ etc by F +6.
       DO 160 IS=1,NS-1
       IF(INDEX('$(OF',S(IS:IS)).NE.0.AND.S(IS+1:IS+1).EQ.'O'.AND.
     -      (P(IS+1).EQ.1.OR.P(IS+1).EQ.2.OR.P(IS+1).EQ.18))THEN
            S(IS+1:IS+1)='F'
            IF(P(IS+1).EQ.1)P(IS+1)=+6
            IF(P(IS+1).EQ.2)P(IS+1)=-6
            IF(P(IS+1).EQ.18)P(IS+1)=10
       ENDIF
       IF(S(IS+1:IS+1).EQ.'O'.AND.P(IS+1).EQ.18)THEN
C           PRINT *,' ###### ALGPRE ERROR   : A "not" symbol (^ or ~)'//
C    -           ' has been used as a binary operator ; rejected.'
C           REJECT=.TRUE.
            P(IS+1)=5
       ENDIF
160    CONTINUE
*** Next check syntax: sequence of symbols.
       DO 200 IS=1,NS-1
       IF(  (S(IS:IS).EQ.'$'.AND.INDEX('RG(F '  ,S(IS+1:IS+1)).EQ.0).OR.
     -      (S(IS:IS).EQ.'('.AND.INDEX('RGF('   ,S(IS+1:IS+1)).EQ.0).OR.
     -      (S(IS:IS).EQ.')'.AND.INDEX('O$),;[]',S(IS+1:IS+1)).EQ.0).OR.
     -      (S(IS:IS).EQ.'['.AND.INDEX('RGF(;]' ,S(IS+1:IS+1)).EQ.0).OR.
     -      (S(IS:IS).EQ.']'.AND.INDEX('O$),;]' ,S(IS+1:IS+1)).EQ.0).OR.
     -      (S(IS:IS).EQ.';'.AND.INDEX('R(F];'  ,S(IS+1:IS+1)).EQ.0).OR.
     -      (S(IS:IS).EQ.','.AND.INDEX('R(F'    ,S(IS+1:IS+1)).EQ.0).OR.
     -      (S(IS:IS).EQ.'R'.AND.INDEX(')O$,;[]',S(IS+1:IS+1)).EQ.0).OR.
     -      (S(IS:IS).EQ.'G'.AND.INDEX(')O$'    ,S(IS+1:IS+1)).EQ.0).OR.
     -      (S(IS:IS).EQ.'O'.AND.INDEX('RGF('   ,S(IS+1:IS+1)).EQ.0).OR.
     -      (S(IS:IS).EQ.'F'.AND.INDEX('RG(F'   ,S(IS+1:IS+1)).EQ.0))
     -      THEN
            PRINT *,' ###### ALGPRE ERROR   : Syntax error (illegal'//
     -           ' sequence of symbols).'
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Error occurs at IS='',I2,
     -           ''  in "'',A2,''".'')') IS,S(IS:IS+1)
            REJECT=.TRUE.
       ENDIF
200    CONTINUE
*** Return if syntax errors have been found.
       IF(REJECT)THEN
            PRINT *,' ###### ALGPRE ERROR   : ',T(1:NT),
     -           ' is rejected because of the above errors.'
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGPRE DEBUG   : End'',
     -           '' of the debugging output.'')')
            RETURN
       ENDIF
*   Print the values of the constants if LDEBUG is on.
       IF(LDEBUG)THEN
            IF(NCONS.LT.ICONS0)THEN
                 WRITE(LUNOUT,'(/,26X,''Constants used in the'',
     -                '' expression, apart from 0, 1, 2 and PI:'')')
                 DO 180 I=ICONS0,NCONS,-1
                 WRITE(LUNOUT,1010) I,REG(I),MODREG(I)
180              CONTINUE
                 WRITE(LUNOUT,'('' '')')
            ELSE
                 WRITE(LUNOUT,'(/,26X,''Apart from 0, 1, 2 and PI,'',
     -                '' no constants have been defined.'',/)')
            ENDIF
       ENDIF
*** Transform into a list of executable instructions.
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Instruction list building:''/)')
       NREG=NVAR
       NPASS=0
       RNDUSE=.FALSE.
210    CONTINUE
       NPASS=NPASS+1
       CHANGE=.FALSE.
**  Replace 'G' by 'R'
       DO 219 IS=2,NS
       IF(S(IS:IS).EQ.'G')THEN
            RNDUSE=.TRUE.
            NINS=NINS+1
            IF(NINS.GT.MXINS)GOTO 3040
            INS(NINS,1)=20+P(IS)
            INS(NINS,2)=6
            INS(NINS,3)=-1
            NREG=NREG+1
            IF(NREG.GT.MXREG)GOTO 3030
            P(IS)=NREG
            INS(NINS,4)=P(IS)
            S(IS:IS)='R'
            CHANGE=.TRUE.
       ENDIF
219    CONTINUE
**  Replace 'FR' by a new 'R'.
       DO 220 IS=2,NS-1
       IF(S(IS:IS+1).EQ.'FR')THEN
            NINS=NINS+1
            IF(NINS.GT.MXINS)GOTO 3040
            INS(NINS,1)=P(IS)
            IF(P(IS).GT.20.AND.P(IS).LE.39)RNDUSE=.TRUE.
            INS(NINS,2)=6
            INS(NINS,3)=P(IS+1)
            IF(P(IS+1).LE.0.AND.(P(IS).LE.20.OR.P(IS).GT.39).AND.
     -           P(IS).NE.15.AND.P(IS).NE.40.AND.P(IS).NE.44.AND.
     -           P(IS).NE.45)THEN
                 NCONS=NCONS-1
                 IF(NCONS.LT.MXCONS)GOTO 3020
                 P(IS)=NCONS
            ELSE
                 NREG=NREG+1
                 IF(NREG.GT.MXREG)GOTO 3030
                 P(IS)=NREG
            ENDIF
            INS(NINS,4)=P(IS)
            S(IS:IS+1)='R '
            P(IS+1)=0
            CHANGE=.TRUE.
       ENDIF
220    CONTINUE
**  Replace 'ROR' by a new 'R'.
       DO 230 IS=2,NS-3
       LASTOP=0
       DO 231 IIS=IS-1,1,-1
       IF(S(IIS:IIS).EQ.'O')THEN
            LASTOP=P(IIS)
       ELSEIF(S(IIS:IIS).NE.' ')THEN
            GOTO 232
       ENDIF
231    CONTINUE
232    CONTINUE
       IF((S(IS:IS+3).EQ.'ROR)'.OR.S(IS:IS+3).EQ.'ROR$'.OR.
     -      S(IS:IS+3).EQ.'ROR]'.OR.S(IS:IS+3).EQ.'ROR,'.OR.
     -      S(IS:IS+3).EQ.'ROR;'.OR.
     -      (S(IS:IS+3).EQ.'RORO'.AND.PREC(P(IS+1),P(IS+3)))).AND.
     -      PRECS(P(IS+1),LASTOP))THEN
            NINS=NINS+1
            IF(NINS.GT.MXINS)GOTO 3040
            INS(NINS,1)=P(IS)
            INS(NINS,2)=P(IS+1)
            INS(NINS,3)=P(IS+2)
            IF(P(IS).LE.0.AND.P(IS+2).LE.0)THEN
                 NCONS=NCONS-1
                 IF(NCONS.LT.MXCONS)GOTO 3020
                 P(IS+2)=NCONS
            ELSE
                 NREG=NREG+1
                 IF(NREG.GT.MXREG)GOTO 3030
                 P(IS+2)=NREG
            ENDIF
            S(IS:IS+2)='  R'
            P(IS)=0
            P(IS+1)=0
            INS(NINS,4)=P(IS+2)
            CHANGE=.TRUE.
       ENDIF
230    CONTINUE
**  Process indexing expressions.
       DO 260 IS=1,NS-1
*   Look for opening 'R[' patterns.
       IF(S(IS:IS+1).EQ.'R[')THEN
*   If found, scan for the closing ] and quit if expressions remain.
            NDIM=1
            DO 261 JS=IS+2,NS
            IF(S(JS:JS).EQ.']')THEN
                 ISEND=JS
                 GOTO 262
            ELSEIF(S(JS:JS).EQ.';')THEN
                 NDIM=NDIM+1
            ELSEIF(INDEX(' ,R',S(JS:JS)).EQ.0)THEN
                 GOTO 260
            ENDIF
261         CONTINUE
*   Closing ] not present, issue warning and quit.
            PRINT *,' !!!!!! ALGPRE WARNING : End of index expression'//
     -           ' not found.'
            IFAIL=1
            RETURN
*   Generate the argument list for the procedure call.
262         CONTINUE
            IARG=0
*   Number of dimensions.
            NINS=NINS+1
            IF(NINS.GT.MXINS)GOTO 3040
            NCONS=NCONS-1
            IF(NCONS.LT.MXCONS)GOTO 3020
            REG(NCONS)=REAL(NDIM)
            MODREG(NCONS)=2
            INS(NINS,1)=3
            INS(NINS,2)=8
            INS(NINS,3)=NCONS
            IARG=IARG+1
            INS(NINS,4)=IARG
*   Number of declarations per dimension.
            IDIM0=NCONS
            DO 263 IDIM=1,NDIM
            NINS=NINS+1
            IF(NINS.GT.MXINS)GOTO 3040
            NCONS=NCONS-1
            IF(NCONS.LT.MXCONS)GOTO 3020
            REG(IDIM0-IDIM)=0
            MODREG(IDIM0-IDIM)=2
            IARG=IARG+1
            INS(NINS,1)=3
            INS(NINS,2)=8
            INS(NINS,3)=IDIM0-IDIM
            INS(NINS,4)=IARG
263         CONTINUE
*   Each of the dimensions.
            IDIM=0
            DO 264 JS=IS+1,ISEND-1
            IF(S(JS:JS).EQ.' ')THEN
                 GOTO 264
            ELSEIF(S(JS:JS).EQ.'R')THEN
                 NINS=NINS+1
                 IF(NINS.GT.MXINS)GOTO 3040
                 IARG=IARG+1
                 INS(NINS,1)=3
                 INS(NINS,2)=8
                 INS(NINS,3)=P(JS)
                 INS(NINS,4)=IARG
                 REG(IDIM0-IDIM)=REG(IDIM0-IDIM)+1
            ELSEIF(INDEX(';[',S(JS:JS)).NE.0)THEN
                 IDIM=IDIM+1
            ENDIF
264         CONTINUE
*   Update the string.
            S(IS+1:IS+1)='I'
            P(IS+1)=IARG
            DO 265 JS=IS+2,ISEND
            S(JS:JS)=' '
            P(JS)=0
265         CONTINUE
*   Replace 'RI' by 'R', add the input matrix as argument.
            NINS=NINS+1
            IF(NINS.GT.MXINS)GOTO 3040
            INS(NINS,1)=3
            INS(NINS,2)=8
            INS(NINS,3)=P(IS)
            INS(NINS,4)=P(IS+1)+1
*   Find the location for the output matrix.
            IF(P(IS).LE.0)THEN
                 NCONS=NCONS-1
                 IF(NCONS.LT.MXCONS)GOTO 3020
                 P(IS)=NCONS
            ELSE
                 NREG=NREG+1
                 IF(NREG.GT.MXREG)GOTO 3030
                 P(IS)=NREG
            ENDIF
*   Add the output matrix as argument.
            NINS=NINS+1
            IF(NINS.GT.MXINS)GOTO 3040
            INS(NINS,1)=1
            INS(NINS,2)=8
            INS(NINS,3)=P(IS)
            INS(NINS,4)=P(IS+1)+2
*   Generate procedure call.
            NINS=NINS+1
            IF(NINS.GT.MXINS)GOTO 3040
            INS(NINS,1)=-80
            INS(NINS,2)=9
            INS(NINS,3)=P(IS+1)+2
            INS(NINS,4)=0
*   Update the string.
            S(IS:IS+1)='R '
            P(IS+1)=0
*   Remember that we changed something.
            CHANGE=.TRUE.
       ENDIF
*   Next element.
260    CONTINUE
**  Replace '(R)' by 'R' and remove blanks.
       IS=1
       DO 240 I=2,NS
       IF(S(I:I).EQ.' ')GOTO 240
       IS=IS+1
       S(IS:IS)=S(I:I)
       IF(I.NE.IS)S(I:I)=' '
       P(IS)=P(I)
       IF(I.NE.IS)P(I)=0
       IF(IS.LE.2)GOTO 240
       IF(S(IS-2:IS).EQ.'(R)')THEN
            S(IS-2:IS)='R  '
            P(IS-2)=P(IS-1)
            P(IS-1)=0
            P(IS)=0
            IS=IS-2
            CHANGE=.TRUE.
       ENDIF
240    CONTINUE
*   Store new string length
       NS=IS
**  Print the current string.
       IF(LDEBUG)THEN
            IF(CHANGE)THEN
                 WRITE(LUNOUT,'(26X,''Pass'',I3,'': '',A)')
     -                NPASS,S(1:NS)
            ELSE
                 WRITE(LUNOUT,'(26X,''No further passes.''/)')
            ENDIF
       ENDIF
*   Check whether further cycles are needed.
       IF(CHANGE)GOTO 210
**  Generate instructions to delete temporary matrices.
       DO 270 I=IINS0+1,NINS
*   Select STORE_SUBMATRIX calls.
       IF(INS(I,1).NE.-80.OR.INS(I,2).NE.9)GOTO 270
*   Make sure the output matrix isn't used as a result.
       DO 280 IS=1,NS-2
       IF(S(IS:IS+2).EQ.'$R$'.AND.P(IS+1).EQ.INS(I-1,3))GOTO 270
280    CONTINUE
*   Add the DELETE_MATRIX call to the list.
       IF(NINS+2.GT.MXINS)GOTO 3040
       NINS=NINS+1
       INS(NINS,1)=0
       INS(NINS,2)=8
       INS(NINS,3)=INS(I-1,3)
       INS(NINS,4)=1
       NINS=NINS+1
       INS(NINS,1)=-86
       INS(NINS,2)=9
       INS(NINS,3)=1
       INS(NINS,4)=0
270    CONTINUE
**  Find the results.
       NRES=0
       DO 250 IS=1,NS-2
       IF(S(IS:IS+2).EQ.'$R$')THEN
            NRES=NRES+1
            IF(NINS.GE.MXINS)GOTO 3040
            NINS=NINS+1
            INS(NINS,2)=0
            INS(NINS,3)=P(IS+1)
            INS(NINS,4)=NRES
       ENDIF
250    CONTINUE
*   Make sure there is at least one.
       IF(NRES.LE.0)THEN
            PRINT *,' !!!!!! ALGPRE WARNING : Unable to find a result'//
     -           ' in the expression;'
            RETURN
       ENDIF
       NNRES=NRES
**  Add a return statement.
       IF(NINS.GE.MXINS)GOTO 3040
       NINS=NINS+1
       INS(NINS,1)=-1
       INS(NINS,2)=-9
       INS(NINS,3)=0
       INS(NINS,4)=0
*** Skip simplications if there are randon number generators.
       IF(RNDUSE)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Simplication is skipped'',
     -           '' because of the use of random number generators.'')')
            GOTO 600
       ENDIF
*** Start of the ALGSIM entry for simplifications.
       ENTRY ALGSIM(VARLIS,NVAR,USE,IFAIL)
*   First check whether there are loop structures.
       LOOP=.FALSE.
       DO 310 I=IINS0,NINS
       IF(INS(I,2).EQ.7)LOOP=.TRUE.
310    CONTINUE
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Loop structure flag:'',L2/)') LOOP
*   Print the list if LDEBUG is on.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(26X,''Raw instruction list:'')')
            CALL ALGPRT(IINS0,NINS)
            WRITE(LUNOUT,'(/,26X,''Simplifications (if any):'')')
       ENDIF
**  Repeat the simplification step until no further changes occur.
300    CONTINUE
       CHANGE=.FALSE.
       MODFLG=' '
*   First simplify the expressions.
       DO 320 I=IINS0,NINS
       IF(INS(I,2).EQ.1.AND.(INS(I,1).EQ.0.OR.INS(I,3).EQ.0))THEN
            IF(INS(I,3).EQ.0)INS(I,3)=INS(I,1)
            INS(I,1)=6
            INS(I,2)=6
            CHANGE=.TRUE.
            MODFLG(1:1)='S'
       ENDIF
       IF(INS(I,2).EQ.2.AND.INS(I,1).EQ.INS(I,3))THEN
            INS(I,1)=6
            INS(I,2)=6
            INS(I,3)=0
            CHANGE=.TRUE.
            MODFLG(1:1)='S'
       ENDIF
       IF(INS(I,2).EQ.2.AND.INS(I,3).EQ.0)THEN
            INS(I,3)=INS(I,1)
            INS(I,1)=6
            INS(I,2)=6
            CHANGE=.TRUE.
            MODFLG(1:1)='S'
       ENDIF
       IF(INS(I,2).EQ.2.AND.INS(I,1).EQ.0)THEN
            INS(I,1)=-6
            INS(I,2)=6
            CHANGE=.TRUE.
            MODFLG(1:1)='S'
       ENDIF
       IF(INS(I,2).EQ.3.AND.(INS(I,1).EQ.0.OR.INS(I,3).EQ.0))THEN
            INS(I,1)=6
            INS(I,2)=6
            INS(I,3)=0
            CHANGE=.TRUE.
            MODFLG(1:1)='S'
       ENDIF
       IF(INS(I,2).EQ.3.AND.(INS(I,1).EQ.-1.OR.INS(I,3).EQ.-1))THEN
            IF(INS(I,3).EQ.-1)INS(I,3)=INS(I,1)
            INS(I,1)=6
            INS(I,2)=6
            CHANGE=.TRUE.
            MODFLG(1:1)='S'
       ENDIF
       IF(INS(I,2).EQ.4.AND.INS(I,1).EQ.INS(I,3))THEN
            INS(I,1)=6
            INS(I,2)=6
            INS(I,3)=-1
            CHANGE=.TRUE.
            MODFLG(1:1)='S'
       ENDIF
       IF(INS(I,2).EQ.4.AND.INS(I,3).EQ.0)THEN
            PRINT *,' ###### ALGPRE ERROR   : Division by 0;'//
     -           ' expression is rejected.'
            RETURN
       ENDIF
       IF(INS(I,2).EQ.5.AND.INS(I,3).EQ.0)THEN
            INS(I,1)=6
            INS(I,2)=6
            INS(I,3)=-1
            CHANGE=.TRUE.
            MODFLG(1:1)='S'
       ENDIF
       IF(INS(I,2).EQ.5.AND.INS(I,3).EQ.-1)THEN
            INS(I,3)=INS(I,1)
            INS(I,1)=6
            INS(I,2)=6
            CHANGE=.TRUE.
            MODFLG(1:1)='S'
       ENDIF
       IF(INS(I,2).EQ.5.AND.INS(I,3).EQ.-2)THEN
            INS(I,2)=3
            INS(I,3)=INS(I,1)
            CHANGE=.TRUE.
            MODFLG(1:1)='S'
       ENDIF
       IF(INS(I,2).EQ.16.AND.(INS(I,1).EQ.-4.OR.INS(I,3).EQ.-4))THEN
            INS(I,1)=6
            INS(I,2)=6
            INS(I,3)=-4
            CHANGE=.TRUE.
            MODFLG(1:1)='S'
       ENDIF
       IF(INS(I,2).EQ.16.AND.(INS(I,1).EQ.-5.OR.INS(I,3).EQ.-5))THEN
            IF(INS(I,3).EQ.-5)INS(I,3)=INS(I,1)
            INS(I,1)=6
            INS(I,2)=6
            CHANGE=.TRUE.
            MODFLG(1:1)='S'
       ENDIF
       IF(INS(I,2).EQ.17.AND.(INS(I,1).EQ.-5.OR.INS(I,3).EQ.-5))THEN
            INS(I,1)=6
            INS(I,2)=6
            INS(I,3)=-5
            CHANGE=.TRUE.
            MODFLG(1:1)='S'
       ENDIF
       IF(INS(I,2).EQ.17.AND.(INS(I,1).EQ.-4.OR.INS(I,3).EQ.-4))THEN
            IF(INS(I,3).EQ.-4)INS(I,3)=INS(I,1)
            INS(I,1)=6
            INS(I,2)=6
            CHANGE=.TRUE.
            MODFLG(1:1)='S'
       ENDIF
320    CONTINUE
*   Remove assignments where possible.
       IF(.NOT.LOOP)THEN
            DO 330 I1=IINS0,NINS
            IF((.NOT.EXEC(I1)).OR.INS(I1,1).NE.6.OR.
     -           INS(I1,2).NE.6.OR.INS(I1,2).EQ.0)GOTO 330
            DO 340 I2=I1+1,NINS
            IF(.NOT.EXEC(I2))GOTO 340
            IF(INS(I2,4).EQ.INS(I1,4).AND.INS(I2,2).NE.0)GOTO 330
            IF(INS(I2,1).EQ.INS(I1,4).AND.INS(I2,2).NE.0.AND.
     -           INS(I2,2).NE.6.AND.INS(I2,2).NE.8.AND.
     -           INS(I2,2).NE.9)INS(I2,1)=INS(I1,3)
            IF(INS(I2,3).EQ.INS(I1,4).AND.ABS(INS(I2,2)).NE.9)
     -           INS(I2,3)=INS(I1,3)
            EXEC(I1)=.FALSE.
            CHANGE=.TRUE.
            MODFLG(2:2)='A'
340         CONTINUE
330         CONTINUE
       ELSE
            MODFLG(2:2)='a'
       ENDIF
*   Evaluate constant expressions, and identify them if possible.
       IFAILC=0
       DO 350 I=IINS0,NINS
       IF((.NOT.EXEC(I)).OR.INS(I,3).GT.0.OR.INS(I,2).EQ.0.OR.
     -      INS(I,2).EQ.7.OR.INS(I,2).EQ.8.OR.ABS(INS(I,2)).EQ.9.OR.
     -      (INS(I,1).GT.0.AND.INS(I,2).NE.6).OR.
     -      (INS(I,1).EQ.6.AND.INS(I,2).EQ.6).OR.
     -      (INS(I,1).EQ.15.AND.INS(I,2).EQ.6).OR.
     -      (INS(I,1).EQ.40.AND.INS(I,2).EQ.6).OR.
     -      (INS(I,1).EQ.44.AND.INS(I,2).EQ.6).OR.
     -      (INS(I,1).EQ.45.AND.INS(I,2).EQ.6))GOTO 350
       IF((INS(I,2).EQ.6.AND.MODREG(INS(I,3)).EQ.0).OR.
     -      (INS(I,2).NE.6.AND.(MODREG(INS(I,1)).EQ.0.OR.
     -      MODREG(INS(I,3)).EQ.0)))THEN
            CALL ALGEX0(I,IFAILC)
       ELSEIF((INS(I,2).EQ.6.AND.MODREG(INS(I,3)).EQ.2).OR.
     -      (INS(I,2).NE.6.AND.MODREG(INS(I,1)).EQ.2.AND.
     -      MODREG(INS(I,3)).EQ.2))THEN
            CALL ALGEX2(I,IFAILC)
       ELSEIF((INS(I,2).EQ.6.AND.MODREG(INS(I,3)).EQ.3).OR.
     -      (INS(I,2).NE.6.AND.MODREG(INS(I,1)).EQ.3.AND.
     -      MODREG(INS(I,3)).EQ.3))THEN
            CALL ALGEX3(I,IFAILC)
       ELSEIF((INS(I,2).EQ.6.AND.MODREG(INS(I,3)).EQ.1).OR.
     -      (INS(I,2).NE.6.AND.MODREG(INS(I,1)).EQ.1.AND.
     -      MODREG(INS(I,3)).EQ.1))THEN
            CALL ALGEX4(I,IFAILC)
       ELSEIF((INS(I,2).EQ.6.AND.MODREG(INS(I,3)).EQ.4).OR.
     -      (INS(I,2).NE.6.AND.MODREG(INS(I,1)).EQ.4.OR.
     -      MODREG(INS(I,3)).EQ.4))THEN
            IF(INS(I,2).NE.6.OR.INS(I,1).NE.15)CALL ALGEX5(I,IFAILC)
       ELSEIF((INS(I,2).EQ.6.AND.MODREG(INS(I,3)).EQ.5).OR.
     -      (INS(I,2).NE.6.AND.MODREG(INS(I,1)).EQ.5.OR.
     -      MODREG(INS(I,3)).EQ.5))THEN
            CALL ALGEX6(I,IFAILC)
       ELSE
            PRINT *,' ###### ALGPRE ERROR   : Unable to evaluate'//
     -           ' a constant because of mode incompatibility.'
            IF(LDEBUG)THEN
                 WRITE(LUNOUT,'(26X,''Error occured in:'')')
                 CALL ALGPRT(I,I)
                 IF(INS(I,2).EQ.6)PRINT 1060,INS(I,3),REG(INS(I,3))
                 IF(INS(I,2).NE.6)PRINT 1060,INS(I,1),REG(INS(I,1)),
     -                INS(I,3),REG(INS(I,3))
            ENDIF
            IFAIL=1
            RETURN
       ENDIF
       IF(IFAILC.NE.0)THEN
            CALL ALGERR
            PRINT *,' ###### ALGPRE ERROR   : Arithmetic error while'//
     -           ' evaluating a constant; expression rejected.'
            IF(LDEBUG)THEN
                 WRITE(LUNOUT,'(26X,''Error occured in:'')')
                 CALL ALGPRT(I,I)
                 IF(INS(I,2).EQ.6)PRINT 1060,INS(I,3),REG(INS(I,3))
                 IF(INS(I,2).NE.6)PRINT 1060,INS(I,1),REG(INS(I,1)),
     -                INS(I,3),REG(INS(I,3))
            ENDIF
            IFAIL=1
            RETURN
       ENDIF
       IFAIL=1
       INS(I,1)=6
       INS(I,2)=6
       DO 351 J=0,NCONS,-1
       IF(J.LT.-5.AND.J.GT.ICONS0)GOTO 351
       IF(ABS(REG(J)-REG(INS(I,4))).LT.
     -      EPS*(ABS(REG(J))+ABS(REG(INS(I,4)))).AND.
     -      MODREG(J).EQ.MODREG(INS(I,4)))THEN
            INS(I,3)=J
            GOTO 352
       ENDIF
351    CONTINUE
       NCONS=NCONS-1
       IF(NCONS.LT.MXCONS)GOTO 3020
       REG(NCONS)=REG(INS(I,4))
       MODREG(NCONS)=MODREG(INS(I,4))
       INS(I,3)=NCONS
352    CONTINUE
       IF(INS(I,4).LT.0)THEN
            EXEC(I)=.FALSE.
            DO 353 J=I+1,NINS
            IF(INS(J,4).EQ.INS(I,4))GOTO 350
            IF(EXEC(J).AND.INS(J,1).EQ.INS(I,4).AND.INS(J,2).NE.0.AND.
     -           INS(J,2).NE.6.AND.INS(J,2).NE.8.AND.
     -           INS(J,2).NE.9)INS(J,1)=INS(I,3)
            IF(EXEC(J).AND.INS(J,3).EQ.INS(I,4).AND.
     -           ABS(INS(J,2)).NE.9)INS(J,3)=INS(I,3)
353         CONTINUE
       ENDIF
       CHANGE=.TRUE.
       MODFLG(3:3)='C'
350    CONTINUE
C*   Rearrange the arguments for +, *, & and |.
C       DO 360 I=IINS0,NINS
C       IF(.NOT.EXEC(I))GOTO 360
C       IF((INS(I,2).EQ.1.OR.INS(I,2).EQ.3.OR.INS(I,2).EQ.16.OR.
C     -      INS(I,2).EQ.17).AND.INS(I,1).GT.INS(I,3))THEN
C            IAUX=INS(I,3)
C            INS(I,3)=INS(I,1)
C            INS(I,1)=IAUX
C            CHANGE=.TRUE.
C            MODFLG(4:4)='R'
C       ENDIF
C360    CONTINUE
*   Identify equal expressions.
       IF(.NOT.LOOP)THEN
            DO 370 I1=IINS0,NINS
            IF((.NOT.EXEC(I1)).OR.INS(I1,2).EQ.0.OR.INS(I1,2).EQ.7.OR.
     -           INS(I1,2).EQ.8.OR.ABS(INS(I1,2)).EQ.9)GOTO 370
            DO 380 I2=I1+1,NINS
            IF(EXEC(I2).AND.INS(I2,4).EQ.INS(I1,4))GOTO 370
            IF((.NOT.EXEC(I2)).OR.INS(I2,2).EQ.0.OR.INS(I2,2).EQ.7.OR.
     -           INS(I2,2).EQ.8.OR.ABS(INS(I2,2)).EQ.9)GOTO 380
            IF(INS(I1,1).EQ.INS(I2,1).AND.INS(I1,2).EQ.INS(I2,2).AND.
     -           INS(I1,3).EQ.INS(I2,3))THEN
                 INS(I2,1)=6
                 INS(I2,2)=6
                 INS(I2,3)=INS(I1,4)
                 CHANGE=.TRUE.
                 MODFLG(5:5)='E'
            ENDIF
380         CONTINUE
370         CONTINUE
*   Remove complementary function calls like log(exp(...)).
            DO 390 I1=IINS0,NINS
            IF((.NOT.EXEC(I1)).OR.INS(I1,2).NE.6)GOTO 390
            IF(ABS(INS(I1,1)).EQ.5.OR.INS(I1,1).EQ.6)GOTO 390
            DO 400 I2=I1+1,NINS
            IF(EXEC(I2).AND.INS(I1,4).EQ.INS(I2,4))GOTO 390
            IF((.NOT.EXEC(I2)).OR.INS(I2,2).NE.6.OR.
     -           ABS(INS(I2,1)).EQ.5.OR.ABS(INS(I2,1)).EQ.12.OR.
     -           INS(I2,1).EQ.6.OR.INS(I2,3).NE.INS(I1,4))GOTO 400
            IF(INS(I1,1).EQ.-INS(I2,1).AND.ABS(INS(I1,1)).EQ.11)THEN
                 INS(I2,1)=6
                 INS(I2,2)=6
                 INS(I2,3)=0
                 CHANGE=.TRUE.
                 MODFLG(6:6)='F'
            ELSEIF(INS(I1,1).EQ.-INS(I2,1).OR.
     -           (INS(I1,1).EQ.-6.AND.INS(I2,1).EQ.-6).OR.
     -           (INS(I1,1).EQ.10.AND.INS(I2,1).EQ.10))THEN
                 INS(I2,1)=6
                 INS(I2,2)=6
                 INS(I2,3)=INS(I1,3)
                 CHANGE=.TRUE.
                 MODFLG(6:6)='F'
            ENDIF
400         CONTINUE
390         CONTINUE
*   Substitute minus x in the expressions when possible.
            DO 430 I1=IINS0,NINS
            IF(INS(I1,1).NE.-6.OR.INS(I1,2).NE.6)GOTO 430
            DO 440 I2=I1+1,NINS
            IF(INS(I1,4).EQ.INS(I2,4))GOTO 430
            IF(INS(I2,3).EQ.INS(I1,4).AND.
     -           (INS(I2,2).EQ.1.OR.INS(I2,2).EQ.2))THEN
                 INS(I2,2)=3-INS(I2,2)
                 INS(I2,3)=INS(I1,3)
                 CHANGE=.TRUE.
                 MODFLG(7:7)='M'
            ELSEIF(INS(I2,1).EQ.INS(I1,4).AND.INS(I2,2).EQ.1)THEN
                 INS(I2,1)=INS(I2,3)
                 INS(I2,2)=2
                 INS(I2,3)=INS(I1,3)
                 CHANGE=.TRUE.
                 MODFLG(7:7)='M'
            ENDIF
440         CONTINUE
430         CONTINUE
*   Remove complementary operations like x-y -> z, z-x -> w.
            DO 410 I1=IINS0,NINS
            IF(.NOT.EXEC(I1))GOTO 410
            DO 420 I2=I1+1,NINS
            IF(.NOT.EXEC(I2))GOTO 420
            IF(INS(I1,4).EQ.INS(I2,4))GOTO 410
            IF(((INS(I1,2).EQ.1.AND.INS(I2,2).EQ.2).OR.
     -          (INS(I1,2).EQ.3.AND.INS(I2,2).EQ.4)).AND.
     -         INS(I1,4).EQ.INS(I2,1).AND.
     -         (INS(I1,1).EQ.INS(I2,3).OR.INS(I1,3).EQ.INS(I2,3)))THEN
                 INS(I2,1)=6
                 INS(I2,2)=6
                 IF(INS(I1,1).EQ.INS(I2,3))THEN
                      INS(I2,3)=INS(I1,3)
                 ELSE
                      INS(I2,3)=INS(I1,1)
                 ENDIF
                 CHANGE=.TRUE.
                 MODFLG(8:8)='O'
            ENDIF
            IF(((INS(I1,2).EQ.2.AND.INS(I2,2).EQ.1).OR.
     -          (INS(I1,2).EQ.4.AND.INS(I2,2).EQ.3)).AND.
     -         ((INS(I1,4).EQ.INS(I2,1).AND.INS(I1,3).EQ.INS(I2,3)).OR.
     -          (INS(I1,4).EQ.INS(I2,3).AND.INS(I1,3).EQ.INS(I2,1))))
     -           THEN
                 INS(I2,1)=6
                 INS(I2,2)=6
                 INS(I2,3)=INS(I1,1)
                 CHANGE=.TRUE.
                 MODFLG(8:8)='O'
            ENDIF
            IF(INS(I1,2).EQ.2.AND.INS(I2,2).EQ.2.AND.
     -           INS(I1,1).EQ.INS(I2,3).AND.INS(I1,4).EQ.INS(I2,1))THEN
                 INS(I2,1)=-6
                 INS(I2,2)=6
                 INS(I2,3)=INS(I1,3)
                 CHANGE=.TRUE.
                 MODFLG(8:8)='O'
            ENDIF
420         CONTINUE
410         CONTINUE
       ELSE
            MODFLG(5:5)='e'
            MODFLG(6:6)='f'
            MODFLG(7:7)='m'
            MODFLG(8:8)='o'
       ENDIF
*   Mark the instructions whose results are not used as EXEC=F.
       DO 470 I1=NINS,IINS0,-1
       IF(.NOT.EXEC(I1).OR.INS(I1,2).EQ.0.OR.INS(I1,2).EQ.7.OR.
     -      INS(I1,2).EQ.8.OR.ABS(INS(I1,2)).EQ.9)GOTO 470
       IF(LOOP)THEN
            ISTART=IINS0
       ELSE
            ISTART=I1+1
       ENDIF
       DO 480 I2=ISTART,NINS
       IF(.NOT.EXEC(I2))GOTO 480
       IF((INS(I2,1).EQ.INS(I1,4).AND.INS(I2,2).NE.0.AND.
     -      INS(I2,2).NE.6.AND.INS(I2,2).NE.8.AND.INS(I2,2).NE.9).OR.
     -      (INS(I2,3).EQ.INS(I1,4).AND.ABS(INS(I2,2)).NE.9))GOTO 470
480    CONTINUE
       CHANGE=.TRUE.
       MODFLG(9:9)='X'
       EXEC(I1)=.FALSE.
470    CONTINUE
*   Remove statements marked not to be executed.
       IEXEC=IINS0-1
       DO 490 I=IINS0,NINS
       IF(EXEC(I))THEN
            IEXEC=IEXEC+1
            INS(IEXEC,1)=INS(I,1)
            INS(IEXEC,2)=INS(I,2)
            INS(IEXEC,3)=INS(I,3)
            INS(IEXEC,4)=INS(I,4)
            EXEC(IEXEC)=.TRUE.
       ENDIF
490    CONTINUE
       IF(IEXEC.EQ.0)THEN
            PRINT *,' ###### ALGPRE ERROR   : No instructions left'//
     -           ' (program bug); expression can not be handled.'
            RETURN
       ENDIF
       NINS=IEXEC
*   Check whether any further cycles are needed.
       IF(LDEBUG.AND.CHANGE)THEN
            WRITE(LUNOUT,'(/26X,''Modification flags: '',A9)') MODFLG
            CALL ALGPRT(IINS0,NINS)
       ENDIF
       IF(CHANGE)GOTO 300
*** Continue here if simplication was skipped.
600    CONTINUE
*** Remove unused registers, first find smallest and largest register.
       MAXREG=0
       MINREG=1
       DO 500 I=IINS0,NINS
       IF(INS(I,2).NE.0.AND.INS(I,2).NE.6.AND.INS(I,2).NE.8.AND.
     -      INS(I,2).NE.9)THEN
            MAXREG=MAX(MAXREG,INS(I,1))
            MINREG=MIN(MINREG,INS(I,1))
       ENDIF
       IF(ABS(INS(I,2)).NE.9)THEN
            MAXREG=MAX(MAXREG,INS(I,3))
            MINREG=MIN(MINREG,INS(I,3))
       ENDIF
500    CONTINUE
*   Remove the largest unused registers.
       NREG=NVAR
       DO 510 I1=NVAR+1,MAXREG
       NREG=NREG+1
       CHANGE=.FALSE.
       DO 520 I2=IINS0,NINS
       IF(INS(I2,1).EQ.I1.AND.INS(I2,2).NE.0.AND.INS(I2,2).NE.6.AND.
     -      INS(I2,2).NE.8.AND.INS(I2,2).NE.9)THEN
            CHANGE=.TRUE.
            INS(I2,1)=NREG
       ENDIF
       IF(INS(I2,3).EQ.I1.AND.ABS(INS(I2,2)).NE.9)THEN
            CHANGE=.TRUE.
            INS(I2,3)=NREG
       ENDIF
       IF(INS(I2,4).EQ.I1)THEN
            CHANGE=.TRUE.
            INS(I2,4)=NREG
       ENDIF
520    CONTINUE
       IF(.NOT.CHANGE)NREG=NREG-1
510    CONTINUE
*   Free memory associated with no longer used constants.
       DO 570 I1=ICONS0,MINREG,-1
       USECON=.FALSE.
       DO 580 I2=IINS0,NINS
       IF((INS(I2,1).EQ.I1.AND.INS(I2,2).NE.0.AND.INS(I2,2).NE.6).OR.
     -      INS(I2,3).EQ.I1)USECON=.TRUE.
580    CONTINUE
       IF(.NOT.USECON)CALL ALGREU(NINT(REG(I1)),MODREG(I1),0)
570    CONTINUE
*   Remove the smallest unused constants.
       NCONS=ICONS0+1
       DO 530 I1=ICONS0,MINREG,-1
       NCONS=NCONS-1
       CHANGE=.FALSE.
       DO 540 I2=IINS0,NINS
       IF(INS(I2,1).EQ.I1.AND.INS(I2,2).NE.0.AND.INS(I2,2).NE.6.AND.
     -      INS(I2,2).NE.8.AND.INS(I2,2).NE.9)THEN
            CHANGE=.TRUE.
            REG(NCONS)=REG(INS(I2,1))
            MODREG(NCONS)=MODREG(INS(I2,1))
            INS(I2,1)=NCONS
       ENDIF
       IF(INS(I2,3).EQ.I1.AND.ABS(INS(I2,2)).NE.9)THEN
            CHANGE=.TRUE.
            REG(NCONS)=REG(INS(I2,3))
            MODREG(NCONS)=MODREG(INS(I2,3))
            INS(I2,3)=NCONS
       ENDIF
540    CONTINUE
       IF(.NOT.CHANGE)NCONS=NCONS+1
530    CONTINUE
*   Find out which variables are effectively used.
       DO 550 I1=1,NVAR
       USE(I1)=.FALSE.
       DO 560 I2=IINS0,NINS
       IF((INS(I2,1).EQ.I1.AND.INS(I2,2).NE.0.AND.INS(I2,2).NE.6).OR.
     -      INS(I2,3).EQ.I1)USE(I1)=.TRUE.
560    CONTINUE
550    CONTINUE
*** Update entry point.
       ALGENT(NALGE,3)=1
       IF(LOOP)THEN
            ALGENT(NALGE,4)=0
       ELSE
            ALGENT(NALGE,4)=1
       ENDIF
       ALGENT(NALGE,6)=NINS-IINS0+1
       ALGENT(NALGE,9)=ICONS0-NCONS+1
       ALGENT(NALGE,10)=NRES
*** Print the final version of the instruction list.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(/,26X,''Final instruction list:'')')
            CALL ALGPRT(IINS0,NINS)
            IF(NCONS.LT.ICONS0)THEN
                 WRITE(LUNOUT,'(/,26X,''Constants appearing'',
     -                '' in the final instruction list:'')')
                 DO 700 I=ICONS0,NCONS,-1
                 WRITE(LUNOUT,1010) I,REG(I),MODREG(I)
700              CONTINUE
            ENDIF
            WRITE(LUNOUT,'(/26X,''Valid variable names:'')')
            DO 710 I=1,NVAR
            IF(USE(I))WRITE(LUNOUT,1040) I,VARLIS(I)
            IF(.NOT.USE(I))WRITE(LUNOUT,1050) I,VARLIS(I)
710         CONTINUE
            WRITE(LUNOUT,1030) NINS-IINS0+1,MXINS,
     -           NREG,MXREG,ICONS0-NCONS+1,1-MXCONS,NRES
            IF(LDEBUG)WRITE(LUNOUT,'(/26X,
     -           ''Entry point '',I4,'' assigned to this list:''/
     -           26X,''Reference number:           '',I4/
     -           26X,''In use (1) or not (0):      '',I4/
     -           26X,''Correct (1) or not (0):     '',I4/
     -           26X,''Sequential (1) or not (0):  '',I4/
     -           26X,''First instruction at line:  '',I4/
     -           26X,''Number of instructions:     '',I4/
     -           26X,''Number of registers used:   '',I4/
     -           26X,''First local constant at:    '',I4/
     -           26X,''Number of local constants:  '',I4/
     -           26X,''Number of results produced: '',I4/)')
     -           NALGE,(ALGENT(NALGE,I),I=1,10)
            WRITE(LUNOUT,'(''  ++++++ ALGPRE DEBUG   : End of'',
     -           '' the debugging output.'')')
       ENDIF
*** Normal end of this routine.
       IFAIL=0
       RETURN
*** Handle error conditions due to lack of storage space.
3010   CONTINUE
       PRINT *,' ###### ALGPRE ERROR   : String resulting from first'//
     -      ' translation (see writeup)'
       PRINT *,'                         is longer than 82 chars;'//
     -      ' expression can not be handled.'
       RETURN
3020   CONTINUE
       PRINT *,' ###### ALGPRE ERROR   : Number of constants used in'//
     -      ' the expression is larger than MXCONS;'
       PRINT *,'                         increase this parameter'//
     -      ' and recompile or simplify the expression.'
       RETURN
3030   CONTINUE
       PRINT *,' ###### ALGPRE ERROR   : Number of registers needed'//
     -      ' is larger than MXREG;'
       PRINT *,'                         increase this parameter'//
     -      ' and recompile or simplify the expression.'
       RETURN
3040   CONTINUE
       PRINT *,' ###### ALGPRE ERROR   : Number of instructions'//
     -      ' needed exceeds MXINS;'
       PRINT *,'                         increase this parameter'//
     -      ' and recompile or simplify the expression.'
       RETURN
       END
