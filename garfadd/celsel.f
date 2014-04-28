CDECK  ID>, CELSEL.
       SUBROUTINE CELSEL(SOURCE)
*-----------------------------------------------------------------------
*   CELSEL - This routine allows the user to change his set of readout
*            electrodes. Wires can be identified by means of their label
*            and by their number. Planes and tubes by their label only.
*   (Last changed on 13/ 1/11.)
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
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
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
       PARAMETER (MXMAP =350000,MXEPS =   10)
       PARAMETER (MXWMAP=     5)
       PARAMETER (MXSOLI=  1000)
       PARAMETER (MXPLAN= 50000, MXPOIN=100000,MXEDGE=100)
       PARAMETER (MXSBUF= 20000)
       PARAMETER (MXMCA = 50000)
*   The parameter MXNBMC must equal MXGNAM (sequence MAGBPARM) !
       INTEGER MXNBMC
       PARAMETER(MXNBMC=60)
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL,
     -      BEMSET
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL,BEMSET
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
       REAL EXMAP,EYMAP,EZMAP,VMAP,EWXMAP,EWYMAP,EWZMAP,VWMAP,
     -      BXMAP,BYMAP,BZMAP,
     -      XMAP,YMAP,ZMAP,XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,
     -      VMMIN,VMMAX,EPSMAT,EPSSUR,XFMOFF,YFMOFF,ZFMOFF
       INTEGER MATMAP,NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS,
     -      NWMAP
       LOGICAL MAPFLG,LMAPPL,SETAX,SETAY,SETAZ,ELMDGN,LSFDER
       CHARACTER EWSTYP
       CHARACTER*10 MATSRC
       COMMON /FLDMAP/ VMAP(MXMAP,10),VWMAP(MXMAP,10,MXWMAP),
     -      EXMAP(MXMAP,10),EYMAP(MXMAP,10),EZMAP(MXMAP,10),
     -      EWXMAP(MXMAP,10,MXWMAP),EWYMAP(MXMAP,10,MXWMAP),
     -      EWZMAP(MXMAP,10,MXWMAP),
     -      BXMAP(MXMAP,10),BYMAP(MXMAP,10),BZMAP(MXMAP,10),
     -      XMAP(MXMAP,10),YMAP(MXMAP,10),ZMAP(MXMAP,10),
     -      XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,VMMIN,VMMAX,
     -      XFMOFF,YFMOFF,ZFMOFF,
     -      EPSMAT(MXEPS),EPSSUR(MXEPS),MATMAP(MXMAP),
     -      NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS(MXWMAP),NWMAP,
     -      MAPFLG(10+4*MXWMAP),ELMDGN(MXMAP),
     -      LMAPPL,SETAX,SETAY,SETAZ,LSFDER
       COMMON /FLDCHR/ EWSTYP(MXWMAP),MATSRC
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       LOGICAL OPEN,OK,EXIST,FOUND,SOLSEL,
     -      USED(MXWIRE+5+MXWMAP+10*MXPSTR+MXSOLI)
       INTEGER NC,I,J,K,L,II,IREAD,NWORD,IFAIL,INEXT,
     -      NPRIM,NPRIMJ,PRIML(MXPLAN),PRIMLJ(MXPLAN)
       CHARACTER*(MXINCH) TEXT,WRONG
       CHARACTER*(*) SOURCE
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE CELSEL ///'
*** Obtain the argument string.
       IF(SOURCE.EQ.' ')THEN
            CALL INPSTR(2,MXWORD,TEXT,NC)
            CALL INPNUM(NWORD)
       ELSE
            TEXT=SOURCE
            NC=LEN(SOURCE)
            NWORD=2
            IF(NC.GT.MXINCH)THEN
                 PRINT *,' !!!!!! CELSEL WARNING : Electrode'//
     -                ' selection too long; truncated.'
                 NC=MXINCH
            ENDIF
       ENDIF
*** If the string is blank, only print the current settings.
       IF(NWORD.LE.1)THEN
            CALL CELPRC(LUNOUT,0)
            RETURN
       ENDIF
*** Clear any BEM weighting fields that may exist.
       DO 500 I=1,NSW
       IF(BEMSET)THEN
            print *,' Deleting weighting field ',IWFBEM(I)
            CALL BEMDLW(IWFBEM(I))
       ENDIF
       IWFBEM(I)=-1
500    CONTINUE
*** Initialse INDSW, the logicals and the error logging array.
       OK=.TRUE.
       WRONG='                              '//
     -       '                              '
       DO 10 I=1,MXWIRE
       USED(I)=.FALSE.
       INDSW(I)=0
10     CONTINUE
       DO 160 I=1,5
       USED(MXWIRE+I)=.FALSE.
       INDPLA(I)=0
       DO 340 J=1,MXPSTR
       INDST1(I,J)=0
       INDST2(I,J)=0
       USED(MXWIRE+5+MXWMAP+(I-1)*MXPSTR+J)=.FALSE.
       USED(MXWIRE+5+MXWMAP+(I+4)*MXPSTR+J)=.FALSE.
340    CONTINUE
160    CONTINUE
       DO 260 I=1,NWMAP
       USED(MXWIRE+5+I)=.FALSE.
       INDEWS(I)=0
260    CONTINUE
       NSW=0
       DO 170 I=1,MXSOLI
       USED(MXWIRE+5+MXWMAP+10*MXPSTR+I)=.FALSE.
       INDSOL(I)=0
170    CONTINUE
       SOLSEL=.FALSE.
*** Loop over all characters in the string.
       OPEN=.FALSE.
       EXIST=.FALSE.
       INEXT=1
       DO 20 I=1,NC
       IF(I.LT.INEXT)GOTO 20
**  Skip blanks, commas and equal signs (the usual separators),
       IF(INDEX(' ,=',TEXT(I:I)).NE.0)GOTO 20
**  "(" open brackets,
       IF(TEXT(I:I).EQ.'(')THEN
            IF(OPEN)THEN
                 OK=.FALSE.
                 WRONG(I:I)='|'
            ELSE
                 OPEN=.TRUE.
                 NSW=NSW+1
                 EXIST=.FALSE.
            ENDIF
*   ")" close brackets,
       ELSEIF(TEXT(I:I).EQ.')')THEN
            IF(OPEN)THEN
                 OPEN=.FALSE.
                 IF(.NOT.EXIST)NSW=NSW-1
            ELSE
                 OK=.FALSE.
                 WRONG(I:I)='|'
            ENDIF
**  Wire, plane, tube and field map code in numeric form,
       ELSEIF(INDEX('+-0123456789',TEXT(I:I)).NE.0)THEN
            J=I
30          CONTINUE
            J=J+1
            IF(J.LE.NC.AND.INDEX('0123456789',TEXT(J:J)).NE.0)GOTO 30
            CALL INPRIC(TEXT(I:J-1),IREAD,0,IFAIL)
            IF(IFAIL.NE.0.OR.IREAD.LT.-5-MXWMAP.OR.IREAD.GT.NWIRE.OR.
     -           IREAD.EQ.0)THEN
                 WRONG(I:I)='#'
                 OK=.FALSE.
                 INEXT=J
                 GOTO 20
            ENDIF
            IF(IREAD.LT.0)IREAD=MXWIRE-IREAD
            IF(USED(IREAD))THEN
                 OK=.FALSE.
                 WRONG(I:I)='2'
            ELSE
                 IF(.NOT.OPEN.AND.NSW.GE.MXSW)THEN
                      PRINT *,' !!!!!! CELSEL WARNING : You have'//
     -                     ' selected more electrodes than the'//
     -                     ' program can store ; increase MXSW.'
                      OK=.FALSE.
                      DO 40 K=I,NC
                      IF(TEXT(K:K).NE.' ')WRONG(K:K)='.'
40                    CONTINUE
                      NSW=MXSW
                      GOTO 100
                 ENDIF
                 IF(IREAD.GE.MXWIRE+6.AND.IREAD.LE.MXWIRE+5+MXWMAP)THEN
                      IF(NWMAP.LT.IREAD-MXWIRE-5)THEN
                           WRONG(I:I)='M'
                           OK=.FALSE.
                      ELSE
                           IF(.NOT.OPEN)NSW=NSW+1
                           INDEWS(IREAD-MXWIRE-5)=NSW
                           USED(IREAD)=.TRUE.
                           EXIST=.TRUE.
                      ENDIF
                 ELSEIF(IREAD.EQ.MXWIRE+5)THEN
                      IF(.NOT.TUBE)THEN
                           WRONG(I:I)='T'
                           OK=.FALSE.
                      ELSE
                           IF(.NOT.OPEN)NSW=NSW+1
                           INDPLA(IREAD-MXWIRE)=NSW
                           USED(IREAD)=.TRUE.
                           EXIST=.TRUE.
                      ENDIF
                 ELSEIF(IREAD.GE.MXWIRE+1.AND.IREAD.LE.MXWIRE+4)THEN
                      IF(.NOT.YNPLAN(IREAD-MXWIRE))THEN
                           WRONG(I:I)='P'
                           OK=.FALSE.
                      ELSE
                           IF(.NOT.OPEN)NSW=NSW+1
                           INDPLA(IREAD-MXWIRE)=NSW
                           USED(IREAD)=.TRUE.
                           EXIST=.TRUE.
                      ENDIF
                 ELSE
                      IF(.NOT.OPEN)NSW=NSW+1
                      INDSW(IREAD)=NSW
                      USED(IREAD)=.TRUE.
                      EXIST=.TRUE.
                 ENDIF
            ENDIF
            INEXT=J
**  Wire and plane code as a letter,
       ELSEIF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',TEXT(I:I)).NE.0)THEN
            FOUND=.FALSE.
*   Check the wires.
            DO 60 J=1,NWIRE
            IF(WIRTYP(J).EQ.TEXT(I:I))THEN
                 EXIST=.TRUE.
                 FOUND=.TRUE.
                 IF(USED(J))THEN
                      OK=.FALSE.
                      WRONG(I:I)='2'
                      GOTO 60
                 ELSE
                      IF(.NOT.OPEN)NSW=NSW+1
                      IF(NSW.GT.MXSW)THEN
                           PRINT *,' !!!!!! CELSEL WARNING : You have'//
     -                          ' selected more electrodes than the'//
     -                          ' program can store ; increase MXSW.'
                           OK=.FALSE.
                           DO 50 K=I,NC
                           IF(TEXT(K:K).NE.' ')WRONG(K:K)='.'
50                         CONTINUE
                           NSW=MXSW
                           GOTO 100
                      ENDIF
                      INDSW(J)=NSW
                      USED(J)=.TRUE.
                 ENDIF
            ENDIF
60          CONTINUE
*   Check the planes and the tube.
            DO 180 J=1,5
            IF(J.LE.4)THEN
                 IF(.NOT.YNPLAN(J))GOTO 180
            ELSE
                 IF(.NOT.TUBE)GOTO 180
            ENDIF
            IF(PLATYP(J).EQ.TEXT(I:I))THEN
                 EXIST=.TRUE.
                 FOUND=.TRUE.
                 IF(USED(MXWIRE+J))THEN
                      OK=.FALSE.
                      WRONG(I:I)='2'
                      GOTO 180
                 ELSE
                      IF(.NOT.OPEN)NSW=NSW+1
                      IF(NSW.GT.MXSW)THEN
                           PRINT *,' !!!!!! CELSEL WARNING : You have'//
     -                          ' selected more electrodes than the'//
     -                          ' program can store ; increase MXSW.'
                           OK=.FALSE.
                           DO 190 K=I,NC
                           IF(TEXT(K:K).NE.' ')WRONG(K:K)='.'
190                        CONTINUE
                           NSW=MXSW
                           GOTO 100
                      ENDIF
                      INDPLA(J)=NSW
                      USED(MXWIRE+J)=.TRUE.
                 ENDIF
            ENDIF
180         CONTINUE
*   Check the strips on the planes and the tube.
            DO 240 J=1,5
            IF(J.LE.4)THEN
                 IF(.NOT.YNPLAN(J))GOTO 240
            ELSE
                 IF(.NOT.TUBE)GOTO 240
            ENDIF
            DO 280 K=1,NPSTR1(J)
            IF(PSLAB1(J,K).EQ.TEXT(I:I))THEN
                 EXIST=.TRUE.
                 FOUND=.TRUE.
                 IF(USED(MXWIRE+5+MXWMAP+(J-1)*MXPSTR+K))THEN
                      OK=.FALSE.
                      WRONG(I:I)='2'
                      GOTO 280
                 ELSE
                      IF(.NOT.OPEN)NSW=NSW+1
                      IF(NSW.GT.MXSW)THEN
                           PRINT *,' !!!!!! CELSEL WARNING : You have'//
     -                          ' selected more electrodes than the'//
     -                          ' program can store ; increase MXSW.'
                           OK=.FALSE.
                           DO 250 L=I,NC
                           IF(TEXT(L:L).NE.' ')WRONG(L:L)='.'
250                        CONTINUE
                           NSW=MXSW
                           GOTO 100
                      ENDIF
                      INDST1(J,K)=NSW
                      USED(MXWIRE+5+MXWMAP+(J-1)*MXPSTR+K)=.TRUE.
                 ENDIF
            ENDIF
280         CONTINUE
            DO 350 K=1,NPSTR2(J)
            IF(PSLAB2(J,K).EQ.TEXT(I:I))THEN
                 EXIST=.TRUE.
                 FOUND=.TRUE.
                 IF(USED(MXWIRE+5+MXWMAP+(J+4)*MXPSTR+K))THEN
                      OK=.FALSE.
                      WRONG(I:I)='2'
                      GOTO 350
                 ELSE
                      IF(.NOT.OPEN)NSW=NSW+1
                      IF(NSW.GT.MXSW)THEN
                           PRINT *,' !!!!!! CELSEL WARNING : You have'//
     -                          ' selected more electrodes than the'//
     -                          ' program can store ; increase MXSW.'
                           OK=.FALSE.
                           DO 360 L=I,NC
                           IF(TEXT(L:L).NE.' ')WRONG(L:L)='.'
360                        CONTINUE
                           NSW=MXSW
                           GOTO 100
                      ENDIF
                      INDST2(J,K)=NSW
                      USED(MXWIRE+5+MXWMAP+(J+4)*MXPSTR+K)=.TRUE.
                 ENDIF
            ENDIF
350         CONTINUE
240         CONTINUE
*   Check the field map.
            DO 270 J=1,NWMAP
            IF(EWSTYP(J).EQ.TEXT(I:I))THEN
                 EXIST=.TRUE.
                 FOUND=.TRUE.
                 IF(USED(MXWIRE+5+J))THEN
                      OK=.FALSE.
                      WRONG(I:I)='2'
                 ELSE
                      IF(.NOT.OPEN)NSW=NSW+1
                      IF(NSW.GT.MXSW)THEN
                           PRINT *,' !!!!!! CELSEL WARNING : You have'//
     -                          ' selected more electrodes than the'//
     -                          ' program can store ; increase MXSW.'
                           OK=.FALSE.
                           DO 210 K=I,NC
                           IF(TEXT(K:K).NE.' ')WRONG(K:K)='.'
210                        CONTINUE
                           NSW=MXSW
                           GOTO 100
                      ENDIF
                      INDEWS(J)=NSW
                      USED(MXWIRE+5+J)=.TRUE.
                 ENDIF
            ENDIF
270         CONTINUE
*   Check the neBEM field generating solids.
            IF(BEMSET)THEN
                 DO 430 J=1,NSOLID
                 IF(SOLTYP(J).EQ.TEXT(I:I))THEN
                      EXIST=.TRUE.
                      FOUND=.TRUE.
                      IF(USED(MXWIRE+5+MXWMAP+10*MXPSTR+J))THEN
                           OK=.FALSE.
                           WRONG(I:I)='2'
                      ELSE
                           IF(.NOT.OPEN)NSW=NSW+1
                           IF(NSW.GT.MXSW)THEN
                                PRINT *,' !!!!!! CELSEL WARNING :'//
     -                               ' More electrodes than can be'//
     -                               ' stored; increase MXSW.'
                                OK=.FALSE.
                                DO 130 K=I,NC
                                IF(TEXT(K:K).NE.' ')WRONG(K:K)='.'
130                             CONTINUE
                                NSW=MXSW
                                GOTO 100
                           ENDIF
                           INDSOL(J)=NSW
                           USED(MXWIRE+5+MXWMAP+10*MXPSTR+J)=.TRUE.
                      ENDIF
                 ENDIF
430              CONTINUE
*   Check the solids, do not assign new groups to these however.
            ELSE
                 DO 150 J=1,NSOLID
                 IF(SOLTYP(J).EQ.TEXT(I:I))THEN
                      FOUND=.TRUE.
                      SOLSEL=.TRUE.
                      INDSOL(J)=-1
                 ENDIF
150              CONTINUE
            ENDIF
*   See that something has been found.
            IF(.NOT.FOUND)THEN
                 OK=.FALSE.
                 WRONG(I:I)='?'
            ENDIF
**  invalid character.
       ELSE
            WRONG(I:I)='*'
            OK=.FALSE.
       ENDIF
*   Next selection character.
20     CONTINUE
*** Match solids and weighting field, if selected.
       DO 290 J=1,NWMAP
       IF(INDEWS(J).NE.0)THEN
            DO 70 I=1,NSOLID
            IF(SOLTYP(I).EQ.EWSTYP(J))THEN
                 IF(INDSOL(I).GT.0.AND.INDSOL(I).NE.INDEWS(J))THEN
                      PRINT *,' !!!!!! CELSEL WARNING : Solid ',I,
     -                     ' matches more than one field map.'
                      OK=.FALSE.
                 ELSE
                      INDSOL(I)=INDEWS(J)
                      SOLSEL=.TRUE.
                 ENDIF
            ENDIF
70          CONTINUE
       ENDIF
290    CONTINUE
*** Check that there are electrodes.
       IF(NSW.EQ.0.AND.SOLSEL)THEN
            PRINT *,' ------ CELSEL MESSAGE : You have only'//
     -           ' selected solids that are not read out.'
       ELSEIF(NSW.EQ.0.AND.JFAIL.EQ.1)THEN
            PRINT *,' !!!!!! CELSEL WARNING : No electrodes found'//
     -           ' that match your selection ; searching for "S".'
*   Consider wires.
            DO 80 I=1,NWIRE
            IF(WIRTYP(I).EQ.'S')THEN
                 NSW=NSW+1
                 INDSW(I)=NSW
            ENDIF
80          CONTINUE
*   Planes and tube.
            DO 200 I=1,5
            IF(PLATYP(I).EQ.'S')THEN
                 NSW=NSW+1
                 INDPLA(I)=NSW
            ENDIF
200         CONTINUE
*   Field map.
            DO 300 I=1,NWMAP
            IF(EWSTYP(I).EQ.'S')THEN
                 NSW=NSW+1
                 INDEWS(I)=NSW
            ENDIF
300         CONTINUE
            IF(NSW.GT.MXSW)NSW=MXSW
            IF(NSW.EQ.0)THEN
                 PRINT *,' !!!!!! CELSEL WARNING : The cell does not'//
     -                ' contain "S" electrodes ; nothing selected.'
                 NSW=0
            ENDIF
       ELSEIF(NSW.EQ.0.AND.JFAIL.EQ.2)THEN
            PRINT *,' !!!!!! CELSEL WARNING : No electrodes found'//
     -           ' that match your selection ; nothing selected.'
       ELSEIF(NSW.EQ.0.AND.JFAIL.EQ.3)THEN
            PRINT *,' !!!!!! CELSEL WARNING : No electrodes found'//
     -           ' that match your selection ; terminating.'
            CALL QUIT
       ENDIF
*** Print an error message if an error occured.
100    CONTINUE
       IF(WRONG(1:NC).NE.' ')WRITE(*,'(''  !!!!!! CELSEL WARNING : An'',
     -      '' error occured in the selection of electrodes''/
     -      9X,''Selection      : '',A/
     -      9X,''Error messages : '',A/
     -      9X,''Error codes    : '',
     -          ''"?" label not found,     "#" number out of range,''/
     -      26X,''"*" invalid character,   "|" unmatched bracket,''/
     -      26X,''"2" referenced twice,    "." (partially) ignored,''/
     -      26X,''"M" no such map,         "P" no such plane,''/
     -      26X,''"T" there is no tube.'')')
     -      TEXT(1:NC),WRONG(1:NC)
*** Print some extra output if the debug option is on/input is blank.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ CELSEL DEBUG   : Number of'',
     -           '' electrode groups: '',I5)') NSW
*   List wires.
            WRITE(LUNOUT,'(''  ++++++ CELSEL DEBUG   : The wires have'',
     -           '' been selected as follows:'')')
            DO 120 II=1,NWIRE,4
            WRITE(WRONG,'(25X,4(I3,'' '',A1,'' - '',I3,'', ''))')
     -           (I,WIRTYP(I),INDSW(I),I=II,MIN(II+3,NWIRE))
            DO 110 I=II,MIN(II+3,NWIRE)
            IF(INDSW(I).EQ.0)WRONG(34+(I-II)*13:36+(I-II)*13)='---'
110         CONTINUE
            IF(II+3.GE.NWIRE)WRONG(37+(NWIRE-II)*13:37+(NWIRE-II)*13)=
     -           '.'
            WRITE(LUNOUT,'(1X,A)') WRONG(1:76)
120         CONTINUE
*   List planes and tubes.
            WRITE(LUNOUT,'(''  ++++++ CELSEL DEBUG   : The planes'',
     -           '' have been selected as follows:'')')
            WRITE(WRONG,'(25X,4(I3,'' '',A1,'' - '',I3,'', ''))')
     -           (I,PLATYP(I),INDPLA(I),I=1,4)
            DO 140 I=1,4
            IF(INDPLA(I).EQ.0)WRONG(21+I*13:23+I*13)='---'
140         CONTINUE
            WRITE(LUNOUT,'(1X,A)') WRONG(1:76)
            WRITE(LUNOUT,'(''  ++++++ CELSEL DEBUG   : The tube'',
     -           '' has been selected as follows:'')')
            WRITE(WRONG,'(25X,I3,'' '',A1,'' - '',I3,''.'')')
     -           1,PLATYP(5),INDPLA(5)
            IF(INDPLA(5).EQ.0)WRONG(34:36)='---'
            WRITE(LUNOUT,'(1X,A)') WRONG(1:76)
*   List strips.
            DO 370 I=1,5
            IF(NPSTR1(I).NE.0)WRITE(LUNOUT,'(''  ++++++ CELSEL'',
     -           '' DEBUG   : The x-y strips of plane '',I3,
     -           '' have been selected as follows:'')') I
            DO 380 II=1,NPSTR1(I),4
            WRITE(WRONG,'(25X,4(I3,'' '',A1,'' - '',I3,'', ''))')
     -           (J,PSLAB1(I,J),INDST1(I,J),J=II,MIN(II+3,NPSTR1(I)))
            DO 390 J=II,MIN(II+3,NPSTR1(I))
            IF(INDST1(I,J).EQ.0)WRONG(34+(J-II)*13:36+(J-II)*13)='---'
390         CONTINUE
            IF(II+3.GE.NPSTR1(I))WRONG(37+(NPSTR1(I)-II)*13:
     -           37+(NPSTR1(I)-II)*13)='.'
            WRITE(LUNOUT,'(1X,A)') WRONG(1:76)
380         CONTINUE
            IF(NPSTR2(I).NE.0)WRITE(LUNOUT,'(''  ++++++ CELSEL'',
     -           '' DEBUG   : The z strips of plane '',I3,
     -           '' have been selected as follows:'')') I
            DO 400 II=1,NPSTR2(I),4
            WRITE(WRONG,'(25X,4(I3,'' '',A1,'' - '',I3,'', ''))')
     -           (J,PSLAB2(I,J),INDST2(I,J),J=II,MIN(II+3,NPSTR2(I)))
            DO 410 J=II,MIN(II+3,NPSTR2(I))
            IF(INDST2(I,J).EQ.0)WRONG(34+(J-II)*13:36+(J-II)*13)='---'
410         CONTINUE
            IF(II+3.GE.NPSTR2(I))WRONG(37+(NPSTR2(I)-II)*13:
     -           37+(NPSTR2(I)-II)*13)='.'
            WRITE(LUNOUT,'(1X,A)') WRONG(1:76)
400         CONTINUE
370         CONTINUE
*   List the field maps.
            WRITE(LUNOUT,'(''  ++++++ CELSEL DEBUG   : The field'',
     -           '' maps have been selected as follows:'')')
            DO 310 II=1,NWMAP,4
            WRITE(WRONG,'(25X,4(I3,'' '',A1,'' - '',I3,'', ''))')
     -           (I,EWSTYP(I),INDEWS(I),I=II,MIN(II+3,NWMAP))
            DO 320 I=II,MIN(II+3,NWMAP)
            IF(INDEWS(I).EQ.0)WRONG(34+(I-II)*13:36+(I-II)*13)='---'
320         CONTINUE
            IF(II+3.GE.NWMAP)WRONG(37+(NWMAP-II)*13:
     -           37+(NWMAP-II)*13)='.'
            WRITE(LUNOUT,'(1X,A)') WRONG(1:76)
310         CONTINUE
*   List solids.
            WRITE(LUNOUT,'(''  ++++++ CELSEL DEBUG   : The solids'',
     -           '' have been selected as follows:'')')
            DO 220 II=1,NSOLID,4
            WRITE(WRONG,'(25X,4(I3,'' '',A1,'' - '',I3,'', ''))')
     -           (I,SOLTYP(I),INDSOL(I),I=II,MIN(II+3,NSOLID))
            DO 230 I=II,MIN(II+3,NSOLID)
            IF(INDSOL(I).EQ.0)WRONG(34+(I-II)*13:36+(I-II)*13)='---'
230         CONTINUE
            IF(II+3.GE.NSOLID)WRONG(37+(NSOLID-II)*13:
     -           37+(NSOLID-II)*13)='.'
            WRITE(LUNOUT,'(1X,A)') WRONG(1:76)
220         CONTINUE
       ENDIF
*** Request weighting fields.
       IF(BEMSET)THEN
*   Loop over read-out groups.
            DO 510 I=1,NSW
*   Pick up the associated solids.
            NPRIM=0
            DO 520 J=1,NSOLID
            IF(INDSOL(J).EQ.I)THEN
*   Pick up the primitives for the solid.
                 CALL BEMVPR(J,NPRIMJ,PRIMLJ)
                 DO 530 K=1,NPRIMJ
                 NPRIM=NPRIM+1
                 PRIML(NPRIM)=PRIMLJ(K)
530              CONTINUE
            ENDIF
520         CONTINUE
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ CELSEL DEBUG   : '',I5,
     -           '' associated primitives:'',
     -           (/26X,20I4))') NPRIM,(PRIML(J),J=1,NPRIM)
*   Request the weighting field for this list of primitives.
            CALL BEMRQW(NPRIM,PRIML,IWFBEM(I))
*   Check the return code.
            IF(IWFBEM(I).LT.0)THEN
                 PRINT *,' !!!!!! CELSEL WARNING : Failure computing'//
     -                ' neBEM weighting field charges; no selected'//
     -                ' electrodes.'
                 NSW=0
            ENDIF
510         CONTINUE
       ENDIF
       END
