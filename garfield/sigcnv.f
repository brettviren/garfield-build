CDECK  ID>, SIGCNV.
       SUBROUTINE SIGCNV(IFAIL)
*-----------------------------------------------------------------------
*   SIGCNV - Convolutes the signals with a transfer function.
*   VARIABLES :
*   (Last changed on  1/ 2/00.)
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
       LOGICAL FPERX,FPERY,LCROSS,TRASET,TRAFLG,LITAIL,LDTAIL,LRTAIL,
     -      LEPULS,LIPULS,SIGSET,RESSET
       INTEGER NPAIR,ICLUST,NFOUR,MFEXP,MXMIN,MXMAX,
     -      MYMIN,MYMAX,NTRBNK,ITRMAJ,NTIME,NORIA,
     -      NASIMP,JIORD,NISIMP,NMQUAD,NCANG,IENANG
       REAL TIMSIG,SIGNAL,TCLUST,SCLUST,ACLUST,BCLUST,FCLUST,
     -      AVALAN,TSTART,TDEV,PRSTHR,
     -      TRABNK,TRAVEC
       CHARACTER*(MXCHAR) FCNANG
       CHARACTER*12 AVATYP
       CHARACTER*3 FCELTP
       COMMON /SIGDAT/ TIMSIG(MXLIST),SIGNAL(MXLIST,MXSW,2),
     -      AVALAN(2),TRAVEC(MXLIST),
     -      TRABNK(MXLIST,9),TSTART,TDEV,PRSTHR,
     -      TCLUST,SCLUST,ACLUST,BCLUST,FCLUST,ICLUST,NPAIR,
     -      NFOUR,ITRMAJ,JIORD,NISIMP,NMQUAD,IENANG,NTIME,NORIA,
     -      MFEXP,MXMIN,MXMAX,MYMIN,MYMAX,NTRBNK,NASIMP,NCANG,
     -      TRASET,TRAFLG(9),FPERX,FPERY,LCROSS,LITAIL,LDTAIL,LRTAIL,
     -      LEPULS,LIPULS,SIGSET,RESSET
       COMMON /SIGCHR/ FCELTP,AVATYP,FCNANG
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
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(MXCHAR) FCNCNV,FCNADD
       CHARACTER*10 VAR(MXVAR),NAME
       LOGICAL USE(MXVAR)
       INTEGER MODVAR(MXVAR),MODRES(1),IENTRY,IENADD,NCCNV,NCADD,IORD,
     -      IORDR,I,J,K,IFAIL,IRTRAN,ISTRAN,IRTIME,ISTIME,MATSLT,NCNAME,
     -      NERR,NRES,ISW,IFAIL1,IFAIL2,INEXT,NWORD,INPCMP
       REAL RES(1),AUX(MXLIST),CNVTAB(1-MXLIST:MXLIST-1),
     -      VAL(MXVAR),CNVMIN,CNVMAX,CNVMIR,CNVMAR
       EXTERNAL MATSLT,INPCMP
       SAVE FCNCNV,NCCNV,CNVMIN,CNVMAX,IENTRY,IRTRAN,IRTIME,IORD
       DATA FCNCNV(1:1)/' '/
       DATA NCCNV /1/, CNVMIN /0.0/, CNVMAX /1.0E10/, IENTRY /0/,
     -      IRTRAN /0/, IRTIME /0/, IORD /2/
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE SIGCNV ///'
*** Reset the add-on function each time.
       NCADD=1
       IENADD=0
       FCNADD=' '
*** Reset matrix slot numbers.
       ISTIME=0
       ISTRAN=0
*** Get hold of the number of words.
       CALL INPNUM(NWORD)
*** Read the words.
       INEXT=1
       DO 100 I=2,NWORD
       IF(INEXT.GT.I)GOTO 100
*   Check for TRANSFER-FUNCTION.
       IF(INPCMP(I,'TR#ANSFER-F#UNCTION').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'The function is not specified.')
            ELSEIF(INPCMP(I+2,'VS').NE.0.AND.I+3.LE.NWORD)THEN
                 IRTIME=0
                 IRTRAN=0
                 CALL INPSTR(I+1,I+1,NAME,NCNAME)
                 DO 110 J=1,NGLB
                 IF(GLBMOD(J).EQ.5.AND.GLBVAR(J).EQ.NAME(1:NCNAME))
     -                IRTRAN=NINT(GLBVAL(J))
110              CONTINUE
                 ISTRAN=MATSLT(IRTRAN)
                 CALL INPSTR(I+3,I+3,NAME,NCNAME)
                 DO 120 J=1,NGLB
                 IF(GLBMOD(J).EQ.5.AND.GLBVAR(J).EQ.NAME(1:NCNAME))
     -                IRTIME=NINT(GLBVAL(J))
120              CONTINUE
                 ISTIME=MATSLT(IRTIME)
                 IF(ISTIME.EQ.0)CALL INPMSG(I+3,
     -                'Not known or not a Matrix')
                 IF(ISTRAN.EQ.0)CALL INPMSG(I+1,
     -                'Not known or not a Matrix')
                 INEXT=I+4
                 IF(IENTRY.GT.0)THEN
                      CALL ALGCLR(IENTRY)
                      IENTRY=0
                 ENDIF
            ELSE
                 CALL INPSTR(I+1,I+1,FCNCNV,NCCNV)
                 IF(NCCNV.GT.0.AND.IENTRY.GT.0)THEN
                      CALL ALGCLR(IENTRY)
                      IENTRY=0
                 ENDIF
                 INEXT=I+2
                 IRTIME=0
                 IRTRAN=0
            ENDIF
*   Check for ADD.
       ELSEIF(INPCMP(I,'ADD-#ON-#FUNCTION').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'The function is not specified.')
            ELSE
                 CALL INPSTR(I+1,I+1,FCNADD,NCADD)
            ENDIF
            INEXT=I+2
*   Check for RANGE.
       ELSEIF(INPCMP(I,'RAN#GE').NE.0)THEN
            IF(I+2.GT.NWORD)THEN
                 CALL INPMSG(I,'RANGE incompletely specified.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,CNVMIR,0.0)
                 CALL INPRDR(I+2,CNVMAR,1.0E10)
                 IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)THEN
                      CNVMIN=0.0
                      CNVMAX=1.0E10
                 ELSEIF(CNVMIR.EQ.CNVMAR)THEN
                      CALL INPMSG(I+1,'Zero range not permitted.')
                      CALL INPMSG(I+2,'See previous message.')
                      CNVMIN=0.0
                      CNVMAX=1.0E10
                 ELSE
                      CNVMIN=MIN(CNVMIR,CNVMAR)
                      CNVMAX=MAX(CNVMIR,CNVMAR)
                 ENDIF
            ENDIF
            INEXT=I+3
*   Check for ORDER.
       ELSEIF(INPCMP(I,'ORD#ER').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'ORDER incompletely specified.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,IORDR,IORD)
                 IF(IFAIL1.EQ.0.AND.IORDR.GE.1)THEN
                      IORD=IORDR
                 ELSEIF(IFAIL1.EQ.0)THEN
                      CALL INPMSG(I+1,'Should be 1 or larger.')
                 ENDIF
            ENDIF
            INEXT=I+2
*   Other keywords are not recognised.
       ELSE
            CALL INPMSG(I,'Unknown keyword.')
       ENDIF
100    CONTINUE
*** Print error messages.
       CALL INPERR
*** Debugging output.
       IF(LDEBUG)THEN
            IF(NCCNV.LE.0)THEN
                 WRITE(LUNOUT,'(''  ++++++ SIGCNV DEBUG   : No'',
     -                '' transfer function, IENTRY='',I5)') IENTRY
            ELSE
                 WRITE(LUNOUT,'(''  ++++++ SIGCNV DEBUG   :'',
     -                '' Transfer function: '',A/26X,''Valid for '',
     -                E15.8,'' <= t <= '',E15.8,'' [microsec]''/26X,
     -                ''Add-on function: '',A/26X,
     -                ''Interpolate global '',I5,'' vs '',I5/26X,
     -                ''Entry point transfer function: '',I5)')
     -                FCNCNV(1:MAX(1,NCCNV)),CNVMIN,CNVMAX,
     -                FCNADD(1:MAX(1,NCADD)),IRTRAN,IRTIME,IENTRY
            ENDIF
       ENDIF
*** Ensure that there is a transfer function.
       IF(IENTRY.LE.0.AND.
     -      (ISTIME.LE.0.OR.ISTRAN.LE.0).AND.
     -      (NCCNV.LE.0.OR.FCNCNV.EQ.' '))THEN
            PRINT *,' !!!!!! SIGCNV WARNING : No transfer function'//
     -           ' available ; no convolution done.'
            IFAIL=1
            RETURN
       ENDIF
*** Test for the time range.
       IF(.NOT.RESSET)THEN
            PRINT *,' !!!!!! SIGCNV WARNING : The time window has'//
     -           ' not yet been set; no convolution done.'
            IFAIL=1
            RETURN
       ENDIF
*** Translate the transfer function, if there is no entry point yet.
       IF(IENTRY.LE.0.AND.(ISTIME.LE.0.OR.ISTRAN.LE.0))THEN
            VAR(1)='T         '
            CALL ALGPRE(FCNCNV(1:NCCNV),NCCNV,VAR,1,
     -           NRES,USE,IENTRY,IFAIL1)
*   Verify that the translation worked correctly.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! SIGCNV WARNING : Transfer function'//
     -                ' could not be translated ; no convolutions done.'
                 CALL ALGCLR(IENTRY)
                 IENTRY=0
                 NCCNV=0
                 IFAIL=1
                 RETURN
*   Make sure that there is only one result coming back.
            ELSEIF(NRES.NE.1)THEN
                 PRINT *,' !!!!!! SIGCNV WARNING : The transfer'//
     -                ' function does not return 1 result ; no'//
     -                ' convolutions done.'
                 CALL ALGCLR(IENTRY)
                 IENTRY=0
                 NCCNV=0
                 IFAIL=1
                 RETURN
*   Ensure there is a time dependence.
            ELSEIF(.NOT.USE(1))THEN
                 PRINT *,' ------ SIGCNV MESSAGE : The transfer'//
     -                ' function does not depend on T.'
            ENDIF
       ENDIF
*** Translate the add function, if there is no entry point yet.
       IF(FCNADD.NE.' ')THEN
            VAR(1)='T         '
            VAR(2)='SIGNAL    '
            CALL ALGPRE(FCNADD(1:NCADD),NCADD,VAR,2,
     -           NRES,USE,IENADD,IFAIL1)
*   Verify that the translation worked correctly.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! SIGCNV WARNING : The add function'//
     -                ' could not be translated ; nothing added.'
                 CALL ALGCLR(IENADD)
                 IENADD=0
                 NCADD=0
*   Make sure that there is only one result coming back.
            ELSEIF(NRES.NE.1)THEN
                 PRINT *,' !!!!!! SIGCNV WARNING : The add-on'//
     -                ' function does not return 1 result ; nothing'//
     -                ' added.'
                 CALL ALGCLR(IENADD)
                 IENADD=0
                 NCADD=0
            ENDIF
       ELSE
            IENADD=0
       ENDIF
*** Reset the error counter.
       NERR=0
*** Evaluate the transfer function.
       ISTIME=0
       ISTRAN=0
       DO 90 I=1,NTIME
*   Negative time part.
       VAL(1)=TIMSIG(1)-TIMSIG(I)
       MODVAR(1)=2
       IF(VAL(1).LT.CNVMIN.OR.VAL(1).GT.CNVMAX)THEN
            CNVTAB(1-I)=0
       ELSEIF(IENTRY.GT.0)THEN
            CALL ALGEXE(IENTRY,VAL,MODVAR,1,RES,MODRES,1,IFAIL1)
            IF(IFAIL1.EQ.0.AND.MODRES(1).EQ.2)THEN
                 CNVTAB(1-I)=RES(1)
            ELSE
                 CNVTAB(1-I)=0
                 NERR=NERR+1
            ENDIF
       ELSE
            IF(NERR.EQ.0)THEN
                 CALL MATIN1(IRTIME,IRTRAN,1,VAL(1),CNVTAB(1-I),
     -                ISTIME,ISTRAN,IORD,IFAIL1)
                 IF(IFAIL1.NE.0)NERR=NERR+1
            ELSE
                 CNVTAB(1-I)=0
                 NERR=NERR+1
            ENDIF
       ENDIF
*   Positive time part.
       IF(I.EQ.1)GOTO 90
       VAL(1)=TIMSIG(I)-TIMSIG(1)
       MODVAR(1)=2
       IF(VAL(1).LT.CNVMIN.OR.VAL(1).GT.CNVMAX)THEN
            CNVTAB(I-1)=0
       ELSEIF(IENTRY.GT.0)THEN
            CALL ALGEXE(IENTRY,VAL,MODVAR,1,RES,MODRES,1,IFAIL1)
            IF(IFAIL1.EQ.0.AND.MODRES(1).EQ.2)THEN
                 CNVTAB(I-1)=RES(1)
            ELSE
                 CNVTAB(I-1)=0
                 NERR=NERR+1
            ENDIF
       ELSE
            IF(NERR.EQ.0)THEN
                 CALL MATIN1(IRTIME,IRTRAN,1,VAL(1),CNVTAB(I-1),
     -                ISTIME,ISTRAN,IORD,IFAIL1)
                 IF(IFAIL1.NE.0)NERR=NERR+1
            ELSE
                 NERR=NERR+1
                 CNVTAB(I-1)=0
            ENDIF
       ENDIF
90     CONTINUE
*** Print error messages, if applicable.
       IF(NERR.NE.0)PRINT *,' !!!!!! SIGCNV WARNING : In total ',NERR,
     -      ' terms skipped in convolutions for arithmetic/mode errors.'
       CALL ALGERR
       NERR=0
*** Loop over all (groups of) sense wires.
       DO 10 ISW=1,NSW
       DO 20 J=1,NTIME
*   Add the add-on function.
       IF(IENADD.GT.0)THEN
            VAL(1)=TIMSIG(J)
            MODVAR(1)=2
            VAL(2)=SIGNAL(J,ISW,1)
            MODVAR(2)=2
            CALL ALGEXE(IENADD,VAL,MODVAR,2,RES,MODRES,1,IFAIL1)
            IF(IFAIL1.NE.0.OR.MODRES(1).NE.2)THEN
                AUX(J)=0
                NERR=NERR+1
            ELSE
                AUX(J)=RES(1)
            ENDIF
       ELSE
            AUX(J)=0
       ENDIF
*   Do the actual convolution.
       DO 30 K=1,NTIME
       AUX(J)=AUX(J)+TDEV*CNVTAB(J-K)*SIGNAL(K,ISW,1)
30     CONTINUE
20     CONTINUE
       DO 70 J=1,NTIME
       SIGNAL(J,ISW,1)=AUX(J)
70     CONTINUE
**  Cross induced signals.
       IF(LCROSS)THEN
            DO 40 J=1,NTIME
*   Add the add-on function.
            IF(IENADD.GT.0)THEN
                 VAL(1)=TIMSIG(J)
                 MODVAR(1)=2
                 VAL(2)=SIGNAL(J,ISW,2)
                 MODVAR(2)=2
                 CALL ALGEXE(IENADD,VAL,MODVAR,2,RES,MODRES,1,IFAIL1)
                 IF(IFAIL1.NE.0.OR.MODRES(1).NE.2)THEN
                     AUX(J)=0
                     NERR=NERR+1
                 ELSE
                     AUX(J)=RES(1)
                 ENDIF
            ELSE
                 AUX(J)=0
            ENDIF
*   Do the actual convolutions.
            DO 50 K=1,NTIME
            AUX(J)=AUX(J)+TDEV*CNVTAB(J-K)*SIGNAL(K,ISW,2)
50          CONTINUE
40          CONTINUE
            ENDIF
            DO 60 J=1,NTIME
            SIGNAL(J,ISW,2)=AUX(J)
60          CONTINUE
10     CONTINUE
*** Print error messages.
       IF(NERR.NE.0)PRINT *,' !!!!!! SIGCNV WARNING : In total ',NERR,
     -      ' add-on terms skipped for arithmetic/mode errors.'
       CALL ALGERR
*** Get rid of add function.
       IF(IENADD.GT.0)CALL ALGCLR(IENADD)
*** Things seem to have worked.
       IFAIL=0
*** Register the amount of CPU time used.
       CALL TIMLOG('Convoluting with transfer function:     ')
       END
