CDECK  ID>, SIGNOI.
       SUBROUTINE SIGNOI(IFAIL)
*-----------------------------------------------------------------------
*   SIGNOI - Adds noise to the signals,
*   VARIABLES :
*   (Last changed on 16/ 1/00.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(MXCHAR) FCNNOI
       CHARACTER*10 VAR(MXVAR)
       LOGICAL USE(MXVAR)
       INTEGER MODVAR(MXVAR),MODRES(1),IENTRY,NCNOI,I,J,INEXT,IFAIL,
     -      IFAIL1,NRES,NWORD,ISW,NERR,INPCMP
       REAL RES(1),AUX(MXLIST),VAL(MXVAR)
       EXTERNAL INPCMP
       SAVE FCNNOI,NCNOI,IENTRY
       DATA FCNNOI(1:1)/' '/
       DATA NCNOI /1/, IENTRY /0/
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE SIGNOI ///'
*** Get hold of the number of words.
       CALL INPNUM(NWORD)
*** Read the words.
       INEXT=1
       DO 100 I=2,NWORD
       IF(INEXT.GT.I)GOTO 100
*   Check for NOISE-FUNCTION.
       IF(INPCMP(I,'NOISE-#FUNCTION').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'The function is not specified.')
            ELSE
                 CALL INPSTR(I+1,I+1,FCNNOI,NCNOI)
                 IF(NCNOI.GT.0.AND.IENTRY.GT.0)THEN
                      CALL ALGCLR(IENTRY)
                      IENTRY=0
                 ENDIF
            ENDIF
            INEXT=I+2
*   Other keywords are not known.
       ELSE
            CALL INPMSG(I,'Unknown keyword.')
       ENDIF
100    CONTINUE
*** Print error messages.
       CALL INPERR
*** Debugging output.
       IF(LDEBUG)THEN
            IF(NCNOI.LE.0)THEN
                 WRITE(LUNOUT,'(''  ++++++ SIGNOI DEBUG   : No'',
     -                '' noise function, IENTRY='',I5)') IENTRY
            ELSE
                 WRITE(LUNOUT,'(''  ++++++ SIGNOI DEBUG   :'',
     -                '' Noise function: '',A/26X,''Entry='',I5)')
     -                FCNNOI(1:NCNOI),IENTRY
            ENDIF
       ENDIF
*** Ensure that there is a noise function.
       IF(IENTRY.LE.0.AND.(NCNOI.LE.0.OR.FCNNOI.EQ.' '))THEN
            PRINT *,' !!!!!! SIGNOI WARNING : No noise function'//
     -           ' available ; no noise added.'
            IFAIL=1
            RETURN
       ENDIF
*** Test for the time range.
       IF(.NOT.RESSET)THEN
            PRINT *,' !!!!!! SIGNOI WARNING : The time window has'//
     -           ' not yet been set; no noise added.'
            IFAIL=1
            RETURN
       ENDIF
*** Translate the noise function, if there is no entry point yet.
       IF(IENTRY.LE.0)THEN
            VAR(1)='T         '
            CALL ALGPRE(FCNNOI(1:NCNOI),NCNOI,VAR,1,
     -           NRES,USE,IENTRY,IFAIL1)
*   Verify that the translation worked correctly.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! SIGNOI WARNING : Noise function'//
     -                ' could not be translated ; no noise added.'
                 CALL ALGCLR(IENTRY)
                 IENTRY=0
                 NCNOI=0
                 IFAIL=1
                 RETURN
*   Make sure that there is only one result coming back.
            ELSEIF(NRES.NE.1)THEN
                 PRINT *,' !!!!!! SIGNOI WARNING : The noise'//
     -                ' function does not return 1 result ; no'//
     -                ' noise added.'
                 CALL ALGCLR(IENTRY)
                 IENTRY=0
                 NCNOI=0
                 IFAIL=1
                 RETURN
*   Ensure there is a time dependence.
C           ELSEIF(.NOT.USE(1))THEN
C                PRINT *,' ------ SIGNOI WARNING : The noise'//
C    -                ' function does not depend on T.'
            ENDIF
       ENDIF
*** Reset the error counter.
       NERR=0
*** Loop over all (groups of) sense wires.
       DO 10 ISW=1,NSW
       DO 20 J=1,NTIME
       VAL(1)=TIMSIG(J)
       MODVAR(1)=2
       CALL ALGEXE(IENTRY,VAL,MODVAR,1,RES,MODRES,1,IFAIL1)
       IF(IFAIL1.EQ.0.AND.MODRES(1).EQ.2)THEN
            AUX(J)=RES(1)
       ELSE
            AUX(J)=0
            NERR=NERR+1
       ENDIF
20     CONTINUE
       DO 30 J=1,NTIME
       SIGNAL(J,ISW,1)=SIGNAL(J,ISW,1)+AUX(J)
       IF(LCROSS)SIGNAL(J,ISW,2)=SIGNAL(J,ISW,2)+AUX(J)
30     CONTINUE
10     CONTINUE
*** Print error messages, if applicable.
       IF(NERR.NE.0)PRINT *,' !!!!!! SIGNOI WARNING : In total ',NERR,
     -      ' noise terms skipped for arithmetic/mode errors.'
       CALL ALGERR
*** Things seem to have worked.
       IFAIL=0
*** Register the amount of CPU time used.
       CALL TIMLOG('Adding noise to the signals:            ')
       END
