CDECK  ID>, GASPCS.
       SUBROUTINE GASPCS(GASID)
*-----------------------------------------------------------------------
*   GASPCS - Plots the cross sections.
*   (Last changed on  2/ 3/08.)
*-----------------------------------------------------------------------
       implicit none
*   Array dimensions.
       integer mxngas
       parameter(mxngas=6)
*   Grouped QIN1 ... QIN6 in QINN
       DOUBLE PRECISION QELM,QSUM,QION,QINN,QSATT
       COMMON /MIX1/ QELM(2048),QSUM(2048),QION(mxngas,2048),
     -      QINn(220,2048,mxngas),QSATT(2048)
*   EVECT is originally called E or ES depending on the routine.
       DOUBLE PRECISION Evect,EROOT,QTOT,QREL,QINEL,QEL
       COMMON /MIX2/ Evect(2048),EROOT(2048),QTOT(2048),QREL(2048),
     -      QINEL(2048),QEL(2048)
*   Extensively reduced.
       INTEGER NINn
*           ,LION,LIN1,LIN2,LIN3,LIN4,LIN5,LIN6
*           DOUBLE PRECISION ALION,ALIN1,ALIN2,ALIN3,ALIN4,ALIN5,ALIN6
       COMMON /MIX3/ NINn(mxngas)
*           ,LION(6),LIN1(220),
*     -      LIN2(220),LIN3(220),LIN4(220),LIN5(220),LIN6(220),ALION(6),
*     -      ALIN1(220),ALIN2(220),ALIN3(220),ALIN4(220),ALIN5(220),
*     -      ALIN6(220)
*   Adjusted size of ICOLL
       DOUBLE PRECISION TIME,SPEC,TMAX1,AVE,DEN,XID,X,Y,Z,ST
       INTEGER ICOLL,NNULL,ICOLN
       COMMON/OUTPT/TIME(300),ICOLL(5*mxngas),SPEC(2048),TMAX1,
     -      AVE,DEN,XID,X,Y,Z,ST,NNULL,ICOLN(512)
       DOUBLE PRECISION EOVB,WB,BTHETA,BMAG
       COMMON/BFLD/EOVB,WB,BTHETA,BMAG
       DOUBLE PRECISION TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX,RSTART,
     -      EMAG
       INTEGER NMAX
       COMMON/SETP/TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX(8),RSTART,
     -      EMAG,NMAX
       INTEGER NGAS,NSTEP,IDBG
       DOUBLE PRECISION EFINAL,ESTEP,AKT,ARY,TEMPC,TORR
       PARAMETER(ARY=13.60569172)
       COMMON/INPT/NGAS,NSTEP,EFINAL,ESTEP,AKT,TEMPC,TORR,IDBG
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
       CHARACTER*100 AUX
       CHARACTER*(*) GASID
       DOUBLE PRECISION YAUX(2048),YMIN,YMAX
       INTEGER NC,STRLEN,I,J
       EXTERNAL STRLEN
*** Determine the range
       YMIN=-1
       YMAX=-1
       DO 30 I=1,2048
       IF((YMIN.LT.0.OR.YMAX.LT.0).AND.QTOT(I).GT.0)THEN
            YMIN=QTOT(I)
            YMAX=QTOT(I)
       ELSEIF(QTOT(I).GT.0)THEN
            YMIN=MIN(YMIN,QTOT(I))
            YMAX=MAX(YMAX,QTOT(I))
       ENDIF
       IF((YMIN.LT.0.OR.YMAX.LT.0).AND.QEL(I).GT.0)THEN
            YMIN=QEL(I)
            YMAX=QEL(I)
       ELSEIF(QEL(I).GT.0)THEN
            YMIN=MIN(YMIN,QEL(I))
            YMAX=MAX(YMAX,QEL(I))
       ENDIF
       IF((YMIN.LT.0.OR.YMAX.LT.0).AND.QSATT(I).GT.0)THEN
            YMIN=QSATT(I)
            YMAX=QSATT(I)
       ELSEIF(QSATT(I).GT.0)THEN
            YMIN=MIN(YMIN,QSATT(I))
            YMAX=MAX(YMAX,QSATT(I))
       ENDIF
       DO 40 J=1,NGAS
       IF((YMIN.LT.0.OR.YMAX.LT.0).AND.QION(J,I).GT.0)THEN
            YMIN=QION(J,I)
            YMAX=QION(J,I)
       ELSEIF(QION(J,I).GT.0)THEN
            YMIN=MIN(YMIN,QION(J,I))
            YMAX=MAX(YMAX,QION(J,I))
       ENDIF
40     CONTINUE
30     CONTINUE
*** Plot frame.
       CALL GRAOPT('LOG-X,LOG-Y')
       CALL GRCART(0.9*REAL(EVECT(1)),0.9*REAL(YMIN),
     -      1.1*REAL(EVECT(2048)),1.1*REAL(YMAX),
     -      'Energy [eV]',
     -      'Density corrected inverse mean free path [1/cm]',
     -      'Cross section terms')
*   Prepare the gas label.
       IF(GASID.NE.' ')THEN
            AUX(1:5)='Gas: '
            AUX(6:)=GASID
            CALL GRCOMM(4,AUX(1:STRLEN(GASID)+5))
       ENDIF
*   Prepare the E, B and angle labels.
       CALL OUTFMT(REAL(EMAG),2,AUX,NC,'LEFT')
       CALL GRCOMM(1,'E = '//AUX(1:NC)//' V/cm')
       CALL OUTFMT(REAL(BMAG/10),2,AUX,NC,'LEFT')
       CALL GRCOMM(2,'B = '//AUX(1:NC)//' T')
       CALL OUTFMT(REAL(BTHETA),2,AUX,NC,'LEFT')
       CALL GRCOMM(3,'Angle = '//AUX(1:NC)//' degrees')
*** Add the total cross section.
       CALL GRATTS('FUNCTION-1','POLYLINE')
       CALL GRLIN2(2048,EVECT,QTOT)
*   Add the elastic term.
       CALL GRATTS('FUNCTION-2','POLYLINE')
       CALL GRLIN2(2048,EVECT,QEL)
*   Add the attachment term.
       CALL GRATTS('FUNCTION-3','POLYLINE')
       CALL GRLIN2(2048,EVECT,QSATT)
*   Add the ionisation terms.
       CALL GRATTS('FUNCTION-4','POLYLINE')
       DO 10 I=1,NGAS
       DO 20 J=1,2048
       YAUX(J)=QION(I,J)
20     CONTINUE
       CALL GRLIN2(2048,EVECT,YAUX)
10     CONTINUE
*** Close the plot and register.
       CALL GRNEXT
       CALL GRALOG('Magboltz cross section plot:')
       END
