CDECK  ID>, SIGPLP.
       SUBROUTINE SIGPLP(IFAIL)
*-----------------------------------------------------------------------
*   SIGPLP - Computes the weighting field charges for the planes and
*            the tube.
*   (Last changed on 14/10/99.)
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
       COMPLEX SIGMAT
       REAL QPLANE,EWXCOR,EWYCOR
       INTEGER IWORK,DUMMY
       COMMON /MATRIX/ SIGMAT(MXWIRE,MXWIRE),QPLANE(5,MXWIRE),
     -      IWORK(MXWIRE),DUMMY(2*MXWIRE+6)
       COMMON /SPLDAT/ EWXCOR(5),EWYCOR(5)
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL VW
       INTEGER MX,MY,IFAIL,IFAIL1,I,J
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE SIGPLP ///'
*** Assume this will fail.
       IFAIL=1
*** Loop over the signal layers.
       DO 100 MX=MXMIN,MXMAX
       DO 110 MY=MYMIN,MYMAX
*** Load the layers of the signal matrices.
       CALL IONIO(MX,MY,2,0,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! SIGPLP WARNING : Signal matrix'//
     -           ' store error; field for planes not prepared.'
            RETURN
       ENDIF
*** Initialise the plane matrices.
       DO 120 I=1,5
       DO 130 J=1,NWIRE
       QPLANE(I,J)=0
130    CONTINUE
120    CONTINUE
*** Charges for plane 1, if present.
       IF(YNPLAN(1))THEN
*   Set the weighting field voltages.
            DO 10 I=1,NWIRE
            IF(YNPLAN(2))THEN
                 VW=-(COPLAN(2)-X(I))/(COPLAN(2)-COPLAN(1))
            ELSEIF(PERX)THEN
                 VW=-(COPLAN(1)+SX-X(I))/SX
            ELSE
                 VW=-1
            ENDIF
*   Multiply with the matrix.
            DO 20 J=1,NWIRE
            QPLANE(1,I)=QPLANE(1,I)+SIGMAT(I,J)*VW
20          CONTINUE
10          CONTINUE
       ENDIF
*** Charges for plane 2, if present.
       IF(YNPLAN(2))THEN
*   Set the weighting field voltages.
            DO 30 I=1,NWIRE
            IF(YNPLAN(1))THEN
                 VW=-(COPLAN(1)-X(I))/(COPLAN(1)-COPLAN(2))
            ELSEIF(PERX)THEN
                 VW=-(X(I)-COPLAN(2)+SX)/SX
            ELSE
                 VW=-1
            ENDIF
*   Multiply with the matrix.
            DO 40 J=1,NWIRE
            QPLANE(2,I)=QPLANE(2,I)+SIGMAT(I,J)*VW
40          CONTINUE
30          CONTINUE
       ENDIF
*** Charges for plane 3, if present.
       IF(YNPLAN(3))THEN
*   Set the weighting field voltages.
            DO 50 I=1,NWIRE
            IF(YNPLAN(4))THEN
                 VW=-(COPLAN(4)-Y(I))/(COPLAN(4)-COPLAN(3))
            ELSEIF(PERY)THEN
                 VW=-(COPLAN(3)+SY-Y(I))/SY
            ELSE
                 VW=-1
            ENDIF
*   Multiply with the matrix.
            DO 60 J=1,NWIRE
            QPLANE(3,I)=QPLANE(3,I)+SIGMAT(I,J)*VW
60          CONTINUE
50          CONTINUE
       ENDIF
*** Charges for plane 4, if present.
       IF(YNPLAN(4))THEN
*   Set the weighting field voltages.
            DO 70 I=1,NWIRE
            IF(YNPLAN(3))THEN
                 VW=-(COPLAN(3)-Y(I))/(COPLAN(3)-COPLAN(4))
            ELSEIF(PERY)THEN
                 VW=-(Y(I)-COPLAN(4)+SY)/SY
            ELSE
                 VW=-1
            ENDIF
*   Multiply with the matrix.
            DO 80 J=1,NWIRE
            QPLANE(4,I)=QPLANE(4,I)+SIGMAT(I,J)*VW
80          CONTINUE
70          CONTINUE
       ENDIF
*** Charges for the tube, if present.
       IF(TUBE)THEN
            DO 160 I=1,NWIRE
            DO 90 J=1,NWIRE
            QPLANE(5,I)=QPLANE(5,I)-SIGMAT(I,J)
90          CONTINUE
160         CONTINUE
       ENDIF
*** Store the plane charges.
       CALL IPLIO(MX,MY,1,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! SIGPLP WARNING : Plane matrix'//
     -           ' store error; field for planes not prepared.'
            RETURN
       ENDIF
*** Next set of periodicities.
110    CONTINUE
100    CONTINUE
*** Compute the background weighting fields, first in x.
       IF(YNPLAN(1).AND.YNPLAN(2))THEN
            EWXCOR(1)=1/(COPLAN(2)-COPLAN(1))
            EWXCOR(2)=1/(COPLAN(1)-COPLAN(2))
       ELSEIF(YNPLAN(1).AND.PERX)THEN
            EWXCOR(1)=1/SX
            EWXCOR(2)=0
       ELSEIF(YNPLAN(2).AND.PERX)THEN
            EWXCOR(1)=0
            EWXCOR(2)=-1/SX
       ELSE
            EWXCOR(1)=0
            EWXCOR(2)=0
       ENDIF
       EWXCOR(3)=0
       EWXCOR(4)=0
       EWXCOR(5)=0
*   Next also in y.
       EWYCOR(1)=0
       EWYCOR(2)=0
       IF(YNPLAN(3).AND.YNPLAN(4))THEN
            EWYCOR(3)=1/(COPLAN(4)-COPLAN(3))
            EWYCOR(4)=1/(COPLAN(3)-COPLAN(4))
       ELSEIF(YNPLAN(3).AND.PERY)THEN
            EWYCOR(3)=1/SY
            EWYCOR(4)=0
       ELSEIF(YNPLAN(4).AND.PERY)THEN
            EWYCOR(3)=0
            EWYCOR(4)=-1/SY
       ELSE
            EWYCOR(3)=0
            EWYCOR(4)=0
       ENDIF
*   The tube has no correction field.
       EWYCOR(5)=0
*** Debugging output.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ SIGPLP DEBUG   : Charges for'',
     -           '' currents induced in the planes:''/26X,
     -           '' Wire        x-Plane 1        x-Plane 2'',
     -           ''        y-Plane 1        y-Plane 2'',
     -           ''             Tube'')')
            DO 140 I=1,NWIRE
            WRITE(LUNOUT,'(26X,I5,5(2X,E15.8))') I,(QPLANE(J,I),J=1,5)
140         CONTINUE
            WRITE(LUNOUT,'(''  ++++++ SIGPLP DEBUG   : Bias fields:''/
     -           26X,''Plane    x-Bias [1/cm]    y-Bias [1/cm]'')')
            DO 150 I=1,5
            WRITE(LUNOUT,'(26X,I5,2(2X,E15.8))') I,EWXCOR(I),EWYCOR(I)
150         CONTINUE
       ENDIF
*** Seems to have worked.
       IFAIL=0
       END
