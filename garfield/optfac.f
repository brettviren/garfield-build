CDECK  ID>, OPTFAC.
       SUBROUTINE OPTFAC
*-----------------------------------------------------------------------
*   OPTFAC - Routine which prints the dependence of the field on the
*            potential of the wires.
*   Variables : FAC(.,I)     : Coefficient of V in V(1), Ex(2), Ey(3)
*               NDATA        : Number of data points used in the average
*               CVTARS etc   : Backup copies of some cell data.
*               FACTYP       : average over (1) grid (2) track (3) wires
*               EXBACK etc   : Background term of the field
*               CHKEX etc    : Debug check on the correctness of the FAC
*               VCOMP        : If .FALSE. grouping is useful.
*   (Last changed on  1/ 7/09.)
*-----------------------------------------------------------------------
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
       DOUBLE PRECISION A
       COMMON /MATRIX/ A(MXWIRE+1,MXWIRE+3)
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       DOUBLE PRECISION WGT,FPRMAT,
     -      FPROJ,FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX,GYBOX,GZBOX
       REAL PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM
       INTEGER NLINED,NGRIDX,NGRIDY,ITRTYP,NTRLIN,NTRSAM,INDPOS,NCTRW,
     -      NTRFLX,NINORD,
     -      NCPNAM,NCXLAB,NCYLAB,NCFPRO,IPRMAT,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,ITFSRM,NTRERR
       LOGICAL LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG,LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       COMMON /PARMS / WGT(MXLIST),FPRMAT(3,3),
     -      FPROJ(3,3),FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX(12),GYBOX(12),GZBOX(12),
     -      PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM,
     -      INDPOS(11000),IPRMAT(3),NCTRW,NCPNAM,
     -      ITRTYP,NTRLIN,NTRSAM,NTRFLX,ITFSRM,NTRERR(10),
     -      NLINED,NINORD,NGRIDX,NGRIDY,NCXLAB,NCYLAB,NCFPRO,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,
     -      LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG(10),LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       CHARACTER*80 PARTID,PXLAB,PYLAB,PROLAB
       CHARACTER*10 PNAME
       CHARACTER*5  PRVIEW
       CHARACTER*(MXCHAR) FCNTRW
       COMMON /PARCHR/ PARTID,FCNTRW,PNAME,PXLAB,PYLAB,PROLAB,PRVIEW
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,MEV2KG,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      MEV2KG = 1.782661845E-30,
     -      BOLTZ=1.380658E-23)
       DOUBLE PRECISION FAC(3,0:MXWIRE),AIJXJ,AIJYJ,EXBACK,EYBACK,
     -      VTBACK,SUMVT,SUMEX,SUMEY,CHKVT,CHKEX,CHKEY
       REAL ERES(MXWIRE)
       INTEGER FACTYP,IANG
       LOGICAL LGROUP,VCOMP,VSET
       SAVE LGROUP,FACTYP
*** Preset the grouping and averaging options.
       DATA LGROUP /.FALSE./
       DATA FACTYP /1/
*** Define the output formats.
1010   FORMAT('  a total of ',I4,' points is effectively used in the',
     -      ' averages.'//'  With the present voltage settings, the',
     -      ' field averages are:'/10X,'V  = ',E12.5,' Volt,'/10X,
     -      'Ex = ',E12.5,' V/cm,'/10X,'Ey = ',E12.5,' V/cm.'//
     -      '  These averages are composed of two parts:'/'    (1) the',
     -      ' field due to a voltage shift or to non-grounded planes:'/
     -      10X,'V  = ',E12.5,' Volt,'/10X,'Ex = ',E12.5,' V/cm,'/10X,
     -      'Ey = ',E12.5,' V/cm.'//'    (2) the field exactly linear',
     -      ' in the wire potentials.'/8X,'The factors for each wire',
     -      ' are printed in the table below.'/)
*** Identify the subroutine.
       IF(LIDENT)PRINT *,' /// ROUTINE OPTFAC ///'
       PRINT *,' !!!!!! ROUTINE BEING WORKED ON !!!!!!'
*** First decode the argument list.
       CALL INPNUM(NWORD)
       DO 10 I=2,NWORD
       IF(INPCMP(I,'GR#ID').NE.0)THEN
            FACTYP=1
       ELSEIF(INPCMP(I,'TR#ACK').NE.0)THEN
            IF(.NOT.TRFLAG(1))THEN
                 CALL INPMSG(I,'The track has not been set.   ')
            ELSE
                 FACTYP=2
            ENDIF
       ELSEIF(INPCMP(I,'WIR#ES').NE.0)THEN
            FACTYP=3
       ELSEIF(INPCMP(I,'GR#OUP').NE.0)THEN
            LGROUP=.TRUE.
       ELSEIF(INPCMP(I,'NOGR#OUP').NE.0)THEN
            LGROUP=.FALSE.
       ELSE
            CALL INPMSG(I,'Not a known option.           ')
       ENDIF
10     CONTINUE
       CALL INPERR
*** Print some debugging output.
       IF(LDEBUG)THEN
            PRINT *,' ++++++ OPTFAC DEBUG   : FACTYP=',FACTYP,
     -           ' LGROUP= ',LGROUP,' NGRIDX=',NGRIDX,' NGRIDY=',NGRIDY
            PRINT *,'                         TYPE=',TYPE,', MODE=',MODE
       ENDIF
*** Quit if FACTYP=2 (track) is still on and no track has been set.
       IF(FACTYP.EQ.2.AND..NOT.TRFLAG(1))THEN
            PRINT *,' !!!!!! OPTFAC WARNING : The track has not been'//
     -           ' set ; the calculations are not carried out.'
            RETURN
       ENDIF
*** Recalculate the capacitance matrix (absent if cell is from dataset).
       CALL SETUP(IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' ###### OPTFAC ERROR   : Setting up the'//
     -           ' capacitance matrix failed ; no valid cell'
            PRINT *,'                         is available from'//
     -           ' now on, the number of wires is set to 0.'
            NWIRE=0
            RETURN
       ENDIF
*** Loop over the wires, I=0 is used to calculate the current V, EX, EY.
       DO 110 I=0,NWIRE
*   First save the charges at the first wire loop.
       IF(I.EQ.1)THEN
            CVTARS=CORVTA
            CVTBRS=CORVTB
            CVTCRS=CORVTC
            V0RES =V0
            C1RES =C1
            CORVTA=0.0
            CORVTB=0.0
            CORVTC=0.0
            V0    =0.0
            C1    =0.0
            DO 100 J=1,NWIRE
            ERES(J)=E(J)
100         CONTINUE
       ENDIF
*   Next swap the charges and the capacitance matrix elements.
       IF(I.GT.0)THEN
            DO 120 J=1,NWIRE
            E(J)=A(I,J)
120         CONTINUE
            NDATRF=0
       ELSE
            NDATA=0
            VTBACK=0.0
            EXBACK=0.0
            EYBACK=0.0
       ENDIF
*   Initialise the output array.
       FAC(1,I)=0.0
       FAC(2,I)=0.0
       FAC(3,I)=0.0
*   Set the linear correction terms for doubly periodic cells.
       IF(TYPE(1:1).EQ.'C'.AND.I.GT.0)THEN
            AIJXJ=0.0
            AIJYJ=0.0
            DO 180 J=1,NWIRE
            IF(TYPE.EQ.'C1 '.AND.MODE.EQ.0)THEN
                 AIJXJ=AIJXJ+A(I,J)*X(J)
            ELSEIF(TYPE.EQ.'C1 '.AND.MODE.EQ.1)THEN
                 AIJYJ=AIJYJ+A(I,J)*Y(J)
            ELSEIF(TYPE.EQ.'C2X'.AND.MODE.EQ.0)THEN
                 AIJXJ=AIJXJ+A(I,J)*(X(J)-COPLAX)
            ELSEIF(TYPE.EQ.'C2Y'.AND.MODE.EQ.1)THEN
                 AIJYJ=AIJYJ+A(I,J)*(Y(J)-COPLAY)
            ENDIF
180         CONTINUE
            AIJXJ=-AIJXJ*2.0*PI/(SX*SY)
            AIJYJ=-AIJYJ*2.0*PI/(SX*SY)
       ENDIF
*   Next do the field calculations, with the modified charges if I > 0.
       IF(FACTYP.EQ.1)THEN
            DO 130 IX=1,NGRIDX
            DO 140 IY=1,NGRIDY
            CALL EFIELD(PXMIN+REAL(IX-1)*(PXMAX-PXMIN)/REAL(NGRIDX-1),
     -           PYMIN+REAL(IY-1)*(PYMAX-PYMIN)/REAL(NGRIDY-1),0.0,
     -           EX,EY,EZ,ETOT,VOLT,1,ILOC)
            IF(ILOC.NE.0)GOTO 140
            FAC(1,I)=FAC(1,I)+VOLT
            FAC(2,I)=FAC(2,I)+EX
            FAC(3,I)=FAC(3,I)+EY
            IF(I.EQ.0)THEN
                 NDATA=NDATA+1
                 VTBACK=VTBACK+V0+
     -                CORVTA*(PXMIN+REAL(IX-1)*(PXMAX-PXMIN)/
     -                     REAL(NGRIDX-1))+
     -                CORVTB*(PYMIN+REAL(IY-1)*(PYMAX-PYMIN)/
     -                     REAL(NGRIDY-1))+
     -                CORVTC
                 EXBACK=EXBACK-CORVTA
                 EYBACK=EYBACK-CORVTB
            ELSE
                 NDATRF=NDATRF+1
                 IF(TYPE.EQ.'C1 '.AND.MODE.EQ.0)THEN
                      FAC(1,I)=FAC(1,I)+AIJXJ*(PXMIN+REAL(IX-1)*
     -                     (PXMAX-PXMIN)/REAL(NGRIDX-1))
                      FAC(2,I)=FAC(2,I)-AIJXJ
                 ELSEIF(TYPE.EQ.'C1 '.AND.MODE.EQ.1)THEN
                      FAC(1,I)=FAC(1,I)+AIJYJ*(PYMIN+REAL(IY-1)*
     -                     (PYMAX-PYMIN)/REAL(NGRIDY-1))
                      FAC(3,I)=FAC(3,I)-AIJYJ
                 ELSEIF(TYPE.EQ.'C2X'.AND.MODE.EQ.0)THEN
                      FAC(1,I)=FAC(1,I)+AIJXJ*(PXMIN+REAL(IX-1)*
     -                     (PXMAX-PXMIN)/REAL(NGRIDX-1)-COPLAX)
                      FAC(2,I)=FAC(2,I)-AIJXJ
                 ELSEIF(TYPE.EQ.'C2Y'.AND.MODE.EQ.1)THEN
                      FAC(1,I)=FAC(1,I)+AIJYJ*(PYMIN+REAL(IY-1)*
     -                     (PYMAX-PYMIN)/REAL(NGRIDY-1)-COPLAY)
                      FAC(3,I)=FAC(3,I)-AIJYJ
                 ENDIF
            ENDIF
140         CONTINUE
130         CONTINUE
*   Average over the track if FACTYP = 2.
       ELSEIF(FACTYP.EQ.2)THEN
            DO 150 IT=1,MXLIST
            CALL EFIELD(XT0+REAL(IT-1)*(XT1-XT0)/REAL(MXLIST-1),
     -           YT0+REAL(IT-1)*(YT1-YT0)/REAL(MXLIST-1),
     -           ZT0+REAL(IT-1)*(ZT1-ZT0)/REAL(MXLIST-1),
     -           EX,EY,EZ,ETOT,VOLT,1,ILOC)
            IF(ILOC.NE.0)GOTO 150
            FAC(1,I)=FAC(1,I)+VOLT
            FAC(2,I)=FAC(2,I)+EX
            FAC(3,I)=FAC(3,I)+EY
            IF(I.EQ.0)THEN
                 NDATA=NDATA+1
                 VTBACK=VTBACK+V0+
     -                CORVTA*(XT0+REAL(IT-1)*(XT1-XT0)/
     -                     REAL(MXLIST-1))+
     -                CORVTB*(YT0+REAL(IT-1)*(YT1-YT0)/
     -                     REAL(MXLIST-1))+
     -                CORVTC
                 EXBACK=EXBACK-CORVTA
                 EYBACK=EYBACK-CORVTB
            ELSE
                 NDATRF=NDATRF+1
                 IF(TYPE.EQ.'C1 '.AND.MODE.EQ.0)THEN
                      FAC(1,I)=FAC(1,I)+AIJXJ*
     -                     (XT0+REAL(IT-1)*(XT1-XT0)/REAL(MXLIST-1))
                      FAC(2,I)=FAC(2,I)-AIJXJ
                 ELSEIF(TYPE.EQ.'C1 '.AND.MODE.EQ.1)THEN
                      FAC(1,I)=FAC(1,I)+AIJYJ*
     -                     (YT0+REAL(IT-1)*(YT1-YT0)/REAL(MXLIST-1))
                      FAC(3,I)=FAC(3,I)-AIJYJ
                 ELSEIF(TYPE.EQ.'C2X'.AND.MODE.EQ.0)THEN
                      FAC(1,I)=FAC(1,I)+AIJXJ*(XT0+REAL(IT-1)*
     -                     (XT1-XT0)/REAL(MXLIST-1)-COPLAX)
                      FAC(2,I)=FAC(2,I)-AIJXJ
                 ELSEIF(TYPE.EQ.'C2Y'.AND.MODE.EQ.1)THEN
                      FAC(1,I)=FAC(1,I)+AIJYJ*(YT0+REAL(IT-1)*
     -                      (YT1-YT0)/REAL(MXLIST-1)-COPLAY)
                      FAC(3,I)=FAC(3,I)-AIJYJ
                 ENDIF
            ENDIF
150         CONTINUE
*   Loop over the surface of the sense wires if FACTYP = 3.
       ELSE
            DO 160 IW=1,NWIRE
            IF(INDSW(IW).EQ.0)GOTO 160
            DO 170 IANG=1,10
            ANG=I*0.2*PI
            CALL EFIELD(X(IW)+0.51*D(IW)*COS(ANG),
     -           Y(IW)+0.51*D(IW)*SIN(ANG),0.0,
     -           EX,EY,EZ,ETOT,VOLT,1,ILOC)
            IF(ILOC.NE.0)GOTO 170
            FAC(1,I)=FAC(1,I)+VOLT
            FAC(2,I)=FAC(2,I)+EX
            FAC(3,I)=FAC(3,I)+EY
            IF(I.EQ.0)THEN
                 NDATA=NDATA+1
                 VTBACK=VTBACK+V0+CORVTA*(X(IW)+0.51*D(IW)*COS(ANG))+
     -                CORVTB*(Y(IW)+0.51*D(IW)*SIN(ANG))+CORVTC
                 EXBACK=EXBACK-CORVTA
                 EYBACK=EYBACK-CORVTB
            ELSE
                 NDATRF=NDATRF+1
                 IF(TYPE.EQ.'C1 '.AND.MODE.EQ.0)THEN
                      FAC(1,I)=FAC(1,I)+AIJXJ*
     -                     (X(IW)+0.51*D(IW)*COS(ANG))
                      FAC(2,I)=FAC(2,I)-AIJXJ
                 ELSEIF(TYPE.EQ.'C1 '.AND.MODE.EQ.1)THEN
                      FAC(1,I)=FAC(1,I)+AIJYJ*
     -                     (Y(IW)+0.51*D(IW)*SIN(ANG))
                      FAC(3,I)=FAC(3,I)-AIJYJ
                 ELSEIF(TYPE.EQ.'C2X'.AND.MODE.EQ.0)THEN
                      FAC(1,I)=FAC(1,I)+AIJXJ*
     -                     (X(IW)+0.51*D(IW)*COS(ANG)-COPLAX)
                      FAC(2,I)=FAC(2,I)-AIJXJ
                 ELSEIF(TYPE.EQ.'C2Y'.AND.MODE.EQ.1)THEN
                      FAC(1,I)=FAC(1,I)+AIJYJ*
     -                     (Y(IW)+0.51*D(IW)*SIN(ANG)-COPLAY)
                      FAC(3,I)=FAC(3,I)-AIJYJ
                 ENDIF
            ENDIF
170         CONTINUE
160         CONTINUE
       ENDIF
*   Stop this routine if NDATA is 0.
       IF(I.EQ.0.AND.NDATA.LE.0)THEN
            PRINT *,' !!!!!! OPTFAC WARNING : No output can be printed',
     -              ' because the field is zero at all sampling points.'
            RETURN
       ENDIF
*   Average the EX, EY and V factors over the sampling points.
       FAC(1,I)=FAC(1,I)/NDATA
       FAC(2,I)=FAC(2,I)/NDATA
       FAC(3,I)=FAC(3,I)/NDATA
*   Add the wire term to the VTBACK sum or average if not yet done.
       IF(I.GT.0)THEN
            VTBACK=VTBACK-
     -           (V0RES+CVTARS*X(I)+CVTBRS*Y(I)+CVTCRS)*FAC(1,I)
            EXBACK=EXBACK-
     -           (V0RES+CVTARS*X(I)+CVTBRS*Y(I)+CVTCRS)*FAC(2,I)
            EYBACK=EYBACK-
     -           (V0RES+CVTARS*X(I)+CVTBRS*Y(I)+CVTCRS)*FAC(3,I)
            IF(NDATA.NE.NDATRF)PRINT *,' !!!!!! OPTFAC WARNING : ',
     -           ' Number of sampling points has changed; data for',
     -           ' wire ',I,' are not reliable.'
       ELSE
            VTBACK=VTBACK/NDATA
            EXBACK=EXBACK/NDATA
            EYBACK=EYBACK/NDATA
       ENDIF
*   Continue with the next wire.
110    CONTINUE
*** Swap the charges back into place.
       DO 200 I=1,NWIRE
       E(I)=ERES(I)
200    CONTINUE
       CORVTA=CVTARS
       CORVTB=CVTBRS
       CORVTC=CVTCRS
       V0=V0RES
       C1=C1RES
*** Print the results obtained.
       WRITE(LUNOUT,'(''1 How the field comes about''/
     -      ''  =========================''/)')
       IF(FACTYP.EQ.1)THEN
            WRITE(LUNOUT,'(''  The data below apply to the average'',
     -           '' field over a grid of '',I3,'' x '',I3,'' points''/
     -           ''  in the area ('',E12.5,'','',E12.5,'') to ('',
     -           E12.5,'','',E12.5,''),'')')
     -           NGRIDX,NGRIDY,PXMIN,PYMIN,PXMAX,PYMAX
       ELSEIF(FACTYP.EQ.2)THEN
            WRITE(LUNOUT,'(''  The data below apply to the average'',
     -           '' field over a track of '',I3,'' points''/
     -           ''  from ('',E12.5,'','',E12.5,'') to ('',
     -           E12.5,'','',E12.5,''),'')')
     -           MXLIST,XT0,XT1,YT0,YT1
       ELSEIF(FACTYP.EQ.3)THEN
            WRITE(LUNOUT,'(''  The data below apply to the average'',
     -           '' field on the surface of the sense wires,'')')
       ENDIF
*** Print the rest of the introductory heading.
       WRITE(LUNOUT,1010) NDATA,(FAC(I,0),I=1,3),VTBACK,EXBACK,EYBACK
**  Printing in case wires should be grouped.
       IF(LGROUP)THEN
            WRITE(LUNOUT,'(''   Wire     V-factor    Ex-factor'',
     -           ''    Ey-factor   Group Tot V-factor Tot Ex-factor'',
     -           '' Tot Ey-factor'')')
            WRITE(LUNOUT,'(''           [numeric]     [cm**-1]'',
     -           ''     [cm**-1]            [numeric]      [cm**-1]'',
     -           ''      [cm**-1]'')')
            DO 300 I=0,NSW
*   Preset summing variables.
            SUMVT=0.0
            SUMEX=0.0
            SUMEY=0.0
            NSUM=0
*   Preset the logicals used to check whether grouping is useful.
            VCOMP=.FALSE.
            VSET=.FALSE.
            VREF=0.0
*   Pick out the wires belonging to the group.
            WRITE(LUNOUT,'('' '')')
            DO 310 J=1,NWIRE
            IF(INDSW(J).NE.I)GOTO 310
            IF(VSET.AND.V(J).NE.VREF)VCOMP=.TRUE.
            IF(.NOT.VSET)THEN
                 VREF=V(J)
                 VSET=.TRUE.
            ENDIF
*   Add to the totals.
            NSUM=NSUM+1
            SUMVT=SUMVT+FAC(1,J)
            SUMEX=SUMEX+FAC(2,J)
            SUMEY=SUMEY+FAC(3,J)
            WRITE(LUNOUT,'(2X,I5,3(1X,E12.5))')
     -           J,FAC(1,J),FAC(2,J),FAC(3,J)
310         CONTINUE
*   Print the information collected for this group.
            IF(I.EQ.0.AND.NSUM.NE.0)WRITE(LUNOUT,'(49X,''Wires not'',
     -           '' belonging to any group.'')')
            IF(NSUM.EQ.0.OR.I.EQ.0)GOTO 300
            IF(.NOT.VCOMP)THEN
                 WRITE(LUNOUT,'(49X,I5,3(1X,E12.5))')
     -                I,SUMVT,SUMEX,SUMEY
            ELSE
                 WRITE(LUNOUT,'(49X,I5,
     -                '' Meaningless: differing wire voltages.'')') I
            ENDIF
300         CONTINUE
**  Printing in case grouping should not be performed.
       ELSE
            WRITE(LUNOUT,'(''   Wire         V-factor'',
     -           ''        Ex-factor        Ey-factor'')')
            WRITE(LUNOUT,'(''               [numeric]'',
     -           ''         [cm**-1]         [cm**-1]''/)')
            DO 320 I=1,NWIRE
            WRITE(LUNOUT,'(2X,I5,3(5X,E12.5))')
     -           I,FAC(1,I),FAC(2,I),FAC(3,I)
320         CONTINUE
       ENDIF
**  In case the debug option was specified, verify the sums.
       IF(LDEBUG)THEN
            CHKVT=VTBACK
            CHKEX=EXBACK
            CHKEY=EYBACK
            DO 400 I=1,NWIRE
            CHKVT=CHKVT+FAC(1,I)*V(I)
            CHKEX=CHKEX+FAC(2,I)*V(I)
            CHKEY=CHKEY+FAC(3,I)*V(I)
400         CONTINUE
            PRINT *,' ++++++ OPTFAC DEBUG  : Summing the wire',
     -              ' contributions and the background yields:'
            WRITE(*,'(10X,''V  = '',E12.5,'' Volt,''/10X,''Ex = '',
     -              E12.5,'' V/cm,''/10X,''Ey = '',E12.5,'' V/cm.'')')
     -              CHKVT,CHKEX,CHKEY
       ENDIF
**  Make sure that the next output line starts on a fresh page.
       WRITE(LUNOUT,'(''1'')')
*** Register the amount of CPU time used with TIMLOG.
       CALL TIMLOG('Printing V, Ex and Ey factors:          ')
       END
