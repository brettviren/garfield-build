CDECK  ID>, CELSYN.
       SUBROUTINE CELSYN
*-----------------------------------------------------------------------
*   CELSYN - Outputs the cell data for use by front end programs.
*   (Last changed on 23/ 2/99.)
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
       INTEGER I,NC1,NC2
       CHARACTER*20 AUX1,AUX2
*** Cell type.
       IF(POLAR)THEN
            WRITE(6,'(''  >>>>>> set cell coordinates polar'')')
       ELSEIF(TUBE)THEN
            WRITE(6,'(''  >>>>>> set cell coordinates tube'')')
       ELSE
            WRITE(6,'(''  >>>>>> set cell coordinates cartesian'')')
       ENDIF
*** Potential type.
       WRITE(6,'(''  >>>>>> set cell type '',A)') TYPE
*** Dimensions.
       WRITE(6,'(''  >>>>>> set cell xmin '',E15.8/
     -      ''  >>>>>> set cell ymin '',E15.8/
     -      ''  >>>>>> set cell zmin '',E15.8/
     -      ''  >>>>>> set cell xmax '',E15.8/
     -      ''  >>>>>> set cell ymax '',E15.8/
     -      ''  >>>>>> set cell zmax '',E15.8/
     -      ''  >>>>>> set cell vmin '',E15.8/
     -      ''  >>>>>> set cell vmax '',E15.8)')
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX
*** Number of wires.
       WRITE(6,'(''  >>>>>> set cell nwire '',I10)') NWIRE
*** Wire data.
       DO 10 I=1,NWIRE
       CALL OUTFMT(REAL(I),2,AUX1,NC1,'LEFT')
       CALL OUTFMT(X(I),2,AUX2,NC2,'LEFT')
       WRITE(6,'(''  >>>>>> set cell x'',A,1X,A)')
     -      AUX1(1:NC1),AUX2(1:NC2)
       CALL OUTFMT(Y(I),2,AUX2,NC2,'LEFT')
       WRITE(6,'(''  >>>>>> set cell y'',A,1X,A)')
     -      AUX1(1:NC1),AUX2(1:NC2)
       CALL OUTFMT(V(I),2,AUX2,NC2,'LEFT')
       WRITE(6,'(''  >>>>>> set cell v'',A,1X,A)')
     -      AUX1(1:NC1),AUX2(1:NC2)
       CALL OUTFMT(E(I),2,AUX2,NC2,'LEFT')
       WRITE(6,'(''  >>>>>> set cell e'',A,1X,A)')
     -      AUX1(1:NC1),AUX2(1:NC2)
       CALL OUTFMT(D(I),2,AUX2,NC2,'LEFT')
       WRITE(6,'(''  >>>>>> set cell d'',A,1X,A)')
     -      AUX1(1:NC1),AUX2(1:NC2)
       CALL OUTFMT(W(I),2,AUX2,NC2,'LEFT')
       WRITE(6,'(''  >>>>>> set cell w'',A,1X,A)')
     -      AUX1(1:NC1),AUX2(1:NC2)
       CALL OUTFMT(U(I),2,AUX2,NC2,'LEFT')
       WRITE(6,'(''  >>>>>> set cell u'',A,1X,A)')
     -      AUX1(1:NC1),AUX2(1:NC2)
       CALL OUTFMT(DENS(I),2,AUX2,NC2,'LEFT')
       WRITE(6,'(''  >>>>>> set cell dens'',A,1X,A)')
     -      AUX1(1:NC1),AUX2(1:NC2)
       WRITE(6,'(''  >>>>>> set cell type'',A,'' "'',A1,''"'')')
     -      AUX1(1:NC1),WIRTYP(I)
10     CONTINUE
*** Plane data.
       DO 20 I=1,4
       IF(YNPLAN(I))THEN
            WRITE(6,'(''  >>>>>> set cell plane'',I1,'' 1'')') I
            CALL OUTFMT(COPLAN(I),2,AUX1,NC1,'LEFT')
            WRITE(6,'(''  >>>>>> set cell coorplane'',I1,1X,A)')
     -           I,AUX1(1:NC1)
            CALL OUTFMT(VTPLAN(I),2,AUX1,NC1,'LEFT')
            WRITE(6,'(''  >>>>>> set cell voltplane'',I1,1X,A)')
     -           I,AUX1(1:NC1)
            WRITE(6,'(''  >>>>>> set cell typeplane'',I1,'' "'',
     -           A1,''"'')') I,PLATYP(I)
       ELSE
            WRITE(6,'(''  >>>>>> set cell plane'',I1,'' 0'')') I
            WRITE(6,'(''  >>>>>> set cell coorplane'',I1,'' 0'')') I
            WRITE(6,'(''  >>>>>> set cell voltplane'',I1,'' 0'')') I
            WRITE(6,'(''  >>>>>> set cell typeplane'',I1,'' "?"'')') I
       ENDIF
20     CONTINUE
*** Tube.
       IF(TUBE)THEN
            WRITE(6,'(''  >>>>>> set cell tube 1'')')
            CALL OUTFMT(COTUBE,2,AUX1,NC1,'LEFT')
            WRITE(6,'(''  >>>>>> set cell coortube '',A)')
     -            AUX1(1:NC1)
            CALL OUTFMT(VTTUBE,2,AUX1,NC1,'LEFT')
            WRITE(6,'(''  >>>>>> set cell volttube '',A)')
     -            AUX1(1:NC1)
            WRITE(6,'(''  >>>>>> set cell ntube '',I10)') NTUBE
            WRITE(6,'(''  >>>>>> set cell mtube '',I10)') MTUBE
            WRITE(6,'(''  >>>>>> set cell typetube "'',A1,''"'')')
     -           PLATYP(5)
       ELSE
            WRITE(6,'(''  >>>>>> set cell tube 0'')')
            WRITE(6,'(''  >>>>>> set cell coortube 0'')')
            WRITE(6,'(''  >>>>>> set cell volttube 0'')')
            WRITE(6,'(''  >>>>>> set cell ntube 0'')')
            WRITE(6,'(''  >>>>>> set cell mtube 0'')')
            WRITE(6,'(''  >>>>>> set cell typetube 0'')')
       ENDIF
*** Declare the cell as having been set.
       WRITE(6,'(''  >>>>>> set cell set 1'')')
       END
