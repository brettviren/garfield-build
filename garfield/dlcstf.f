CDECK  ID>, DLCSTF.
       SUBROUTINE DLCSTF(ISTAT,STATUS,NCSTAT)
*-----------------------------------------------------------------------
*   DLCSTF - Formats the status code into a string.
*   (Last changed on 12/ 4/08.)
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
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
       CHARACTER*(*) STATUS
       CHARACTER*80 AUX
       INTEGER ISTAT,NCSTAT,NC
*   Drift line left the area.
       IF(ISTAT.EQ.-1)THEN
            STATUS='Left the drift area'
            NCSTAT=19
*   Too many curvature.
       ELSEIF(ISTAT.EQ.-2)THEN
            STATUS='Too many steps'
            NCSTAT=14
*   Calculations failed.
       ELSEIF(ISTAT.EQ.-3)THEN
            STATUS='Calculations abandoned'
            NCSTAT=22
*   Plane hit.
       ELSEIF(ISTAT.EQ.-4)THEN
            STATUS='Hit a plane'
            NCSTAT=11
*   Left drift medium.
       ELSEIF(ISTAT.EQ.-5)THEN
            STATUS='Left the drift medium'
            NCSTAT=21
*   Left the mesh.
       ELSEIF(ISTAT.EQ.-6)THEN
            STATUS='Left the mesh'
            NCSTAT=13
*   Lost to attachment.
       ELSEIF(ISTAT.EQ.-7)THEN
            STATUS='Attached'
            NCSTAT=8
*   Lost to due to a sharp kink.
       ELSEIF(ISTAT.EQ.-8)THEN
            STATUS='Bend sharper than pi/2'
            NCSTAT=22
*   Lost to for energy excess.
       ELSEIF(ISTAT.EQ.-9)THEN
            STATUS='Energy exceeds E_maximum'
            NCSTAT=24
*   Plane hit.
       ELSEIF(ISTAT.EQ.-11)THEN
            IF(POLAR)THEN
                 STATUS='Hit the minimum r plane'
            ELSE
                 STATUS='Hit the minimum x plane'
            ENDIF
            NCSTAT=23
       ELSEIF(ISTAT.EQ.-12)THEN
            IF(POLAR)THEN
                 STATUS='Hit the maximum r plane'
            ELSE
                 STATUS='Hit the maximum x plane'
            ENDIF
            NCSTAT=23
       ELSEIF(ISTAT.EQ.-13)THEN
            IF(POLAR)THEN
                 STATUS='Hit the minimum phi plane'
                 NCSTAT=25
            ELSE
                 STATUS='Hit the minimum y plane'
                 NCSTAT=23
            ENDIF
       ELSEIF(ISTAT.EQ.-14)THEN
            IF(POLAR)THEN
                 STATUS='Hit the maximum phi plane'
                 NCSTAT=25
            ELSE
                 STATUS='Hit the maximum y plane'
                 NCSTAT=23
            ENDIF
       ELSEIF(ISTAT.EQ.-15)THEN
            STATUS='Hit the tube'
            NCSTAT=12
       ELSEIF(ISTAT.EQ.-20)THEN
            STATUS='Started from a line or an edge'
            NCSTAT=30
*   Original copy of a wire.
       ELSEIF(ISTAT.GT.0.AND.ISTAT.LE.MXWIRE)THEN
            CALL OUTFMT(REAL(ISTAT),2,AUX,NC,'LEFT')
            STATUS='Hit '//WIRTYP(ISTAT)//' wire '//AUX(1:NC)
            NCSTAT=11+NC
*   Wire replicas.
       ELSEIF(ISTAT.GT.MXWIRE.AND.ISTAT.LE.2*MXWIRE)THEN
            CALL OUTFMT(REAL(ISTAT)-MXWIRE,2,AUX,NC,'LEFT')
            STATUS='Hit a replica of '//WIRTYP(ISTAT-MXWIRE)//
     -         ' wire '//AUX(1:NC)
            NCSTAT=24+NC
*   Solids.
       ELSEIF(ISTAT.GT.2*MXWIRE.AND.ISTAT.LE.2*MXWIRE+MXSOLI)THEN
            CALL OUTFMT(REAL(ISTAT)-2*MXWIRE,2,AUX,NC,'LEFT')
            STATUS='Hit '//SOLTYP(ISTAT-2*MXWIRE)//
     -         ' solid '//AUX(1:NC)
            NCSTAT=12+NC
*   Invalid status code.
       ELSE
            STATUS='Unknown'
            NCSTAT=7
       ENDIF
*** Ensure that the string length does not become invalid.
       IF(NCSTAT.GT.LEN(STATUS))THEN
            PRINT *,' !!!!!! DLCSTF WARNING : Status string has been'//
     -           ' truncated.'
            NCSTAT=LEN(STATUS)
       ENDIF
       END
