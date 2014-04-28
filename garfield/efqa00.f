CDECK  ID>, EFQA00.
       SUBROUTINE EFQA00(IFAIL)
*-----------------------------------------------------------------------
*   EFQA00 - Routine preparing the field calculations by filling the
*            capacitance matrix. This routines handles configurations
*            with not more than one plane in either x or y and not more
*            than one dielectricum in total.
*   VARIABLES : No local variables.
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
       COMMON /TMPA00/ EPSMT1,EPSMT2
*** Check the configuration of dielectrica is acceptable.
       IF(NXMATT+NYMATT.GT.1.OR.
     -      (NXMATT.EQ.1.AND.XMATT(1,3).EQ.0.AND.XMATT(1,4).EQ.0).OR.
     -      (NYMATT.EQ.1.AND.YMATT(1,3).EQ.0.AND.YMATT(1,4).EQ.0))THEN
            PRINT *,' ###### EFQA00 ERROR   : The configuration of'//
     -           ' dielectrica can not yet be handled ; cell rejected.'
            IFAIL=1
            RETURN
       ELSE
            PRINT *,' ------ EFQA00 MESSAGE : Potentials handled by'//
     -           ' experimental routine.'
       ENDIF
*** Prepare some auxilliary variables for dielectrica.
       YNMATX=.FALSE.
       YNMATY=.FALSE.
       COMATX=0.0
       COMATY=0.0
       EPSMT1=0.0
       EPSMT2=0.0
       IF(NXMATT.EQ.1)THEN
            YNMATX=.TRUE.
            IF(XMATT(1,3).NE.0)COMATX=XMATT(1,2)
            IF(XMATT(1,4).NE.0)COMATX=XMATT(1,1)
            EPSMT1=(1-XMATT(1,5))/(1+XMATT(1,5))
            EPSMT2=2/(1+XMATT(1,5))
       ELSEIF(NYMATT.EQ.1)THEN
            YNMATY=.TRUE.
            IF(YMATT(1,3).NE.0)COMATY=YMATT(1,2)
            IF(YMATT(1,4).NE.0)COMATY=YMATT(1,1)
            EPSMT1=(1-YMATT(1,5))/(1+YMATT(1,5))
            EPSMT2=2/(1+YMATT(1,5))
       ENDIF
*** Loop over all wire combinations.
       DO 10 I=1,NWIRE
       A(I,I)=0.25*D(I)**2
*** Take care of the equipotential planes.
       IF(YNPLAX)A(I,I)=A(I,I)/(2.0*(X(I)-COPLAX))**2
       IF(YNPLAY)A(I,I)=A(I,I)/(2.0*(Y(I)-COPLAY))**2
*** Take care of combinations of equipotential planes.
       IF(YNPLAX.AND.YNPLAY)A(I,I)=4.0*A(I,I)*((X(I)-COPLAX)**2+
     -      (Y(I)-COPLAY)**2)
*** Before adding dielectrica, take the log.
       A(I,I)=-0.5*LOG(A(I,I))
*** One x-dielectricum.
       IF(YNMATX)THEN
*   Dielectricum charge.
            A(I,I)=A(I,I)-EPSMT1*LOG(2*ABS(X(I)-COMATX))
*   Add single plane reflected dielectricum charges.
            IF(YNPLAX)A(I,I)=A(I,I)+
     -           EPSMT1*LOG(ABS(2*COPLAX-2*COMATX+X(I)))
            IF(YNPLAY)A(I,I)=A(I,I)+
     -           EPSMT1*0.5*LOG((2*COMATX-X(I))**2+(2*COPLAY-Y(I))**2)
*   Add double plane reflected dielectricum charges.
            IF(YNPLAX.AND.YNPLAY)A(I,I)=A(I,I)-
     -           EPSMT1*0.5*LOG((2*COPLAX-2*COMATX+X(I))**2+
     -           (2*COPLAY-Y(I))**2)
*** One y-dielectricum.
       ELSEIF(YNMATY)THEN
*   Dielectricum charge.
            A(I,I)=A(I,I)-EPSMT1*LOG(2*ABS(Y(I)-COMATY))
*   Add single plane reflected dielectricum charges.
            IF(YNPLAX)A(I,I)=A(I,I)+
     -           EPSMT1*0.5*LOG((2*COPLAX-X(I))**2+(2*COMATY-Y(I))**2)
            IF(YNPLAY)A(I,I)=A(I,I)+
     -           EPSMT1*LOG(ABS(2*COPLAY-2*COMATY+Y(I)))
*   Add double plane reflected dielectricum charges.
            IF(YNPLAX.AND.YNPLAY)A(I,I)=A(I,I)-
     -           EPSMT1*0.5*LOG((2*COPLAX-X(I))**2+
     -           (2*COPLAY-2*COMATY+Y(I))**2)
       ENDIF
*** Loop over all other wires for the off-diagonal elements.
       DO 20 J=I+1,NWIRE
       A(I,J)=(X(I)-X(J))**2+(Y(I)-Y(J))**2
*** Take care of equipotential planes.
       IF(YNPLAX)A(I,J)=A(I,J)/((X(I)+X(J)-2.*COPLAX)**2+(Y(I)-Y(J))**2)
       IF(YNPLAY)A(I,J)=A(I,J)/((X(I)-X(J))**2+(Y(I)+Y(J)-2.*COPLAY)**2)
*** Take care of pairs of equipotential planes in different directions.
       IF(YNPLAX.AND.YNPLAY)A(I,J)=
     -      A(I,J)*((X(I)+X(J)-2.*COPLAX)**2+(Y(I)+Y(J)-2.*COPLAY)**2)
*** Take the log before adding dielectrica.
       A(I,J)=-0.5*LOG(A(I,J))
*** One x-dielectricum.
       IF(YNMATX)THEN
*   Dielectricum charge.
            A(I,J)=A(I,J)-EPSMT1*0.5*
     -           LOG((X(I)+X(J)-2*COMATX)**2+(Y(I)-Y(J))**2)
*   Add single plane reflected dielectricum charges.
            IF(YNPLAX)A(I,J)=A(I,J)+
     -           EPSMT1*0.5*LOG((2*COPLAX-2*COMATX+X(I)-X(J))**2+
     -           (Y(I)-Y(J))**2)
            IF(YNPLAY)A(I,J)=A(I,J)+
     -           EPSMT1*0.5*LOG((X(I)+X(J)-2*COMATX)**2+
     -           (Y(I)+Y(J)-2*COPLAY)**2)
*   Add double plane reflected dielectricum charges.
            IF(YNPLAX.AND.YNPLAY)A(I,J)=A(I,J)-
     -           EPSMT1*0.5*LOG((2*COPLAX-2*COMATX+X(I)-X(J))**2+
     -           (Y(I)+Y(J)-2*COPLAY)**2)
*** One y-dielectricum.
       ELSEIF(YNMATY)THEN
*   Dielectricum charge.
            A(I,J)=A(I,J)-EPSMT1*0.5*
     -           LOG((X(I)-X(J))**2+(Y(I)+Y(J)-2*COMATY)**2)
*   Add single plane reflected dielectricum charges.
            IF(YNPLAX)A(I,J)=A(I,J)+
     -           EPSMT1*0.5*LOG((X(I)+X(J)-2*COPLAX)**2+
     -           (Y(I)+Y(J)-2*COMATY)**2)
            IF(YNPLAY)A(I,J)=A(I,J)+
     -           EPSMT1*0.5*LOG((X(I)-X(J))**2+
     -           (2*COPLAY-2*COMATY+Y(I)-Y(J))**2)
*   Add double plane reflected dielectricum charges.
            IF(YNPLAX.AND.YNPLAY)A(I,J)=A(I,J)-
     -           EPSMT1*0.5*LOG((X(I)+X(J)-2*COPLAX)**2+
     -           (2*COPLAY-2*COMATY+Y(I)-Y(J))**2)
       ENDIF
*** Copy this to A(J,I) since the capacitance matrix is symmetric.
       A(J,I)=A(I,J)
20     CONTINUE
10     CONTINUE
*** Call CHARGE to calculate the charges really.
       CALL CHARGE(IFAIL)
       END
