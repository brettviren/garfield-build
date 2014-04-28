CDECK  ID>, CHARGE.
       SUBROUTINE CHARGE(IFAIL)
*-----------------------------------------------------------------------
*   CHARGE - Routine actually inverting the capacitance matrix filled in
*            the SET... routines thereby providing the charges.
*   (Last changed on 30/ 1/93.)
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
       DOUBLE PRECISION T
*** Identify the routine, if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE CHARGE ///'
*** Dump the capacitance matrix before inversion, if DEBUG is requested.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(/''  ++++++ CHARGE DEBUG   : Dump of the'',
     -           '' capacitance matrix before inversion follows:''/)')
            DO 160 I=0,NWIRE-1,10
            DO 170 J=0,NWIRE-1,10
            WRITE(LUNOUT,'(''1 Block '',I2,''.'',I2/)') I/10,J/10
            DO 180 II=1,10
            IF(I+II.GT.NWIRE)GOTO 180
            WRITE(LUNOUT,'(2X,10(E12.5,1X:))')
     -           (A(I+II,J+JJ),JJ=1,MIN(NWIRE-J,10))
180         CONTINUE
170         CONTINUE
160         CONTINUE
            WRITE(LUNOUT,'(/''  ++++++ CHARGE DEBUG   : End of the'',
     -           '' uninverted capacitance matrix dump.''/)')
       ENDIF
*** Transfer the voltages to A, correcting for the equipotential planes.
       DO 10 I=1,NWIRE
       A(I,MXWIRE+3)=V(I)-(CORVTA*X(I)+CORVTB*Y(I)+CORVTC)
10     CONTINUE
*** Force sum charges =0 in case of absence of equipotential planes.
       IF(.NOT.(YNPLAN(1).OR.YNPLAN(2).OR.
     -      YNPLAN(3).OR.YNPLAN(4).OR.TUBE))THEN
*   Add extra elements to A, acting as constraints.
            A(NWIRE+1,MXWIRE+3)=0.0
            DO 20 I=1,NWIRE
            A(I,NWIRE+1)=1.0
            A(NWIRE+1,I)=1.0
20          CONTINUE
            A(NWIRE+1,NWIRE+1)=0.0
*   Solve equations to yield charges, using KERNLIB (scalar).
            CALL DEQINV(NWIRE+1,A,MXWIRE+1,A(1,NWIRE+2),IFAIL,1,
     -           A(1,MXWIRE+3))
*   Modify A to give true inverse of capacitance matrix.
            IF(A(NWIRE+1,NWIRE+1).NE.0.0)THEN
                 T=1.0/A(NWIRE+1,NWIRE+1)
                 DO 40 I=1,NWIRE
                 DO 30 J=1,NWIRE
                 A(I,J)=A(I,J)-T*A(I,NWIRE+1)*A(NWIRE+1,J)
30               CONTINUE
40               CONTINUE
            ELSE
                 PRINT *,' !!!!!! CHARGE WARNING : True inverse of'//
     -                ' the capacitance matrix could not be calculated.'
                 PRINT *,'                         Use of the FACTOR'//
     -                ' instruction should be avoided.'
            ENDIF
*   Store reference potential.
            V0=A(NWIRE+1,MXWIRE+3)
       ELSE
*** Handle the case when the sum of the charges is zero automatically.
            CALL DEQINV(NWIRE,A,MXWIRE+1,A(1,NWIRE+2),IFAIL,1,
     -           A(1,MXWIRE+3))
*   Reference potential chosen to be zero.
            V0=0.0
       ENDIF
*** Check the error condition flag.
       IF(IFAIL.NE.0)THEN
            PRINT *,' ###### CHARGE ERROR   : Failure to solve the'//
     -           ' capacitance equations; no charges are available.'
            IFAIL=1
            RETURN
       ENDIF
*** Copy the charges to E.
       DO 50 I=1,NWIRE
       E(I)=A(I,MXWIRE+3)
50     CONTINUE
*** If LDEBUG is on, print the capacitance matrix.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(/''  ++++++ CHARGE DEBUG   : Dump of the'',
     -           '' capacitance matrix follows:''/)')
            DO 60 I=0,NWIRE-1,10
            DO 70 J=0,NWIRE-1,10
            WRITE(LUNOUT,'(''1 Block '',I2,''.'',I2/)') I/10,J/10
            DO 80 II=1,10
            IF(I+II.GT.NWIRE)GOTO 80
            WRITE(LUNOUT,'(2X,10(E12.5,1X:))')
     -           (A(I+II,J+JJ),JJ=1,MIN(NWIRE-J,10))
80          CONTINUE
70          CONTINUE
60          CONTINUE
            WRITE(LUNOUT,'(/''  ++++++ CHARGE DEBUG   : End of the'',
     -           '' capacitance matrix.''/)')
       ENDIF
*   And also check the quality of the matrix inversion.
       IF(LCHGCH)THEN
            WRITE(LUNOUT,'(/''  QUALITY CHECK'',
     -           '' OF THE CHARGE CALCULATION.''//
     -           ''                          wire       E as obtained'',
     -           ''     E reconstructed''/)')
            DO 100 I=1,NWIRE
            A(I,MXWIRE+2)=0
            DO 110 J=1,NWIRE
            A(I,MXWIRE+2)=A(I,MXWIRE+2)+
     -           A(I,J)*(V(J)-V0-(CORVTA*X(J)+CORVTB*Y(J)+CORVTC))
110         CONTINUE
            WRITE(LUNOUT,'(26X,I4,5X,E15.8,5X,E15.8)')
     -           I,E(I),A(I,MXWIRE+2)
100         CONTINUE
            WRITE(LUNOUT,'('' '')')
       ENDIF
       END
