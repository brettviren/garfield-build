CDECK  ID>, CELCAL.
       SUBROUTINE CELCAL(INSTR,IFAIL)
*-----------------------------------------------------------------------
*   CELCAL - Processes cell related procedure calls.
*   (Last changed on  8/12/10.)
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
       INTEGER INS(MXINS,4),ALGENT(MXALGE,10),MODREG(MXCONS:MXREG),
     -      ISYNCH,IINS0,ICONS0,ARGREF(MXARG,2),MODARG(MXARG),
     -      NREG,NCONS,NINS,NERR,NRES,NALGE,IENTRL,NAERR(100)
       REAL REG(MXCONS:MXREG),ARG(MXARG),EXPMAX
       PARAMETER(EXPMAX=40.0)
       LOGICAL EXEC(MXINS),LIGUND,LINUND
       COMMON /ALGDAT/ REG,ARG,MODARG,ARGREF,INS,MODREG,ALGENT,
     -      NREG,NCONS,NINS,NERR,NAERR,
     -      NRES,NALGE,IENTRL,ISYNCH,IINS0,ICONS0,EXEC,LIGUND,LINUND
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
       INTEGER INPCMX,IFAIL1,IFAIL2,IFAIL3,ISTR,IAUX,NARG,IPROC,IW,
     -      INSTR,IFAIL,STRLEN
       EXTERNAL INPCMX,STRLEN
       DOUBLE PRECISION QVOL
*** Assume the CALL will fail.
       IFAIL=1
*** Verify that we really have a cell.
       IF(.NOT.CELSET)THEN
            PRINT *,' !!!!!! CELCAL WARNING : Cell data not available'//
     -           ' ; call not executed.'
            RETURN
       ENDIF
*** Some easy reference variables.
       NARG=INS(INSTR,3)
       IPROC=INS(INSTR,1)
**  Get general information about the cell.
       IF(IPROC.EQ.-11)THEN
*   Check number of arguments.
            IF(NARG.GT.4)THEN
                 PRINT *,' !!!!!! CELCAL WARNING : Incorrect number'//
     -                ' of arguments for GET_CELL_DATA.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.1.AND.ARGREF(1,1).GE.2).OR.
     -           (NARG.GE.2.AND.ARGREF(2,1).GE.2).OR.
     -           (NARG.GE.3.AND.ARGREF(3,1).GE.2).OR.
     -           (NARG.GE.4.AND.ARGREF(4,1).GE.2))THEN
                 PRINT *,' !!!!!! CELCAL WARNING : Some arguments'//
     -                ' of GET_CELL_DATA can not be modified.'
                 RETURN
            ENDIF
*   Variables already in use as strings ?
            DO 200 ISTR=1,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
200         CONTINUE
*   Store the cell information.
            IF(NARG.GE.1)THEN
                 ARG(1)=REAL(NWIRE)
                 MODARG(1)=2
            ENDIF
            IF(NARG.GE.2)THEN
                 CALL STRBUF('STORE',IAUX,TYPE,3,IFAIL1)
                 ARG(2)=REAL(IAUX)
                 MODARG(2)=1
            ELSE
                 IFAIL1=0
            ENDIF
            IF(NARG.GE.3)THEN
                 IF(POLAR)THEN
                      CALL STRBUF('STORE',IAUX,'Polar',5,IFAIL2)
                 ELSEIF(TUBE)THEN
                      CALL STRBUF('STORE',IAUX,'Tube',4,IFAIL2)
                 ELSE
                      CALL STRBUF('STORE',IAUX,'Cartesian',9,IFAIL2)
                 ENDIF
                 ARG(3)=REAL(IAUX)
                 MODARG(3)=1
            ELSE
                 IFAIL2=0
            ENDIF
            IF(NARG.GE.4)THEN
                 CALL STRBUF('STORE',IAUX,CELLID,STRLEN(CELLID),IFAIL3)
                 ARG(4)=REAL(IAUX)
                 MODARG(4)=1
            ELSE
                 IFAIL3=0
            ENDIF
*   Error processing.
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0)
     -           PRINT *,' !!!!!! CELCAL WARNING : Error storing'//
     -           ' strings for GET_CELL_DATA.'
*** Get the cell size.
       ELSEIF(IPROC.EQ.-12)THEN
*   Check number of arguments.
            IF(NARG.NE.6.AND.NARG.NE.4)THEN
                 PRINT *,' !!!!!! CELCAL WARNING : Incorrect number'//
     -                ' of arguments for GET_CELL_SIZE.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF(ARGREF(1,1).GE.2.OR.ARGREF(2,1).GE.2.OR.
     -           ARGREF(3,1).GE.2.OR.ARGREF(4,1).GE.2.OR.
     -           (NARG.EQ.6.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.EQ.6.AND.ARGREF(6,1).GE.2))THEN
                 PRINT *,' !!!!!! CELCAL WARNING : Some arguments'//
     -                ' of GET_CELL_SIZE can not be modified.'
                 RETURN
            ENDIF
*   Variables already in use as strings ?
            DO 210 ISTR=1,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
210         CONTINUE
*   Store the cell size.
            IF(NARG.EQ.4)THEN
                 ARG(1)=XMIN
                 MODARG(1)=2
                 ARG(2)=YMIN
                 MODARG(2)=2
                 ARG(3)=XMAX
                 MODARG(3)=2
                 ARG(4)=YMAX
                 MODARG(4)=2
            ELSE
                 ARG(1)=XMIN
                 MODARG(1)=2
                 ARG(2)=YMIN
                 MODARG(2)=2
                 ARG(3)=ZMIN
                 MODARG(3)=2
                 ARG(4)=XMAX
                 MODARG(4)=2
                 ARG(5)=YMAX
                 MODARG(5)=2
                 ARG(6)=ZMAX
                 MODARG(6)=2
            ENDIF
*** Get wire information.
       ELSEIF(IPROC.EQ.-13)THEN
*   Check number of arguments.
            IF(NARG.LT.2.OR.NARG.GT.10)THEN
                 PRINT *,' !!!!!! CELCAL WARNING : Incorrect number'//
     -                ' of arguments for GET_WIRE_DATA.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.2.AND.ARGREF(2,1).GE.2).OR.
     -           (NARG.GE.3.AND.ARGREF(3,1).GE.2).OR.
     -           (NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2).OR.
     -           (NARG.GE.8.AND.ARGREF(8,1).GE.2).OR.
     -           (NARG.GE.9.AND.ARGREF(9,1).GE.2).OR.
     -           (NARG.GE.10.AND.ARGREF(10,1).GE.2))THEN
                 PRINT *,' !!!!!! CELCAL WARNING : Some arguments'//
     -                ' of GET_WIRE_DATA can not be modified.'
                 RETURN
            ENDIF
*   Verify the wire number.
            IF(MODARG(1).NE.2.OR.ABS(ARG(1)-ANINT(ARG(1))).GT.1E-3.OR.
     -           NINT(ARG(1)).LE.0.OR.NINT(ARG(1)).GT.NWIRE)THEN
                 PRINT *,' CELCAL WARNING : The wire number in the'//
     -                ' GET_WIRE_DATA call is not valid.'
                 RETURN
            ENDIF
*   Variables already in use as strings ?
            DO 220 ISTR=2,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
220         CONTINUE
*   Store the wire information.
            IW=NINT(ARG(1))
            IF(NARG.GE.2)THEN
                 ARG(2)=X(IW)
                 MODARG(2)=2
            ENDIF
            IF(NARG.GE.3)THEN
                 ARG(3)=Y(IW)
                 MODARG(3)=2
            ENDIF
            IF(NARG.GE.4)THEN
                 ARG(4)=V(IW)
                 MODARG(4)=2
            ENDIF
            IF(NARG.GE.5)THEN
                 ARG(5)=D(IW)
                 MODARG(5)=2
            ENDIF
            IF(NARG.GE.6)THEN
                 ARG(6)=E(IW)
                 MODARG(6)=2
            ENDIF
            IF(NARG.GE.7)THEN
                 CALL STRBUF('STORE',IAUX,WIRTYP(IW),1,IFAIL1)
                 ARG(7)=IAUX
                 MODARG(7)=1
            ELSE
                 IFAIL1=0
            ENDIF
            IF(NARG.GE.8)THEN
                 ARG(8)=U(IW)
                 MODARG(8)=2
            ENDIF
            IF(NARG.GE.9)THEN
                 ARG(9)=W(IW)
                 MODARG(9)=2
            ENDIF
            IF(NARG.GE.10)THEN
                 ARG(10)=DENS(IW)
                 MODARG(10)=2
            ENDIF
*   Error processing.
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! CELCAL WARNING : Error'//
     -           ' storing strings for GET_WIRE_DATA.'
*** Get information about the planes in x.
       ELSEIF(IPROC.EQ.-14)THEN
*   Check number of arguments.
            IF(NARG.NE.8)THEN
                 PRINT *,' !!!!!! CELCAL WARNING : Incorrect number'//
     -                ' of arguments for GET_X_PLANES.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF(ARGREF(1,1).GE.2.OR.ARGREF(2,1).GE.2.OR.
     -           ARGREF(3,1).GE.2.OR.ARGREF(4,1).GE.2.OR.
     -           ARGREF(5,1).GE.2.OR.ARGREF(6,1).GE.2.OR.
     -           ARGREF(7,1).GE.2.OR.ARGREF(8,1).GE.2)THEN
                 PRINT *,' !!!!!! CELCAL WARNING : Some arguments'//
     -                ' of GET_X_PLANES can not be modified.'
                 RETURN
            ENDIF
*   Variables already in use as strings ?
            DO 230 ISTR=1,8
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
230         CONTINUE
*   Store the information about the planes.
            IF(YNPLAN(1))THEN
                 ARG(1)=1
                 ARG(2)=COPLAN(1)
                 ARG(3)=VTPLAN(1)
                 CALL STRBUF('STORE',IAUX,PLATYP(1),1,IFAIL1)
                 ARG(4)=REAL(IAUX)
                 MODARG(1)=3
                 MODARG(2)=2
                 MODARG(3)=2
                 MODARG(4)=1
            ELSE
                 ARG(1)=0
                 ARG(2)=0
                 ARG(3)=0
                 ARG(4)=0
                 MODARG(1)=3
                 MODARG(2)=0
                 MODARG(3)=0
                 MODARG(4)=0
            ENDIF
            IF(YNPLAN(2))THEN
                 ARG(5)=1
                 ARG(6)=COPLAN(2)
                 ARG(7)=VTPLAN(2)
                 CALL STRBUF('STORE',IAUX,PLATYP(2),1,IFAIL1)
                 ARG(8)=REAL(IAUX)
                 MODARG(5)=3
                 MODARG(6)=2
                 MODARG(7)=2
                 MODARG(8)=1
            ELSE
                 ARG(5)=0
                 ARG(6)=0
                 ARG(7)=0
                 ARG(8)=0
                 MODARG(5)=3
                 MODARG(6)=0
                 MODARG(7)=0
                 MODARG(8)=0
            ENDIF
*** Get information about the planes in y.
       ELSEIF(IPROC.EQ.-15)THEN
*   Check number of arguments.
            IF(NARG.NE.8)THEN
                 PRINT *,' !!!!!! CELCAL WARNING : Incorrect number'//
     -                ' of arguments for GET_Y_PLANES.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF(ARGREF(1,1).GE.2.OR.ARGREF(2,1).GE.2.OR.
     -           ARGREF(3,1).GE.2.OR.ARGREF(4,1).GE.2.OR.
     -           ARGREF(5,1).GE.2.OR.ARGREF(6,1).GE.2.OR.
     -           ARGREF(7,1).GE.2.OR.ARGREF(8,1).GE.2)THEN
                 PRINT *,' !!!!!! CELCAL WARNING : Some arguments'//
     -                ' of GET_Y_PLANES can not be modified.'
                 RETURN
            ENDIF
*   Variables already in use as strings ?
            DO 235 ISTR=1,8
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
235         CONTINUE
*   Store the information about the planes.
            IF(YNPLAN(3))THEN
                 ARG(1)=1
                 ARG(2)=COPLAN(3)
                 ARG(3)=VTPLAN(3)
                 CALL STRBUF('STORE',IAUX,PLATYP(3),1,IFAIL1)
                 ARG(4)=REAL(IAUX)
                 MODARG(1)=3
                 MODARG(2)=2
                 MODARG(3)=2
                 MODARG(4)=1
            ELSE
                 ARG(1)=0
                 ARG(2)=0
                 ARG(3)=0
                 ARG(4)=0
                 MODARG(1)=3
                 MODARG(2)=0
                 MODARG(3)=0
                 MODARG(4)=0
            ENDIF
            IF(YNPLAN(4))THEN
                 ARG(5)=1
                 ARG(6)=COPLAN(4)
                 ARG(7)=VTPLAN(4)
                 CALL STRBUF('STORE',IAUX,PLATYP(4),1,IFAIL1)
                 ARG(8)=REAL(IAUX)
                 MODARG(5)=3
                 MODARG(6)=2
                 MODARG(7)=2
                 MODARG(8)=1
            ELSE
                 ARG(5)=0
                 ARG(6)=0
                 ARG(7)=0
                 ARG(8)=0
                 MODARG(5)=3
                 MODARG(6)=0
                 MODARG(7)=0
                 MODARG(8)=0
            ENDIF
*** Get information about periodicities.
       ELSEIF(IPROC.EQ.-16)THEN
*   Check number of arguments.
            IF(NARG.NE.4)THEN
                 PRINT *,' !!!!!! CELCAL WARNING : Incorrect number'//
     -                ' of arguments for GET_PERIODS.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF(ARGREF(1,1).GE.2.OR.ARGREF(2,1).GE.2.OR.
     -           ARGREF(3,1).GE.2.OR.ARGREF(4,1).GE.2)THEN
                 PRINT *,' !!!!!! CELCAL WARNING : Some arguments'//
     -                ' of GET_PERIODS can not be modified.'
                 RETURN
            ENDIF
*   Variables already in use ?
            DO 240 ISTR=1,4
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
240         CONTINUE
*   Store the periodicity information.
            MODARG(1)=3
            MODARG(2)=2
            IF(PERX)THEN
                 ARG(1)=1
                 ARG(2)=SX
            ELSE
                 ARG(1)=0
                 ARG(2)=0
            ENDIF
            MODARG(3)=3
            MODARG(4)=2
            IF(PERY)THEN
                 ARG(3)=1
                 ARG(4)=SY
            ELSE
                 ARG(3)=0
                 ARG(4)=0
            ENDIF
*** Get information on a solid.
       ELSEIF(IPROC.EQ.-17)THEN
*   Check number of arguments.
            IF(NARG.NE.2)THEN
                 PRINT *,' !!!!!! CELCAL WARNING : Incorrect number'//
     -                ' of arguments for GET_SOLID_DATA.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF(ARGREF(2,1).GE.2)THEN
                 PRINT *,' !!!!!! CELCAL WARNING : The 2nd argument'//
     -                ' of GET_SOLID_DATA can not be modified.'
                 RETURN
*   Verify the solid number.
            ELSEIF(MODARG(1).NE.2.OR.
     -           ABS(ARG(1)-ANINT(ARG(1))).GT.1E-3.OR.
     -           NINT(ARG(1)).LE.0.OR.NINT(ARG(1)).GT.NSOLID)THEN
                 PRINT *,' CELCAL WARNING : The solid number in the'//
     -                ' GET_SOLID_DATA call is not valid.'
                 RETURN
            ENDIF
*   Variables already in use ?
            CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
*   Return the charge
            CALL BEMVOQ(NINT(ARG(1)), QVOL)
            ARG(2)=REAL(QVOL)/(4*PI*EPS0)
            MODARG(2)=2
*** Unknown cell operation.
       ELSE
            PRINT *,' !!!!!! CELCAL WARNING : Unknown procedure code'//
     -           ' received; nothing done.'
            IFAIL=1
            RETURN
       ENDIF
*** Seems to have worked.
       IFAIL=0
       END
