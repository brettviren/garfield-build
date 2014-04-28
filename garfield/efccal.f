CDECK  ID>, EFCCAL.
       SUBROUTINE EFCCAL(INSTR,IFAIL)
*-----------------------------------------------------------------------
*   EFCCAL - Processes electric and magnetic field related procedure
*            calls.
*   (Last changed on 16/12/09.)
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
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
       REAL EXMAP,EYMAP,EZMAP,VMAP,EWXMAP,EWYMAP,EWZMAP,VWMAP,
     -      BXMAP,BYMAP,BZMAP,
     -      XMAP,YMAP,ZMAP,XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,
     -      VMMIN,VMMAX,EPSMAT,EPSSUR,XFMOFF,YFMOFF,ZFMOFF
       INTEGER MATMAP,NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS,
     -      NWMAP
       LOGICAL MAPFLG,LMAPPL,SETAX,SETAY,SETAZ,ELMDGN,LSFDER
       CHARACTER EWSTYP
       CHARACTER*10 MATSRC
       COMMON /FLDMAP/ VMAP(MXMAP,10),VWMAP(MXMAP,10,MXWMAP),
     -      EXMAP(MXMAP,10),EYMAP(MXMAP,10),EZMAP(MXMAP,10),
     -      EWXMAP(MXMAP,10,MXWMAP),EWYMAP(MXMAP,10,MXWMAP),
     -      EWZMAP(MXMAP,10,MXWMAP),
     -      BXMAP(MXMAP,10),BYMAP(MXMAP,10),BZMAP(MXMAP,10),
     -      XMAP(MXMAP,10),YMAP(MXMAP,10),ZMAP(MXMAP,10),
     -      XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,VMMIN,VMMAX,
     -      XFMOFF,YFMOFF,ZFMOFF,
     -      EPSMAT(MXEPS),EPSSUR(MXEPS),MATMAP(MXMAP),
     -      NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS(MXWMAP),NWMAP,
     -      MAPFLG(10+4*MXWMAP),ELMDGN(MXMAP),
     -      LMAPPL,SETAX,SETAY,SETAZ,LSFDER
       COMMON /FLDCHR/ EWSTYP(MXWMAP),MATSRC
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
       INTEGER INSTR,IFAIL,IFAIL1,IPROC,NARG,ISTR,ILOC,IAUX,NU,NV,NC,
     -      IREF(9),ISLOT(9),NDAT,MATSLT,ISIZ(MXMDIM),IDIM,I,J,IMAP,NC1
       REAL XPOS,YPOS,ZPOS,T1,T2,T3,T4,VXMIN,VYMIN,VXMAX,VYMAX
       DOUBLE PRECISION JAC(4,4),DET
       CHARACTER*80 TITLE
       CHARACTER*20 AUX1
       EXTERNAL MATSLT
*** Assume the CALL will fail.
       IFAIL=1
*** Make sure that a cell is available
       IF(.NOT.CELSET)THEN
            PRINT *,' !!!!!! EFCCAL WARNING : Cell data not available'//
     -           ' ; call not executed.'
            RETURN
       ENDIF
*** Some easy reference variables.
       NARG=INS(INSTR,3)
       IPROC=INS(INSTR,1)
*** Electric field in 2 dimensions.
       IF(IPROC.EQ.-301)THEN
*   Check number of arguments.
            IF(NARG.LT.3.OR.NARG.GT.8)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Incorrect number'//
     -                ' of arguments for ELECTRIC_FIELD.'
                 RETURN
*   Check argument mode.
            ELSEIF((MODARG(1).NE.2.AND.MODARG(1).NE.5).OR.
     -           (MODARG(2).NE.2.AND.MODARG(2).NE.5))THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Some arguments of'//
     -                ' ELECTRIC_FIELD are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.3.AND.ARGREF(3,1).GE.2).OR.
     -           (NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2).OR.
     -           (NARG.GE.8.AND.ARGREF(8,1).GE.2))THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Some arguments'//
     -                ' of ELECTRIC_FIELD can not be modified.'
                 RETURN
            ENDIF
*   Clear variables that will be overwritten.
            DO 200 ISTR=3,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
200         CONTINUE
**  Carry out the calculation, first for all scalar arguments.
            IF(MODARG(1).EQ.2.AND.MODARG(2).EQ.2)THEN
                 CALL EFIELD(ARG(1),ARG(2),0.0,ARG(3),
     -                ARG(4),ARG(5),ARG(6),ARG(7),1,ILOC)
                 MODARG(3)=2
                 MODARG(4)=2
                 MODARG(5)=2
                 MODARG(6)=2
                 MODARG(7)=2
                 IF(NARG.GE.8)THEN
                      IF(ILOC.EQ.-10)THEN
                           CALL STRBUF('STORE',IAUX,
     -                          'Unknown potential',17,IFAIL1)
                      ELSEIF(ILOC.EQ.-5)THEN
                           CALL STRBUF('STORE',IAUX,
     -                          'In a material',13,IFAIL1)
                      ELSEIF(ILOC.EQ.-6)THEN
                           CALL STRBUF('STORE',IAUX,
     -                          'Outside mesh',12,IFAIL1)
                      ELSEIF(ILOC.LT.0)THEN
                           CALL STRBUF('STORE',IAUX,
     -                          'Outside plane',13,IFAIL1)
                      ELSEIF(ILOC.EQ.0)THEN
                           CALL STRBUF('STORE',IAUX,
     -                          'Normal',6,IFAIL1)
                      ELSEIF(ILOC.LE.NWIRE)THEN
                           CALL OUTFMT(REAL(ILOC),2,AUX1,NC1,'LEFT')
                           CALL STRBUF('STORE',IAUX,'In '//
     -                          WIRTYP(ILOC)//'-wire '//AUX1(1:NC1),
     -                          10+NC1,IFAIL1)
                      ELSEIF(ILOC.GT.2*MXWIRE.AND.
     -                     ILOC.LE.2*MXWIRE+NSOLID)THEN
                           CALL OUTFMT(REAL(ILOC-2*MXWIRE),2,AUX1,NC1,
     -                          'LEFT')
                           CALL STRBUF('STORE',IAUX,'In '//
     -                          SOLTYP(ILOC-2*MXWIRE)//'-solid '//
     -                          AUX1(1:NC1),11+NC1,IFAIL1)
                      ELSE
                           CALL STRBUF('STORE',IAUX,'Unknown',7,IFAIL1)
                      ENDIF
                      ARG(8)=REAL(IAUX)
                      MODARG(8)=1
*   Error processing.
                      IF(IFAIL1.NE.0)
     -                     PRINT *,' !!!!!! EFCCAL WARNING : '//
     -                     'Error storing a string for ELECTRIC_FIELD.'
                 ENDIF
**  At least one of them is a matrix.
            ELSE
*   Figure out what the dimensions are.
                 NDAT=-1
                 DO 30 I=1,2
                 IF(MODARG(I).EQ.5)THEN
                      IREF(I)=NINT(ARG(I))
                      ISLOT(I)=MATSLT(IREF(I))
                      IF(ISLOT(I).LE.0)THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                          ' locate a input matrix.'
                           RETURN
                      ELSEIF(MMOD(ISLOT(I)).NE.2)THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : x, y'//
     -                          ' Or z matrix of incorrect type.'
                           RETURN
                      ENDIF
                      IF(NDAT.LT.0)THEN
                           NDAT=MLEN(ISLOT(I))
                           DO 10 J=1,MDIM(ISLOT(I))
                           ISIZ(J)=MSIZ(ISLOT(I),J)
10                         CONTINUE
                           IDIM=MDIM(ISLOT(I))
                      ELSEIF(NDAT.NE.MLEN(ISLOT(I)))THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : x, y'//
     -                          ' And z have inconsistent lengths.'
                           RETURN
                      ENDIF
                 ENDIF
30               CONTINUE
                 IF(NDAT.LT.1)THEN
                      PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                     ' to find an x, y or z matrix.'
                      RETURN
                 ENDIF
*   Now book matrices for the missing elements and initialise them.
                 DO 40 I=1,2
                 IF(MODARG(I).NE.5)THEN
                      CALL MATADM('ALLOCATE',IREF(I),IDIM,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                          ' to get a replacement matrix.'
                           RETURN
                      ENDIF
                      ISLOT(I)=MATSLT(IREF(I))
                      IF(ISLOT(I).LE.0)THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                          ' to locate a replacement matrix.'
                           RETURN
                      ENDIF
                      DO 50 J=1,MLEN(ISLOT(I))
                      MVEC(MORG(ISLOT(I))+J)=ARG(I)
50                    CONTINUE
                 ENDIF
40               CONTINUE
*   Allocate the 6 output arrays (Ex, Ey, Ez, E, V, status).
                 DO 20 I=4,9
                 CALL MATADM('ALLOCATE',IREF(I),IDIM,ISIZ,2,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                     ' to get an output matrix.'
                      RETURN
                 ENDIF
20               CONTINUE
*   And finally locate all 9 matrices.
                 DO 60 I=1,9
                 IF(I.EQ.3)GOTO 60
                 ISLOT(I)=MATSLT(IREF(I))
                 IF(ISLOT(I).LE.0)THEN
                      PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                     ' to locate an input or output matrix.'
                      RETURN
                 ENDIF
60               CONTINUE
*   And compute the data.
                 DO 70 I=1,NDAT
                 CALL EFIELD(MVEC(MORG(ISLOT(1))+I),
     -                MVEC(MORG(ISLOT(2))+I),0.0,
     -                MVEC(MORG(ISLOT(4))+I),MVEC(MORG(ISLOT(5))+I),
     -                MVEC(MORG(ISLOT(6))+I),MVEC(MORG(ISLOT(7))+I),
     -                MVEC(MORG(ISLOT(8))+I),1,ILOC)
                 MVEC(MORG(ISLOT(9))+I)=REAL(ILOC)
70               CONTINUE
*   Delete temporary input matrices.
                 DO 80 I=1,2
                 IF(MODARG(I).NE.5)THEN
                      ISIZ(1)=NDAT
                      CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)PRINT *,' !!!!!! EFCCAL WARNING'//
     -                     ' : Unable to delete a replacement matrix.'
                 ENDIF
80               CONTINUE
*   And save the requested output matrices, delete the others.
                 DO 90 I=4,9
                 IF(NARG.GE.I-1)THEN
                      ARG(I-1)=IREF(I)
                      MODARG(I-1)=5
                 ELSE
                      ISIZ(1)=NDAT
                      CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)PRINT *,' !!!!!! EFCCAL WARNING'//
     -                     ' : Unable to delete an unused output.'
                 ENDIF
90               CONTINUE
            ENDIF
*** Electric field in 3 dimensions.
       ELSEIF(IPROC.EQ.-302)THEN
*   Check number of arguments.
            IF(NARG.LT.4.OR.NARG.GT.9)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Incorrect number'//
     -                ' of arguments for ELECTRIC_FIELD_3.'
                 RETURN
*   Check argument mode.
            ELSEIF((MODARG(1).NE.2.AND.MODARG(1).NE.5).OR.
     -           (MODARG(2).NE.2.AND.MODARG(2).NE.5).OR.
     -           (MODARG(3).NE.2.AND.MODARG(3).NE.5))THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Some arguments of'//
     -                ' ELECTRIC_FIELD_3 are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2).OR.
     -           (NARG.GE.8.AND.ARGREF(8,1).GE.2).OR.
     -           (NARG.GE.9.AND.ARGREF(9,1).GE.2))THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Some arguments'//
     -                ' of ELECTRIC_FIELD_3 can not be modified.'
                 RETURN
            ENDIF
*   Variables already in use ?
            DO 210 ISTR=4,9
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
210         CONTINUE
**  Carry out the calculation, first for all scalar arguments.
            IF(MODARG(1).EQ.2.AND.MODARG(2).EQ.2.AND.
     -           MODARG(3).EQ.2)THEN
                 CALL EFIELD(ARG(1),ARG(2),ARG(3),
     -                ARG(4),ARG(5),ARG(6),ARG(7),ARG(8),1,ILOC)
                 MODARG(4)=2
                 MODARG(5)=2
                 MODARG(6)=2
                 MODARG(7)=2
                 MODARG(8)=2
                 IF(NARG.GE.9)THEN
                      IF(ILOC.EQ.-10)THEN
                           CALL STRBUF('STORE',IAUX,
     -                          'Unknown potential',17,IFAIL1)
                      ELSEIF(ILOC.EQ.-5)THEN
                           CALL STRBUF('STORE',IAUX,
     -                          'In a material',13,IFAIL1)
                      ELSEIF(ILOC.EQ.-6)THEN
                           CALL STRBUF('STORE',IAUX,
     -                          'Outside mesh',12,IFAIL1)
                      ELSEIF(ILOC.LT.0)THEN
                           CALL STRBUF('STORE',IAUX,
     -                          'Outside plane',13,IFAIL1)
                      ELSEIF(ILOC.EQ.0)THEN
                           CALL STRBUF('STORE',IAUX,
     -                          'Normal',6,IFAIL1)
                      ELSEIF(ILOC.LE.NWIRE)THEN
                           CALL OUTFMT(REAL(ILOC),2,AUX1,NC1,'LEFT')
                           CALL STRBUF('STORE',IAUX,'In '//
     -                          WIRTYP(ILOC)//'-wire '//AUX1(1:NC1),
     -                          10+NC1,IFAIL1)
                      ELSEIF(ILOC.GT.2*MXWIRE.AND.
     -                     ILOC.LE.2*MXWIRE+NSOLID)THEN
                           CALL OUTFMT(REAL(ILOC-2*MXWIRE),2,AUX1,NC1,
     -                          'LEFT')
                           CALL STRBUF('STORE',IAUX,'In '//
     -                          SOLTYP(ILOC-2*MXWIRE)//'-solid '//
     -                          AUX1(1:NC1),11+NC1,IFAIL1)
                      ELSE
                           CALL STRBUF('STORE',IAUX,'Unknown',7,IFAIL1)
                      ENDIF
                      ARG(9)=REAL(IAUX)
                      MODARG(9)=1
*   Error processing.
                      IF(IFAIL1.NE.0)
     -                     PRINT *,' !!!!!! EFCCAL WARNING : '//
     -                     'Error storing a string for'//
     -                     ' ELECTRIC_FIELD_3.'
                 ENDIF
**  At least one of them is a matrix.
            ELSE
*   Figure out what the dimensions are.
                 NDAT=-1
                 DO 130 I=1,3
                 IF(MODARG(I).EQ.5)THEN
                      IREF(I)=NINT(ARG(I))
                      ISLOT(I)=MATSLT(IREF(I))
                      IF(ISLOT(I).LE.0)THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                          ' locate a input matrix.'
                           RETURN
                      ELSEIF(MMOD(ISLOT(I)).NE.2)THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : x, y'//
     -                          ' Or z matrix of incorrect type.'
                           RETURN
                      ENDIF
                      IF(NDAT.LT.0)THEN
                           NDAT=MLEN(ISLOT(I))
                           DO 110 J=1,MDIM(ISLOT(I))
                           ISIZ(J)=MSIZ(ISLOT(I),J)
110                        CONTINUE
                           IDIM=MDIM(ISLOT(I))
                      ELSEIF(NDAT.NE.MLEN(ISLOT(I)))THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : x, y'//
     -                          ' And z have inconsistent lengths.'
                           RETURN
                      ENDIF
                 ENDIF
130              CONTINUE
                 IF(NDAT.LT.1)THEN
                      PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                     ' to find an x, y or z matrix.'
                      RETURN
                 ENDIF
*   Now book matrices for the missing elements and initialise them.
                 DO 140 I=1,3
                 IF(MODARG(I).NE.5)THEN
                      CALL MATADM('ALLOCATE',IREF(I),IDIM,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                          ' to get a replacement matrix.'
                           RETURN
                      ENDIF
                      ISLOT(I)=MATSLT(IREF(I))
                      IF(ISLOT(I).LE.0)THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                          ' to locate a replacement matrix.'
                           RETURN
                      ENDIF
                      DO 150 J=1,MLEN(ISLOT(I))
                      MVEC(MORG(ISLOT(I))+J)=ARG(I)
150                   CONTINUE
                 ENDIF
140              CONTINUE
*   Allocate the 6 output arrays (Ex, Ey, Ez, E, V, status).
                 DO 120 I=4,9
                 CALL MATADM('ALLOCATE',IREF(I),IDIM,ISIZ,2,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                     ' to get an output matrix.'
                      RETURN
                 ENDIF
120              CONTINUE
*   And finally locate all 9 matrices.
                 DO 160 I=1,9
                 ISLOT(I)=MATSLT(IREF(I))
                 IF(ISLOT(I).LE.0)THEN
                      PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                     ' to locate an input or output array.'
                      RETURN
                 ENDIF
160              CONTINUE
*   And compute the data.
                 DO 170 I=1,NDAT
                 CALL EFIELD(MVEC(MORG(ISLOT(1))+I),
     -                MVEC(MORG(ISLOT(2))+I),MVEC(MORG(ISLOT(3))+I),
     -                MVEC(MORG(ISLOT(4))+I),MVEC(MORG(ISLOT(5))+I),
     -                MVEC(MORG(ISLOT(6))+I),MVEC(MORG(ISLOT(7))+I),
     -                MVEC(MORG(ISLOT(8))+I),1,ILOC)
                 MVEC(MORG(ISLOT(9))+I)=REAL(ILOC)
170              CONTINUE
*   Delete temporary input matrices.
                 DO 180 I=1,3
                 IF(MODARG(I).NE.5)THEN
                      ISIZ(1)=NDAT
                      CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)PRINT *,' !!!!!! EFCCAL WARNING'//
     -                     ' : Unable to delete a replacement array.'
                 ENDIF
180              CONTINUE
*   And save the requested output matrices, delete the others.
                 DO 190 I=4,9
                 IF(NARG.GE.I)THEN
                      ARG(I)=IREF(I)
                      MODARG(I)=5
                 ELSE
                      ISIZ(1)=NDAT
                      CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)PRINT *,' !!!!!! EFCCAL WARNING'//
     -                     ' : Unable to delete an unused output.'
                 ENDIF
190              CONTINUE
            ENDIF
*** Force field in 2 dimensions.
       ELSEIF(IPROC.EQ.-303)THEN
*   Check number of arguments.
            IF(NARG.NE.4)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Incorrect number'//
     -                ' of arguments for FORCE_FIELD.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Some arguments of'//
     -                ' FORCE_FIELD are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF(ARGREF(3,1).GE.2.OR.ARGREF(4,1).GE.2)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Some arguments'//
     -                ' of FORCE_FIELD can not be modified.'
                 RETURN
            ENDIF
*   Variables already in use ?
            CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
*   Carry out the calculation.
            CALL FFDBG(ARG(1),ARG(2),ARG(3),ARG(4))
            MODARG(3)=2
            MODARG(4)=2
*** Magnetic field in 2 dimensions.
       ELSEIF(IPROC.EQ.-304)THEN
*   Check number of arguments.
            IF(NARG.LT.3.OR.NARG.GT.6)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Incorrect number'//
     -                ' of arguments for MAGNETIC_FIELD.'
                 RETURN
*   Check argument mode.
            ELSEIF((MODARG(1).NE.2.AND.MODARG(1).NE.5).OR.
     -           (MODARG(2).NE.2.AND.MODARG(2).NE.5))THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Some arguments of'//
     -                ' MAGNETIC_FIELD are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.3.AND.ARGREF(3,1).GE.2).OR.
     -           (NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2))THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Some arguments'//
     -                ' of MAGNETIC_FIELD can not be modified.'
                 RETURN
            ENDIF
*   Clear variables that will be overwritten.
            DO 300 ISTR=3,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
300         CONTINUE
**  Carry out the calculation, first for all scalar arguments.
            IF(MODARG(1).EQ.2.AND.MODARG(2).EQ.2)THEN
                 CALL BFIELD(ARG(1),ARG(2),0.0,
     -                ARG(3),ARG(4),ARG(5),ARG(6))
                 MODARG(3)=2
                 MODARG(4)=2
                 MODARG(5)=2
                 MODARG(6)=2
**  At least one of them is a matrix.
            ELSE
*   Figure out what the dimensions are.
                 NDAT=-1
                 DO 310 I=1,2
                 IF(MODARG(I).EQ.5)THEN
                      IREF(I)=NINT(ARG(I))
                      ISLOT(I)=MATSLT(IREF(I))
                      IF(ISLOT(I).LE.0)THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                          ' locate a input matrix.'
                           RETURN
                      ELSEIF(MMOD(ISLOT(I)).NE.2)THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : x, y'//
     -                          ' Or z matrix of incorrect type.'
                           RETURN
                      ENDIF
                      IF(NDAT.LT.0)THEN
                           NDAT=MLEN(ISLOT(I))
                           DO 320 J=1,MDIM(ISLOT(I))
                           ISIZ(J)=MSIZ(ISLOT(I),J)
320                        CONTINUE
                           IDIM=MDIM(ISLOT(I))
                      ELSEIF(NDAT.NE.MLEN(ISLOT(I)))THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : x, y'//
     -                          ' And z have inconsistent lengths.'
                           RETURN
                      ENDIF
                 ENDIF
310              CONTINUE
                 IF(NDAT.LT.1)THEN
                      PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                     ' to find an x, y or z matrix.'
                      RETURN
                 ENDIF
*   Now book matrices for the missing elements and initialise them.
                 DO 330 I=1,2
                 IF(MODARG(I).NE.5)THEN
                      CALL MATADM('ALLOCATE',IREF(I),IDIM,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                          ' to get a replacement matrix.'
                           RETURN
                      ENDIF
                      ISLOT(I)=MATSLT(IREF(I))
                      IF(ISLOT(I).LE.0)THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                          ' to locate a replacement matrix.'
                           RETURN
                      ENDIF
                      DO 340 J=1,MLEN(ISLOT(I))
                      MVEC(MORG(ISLOT(I))+J)=ARG(I)
340                   CONTINUE
                 ENDIF
330              CONTINUE
*   Allocate the 4 output arrays (Bx, By, Bz, B).
                 DO 350 I=4,7
                 CALL MATADM('ALLOCATE',IREF(I),IDIM,ISIZ,2,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                     ' to get an output matrix.'
                      RETURN
                 ENDIF
350              CONTINUE
*   And finally locate all 7 matrices.
                 DO 360 I=1,7
                 IF(I.EQ.3)GOTO 360
                 ISLOT(I)=MATSLT(IREF(I))
                 IF(ISLOT(I).LE.0)THEN
                      PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                     ' to locate an input or output matrix.'
                      RETURN
                 ENDIF
360               CONTINUE
*   And compute the data.
                 DO 370 I=1,NDAT
                 CALL BFIELD(MVEC(MORG(ISLOT(1))+I),
     -                MVEC(MORG(ISLOT(2))+I),0.0,
     -                MVEC(MORG(ISLOT(4))+I),MVEC(MORG(ISLOT(5))+I),
     -                MVEC(MORG(ISLOT(6))+I),MVEC(MORG(ISLOT(7))+I))
370               CONTINUE
*   Delete temporary input matrices.
                 DO 380 I=1,2
                 IF(MODARG(I).NE.5)THEN
                      ISIZ(1)=NDAT
                      CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)PRINT *,' !!!!!! EFCCAL WARNING'//
     -                     ' : Unable to delete a replacement matrix.'
                 ENDIF
380               CONTINUE
*   And save the requested output matrices, delete the others.
                 DO 390 I=4,7
                 IF(NARG.GE.I-1)THEN
                      ARG(I-1)=IREF(I)
                      MODARG(I-1)=5
                 ELSE
                      ISIZ(1)=NDAT
                      CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)PRINT *,' !!!!!! EFCCAL WARNING'//
     -                     ' : Unable to delete an unused output.'
                 ENDIF
390              CONTINUE
            ENDIF
*** Magnetic field in 3 dimensions.
       ELSEIF(IPROC.EQ.-305)THEN
*   Check number of arguments.
            IF(NARG.LT.4.OR.NARG.GT.7)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Incorrect number'//
     -                ' of arguments for MAGNETIC_FIELD_3.'
                 RETURN
*   Check argument mode.
            ELSEIF((MODARG(1).NE.2.AND.MODARG(1).NE.5).OR.
     -           (MODARG(2).NE.2.AND.MODARG(2).NE.5).OR.
     -           (MODARG(3).NE.2.AND.MODARG(3).NE.5))THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Some arguments of'//
     -                ' MAGNETIC_FIELD_3 are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2))THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Some arguments'//
     -                ' of MAGNETIC_FIELD_3 can not be modified.'
                 RETURN
            ENDIF
*   Variables already in use ?
            DO 400 ISTR=4,7
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
400         CONTINUE
**  Carry out the calculation, first for all scalar arguments.
            IF(MODARG(1).EQ.2.AND.MODARG(2).EQ.2.AND.
     -           MODARG(3).EQ.2)THEN
                 CALL BFIELD(ARG(1),ARG(2),ARG(3),
     -                ARG(4),ARG(5),ARG(6),ARG(7))
                 MODARG(4)=2
                 MODARG(5)=2
                 MODARG(6)=2
                 MODARG(7)=2
**  At least one of them is a matrix.
            ELSE
*   Figure out what the dimensions are.
                 NDAT=-1
                 DO 410 I=1,3
                 IF(MODARG(I).EQ.5)THEN
                      IREF(I)=NINT(ARG(I))
                      ISLOT(I)=MATSLT(IREF(I))
                      IF(ISLOT(I).LE.0)THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                          ' locate a input matrix.'
                           RETURN
                      ELSEIF(MMOD(ISLOT(I)).NE.2)THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : x, y'//
     -                          ' Or z matrix of incorrect type.'
                           RETURN
                      ENDIF
                      IF(NDAT.LT.0)THEN
                           NDAT=MLEN(ISLOT(I))
                           DO 420 J=1,MDIM(ISLOT(I))
                           ISIZ(J)=MSIZ(ISLOT(I),J)
420                        CONTINUE
                           IDIM=MDIM(ISLOT(I))
                      ELSEIF(NDAT.NE.MLEN(ISLOT(I)))THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : x, y'//
     -                          ' And z have inconsistent lengths.'
                           RETURN
                      ENDIF
                 ENDIF
410              CONTINUE
                 IF(NDAT.LT.1)THEN
                      PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                     ' to find an x, y or z matrix.'
                      RETURN
                 ENDIF
*   Now book matrices for the missing elements and initialise them.
                 DO 430 I=1,3
                 IF(MODARG(I).NE.5)THEN
                      CALL MATADM('ALLOCATE',IREF(I),IDIM,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                          ' to get a replacement matrix.'
                           RETURN
                      ENDIF
                      ISLOT(I)=MATSLT(IREF(I))
                      IF(ISLOT(I).LE.0)THEN
                           PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                          ' to locate a replacement matrix.'
                           RETURN
                      ENDIF
                      DO 440 J=1,MLEN(ISLOT(I))
                      MVEC(MORG(ISLOT(I))+J)=ARG(I)
440                   CONTINUE
                 ENDIF
430              CONTINUE
*   Allocate the 6 output arrays (Bx, By, Bz, B).
                 DO 450 I=4,7
                 CALL MATADM('ALLOCATE',IREF(I),IDIM,ISIZ,2,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                     ' to get an output matrix.'
                      RETURN
                 ENDIF
450              CONTINUE
*   And finally locate all 7 matrices.
                 DO 460 I=1,7
                 ISLOT(I)=MATSLT(IREF(I))
                 IF(ISLOT(I).LE.0)THEN
                      PRINT *,' !!!!!! EFCCAL WARNING : Unable'//
     -                     ' to locate an input or output array.'
                      RETURN
                 ENDIF
460              CONTINUE
*   And compute the data.
                 DO 470 I=1,NDAT
                 CALL BFIELD(MVEC(MORG(ISLOT(1))+I),
     -                MVEC(MORG(ISLOT(2))+I),MVEC(MORG(ISLOT(3))+I),
     -                MVEC(MORG(ISLOT(4))+I),MVEC(MORG(ISLOT(5))+I),
     -                MVEC(MORG(ISLOT(6))+I),MVEC(MORG(ISLOT(7))+I))
470              CONTINUE
*   Delete temporary input matrices.
                 DO 480 I=1,3
                 IF(MODARG(I).NE.5)THEN
                      ISIZ(1)=NDAT
                      CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)PRINT *,' !!!!!! EFCCAL WARNING'//
     -                     ' : Unable to delete a replacement array.'
                 ENDIF
480              CONTINUE
*   And save the requested output matrices, delete the others.
                 DO 490 I=4,7
                 IF(NARG.GE.I)THEN
                      ARG(I)=IREF(I)
                      MODARG(I)=5
                 ELSE
                      ISIZ(1)=NDAT
                      CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)PRINT *,' !!!!!! EFCCAL WARNING'//
     -                     ' : Unable to delete an unused output.'
                 ENDIF
490              CONTINUE
            ENDIF
*** Charge integration in 2 and 3 dimensions.
       ELSEIF(IPROC.EQ.-306)THEN
*   Check number of arguments.
            IF(NARG.LT.4.OR.NARG.GT.5)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Incorrect number'//
     -                ' of arguments for INTEGRATE_CHARGE.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           (NARG.EQ.5.AND.MODARG(3).NE.2))THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Some arguments of'//
     -                ' INTEGRATE_CHARGE are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF(ARGREF(NARG,1).GE.2)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : The result'//
     -                ' of INTEGRATE_CHARGE can not be assigned.'
                 RETURN
            ENDIF
*   Variables already in use ?
            CALL ALGREU(NINT(ARG(NARG)),MODARG(NARG),ARGREF(NARG,1))
*   Carry out the calculation.
            IF(NARG.EQ.4)THEN
                 CALL FLDIN2(ARG(1),ARG(2),ARG(3),ARG(4))
                 MODARG(4)=2
            ELSE
                 CALL FLDIN3(ARG(1),ARG(2),ARG(3),ARG(4),ARG(5))
                 MODARG(5)=2
            ENDIF
*** Flux integration over a parallelogram.
       ELSEIF(IPROC.EQ.-307)THEN
*   Check number of arguments.
            IF(NARG.LT.10.OR.NARG.GT.12)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Incorrect number'//
     -                ' of arguments for INTEGRATE_FLUX.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.2.OR.MODARG(4).NE.2.OR.
     -           MODARG(5).NE.2.OR.MODARG(6).NE.2.OR.
     -           MODARG(7).NE.2.OR.MODARG(8).NE.2.OR.
     -           MODARG(9).NE.2.OR.
     -           (NARG.GE.11.AND.MODARG(11).NE.2).OR.
     -           (NARG.GE.12.AND.MODARG(12).NE.2))THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Some arguments of'//
     -                ' INTEGRATE_FLUX are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF(ARGREF(10,1).GE.2)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : The result'//
     -                ' of INTEGRATE_FLUX can not be assigned.'
                 RETURN
            ENDIF
*   Variables already in use ?
            CALL ALGREU(NINT(ARG(10)),MODARG(10),ARGREF(10,1))
*   Fetch the number of integration points, if present.
            NU=20
            NV=20
            IF(NARG.GE.11)NU=NINT(ARG(11))
            IF(NARG.GE.12)NV=NINT(ARG(12))
*   Integrate the flux.
            CALL FLDIN4(ARG(1),ARG(2),ARG(3),ARG(4),ARG(5),
     -           ARG(6),ARG(7),ARG(8),ARG(9),ARG(10),NU,NV)
            MODARG(10)=2
*** Returns map indices.
       ELSEIF(IPROC.EQ.-310)THEN
*   See whether there is a field map at all.
            IF(NMAP.LE.0.OR..NOT.MAPFLG(1))THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : There is no'//
     -                ' field map; MAP_INDEX not executed.'
                 RETURN
*   Check number of arguments.
            ELSEIF((MAPTYP.EQ.1.OR.MAPTYP.EQ.2.OR.MAPTYP.EQ.3).AND.
     -           NARG.NE.3.AND.NARG.NE.6)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : The map is made of'//
     -                ' triangles; provide 3 or 6 arguments.'
                 RETURN
            ELSEIF((MAPTYP.EQ.4.OR.MAPTYP.EQ.5).AND.
     -           NARG.NE.3.AND.NARG.NE.5)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : The map is made of'//
     -                ' quadrilaterals; provide 3 or 5 arguments.'
                 RETURN
            ELSEIF((MAPTYP.EQ.11.OR.MAPTYP.EQ.12.OR.MAPTYP.EQ.13).AND.
     -           NARG.NE.4.AND.NARG.NE.8)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : The map is made of'//
     -                ' tetrahedrons; provide 4 or 8 arguments.'
                 RETURN
            ELSEIF((MAPTYP.EQ.14.OR.MAPTYP.EQ.15.OR.MAPTYP.EQ.16).AND.
     -           NARG.NE.4.AND.NARG.NE.7)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : The map is made of'//
     -                ' hexahedra; provide 4 or 7 arguments.'
                 RETURN
            ELSEIF(MAPTYP.LE.0.OR.(MAPTYP.GE.6.AND.MAPTYP.LE.10).OR.
     -           MAPTYP.GE.17)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Unknown element'//
     -                ' type; MAP_INDEX not executed.'
                 RETURN
*   Check argument mode and return possibilities.
            ELSEIF(
     -           (NARG.EQ.3.AND.
     -           (MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           ARGREF(3,1).GE.2)).OR.
     -           (NARG.EQ.4.AND.
     -           (MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.2.OR.ARGREF(4,1).GE.2)).OR.
     -           (NARG.EQ.6.AND.
     -           (MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           ARGREF(3,1).GE.2.OR.ARGREF(4,1).GE.2.OR.
     -           ARGREF(5,1).GE.2.OR.ARGREF(6,1).GE.2)).OR.
     -           (NARG.EQ.8.AND.
     -           (MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.2.OR.
     -           ARGREF(4,1).GE.2.OR.ARGREF(5,1).GE.2.OR.
     -           ARGREF(6,1).GE.2.OR.ARGREF(7,1).GE.2.OR.
     -           ARGREF(8,1).GE.2)))THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Some arguments of'//
     -                ' MAP_INDEX are of incorrect type.'
                 RETURN
            ENDIF
*   Find the map indices.
            XPOS=ARG(1)
            YPOS=ARG(2)
            IF(MAPTYP.GT.10)THEN
                 ZPOS=ARG(3)
            ELSE
                 ZPOS=0
            ENDIF
            CALL MAPIND(XPOS,YPOS,ZPOS,T1,T2,T3,T4,JAC,DET,IMAP)
            IF(IMAP.LE.0)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Point is not'//
     -                ' located in an element.'
                 RETURN
            ENDIF
*   Variables already in use ?
            IF(MAPTYP.GT.10)THEN
                 DO 240 I=4,NARG
                 CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
240              CONTINUE
            ELSE
                 DO 250 I=3,NARG
                 CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
250              CONTINUE
            ENDIF
*   Return the results.
            IF(MAPTYP.GT.10)THEN
                 ARG(4)=IMAP
                 ARG(5)=T1
                 ARG(6)=T2
                 ARG(7)=T3
                 ARG(8)=T4
                 MODARG(4)=2
                 MODARG(5)=2
                 MODARG(6)=2
                 MODARG(7)=2
                 MODARG(8)=2
            ELSE
                 ARG(3)=IMAP
                 ARG(4)=T1
                 ARG(5)=T2
                 ARG(6)=T3
                 MODARG(3)=2
                 MODARG(4)=2
                 MODARG(5)=2
                 MODARG(6)=2
            ENDIF
*** Return the volume element.
       ELSEIF(IPROC.EQ.-311)THEN
*   See whether there is a field map at all.
            IF(NMAP.LE.0.OR..NOT.MAPFLG(1))THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : There is no'//
     -                ' field map; MAP_ELEMENT not executed.'
                 RETURN
*   Check number of arguments.
            ELSEIF((MAPTYP.EQ.1.OR.MAPTYP.EQ.2.OR.MAPTYP.EQ.3).AND.
     -           NARG.NE.7)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : The map is made of'//
     -                ' triangles; provide 7 arguments.'
                 RETURN
            ELSEIF((MAPTYP.EQ.4.OR.MAPTYP.EQ.5).AND.NARG.NE.9)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : The map is made of'//
     -                ' quadrilaterals; provide 9 arguments.'
                 RETURN
            ELSEIF((MAPTYP.EQ.11.OR.MAPTYP.EQ.12.OR.MAPTYP.EQ.13).AND.
     -           NARG.NE.13)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : The map is made of'//
     -                ' tetrahedrons; provide 13 arguments.'
                 RETURN
            ELSEIF((MAPTYP.EQ.14.OR.MAPTYP.EQ.15.OR.MAPTYP.EQ.16).AND.
     -           NARG.NE.25)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : The map is made of'//
     -                ' hexahedra; provide 25 arguments.'
                 RETURN
*   Check argument mode and return possibilities.
            ELSEIF(MODARG(1).NE.2.OR.
     -           (NARG.GE.2.AND.ARGREF(2,1).GE.2).OR.
     -           (NARG.GE.3.AND.ARGREF(3,1).GE.2).OR.
     -           (NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2).OR.
     -           (NARG.GE.8.AND.ARGREF(8,1).GE.2).OR.
     -           (NARG.GE.9.AND.ARGREF(9,1).GE.2).OR.
     -           (NARG.GE.10.AND.ARGREF(10,1).GE.2).OR.
     -           (NARG.GE.11.AND.ARGREF(11,1).GE.2).OR.
     -           (NARG.GE.12.AND.ARGREF(12,1).GE.2).OR.
     -           (NARG.GE.13.AND.ARGREF(13,1).GE.2).OR.
     -           (NARG.GE.14.AND.ARGREF(14,1).GE.2).OR.
     -           (NARG.GE.15.AND.ARGREF(15,1).GE.2).OR.
     -           (NARG.GE.16.AND.ARGREF(16,1).GE.2).OR.
     -           (NARG.GE.17.AND.ARGREF(17,1).GE.2).OR.
     -           (NARG.GE.18.AND.ARGREF(18,1).GE.2).OR.
     -           (NARG.GE.19.AND.ARGREF(19,1).GE.2).OR.
     -           (NARG.GE.20.AND.ARGREF(20,1).GE.2).OR.
     -           (NARG.GE.21.AND.ARGREF(21,1).GE.2).OR.
     -           (NARG.GE.22.AND.ARGREF(22,1).GE.2).OR.
     -           (NARG.GE.23.AND.ARGREF(23,1).GE.2).OR.
     -           (NARG.GE.24.AND.ARGREF(24,1).GE.2).OR.
     -           (NARG.GE.25.AND.ARGREF(25,1).GE.2))THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Some arguments of'//
     -                ' MAP_ELEMENT are of incorrect type.'
                 RETURN
*   Verify map type.
            ELSEIF(MAPTYP.LE.0.OR.(MAPTYP.GE.6.AND.MAPTYP.LE.10).OR.
     -           MAPTYP.GE.17)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Unknown element'//
     -                ' type; MAP_ELEMENT not executed.'
                 RETURN
*   Verify that the element is within range.
            ELSEIF(NINT(ARG(1)).LT.1.OR.NINT(ARG(1)).GT.NMAP)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : MAP_ELEMENT not'//
     -                ' executed, element is out of range.'
                 RETURN
            ENDIF
*   Variables already in use ?
            DO 260 I=2,NARG
            CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
260         CONTINUE
*   Return the element.
            IF(MAPTYP.GE.1.AND.MAPTYP.LE.3)THEN
                 ARG(2)= XMAP(NINT(ARG(1)),1)
                 ARG(3)= YMAP(NINT(ARG(1)),1)
                 ARG(4)= XMAP(NINT(ARG(1)),2)
                 ARG(5)= YMAP(NINT(ARG(1)),2)
                 ARG(6)= XMAP(NINT(ARG(1)),3)
                 ARG(7)= YMAP(NINT(ARG(1)),3)
            ELSEIF(MAPTYP.GE.4.AND.MAPTYP.LE.5)THEN
                 ARG(2)= XMAP(NINT(ARG(1)),1)
                 ARG(3)= YMAP(NINT(ARG(1)),1)
                 ARG(4)= XMAP(NINT(ARG(1)),2)
                 ARG(5)= YMAP(NINT(ARG(1)),2)
                 ARG(6)= XMAP(NINT(ARG(1)),3)
                 ARG(7)= YMAP(NINT(ARG(1)),3)
                 ARG(8)= XMAP(NINT(ARG(1)),4)
                 ARG(9)= YMAP(NINT(ARG(1)),4)
            ELSEIF(MAPTYP.GE.11.AND.MAPTYP.LE.13)THEN
                 ARG(2)= XMAP(NINT(ARG(1)),1)
                 ARG(3)= YMAP(NINT(ARG(1)),1)
                 ARG(4)= ZMAP(NINT(ARG(1)),1)
                 ARG(5)= XMAP(NINT(ARG(1)),2)
                 ARG(6)= YMAP(NINT(ARG(1)),2)
                 ARG(7)= ZMAP(NINT(ARG(1)),2)
                 ARG(8)= XMAP(NINT(ARG(1)),3)
                 ARG(9)= YMAP(NINT(ARG(1)),3)
                 ARG(10)=ZMAP(NINT(ARG(1)),3)
                 ARG(11)=XMAP(NINT(ARG(1)),4)
                 ARG(12)=YMAP(NINT(ARG(1)),4)
                 ARG(13)=ZMAP(NINT(ARG(1)),4)
            ELSEIF(MAPTYP.GE.14.AND.MAPTYP.LE.16)THEN
                 ARG(2)= XMAP(NINT(ARG(1)),1)
                 ARG(3)= YMAP(NINT(ARG(1)),1)
                 ARG(4)= ZMAP(NINT(ARG(1)),1)
                 ARG(5)= XMAP(NINT(ARG(1)),2)
                 ARG(6)= YMAP(NINT(ARG(1)),2)
                 ARG(7)= ZMAP(NINT(ARG(1)),2)
                 ARG(8)= XMAP(NINT(ARG(1)),3)
                 ARG(9)= YMAP(NINT(ARG(1)),3)
                 ARG(10)=ZMAP(NINT(ARG(1)),3)
                 ARG(11)=XMAP(NINT(ARG(1)),4)
                 ARG(12)=YMAP(NINT(ARG(1)),4)
                 ARG(13)=ZMAP(NINT(ARG(1)),4)
                 ARG(14)=XMAP(NINT(ARG(1)),5)
                 ARG(15)=YMAP(NINT(ARG(1)),5)
                 ARG(16)=ZMAP(NINT(ARG(1)),5)
                 ARG(17)=XMAP(NINT(ARG(1)),6)
                 ARG(18)=YMAP(NINT(ARG(1)),6)
                 ARG(19)=ZMAP(NINT(ARG(1)),6)
                 ARG(20)=XMAP(NINT(ARG(1)),7)
                 ARG(21)=YMAP(NINT(ARG(1)),7)
                 ARG(22)=ZMAP(NINT(ARG(1)),7)
                 ARG(23)=XMAP(NINT(ARG(1)),8)
                 ARG(24)=YMAP(NINT(ARG(1)),8)
                 ARG(25)=ZMAP(NINT(ARG(1)),8)
             ENDIF
             DO 270 I=2,NARG
             MODARG(I)=2
270          CONTINUE
*** Material index.
       ELSEIF(IPROC.EQ.-312)THEN
*   Check argument list.
            IF(NARG.LT.2.OR.NARG.GT.3.OR.
     -           MODARG(1).NE.2.OR.
     -           (NARG.GE.2.AND.ARGREF(2,1).GE.2).OR.
     -           (NARG.GE.3.AND.ARGREF(3,1).GE.2))THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Incorrect argument'//
     -                ' list for MAP_MATERIAL'
                 RETURN
*   Make sure the materials are known.
            ELSEIF(.NOT.MAPFLG(9))THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Materials are not'//
     -                ' defined; MAP_MATERIAL not executed.'
                 RETURN
*   Make sure index is in range.
            ELSEIF(NINT(ARG(1)).LT.1.OR.NINT(ARG(1)).GT.NMAP)THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Field map index is'//
     -                ' out of range; MAP_MATERIAL not executed.'
                 RETURN
            ENDIF
*   Clean up variables.
            DO 280 I=2,NARG
            CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
280         CONTINUE
*   Return the values.
            ARG(2)=REAL(MATMAP(NINT(ARG(1))))
            IF(NINT(ARG(2)).GE.1.AND.NINT(ARG(2)).LE.NEPS)THEN
                 ARG(3)=EPSMAT(NINT(ARG(2)))
            ELSE
                 ARG(3)=-1
            ENDIF
            MODARG(2)=2
            MODARG(3)=2
*** Plot the field area.
       ELSEIF(IPROC.EQ.-320)THEN
*   Check arguments.
            IF((NARG.NE.0.AND.NARG.NE.1).OR.
     -           (NARG.EQ.1.AND.MODARG(1).NE.1))THEN
                 PRINT *,' !!!!!! EFCCAL WARNING : Incorrect list'//
     -                ' of arguments for PLOT_FIELD_AREA; no plot.'
                 RETURN
            ENDIF
*   See whether there is a title.
            IF(NARG.EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(1)),TITLE,NC,IFAIL1)
            ELSEIF(CELLID.EQ.' ')THEN
                 TITLE='Layout of the cell'
                 NC=18
            ELSE
                 TITLE=CELLID
                 NC=LEN(CELLID)
            ENDIF
*   Plot the frame.
            CALL GRASET(PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX)
            CALL GRCELL(VXMIN,VYMIN,VXMAX,VYMAX,TITLE(1:NC))
*** Unknown electric field operation.
       ELSE
            PRINT *,' !!!!!! EFCCAL WARNING : Unknown procedure code'//
     -           ' received; nothing done.'
            IFAIL=1
            RETURN
       ENDIF
*** Seems to have worked.
       IFAIL=0
       END
