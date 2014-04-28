CDECK  ID>, MAGCMP.
       SUBROUTINE MAGCMP
*-----------------------------------------------------------------------
*   MAGCMP - Reads the B field components.
*   (Last changed on 25/ 2/00.)
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
       LOGICAL MAGOK
       REAL ALFA,B0X,B0Y,B0Z,SUSWIR,SUSGAS,BSCALE,BFMIN,BFMAX,
     -      BFXMIN,BFYMIN,BFZMIN,BFXMAX,BFYMAX,BFZMAX
       INTEGER MAGSRC,
     -      IBXTYP,IBYTYP,IBZTYP,
     -      IRB0X,IRB0Y,IRB0Z,IRV0X,IRV0Y,IRV0Z,
     -      IENB0X,IENB0Y,IENB0Z,IBXDIR,IBYDIR,IBZDIR,
     -      NCB0X,NCB0Y,NCB0Z
       CHARACTER*(MXCHAR) FUNB0X,FUNB0Y,FUNB0Z
       COMMON /MAGDAT/ ALFA,SUSWIR,SUSGAS,
     -      B0X,B0Y,B0Z,BSCALE,BFMIN,BFMAX,
     -      BFXMIN,BFYMIN,BFZMIN,BFXMAX,BFYMAX,BFZMAX,
     -      MAGSRC,IBXTYP,IBYTYP,IBZTYP,
     -      IRB0X,IRB0Y,IRB0Z,IRV0X,IRV0Y,IRV0Z,
     -      IENB0X,IENB0Y,IENB0Z,IBXDIR,IBYDIR,IBZDIR,
     -      NCB0X,NCB0Y,NCB0Z,
     -      MAGOK
       COMMON /MAGCHR/ FUNB0X,FUNB0Y,FUNB0Z
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(MXCHAR) FUN
       CHARACTER*30 AUX1,AUX2,UNIT
       CHARACTER*10 VARLIS(MXVAR)
       REAL B0,BMIN,BMAX
       INTEGER NWORD,NC1,NC2,NCUNIT,NVAR,I,J,K,INEXT,
     -      ISLOT,MATSLT,NDIM,IDIM(1),IMOD,
     -      ISCOPY,ISDATA,INPTYP,INPCMP,NRES,IFAIL,NCFUN,IENTRY,
     -      IREFB,IREFV,IBTYPE,IDIR,IGLB
       LOGICAL USE(MXVAR),OK
       EXTERNAL INPCMP,INPTYP,MATSLT
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE MAGCMP ///'
*** Count words.
       CALL INPNUM(NWORD)
*** Display current state if there are no arguments.
       IF(NWORD.EQ.1)THEN
*   Scale factor.
            IF(ABS(BSCALE-100.0).LT.0.01)THEN
                 UNIT(1:1)='T'
                 NCUNIT=1
            ELSEIF(ABS(BSCALE-0.01).LT.0.00001)THEN
                 UNIT(1:1)='G'
                 NCUNIT=1
            ELSE
                 CALL OUTFMT(BSCALE/100,2,AUX1,NC1,'LEFT')
                 UNIT=' * '//AUX1(1:NC1)//' T'
                 NCUNIT=NC1+5
            ENDIF
*   x-component.
            IF(POLAR)THEN
                 WRITE(LUNOUT,'(''  Magnetic field components:'',
     -                '' Br = 0 T,'')')
            ELSEIF(IBXTYP.EQ.0)THEN
                 WRITE(LUNOUT,'(''  Magnetic field components: Bx = '',
     -                ''Undefined, assumed to be 0 T,'')')
            ELSEIF(IBXTYP.EQ.1)THEN
                 CALL OUTFMT(B0X*BSCALE/100,2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(''  Magnetic field components: Bx = '',
     -                A,'' T,'')') AUX1(1:NC1)
            ELSEIF(IBXTYP.EQ.2)THEN
                 WRITE(LUNOUT,'(''  Magnetic field components: Bx = '',
     -                A,'' '',A,'','')') FUNB0X(1:NCB0X),UNIT(1:NCUNIT)
            ELSEIF(IBXTYP.EQ.3)THEN
                 CALL OUTFMT(REAL(IRB0X),5,AUX1,NC1,'LEFT')
                 CALL OUTFMT(REAL(IRV0X),5,AUX2,NC2,'LEFT')
                 IF(IBXDIR.EQ.1)THEN
                      WRITE(LUNOUT,'(''  Magnetic field components:'',
     -                     '' Bx = '',A,'' '',A,'' vs''/34X,A,
     -                     '' cm as x,'')') AUX1(1:NC1),UNIT(1:NCUNIT),
     -                     AUX2(1:NC2)
                 ELSEIF(IBXDIR.EQ.2)THEN
                      WRITE(LUNOUT,'(''  Magnetic field components:'',
     -                     '' Bx = '',A,'' '',A,'' vs''/34X,A,
     -                     '' cm as x,'')') AUX1(1:NC1),UNIT(1:NCUNIT),
     -                     AUX2(1:NC2)
                 ELSEIF(IBXDIR.EQ.3)THEN
                      WRITE(LUNOUT,'(''  Magnetic field components:'',
     -                     '' Bx = '',A,'' '',A,'' vs''/34X,A,
     -                     '' cm as x,'')') AUX1(1:NC1),UNIT(1:NCUNIT),
     -                     AUX2(1:NC2)
                 ELSE
                      WRITE(LUNOUT,'(''  Magnetic field components:'',
     -                     '' Bx = Invalid interpolation,'',
     -                     '' assumed to 0 T,'')')
                 ENDIF
            ELSE
                 WRITE(LUNOUT,'(''  Magnetic field components: Bx = '',
     -                '' Unknown, assumed to be 0 T,'')')
            ENDIF
*   y-component.
            IF(POLAR)THEN
                 WRITE(LUNOUT,'(29X,''By = 0 T,'')')
            ELSEIF(IBYTYP.EQ.0)THEN
                 WRITE(LUNOUT,'(29X,''By = Undefined, assumed to be'',
     -                '' 0 T,'')')
            ELSEIF(IBYTYP.EQ.1)THEN
                 CALL OUTFMT(B0Y*BSCALE/100,2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(29X,''By = '',A,'' T,'')') AUX1(1:NC1)
            ELSEIF(IBYTYP.EQ.2)THEN
                 WRITE(LUNOUT,'(29X,''By = '',A,'' '',A,'','')')
     -                FUNB0Y(1:NCB0Y),UNIT(1:NCUNIT)
            ELSEIF(IBYTYP.EQ.3)THEN
                 CALL OUTFMT(REAL(IRB0Y),5,AUX1,NC1,'LEFT')
                 CALL OUTFMT(REAL(IRV0Y),5,AUX2,NC2,'LEFT')
                 IF(IBYDIR.EQ.1)THEN
                      WRITE(LUNOUT,'(29X,''By = '',A,'' '',A,'' vs''/
     -                     34X,A,'' cm as x,'')')
     -                     AUX1(1:NC1),UNIT(1:NCUNIT),AUX2(1:NC2)
                 ELSEIF(IBYDIR.EQ.2)THEN
                      WRITE(LUNOUT,'(29X,''By = '',A,'' '',A,'' vs''/
     -                     34X,A,'' cm as y,'')')
     -                     AUX1(1:NC1),UNIT(1:NCUNIT),AUX2(1:NC2)
                 ELSEIF(IBYDIR.EQ.3)THEN
                      WRITE(LUNOUT,'(29X,''By = '',A,'' '',A,'' vs''/
     -                     34X,A,'' cm as z,'')')
     -                     AUX1(1:NC1),UNIT(1:NCUNIT),AUX2(1:NC2)
                 ELSE
                      WRITE(LUNOUT,'(29X,''By = Invalid'',
     -                     '' interpolation, assumed to 0 T,'')')
                 ENDIF
            ELSE
                 WRITE(LUNOUT,'(29X,''By = Unknown, assumed to be'',
     -                '' 0 T,'')')
            ENDIF
*   z-component.
            IF(IBZTYP.EQ.0)THEN
                 WRITE(LUNOUT,'(29X,''Bz = Undefined, assumed to be'',
     -                '' 0 T.'')')
            ELSEIF(IBZTYP.EQ.1)THEN
                 CALL OUTFMT(B0Z*BSCALE/100,2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(29X,''Bz = '',A,'' T.'')') AUX1(1:NC1)
            ELSEIF(IBZTYP.EQ.2)THEN
                 WRITE(LUNOUT,'(29X,''Bz = '',A,'' '',A,''.'')')
     -                FUNB0Z(1:NCB0Z),UNIT(1:NCUNIT)
            ELSEIF(IBZTYP.EQ.3)THEN
                 CALL OUTFMT(REAL(IRB0Z),5,AUX1,NC1,'LEFT')
                 CALL OUTFMT(REAL(IRV0Z),5,AUX2,NC2,'LEFT')
                 IF(IBZDIR.EQ.1)THEN
                      WRITE(LUNOUT,'(29X,''Bz = '',A,'' '',A,'' vs''/
     -                     34X,A,'' cm as x.'')')
     -                     AUX1(1:NC1),UNIT(1:NCUNIT),AUX2(1:NC2)
                 ELSEIF(IBZDIR.EQ.2)THEN
                      WRITE(LUNOUT,'(29X,''Bz = '',A,'' '',A,'' vs''/
     -                     34X,A,'' cm as y.'')')
     -                     AUX1(1:NC1),UNIT(1:NCUNIT),AUX2(1:NC2)
                 ELSEIF(IBZDIR.EQ.3)THEN
                      WRITE(LUNOUT,'(29X,''Bz = '',A,'' '',A,'' vs''/
     -                     34X,A,'' cm as z.'')')
     -                     AUX1(1:NC1),UNIT(1:NCUNIT),AUX2(1:NC2)
                 ELSE
                      WRITE(LUNOUT,'(29X,''Bz = Invalid'',
     -                     '' interpolation, assumed to 0 T.'')')
                 ENDIF
            ELSE
                 WRITE(LUNOUT,'(29X,''Bz = Unknown, assumed to be'',
     -                '' 0 T.'')')
            ENDIF
*   Range.
            CALL OUTFMT(BFMIN*BSCALE/100,2,AUX1,NC1,'LEFT')
            CALL OUTFMT(BFMAX*BSCALE/100,2,AUX2,NC2,'LEFT')
            WRITE(LUNOUT,'(/''  Default magnetic field range is '',A,
     -           '' < B < '',A,'' T.'')') AUX1(1:NC1),AUX2(1:NC2)
*   Nothing more to be done.
            RETURN
       ENDIF
*** Set the list of variables.
       IF(POLAR)THEN
            VARLIS(1)='R'
            VARLIS(2)='PHI'
            VARLIS(3)='Z'
       ELSE
            VARLIS(1)='X'
            VARLIS(2)='Y'
            VARLIS(3)='Z'
       ENDIF
       NVAR=3
*** Reset the OK flag.
       OK=.TRUE.
*** Reset the scale to Tesla.
       BSCALE=100.0
*** Get each of the 3 components in turn.
       INEXT=2
       DO 100 K=1,3
*   Preset the variables.
       IF(K.EQ.1)THEN
            B0=B0X
            FUN=FUNB0X
            NCFUN=NCB0X
            IENTRY=IENB0X
            IREFB=IRB0X
            IREFV=IRV0X
            IBTYPE=IBXTYP
            IDIR=IBXDIR
            BMIN=BFXMIN
            BMAX=BFXMAX
       ELSEIF(K.EQ.2)THEN
            B0=B0Y
            FUN=FUNB0Y
            NCFUN=NCB0Y
            IENTRY=IENB0Y
            IREFB=IRB0Y
            IREFV=IRV0Y
            IBTYPE=IBYTYP
            IDIR=IBYDIR
            BMIN=BFYMIN
            BMAX=BFYMAX
       ELSEIF(K.EQ.3)THEN
            B0=B0Z
            FUN=FUNB0Z
            NCFUN=NCB0Z
            IENTRY=IENB0Z
            IREFB=IRB0Z
            IREFV=IRV0Z
            IBTYPE=IBZTYP
            IDIR=IBZDIR
            BMIN=BFZMIN
            BMAX=BFZMAX
       ENDIF
*** Get the component, try the format "Matrix VS {X|Y|Z} Matrix".
       IF(INPCMP(INEXT+1,'VS').NE.0.AND.INEXT+3.LE.NWORD)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAGCMP DEBUG   :'',
     -           '' Component '',I1,'': Matrix interpolation.'')') K
**  Extract the name of the B vector.
            CALL INPSTR(INEXT,INEXT,AUX1,NC1)
            IF(NC1.LT.1)THEN
                 AUX1='?'
                 NC1=1
            ENDIF
*   Scan the list of globals for the B vector.
            ISDATA=0
            ISCOPY=0
            IGLB=0
            DO 10 I=1,NGLB
            IF(GLBVAR(I).EQ.AUX1(1:NC1))THEN
*   Ensure this is a Matrix.
                 IF(GLBMOD(I).NE.5)THEN
                      CALL INPMSG(INEXT,AUX1(1:NC1)//
     -                     ' is not a Matrix.')
                      OK=.FALSE.
                      GOTO 10
                 ENDIF
*   Locate it.
                 ISLOT=MATSLT(NINT(GLBVAL(I)))
                 IF(ISLOT.LE.0)THEN
                      PRINT *,' !!!!!! MAGCMP WARNING : '//AUX1(1:NC1)//
     -                     ' can not be located in the Matrix buffer.'
                      OK=.FALSE.
*   Ensure it is 1-dimensional.
                 ELSEIF(MDIM(ISLOT).NE.1)THEN
                      CALL INPMSG(INEXT,AUX1(1:NC1)//
     -                     ' is not 1-dimensional.')
                      OK=.FALSE.
*   And that it has a length of at least 1.
                 ELSEIF(MLEN(ISLOT).LT.2)THEN
                      CALL INPMSG(INEXT,'Length of '//AUX1(1:NC1)//
     -                     ' < 2.')
                      OK=.FALSE.
*   Duplicate B, delete an old copy if there is one.
                 ELSE
                      IF(IREFB.NE.0)CALL MATADM('DELETE',IREFB,NDIM,
     -                     IDIM,IMOD,IFAIL)
                      NDIM=MDIM(ISLOT)
                      IMOD=MMOD(ISLOT)
                      IDIM(1)=MSIZ(ISLOT,1)
                      CALL MATADM('ALLOCATE',IREFB,NDIM,IDIM,IMOD,IFAIL)
                      IF(IFAIL.NE.0)THEN
                           PRINT *,' !!!!!! MAGCMP WARNING : Failed'//
     -                          ' to obtain space for a copy of B.'
                           OK=.FALSE.
                      ENDIF
                      ISCOPY=MATSLT(IREFB)
                      IF(ISCOPY.LE.0)THEN
                           PRINT *,' !!!!!! MAGCMP WARNING : Failed'//
     -                          ' to locate the copy of B.'
                           OK=.FALSE.
                      ENDIF
                      DO 20 J=1,MLEN(ISLOT)
                      MVEC(MORG(ISCOPY)+J)=MVEC(MORG(ISLOT)+J)
                      IF(J.EQ.1)THEN
                           BMIN=MVEC(MORG(ISLOT)+J)
                           BMAX=MVEC(MORG(ISLOT)+J)
                      ELSE
                           BMIN=MIN(BMIN,MVEC(MORG(ISLOT)+J))
                           BMAX=MAX(BMAX,MVEC(MORG(ISLOT)+J))
                      ENDIF
20                    CONTINUE
                 ENDIF
*   Also look for the name of the copy in the global list.
            ELSEIF((GLBVAR(I).EQ.'Bx field'.AND.K.EQ.1).OR.
     -           (GLBVAR(I).EQ.'By field'.AND.K.EQ.2).OR.
     -           (GLBVAR(I).EQ.'Bz field'.AND.K.EQ.3))THEN
                 IGLB=I
            ENDIF
10          CONTINUE
*   Ensure we did find the vector.
            IF(ISCOPY.LE.0)THEN
                 CALL INPMSG(INEXT,AUX1(1:NC1)//' is not a Global.')
                 OK=.FALSE.
                 INEXT=INEXT+4
                 GOTO 100
            ELSE
                 ISDATA=ISCOPY
            ENDIF
*   Add to the globals list, if not yet done.
            IF(IGLB.EQ.0.AND.NGLB.LT.MXVAR)THEN
                 NGLB=NGLB+1
                 IF(K.EQ.1)THEN
                      GLBVAR(NGLB)='Bx field'
                 ELSEIF(K.EQ.2)THEN
                      GLBVAR(NGLB)='By field'
                 ELSEIF(K.EQ.3)THEN
                      GLBVAR(NGLB)='Bz field'
                 ENDIF
                 IGLB=NGLB
            ELSEIF(IGLB.EQ.0.AND.NGLB.GE.MXVAR)THEN
                 PRINT *,' !!!!!! MAGCMP WARNING : Unable to obtain'//
     -                ' naming space for an interpolation vector;'//
     -                ' do not reference the vector.'
            ENDIF
            IF(IGLB.NE.0)THEN
                 GLBVAL(IGLB)=IREFB
                 GLBMOD(IGLB)=5
            ENDIF
*   Debugging.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAGCMP DEBUG   :'',
     -           '' Field: name='',A,'', ref='',I3,'', slot='',I3)')
     -           AUX1(1:NC1),IREFB,ISCOPY
**  Find out which direction the coordinate vector represents.
            IF(INPCMP(INEXT+2,'X')+INPCMP(INEXT+2,'R').NE.0)THEN
                 IDIR=1
            ELSEIF(INPCMP(INEXT+2,'Y')+INPCMP(INEXT+2,'PHI').NE.0)THEN
                 IDIR=2
            ELSEIF(INPCMP(INEXT+2,'Z').NE.0)THEN
                 IDIR=3
            ELSE
                 CALL INPMSG(INEXT+2,'Not a valid direction.')
                 IDIR=0
                 OK=.FALSE.
                 INEXT=INEXT+4
                 GOTO 100
            ENDIF
*   Debugging.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAGCMP DEBUG   :'',
     -           '' Direction: '',I1)') IDIR
**  Extract the name of the coordinate vector.
            CALL INPSTR(INEXT+3,INEXT+3,AUX1,NC1)
            IF(NC1.LT.1)THEN
                 AUX1='?'
                 NC1=1
            ENDIF
*   Scan the list of globals for the B vector.
            ISCOPY=0
            IGLB=0
            DO 30 I=1,NGLB
            IF(GLBVAR(I).EQ.AUX1(1:NC1))THEN
*   Ensure this is a Matrix.
                 IF(GLBMOD(I).NE.5)THEN
                      CALL INPMSG(INEXT+3,AUX1(1:NC1)//
     -                     ' is not a Matrix.')
                      OK=.FALSE.
                      GOTO 30
                 ENDIF
*   Locate it.
                 ISLOT=MATSLT(NINT(GLBVAL(I)))
                 IF(ISLOT.LE.0)THEN
                      PRINT *,' !!!!!! MAGCMP WARNING : '//AUX1(1:NC1)//
     -                     ' can not be located in the Matrix buffer.'
                      OK=.FALSE.
*   Ensure it is 1-dimensional.
                 ELSEIF(MDIM(ISLOT).NE.1)THEN
                      CALL INPMSG(INEXT+3,AUX1(1:NC1)//
     -                     ' is not 1-dimensional.')
                      OK=.FALSE.
*   Ensure the length is the same as the B vector.
                 ELSEIF(MLEN(ISLOT).NE.MLEN(ISDATA))THEN
                      CALL INPMSG(INEXT+3,
     -                     'Lengths of B and coord differ.')
                      OK=.FALSE.
*   Duplicate coordinate, delete an old copy if there is one.
                 ELSE
                      IF(IREFV.NE.0)CALL MATADM('DELETE',IREFV,NDIM,
     -                     IDIM,IMOD,IFAIL)
                      NDIM=MDIM(ISLOT)
                      IMOD=MMOD(ISLOT)
                      IDIM(1)=MSIZ(ISLOT,1)
                      CALL MATADM('ALLOCATE',IREFV,NDIM,IDIM,IMOD,IFAIL)
                      IF(IFAIL.NE.0)THEN
                           PRINT *,' !!!!!! MAGCMP WARNING : Failed'//
     -                          ' to obtain space for a copy of the'//
     -                          ' coordinate vector.'
                           OK=.FALSE.
                      ENDIF
                      ISCOPY=MATSLT(IREFV)
                      IF(ISCOPY.LE.0)THEN
                           PRINT *,' !!!!!! MAGCMP WARNING : Failed'//
     -                          ' locate the copy of a coordinate'//
     -                          ' vector.'
                           OK=.FALSE.
                      ENDIF
                      DO 40 J=1,MLEN(ISLOT)
                      MVEC(MORG(ISCOPY)+J)=MVEC(MORG(ISLOT)+J)
40                    CONTINUE
                 ENDIF
*   Also look for the name of the copy in the global list.
            ELSEIF((GLBVAR(I).EQ.'Bx coord'.AND.K.EQ.1).OR.
     -           (GLBVAR(I).EQ.'By coord'.AND.K.EQ.2).OR.
     -           (GLBVAR(I).EQ.'Bz coord'.AND.K.EQ.3))THEN
                 IGLB=I
            ENDIF
30          CONTINUE
*   Be sure we found the vector.
            IF(ISCOPY.LE.0)THEN
                 CALL INPMSG(INEXT+3,AUX1(1:NC1)//' is not a Global.')
                 OK=.FALSE.
                 INEXT=INEXT+4
                 GOTO 100
            ENDIF
*   Add to the globals list, if not yet done.
            IF(IGLB.EQ.0.AND.NGLB.LT.MXVAR)THEN
                 NGLB=NGLB+1
                 IF(K.EQ.1)THEN
                      GLBVAR(NGLB)='Bx coord'
                 ELSEIF(K.EQ.2)THEN
                      GLBVAR(NGLB)='By coord'
                 ELSEIF(K.EQ.3)THEN
                      GLBVAR(NGLB)='Bz coord'
                 ENDIF
                 IGLB=NGLB
            ELSEIF(IGLB.EQ.0.AND.NGLB.GE.MXVAR)THEN
                 PRINT *,' !!!!!! MAGCMP WARNING : Unable to obtain'//
     -                ' naming space for an interpolation vector;'//
     -                ' do not reference the vector.'
            ENDIF
            IF(IGLB.NE.0)THEN
                 GLBVAL(IGLB)=IREFV
                 GLBMOD(IGLB)=5
            ENDIF
*   Debugging.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAGCMP DEBUG   :'',
     -           '' Coordinates: name='',A,'', ref='',I3,'', slot='',
     -            I3)') AUX1(1:NC1),IREFV,ISCOPY
*   Remember the source of B.
            IBTYPE=3
**  Update the pointer.
            INEXT=INEXT+4
*** Try the fixed value format.
       ELSEIF(INPTYP(INEXT).EQ.1.OR.INPTYP(INEXT).EQ.2)THEN
*   Read the value.
            CALL INPCHK(INEXT,2,IFAIL)
            IF(IFAIL.EQ.0)THEN
                 CALL INPRDR(INEXT,B0,0.0)
                 IBTYPE=1
            ENDIF
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAGCMP DEBUG   :'',
     -           '' Component '',I1,'': fixed value '',E10.3,
     -           '', IFAIL='',I1)') K,B0,IFAIL
*   Update the limits.
            BMIN=B0
            BMAX=B0
*   Update the pointer.
            INEXT=INEXT+1
*** Try the formula format.
       ELSEIF(INEXT.LE.NWORD)THEN
*   Retrieve the formula.
            CALL INPSTR(INEXT,INEXT,FUN,NCFUN)
            IF(NCFUN.LT.1)THEN
                 FUN='?'
                 NCFUN=1
            ENDIF
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAGCMP DEBUG   :'',
     -           '' Component '',I1,'': formula '',A)') K,FUN(1:NCFUN)
*   Clear old entry point, if there is one.
            IF(IENTRY.NE.0)CALL ALGCLR(IENTRY)
*   Translate it.
            IF(INDEX(FUN(1:NCFUN),'@').NE.0)THEN
                 NRES=1
                 PRINT *,' ------ MAGCMP MESSAGE : Please edit the'//
     -                ' function.'
                 CALL ALGEDT(VARLIS,NVAR,IENTRY,USE,NRES)
                 IFAIL=0
*   Usual function translation if not.
            ELSE
                 CALL ALGPRE(FUN(1:NCFUN),NCFUN,VARLIS,NVAR,NRES,USE,
     -                IENTRY,IFAIL)
            ENDIF
*   Check return code of translation.
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! MAGCMP WARNING : Error translating'//
     -                ' a B function.'
                 OK=.FALSE.
                 CALL ALGCLR(IENTRY)
*   Check number of results returned by the function.
            ELSEIF(NRES.NE.1)THEN
                 PRINT *,' !!!!!! MAGCMP WARNING : A B function'//
     -                ' does not return 1 result.'
                 OK=.FALSE.
                 CALL ALGCLR(IENTRY)
            ELSE
*   Remember the source of B.
                 IBTYPE=2
            ENDIF
*   Set the limits.
            BMIN=0
            BMAX=0
*   Update the pointer.
            INEXT=INEXT+1
*** Unknown format.
       ELSE
*   Issue message.
            CALL INPMSG(INEXT,'Not a recognised format')
*   Set type to non-set.
            IBTYPE=0
*   Set the limits.
            BMIN=0
            BMAX=0
*   Set OK flag.
            OK=.FALSE.
       ENDIF
*** Transfer the results.
       IF(K.EQ.1)THEN
            B0X=B0
            FUNB0X=FUN
            NCB0X=NCFUN
            IENB0X=IENTRY
            IRB0X=IREFB
            IRV0X=IREFV
            IBXTYP=IBTYPE
            IBXDIR=IDIR
            BFXMIN=BMIN
            BFXMAX=BMAX
       ELSEIF(K.EQ.2)THEN
            B0Y=B0
            FUNB0Y=FUN
            NCB0Y=NCFUN
            IENB0Y=IENTRY
            IRB0Y=IREFB
            IRV0Y=IREFV
            IBYTYP=IBTYPE
            IBYDIR=IDIR
            BFYMIN=BMIN
            BFYMAX=BMAX
       ELSEIF(K.EQ.3)THEN
            B0Z=B0
            FUNB0Z=FUN
            NCB0Z=NCFUN
            IENB0Z=IENTRY
            IRB0Z=IREFB
            IRV0Z=IREFV
            IBZTYP=IBTYPE
            IBZDIR=IDIR
            BFZMIN=BMIN
            BFZMAX=BMAX
       ENDIF
*** Next component.
100    CONTINUE
*** Now look for other elements, such as units.
       DO 50 I=INEXT,NWORD
       IF(INPCMP(I,'T#ESLA').NE.0)THEN
            BSCALE=100.0
       ELSEIF(INPCMP(I,'G#AUSS')+INPCMP(I,'OE#RSTED').NE.0)THEN
            BSCALE=0.01
       ELSEIF(INPCMP(I,'V.MICROSEC/CM2').NE.0)THEN
            BSCALE=1.0
       ELSE
            CALL INPMSG(I,'Not a known keyword')
            OK=.FALSE.
       ENDIF
50     CONTINUE
*** Dump the error messages.
       CALL INPERR
*** Ensure no extra fields have been entered in polar coordinates.
       IF(POLAR.AND.(IBXTYP.NE.1.OR.B0X.NE.0.OR.
     -      IBXTYP.NE.1.OR.B0X.NE.0))THEN
            PRINT *,' !!!!!! MAGCMP WARNING : In polar coordinates,'//
     -           ' only Bz may be non-zero; Br and Bphi ignored.'
            OK=.FALSE.
       ENDIF
*** See whether we have all components.
       IF(IBXTYP.EQ.0.OR.IBYTYP.EQ.0.OR.IBZTYP.EQ.0)THEN
            PRINT *,' !!!!!! MAGCMP WARNING : Not all magnetic'//
     -           ' field components have been entered.'
            OK=.FALSE.
       ENDIF
*** See whether we continue.
       IF(JFAIL.EQ.2.AND..NOT.OK)THEN
            PRINT *,' ###### MAGCMP ERROR   : No magnetic field'//
     -           ' because of the above errors.'
            IBXTYP=0
            IBYTYP=0
            IBZTYP=0
            MAGSRC=0
            MAGOK=.FALSE.
            RETURN
       ELSEIF(JFAIL.EQ.3.AND..NOT.OK)THEN
            PRINT *,' ###### MAGCMP ERROR   : Program terminated'//
     -           ' because of the above errors.'
            CALL QUIT
            RETURN
       ENDIF
*** Determine the range of the magnetic field.
       IF(IBXTYP.EQ.2.OR.IBYTYP.EQ.2.OR.IBZTYP.EQ.2)THEN
            BFMIN=0
            BFMAX=5
       ELSE
            BFMIN=SQRT(BFXMIN**2+BFYMIN**2+BFZMIN**2)
            BFMAX=SQRT(BFXMAX**2+BFYMAX**2+BFZMAX**2)
       ENDIF
*** This B field is not defined by a field map.
       MAGSRC=1
*** Set the magnetic field flag.
       IF((IBXTYP.EQ.1.AND.B0X.NE.0).OR.IBXTYP.GE.2.OR.
     -      (IBYTYP.EQ.1.AND.B0Y.NE.0).OR.IBYTYP.GE.2.OR.
     -      (IBZTYP.EQ.1.AND.B0Z.NE.0).OR.IBZTYP.GE.2)THEN
            MAGOK=.TRUE.
       ELSE
            MAGOK=.FALSE.
       ENDIF
       END
