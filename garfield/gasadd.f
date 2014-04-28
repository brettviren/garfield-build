CDECK  ID>, GASADD.
       SUBROUTINE GASADD
*-----------------------------------------------------------------------
*   GASADD - Adds pieces to the gas table.
*   (Last changed on 12/12/12.)
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
       DOUBLE PRECISION CLSDIS,CLSAVE
       REAL EGAS,VGAS,XGAS,YGAS,DGAS,AGAS,BGAS,HGAS,MGAS,WGAS,OGAS,SGAS,
     -      EXGAS,IOGAS,
     -      CVGAS,CXGAS,CYGAS,CDGAS,CAGAS,CBGAS,CHGAS,CMGAS,CWGAS,COGAS,
     -      CSGAS,CEXGAS,CIOGAS,
     -      VGAS2,XGAS2,YGAS2,DGAS2,AGAS2,BGAS2,HGAS2,MGAS2,WGAS2,OGAS2,
     -      SGAS2,EXGAS2,IOGAS2,
     -      AORIG,AORIG2,PENPRB,PENRMS,PENDT,ENIOG,ENEXG,
     -      BANG,BTAB,
     -      VEXTR1,VEXTR2,VEXTR3,VEXTR4,
     -      XEXTR1,XEXTR2,XEXTR3,XEXTR4,
     -      YEXTR1,YEXTR2,YEXTR3,YEXTR4,
     -      DEXTR1,DEXTR2,DEXTR3,DEXTR4,
     -      AEXTR1,AEXTR2,AEXTR3,AEXTR4,
     -      BEXTR1,BEXTR2,BEXTR3,BEXTR4,
     -      HEXTR1,HEXTR2,HEXTR3,HEXTR4,
     -      MEXTR1,MEXTR2,MEXTR3,MEXTR4,
     -      WEXTR1,WEXTR2,WEXTR3,WEXTR4,
     -      OEXTR1,OEXTR2,OEXTR3,OEXTR4,
     -      SEXTR1,SEXTR2,SEXTR3,SEXTR4,
     -      EEXTR1,EEXTR2,EEXTR3,EEXTR4,
     -      ZEXTR1,ZEXTR2,ZEXTR3,ZEXTR4,
     -      GASRNG,
     -      Z,A,RHO,CMEAN,EMPROB,EPAIR,PGAS,TGAS,GASDEN,
     -      DTION,DLION,GASFRM,ELOSCS
       LOGICAL GASOK,TAB2D,GASOPT,HEEDOK,SRIMOK,TRIMOK,GASSET
       INTEGER NGAS,NCLS,NBANG,NBTAB,NFTAB,NFCLS,
     -      IVMETH,IXMETH,IYMETH,IDMETH,IAMETH,IBMETH,IHMETH,IMMETH,
     -      IWMETH,IOMETH,ISMETH,IEMETH,IZMETH,
     -      IVEXTR,IXEXTR,IYEXTR,IDEXTR,IAEXTR,IBEXTR,IHEXTR,IMEXTR,
     -      IWEXTR,IOEXTR,ISEXTR,IEEXTR,IZEXTR,
     -      JVEXTR,JXEXTR,JYEXTR,JDEXTR,JAEXTR,JBEXTR,JHEXTR,JMEXTR,
     -      JWEXTR,JOEXTR,JSEXTR,JEEXTR,JZEXTR,
     -      IATHR,IBTHR,IHTHR,
     -      NEXGAS,NIOGAS,NCSGAS,ICSTYP
       CHARACTER*80 GASID
       CHARACTER*(MXCHAR) FCNTAB,FCNCLS
       CHARACTER*10 CLSTYP
       CHARACTER*45 DSCEXG(MXEXG),DSCIOG(MXIOG),DSCCSG(MXCSG)
       COMMON /GASDAT/ CLSDIS(MXPAIR),CLSAVE,
     -      EGAS(MXLIST),
     -      VGAS(MXLIST),XGAS(MXLIST),YGAS(MXLIST),WGAS(MXLIST),
     -      DGAS(MXLIST),OGAS(MXLIST),AGAS(MXLIST),BGAS(MXLIST),
     -      HGAS(MXLIST),MGAS(MXLIST),SGAS(MXLIST,6),
     -      EXGAS(MXLIST,MXEXG),IOGAS(MXLIST,MXIOG),
     -      CVGAS(MXLIST),CXGAS(MXLIST),CYGAS(MXLIST),CWGAS(MXLIST),
     -      CDGAS(MXLIST),COGAS(MXLIST),CAGAS(MXLIST),CBGAS(MXLIST),
     -      CHGAS(MXLIST),CMGAS(MXLIST),CSGAS(MXLIST,6),
     -      CEXGAS(MXLIST,MXEXG),CIOGAS(MXLIST,MXIOG),
     -      VGAS2(MXLIST,MXBANG,MXBTAB),WGAS2(MXLIST,MXBANG,MXBTAB),
     -      XGAS2(MXLIST,MXBANG,MXBTAB),YGAS2(MXLIST,MXBANG,MXBTAB),
     -      AGAS2(MXLIST,MXBANG,MXBTAB),BGAS2(MXLIST,MXBANG,MXBTAB),
     -      DGAS2(MXLIST,MXBANG,MXBTAB),OGAS2(MXLIST,MXBANG,MXBTAB),
     -      HGAS2(MXLIST,MXBANG,MXBTAB),MGAS2(MXLIST,MXBANG,MXBTAB),
     -      SGAS2(MXLIST,MXBANG,MXBTAB,6),
     -      EXGAS2(MXLIST,MXBANG,MXBTAB,MXEXG),
     -      IOGAS2(MXLIST,MXBANG,MXBTAB,MXIOG),
     -      AORIG(MXLIST),AORIG2(MXLIST,MXBANG,MXBTAB),
     -      PENPRB(MXEXG),PENRMS(MXEXG),PENDT(MXEXG),
     -      ENIOG(MXIOG),ENEXG(MXEXG),
     -      BANG(MXBANG),BTAB(MXBTAB),
     -      GASRNG(20,2),GASFRM(MXNBMC),ELOSCS(MXCSG),
     -      Z,A,RHO,CMEAN,EMPROB,EPAIR,PGAS,TGAS,GASDEN,
     -      DTION,DLION,
     -      VEXTR1,VEXTR2,VEXTR3,VEXTR4,
     -      XEXTR1,XEXTR2,XEXTR3,XEXTR4,
     -      YEXTR1,YEXTR2,YEXTR3,YEXTR4,
     -      DEXTR1,DEXTR2,DEXTR3,DEXTR4,
     -      AEXTR1,AEXTR2,AEXTR3,AEXTR4,
     -      BEXTR1,BEXTR2,BEXTR3,BEXTR4,
     -      HEXTR1,HEXTR2,HEXTR3,HEXTR4,
     -      MEXTR1,MEXTR2,MEXTR3,MEXTR4,
     -      WEXTR1,WEXTR2,WEXTR3,WEXTR4,
     -      OEXTR1,OEXTR2,OEXTR3,OEXTR4,
     -      SEXTR1(6),SEXTR2(6),SEXTR3(6),SEXTR4(6),
     -      EEXTR1(MXEXG),EEXTR2(MXEXG),EEXTR3(MXEXG),EEXTR4(MXEXG),
     -      ZEXTR1(MXIOG),ZEXTR2(MXIOG),ZEXTR3(MXIOG),ZEXTR4(MXIOG),
     -      IVMETH,IXMETH,IYMETH,IDMETH,IAMETH,IBMETH,IHMETH,IMMETH,
     -      IWMETH,IOMETH,ISMETH,IEMETH,IZMETH,
     -      IVEXTR,IXEXTR,IYEXTR,IDEXTR,IAEXTR,IBEXTR,IHEXTR,IMEXTR,
     -      IWEXTR,IOEXTR,ISEXTR,IEEXTR,IZEXTR,
     -      JVEXTR,JXEXTR,JYEXTR,JDEXTR,JAEXTR,JBEXTR,JHEXTR,JMEXTR,
     -      JWEXTR,JOEXTR,JSEXTR,JEEXTR,JZEXTR,
     -      NGAS,NCLS,NBANG,NBTAB,NFTAB,NFCLS,
     -      IATHR,IBTHR,IHTHR,
     -      NEXGAS,NIOGAS,NCSGAS,ICSTYP(MXCSG),
     -      GASOK(20),GASOPT(20,4),
     -      TAB2D,HEEDOK,SRIMOK,TRIMOK,GASSET
       COMMON /GASCHR/ FCNTAB,FCNCLS,CLSTYP,GASID,DSCEXG,DSCIOG,DSCCSG
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       CHARACTER*(MXCHAR) STRFUN
       CHARACTER*10 VARLIS(MXVAR),NAME
       REAL VAR(MXVAR),RES(1)
       INTEGER NWORD,I,J,K,L,INEXT,INPCMP,IOBJ,NVAR,NCFUN,NCNAME,IENTRY,
     -      IRMAT1,ISMAT1,IRMAT2,ISMAT2,MATSLT,NRES,IFAIL1,NERR,IORD,
     -      IORDR,MODVAR(MXVAR),MODRES(1)
       LOGICAL USE(MXVAR),OK
       EXTERNAL INPCMP,MATSLT
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE GASADD ///'
*** Make sure that the electric field table is present.
       IF(NGAS.LT.2)THEN
            PRINT *,' !!!!!! GASADD WARNING : The electric field'//
     -           ' vector has not been set yet; nothing added.'
            RETURN
       ENDIF
*** Set the list of variables.
       VARLIS(1)='EP'
       VARLIS(2)='ANGLE_EB'
       VARLIS(3)='VELOCITY'
       VARLIS(4)='MOBILITY'
       VARLIS(5)='SIGMA_L'
       VARLIS(6)='SIGMA_T'
       VARLIS(7)='TOWNSEND'
       VARLIS(8)='ATTACHMENT'
       VARLIS(9)='LORENTZ'
       VARLIS(10)='B'
       VARLIS(11)='BOLTZMANN'
       VARLIS(12)='ECHARGE'
       VARLIS(13)='P'
       VARLIS(14)='T'
       VARLIS(15)='DISS'
       NVAR=15
*** Count words.
       CALL INPNUM(NWORD)
*** Loop over the components.
       INEXT=2
       DO 10 I=2,NWORD
       IF(I.LT.INEXT)GOTO 10
*** Find out which element is to be changed.
       IOBJ=0
       IF(INPCMP(I,'DR#IFT-#VELOCITY').NE.0)THEN
            IOBJ=1
       ELSEIF(INPCMP(I,'ION-MOB#ILITY')+
     -      INPCMP(I,'MOB#ILITY').NE.0)THEN
            IOBJ=2
       ELSEIF(INPCMP(I,'LONG#ITUDINAL-DIFF#USION')+
     -      INPCMP(I,'DIFF#USION').NE.0)THEN
            IOBJ=3
       ELSEIF(INPCMP(I,'TRANS#VERSE-DIFF#USION').NE.0)THEN
            IOBJ=8
       ELSEIF(INPCMP(I,'TOWN#SEND-#COEFFICIENTS').NE.0)THEN
            IOBJ=4
       ELSEIF(INPCMP(I,'ATT#ACHMENT-#COEFFICIENTS').NE.0)THEN
            IOBJ=6
       ELSEIF(INPCMP(I,'LOR#ENTZ-#ANGLES').NE.0)THEN
            IF(MAGOK)THEN
                 IOBJ=7
            ELSE
                 CALL INPMSG(I,'There is no B field.')
                 GOTO 10
            ENDIF
       ELSEIF(INPCMP(I,'ION-DISS#OCIATION-#COEFFICIENTS')+
     -      INPCMP(I,'DISS#OCIATION-#COEFFICIENTS').NE.0)THEN
            IOBJ=12
       ELSE
            CALL INPMSG(I,'Not a known object.')
            GOTO 10
       ENDIF
*** Pick up the function string or the pair of matrices.
       IF(I+1.GT.NWORD)THEN
            CALL INPMSG(I,'Should have an argument.')
            GOTO 10
**  Could be a set of matrices.
       ELSEIF(INPCMP(I+2,'VS').NE.0.AND.I+3.LE.NWORD)THEN
*   Continue 4 words from here.
            INEXT=I+4
*   Locate both matrices.
            IRMAT1=0
            IRMAT2=0
            CALL INPSTR(I+1,I+1,NAME,NCNAME)
            DO 110 J=1,NGLB
            IF(GLBMOD(J).EQ.5.AND.GLBVAR(J).EQ.NAME(1:NCNAME))
     -           IRMAT1=NINT(GLBVAL(J))
110         CONTINUE
            ISMAT1=MATSLT(IRMAT1)
            CALL INPSTR(I+3,I+3,NAME,NCNAME)
            DO 120 J=1,NGLB
            IF(GLBMOD(J).EQ.5.AND.GLBVAR(J).EQ.NAME(1:NCNAME))
     -           IRMAT2=NINT(GLBVAL(J))
120         CONTINUE
            ISMAT2=MATSLT(IRMAT2)
*   Make sure both exist.
            IF(ISMAT1.EQ.0)CALL INPMSG(I+1,'Not a known matrix.')
            IF(ISMAT2.EQ.0)CALL INPMSG(I+3,'Not a known matrix.')
            IF(ISMAT1.EQ.0.OR.ISMAT2.EQ.0)GOTO 10
*   Make sure they are 1-dimensional.
            IF(MDIM(ISMAT1).NE.1)CALL INPMSG(I+1,'Not 1-dimensional.')
            IF(MDIM(ISMAT2).NE.1)CALL INPMSG(I+3,'Not 1-dimensional.')
            IF(MDIM(ISMAT1).NE.1.OR.MDIM(ISMAT2).NE.1)GOTO 10
            IENTRY=0
*   Make sure the table range is covered.
            IF(MVEC(MORG(ISMAT2)+1).GT.EGAS(1).OR.
     -           MVEC(MORG(ISMAT2)+MLEN(ISMAT2)).LT.EGAS(NGAS))THEN
                 IF(IOBJ.EQ.1)THEN
                      PRINT *,' ------ GASADD MESSAGE : New drift'//
     -                     ' velocity data spans only part of current'//
     -                     ' table; keeping old values where needed.'
                 ELSEIF(IOBJ.EQ.2)THEN
                      PRINT *,' ------ GASADD MESSAGE : New ion'//
     -                     ' mobility data spans only part of current'//
     -                     ' table; keeping old values where needed.'
                 ELSEIF(IOBJ.EQ.3)THEN
                      PRINT *,' ------ GASADD MESSAGE : Longitudinal'//
     -                     ' diff. data spans only part of current'//
     -                     ' table; keeping old values where needed.'
                 ELSEIF(IOBJ.EQ.4)THEN
                      PRINT *,' ------ GASADD MESSAGE : New Townsend'//
     -                     ' data spans only part of current'//
     -                     ' table; keeping old values where needed.'
                 ELSEIF(IOBJ.EQ.6)THEN
                      PRINT *,' ------ GASADD MESSAGE : Attachment'//
     -                     ' data spans only part of current'//
     -                     ' table; keeping old values where needed.'
                 ELSEIF(IOBJ.EQ.7)THEN
                      PRINT *,' ------ GASADD MESSAGE : New Lorentz'//
     -                     ' angle data spans only part of current'//
     -                     ' table; keeping old values where needed.'
                 ELSEIF(IOBJ.EQ.8)THEN
                      PRINT *,' ------ GASADD MESSAGE : Transverse'//
     -                     ' diff. data spans only part of current'//
     -                     ' table; keeping old values where needed.'
                 ELSEIF(IOBJ.EQ.12)THEN
                      PRINT *,' ------ GASADD MESSAGE : Dissociation'//
     -                     ' data spans only part of current'//
     -                     ' table; keeping old values where needed.'
                 ELSE
                      PRINT *,' ------ GASADD MESSAGE : Data covers'//
     -                     ' table only partially; keeping old values'//
     -                     ' where needed.'
                 ENDIF
            ENDIF
*   There could still be an order of interpolation.
            IORD=2
            IF(INPCMP(INEXT,'LIN#EAR').NE.0)THEN
                 IORD=1
                 INEXT=INEXT+1
            ELSEIF(INPCMP(INEXT,'QUA#DRATIC').NE.0)THEN
                 IORD=2
                 INEXT=INEXT+1
            ELSEIF(INPCMP(INEXT,'CUB#IC').NE.0)THEN
                 IORD=3
                 INEXT=INEXT+1
            ELSEIF(INPCMP(INEXT,'ORD#ER').NE.0)THEN
                 IF(INEXT+1.LT.NWORD)THEN
                      CALL INPMSG(INEXT,'Should have an argument.')
                 ELSE
                      CALL INPCHK(INEXT+1,IORDR,2)
                      IF(IORDR.GT.0.AND.IORDR.LT.10)THEN
                           IORD=IORDR
                      ELSE
                           CALL INPMSG(INEXT+1,'Out of range [1,10].')
                      ENDIF
                 ENDIF
                 INEXT=INEXT+2
            ENDIF
*   Debugging information.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASADD DEBUG   :'',
     -           '' Component '',I1,'': matrix '',I4,''('',I4,
     -           '') vs '',I4,''('',I4,'').'')') IOBJ,IRMAT1,ISMAT1,
     -           IRMAT2,ISMAT2
**  Could be an incomplete set of matrices.
       ELSEIF(INPCMP(I+2,'VS').NE.0.AND.I+3.GT.NWORD)THEN
            CALL INPMSG(I,'Argument invalid or missing.')
            GOTO 10
**  If a function, translate.
       ELSE
*   Continue after the function.
            INEXT=I+2
*   Get the string.
            CALL INPSTR(I+1,I+1,STRFUN,NCFUN)
*   Call editor of specified as @.
            IF(INDEX(STRFUN(1:NCFUN),'@').NE.0)THEN
                 NRES=1
                 PRINT *,' ------ GASADD MESSAGE : Please edit the'//
     -                ' function.'
                 CALL ALGEDT(VARLIS,NVAR,IENTRY,USE,NRES)
                 IFAIL1=0
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASADD DEBUG   :'',
     -                '' Component '',I1,'': edited function with'',
     -                '' entry '',I5,'', results '',I5)') IENTRY,NRES
*   Usual function translation if not.
            ELSE
                 CALL ALGPRE(STRFUN,NCFUN,VARLIS,NVAR,NRES,USE,
     -                IENTRY,IFAIL1)
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASADD DEBUG   :'',
     -                '' Component '',I1,'': function '',A/26X,
     -                ''entry '',I5,'', results '',I5)')
     -                IOBJ,STRFUN(1:NCFUN),IENTRY,NRES
            ENDIF
*   Check use of angle.
            IF((USE(2).OR.USE(10)).AND..NOT.TAB2D)THEN
                 CALL INPMSG(I+1,'Uses B but there is no B field.')
                 CALL ALGCLR(IENTRY)
                 GOTO 10
*   Check use of mobility.
            ELSEIF(USE(3).AND..NOT.GASOK(1))THEN
                 CALL INPMSG(I+1,'Uses drift velocity data.')
                 CALL ALGCLR(IENTRY)
                 GOTO 10
*   Check use of mobility.
            ELSEIF(USE(4).AND..NOT.GASOK(2))THEN
                 CALL INPMSG(I+1,'Tries to use mobility data.')
                 CALL ALGCLR(IENTRY)
                 GOTO 10
*   Check use of longitudinal diffusion.
            ELSEIF(USE(5).AND..NOT.GASOK(3))THEN
                 CALL INPMSG(I+1,'Uses longitudinal diffusion.')
                 CALL ALGCLR(IENTRY)
                 GOTO 10
*   Check use of transverse diffusion.
            ELSEIF(USE(6).AND..NOT.GASOK(8))THEN
                 CALL INPMSG(I+1,'Uses transversal diffusion.')
                 CALL ALGCLR(IENTRY)
                 GOTO 10
*   Check use of Townsend coefficients.
            ELSEIF(USE(7).AND..NOT.GASOK(4))THEN
                 CALL INPMSG(I+1,'Uses Townsend coefficients.')
                 CALL ALGCLR(IENTRY)
                 GOTO 10
*   Check use of attachment coefficients.
            ELSEIF(USE(8).AND..NOT.GASOK(6))THEN
                 CALL INPMSG(I+1,'Uses attachment coefficients.')
                 CALL ALGCLR(IENTRY)
                 GOTO 10
*   Check use of Lorentz angle.
            ELSEIF(USE(9).AND..NOT.GASOK(7))THEN
                 CALL INPMSG(I+1,'Tries to use (v,E) angles.')
                 CALL ALGCLR(IENTRY)
                 GOTO 10
*   Check use of ion dissociation coefficients.
            ELSEIF(USE(15).AND..NOT.GASOK(12))THEN
                 CALL INPMSG(I+1,'Tries to use dissociation.')
                 CALL ALGCLR(IENTRY)
                 GOTO 10
            ENDIF
*   Check return code of translation.
            IF(IFAIL1.NE.0)THEN
                 CALL INPMSG(I+1,'Not a valid function.')
                 CALL ALGCLR(IENTRY)
                 GOTO 10
            ENDIF
*   Check number of results returned by the function.
            IF(NRES.NE.1)THEN
                 CALL INPMSG(I+1,'Does not give 1 result.')
                 CALL ALGCLR(IENTRY)
                 GOTO 10
            ENDIF
       ENDIF
*** Perform the actual interpolation.
       NERR=0
       OK=.TRUE.
**  First the 2-dimensional tables.
       IF(TAB2D)THEN
*   Loop over the E/p points, skipping points outside the table.
            DO 20 J=1,NGAS
            IF(IENTRY.EQ.0)THEN
                 IF(EGAS(J).LT.MVEC(MORG(ISMAT2)+1).OR.
     -              EGAS(J).GT.MVEC(MORG(ISMAT2)+MLEN(ISMAT2)))GOTO 20
            ENDIF
*   Loop over cos(E-B).
            DO 30 K=1,NBANG
            DO 50 L=1,NBTAB
            VAR(1)=EGAS(J)
            VAR(2)=180*BANG(K)/PI
            VAR(3)=VGAS2(J,K,L)
            VAR(4)=MGAS2(J,K,L)
            VAR(5)=DGAS2(J,K,L)
            VAR(6)=OGAS2(J,K,L)
            VAR(7)=EXP(AGAS2(J,K,L))
            VAR(8)=EXP(BGAS2(J,K,L))
            VAR(9)=180*WGAS2(J,K,L)/PI
            VAR(10)=BTAB(L)/100
            VAR(11)=BOLTZ
            VAR(12)=ECHARG
            VAR(13)=PGAS
            VAR(14)=TGAS
            VAR(15)=EXP(HGAS2(J,K,L))
            MODVAR(1)=2
            MODVAR(2)=2
            MODVAR(3)=2
            MODVAR(4)=2
            MODVAR(5)=2
            MODVAR(6)=2
            MODVAR(7)=2
            MODVAR(8)=2
            MODVAR(9)=2
            MODVAR(10)=2
            MODVAR(11)=2
            MODVAR(12)=2
            MODVAR(13)=2
            MODVAR(14)=2
            MODVAR(15)=2
*   Evaluate the formula ...
            IF(IENTRY.NE.0)THEN
                 CALL ALGEXE(IENTRY,VAR,MODVAR,NVAR,RES,
     -                MODRES,1,IFAIL1)
                 IF(MODRES(1).NE.2.OR.IFAIL1.NE.0)THEN
                      NERR=NERR+1
                      OK=.FALSE.
                      RES(1)=0
                 ENDIF
*   or interpolate the matrices.
            ELSE
                 CALL MATIN1(IRMAT2,IRMAT1,1,VAR(1),RES(1),
     -                ISMAT2,ISMAT1,IORD,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      NERR=NERR+1
                      OK=.FALSE.
                      RES(1)=0
                 ENDIF
            ENDIF
*   Assign the result.
            IF(IOBJ.EQ.1)THEN
                 VGAS2(J,K,L)=RES(1)
            ELSEIF(IOBJ.EQ.2)THEN
                 MGAS2(J,K,L)=RES(1)
            ELSEIF(IOBJ.EQ.3)THEN
                 DGAS2(J,K,L)=RES(1)
            ELSEIF(IOBJ.EQ.4)THEN
                 IF(RES(1).GT.0)THEN
                      AGAS2(J,K,L)=LOG(RES(1))
                 ELSE
                      AGAS2(J,K,L)=-30.0
                 ENDIF
            ELSEIF(IOBJ.EQ.6)THEN
                 IF(RES(1).GT.0)THEN
                      BGAS2(J,K,L)=LOG(RES(1))
                 ELSE
                      BGAS2(J,K,L)=-30.0
                 ENDIF
            ELSEIF(IOBJ.EQ.12)THEN
                 IF(RES(1).GT.0)THEN
                      HGAS2(J,K,L)=LOG(RES(1))
                 ELSE
                      HGAS2(J,K,L)=-30.0
                 ENDIF
            ELSEIF(IOBJ.EQ.7)THEN
                 WGAS2(J,K,L)=PI*RES(1)/180
            ELSEIF(IOBJ.EQ.8)THEN
                 OGAS2(J,K,L)=RES(1)
            ELSE
                 PRINT *,' ###### GASADD ERROR   : Unidentified'//
     -                ' field; program bug - please report.'
                 OK=.FALSE.
            ENDIF
*   Next point.
50          CONTINUE
30          CONTINUE
20          CONTINUE
**  And the 1-dimensional case.
       ELSE
*   Loop over the E/p points, skipping points outside the table.
            DO 40 J=1,NGAS
            VAR(1)=EGAS(J)
            VAR(2)=0
            VAR(3)=VGAS(J)
            VAR(4)=MGAS(J)
            VAR(5)=DGAS(J)
            VAR(6)=OGAS(J)
            VAR(7)=EXP(AGAS(J))
            VAR(8)=EXP(BGAS(J))
            VAR(9)=180*WGAS(J)/PI
            VAR(10)=0
            VAR(11)=BOLTZ
            VAR(12)=ECHARG
            VAR(13)=PGAS
            VAR(14)=TGAS
            VAR(15)=EXP(HGAS(J))
            MODVAR(1)=2
            MODVAR(2)=2
            MODVAR(3)=2
            MODVAR(4)=2
            MODVAR(5)=2
            MODVAR(6)=2
            MODVAR(7)=2
            MODVAR(8)=2
            MODVAR(9)=2
            MODVAR(10)=2
            MODVAR(11)=2
            MODVAR(12)=2
            MODVAR(13)=2
            MODVAR(14)=2
            MODVAR(15)=2
*   Evaluate the formula ...
            IF(IENTRY.NE.0)THEN
                 CALL ALGEXE(IENTRY,VAR,MODVAR,NVAR,RES,
     -                MODRES,1,IFAIL1)
                 IF(MODRES(1).NE.2.OR.IFAIL1.NE.0)THEN
                      NERR=NERR+1
                      OK=.FALSE.
                      RES(1)=0
                 ENDIF
*   or interpolate the matrices.
            ELSE
                 IF(EGAS(J).LT.MVEC(MORG(ISMAT2)+1).OR.
     -                EGAS(J).GT.MVEC(MORG(ISMAT2)+MLEN(ISMAT2)))GOTO 40
                 CALL MATIN1(IRMAT2,IRMAT1,1,VAR(1),RES(1),
     -                ISMAT2,ISMAT1,IORD,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      NERR=NERR+1
                      OK=.FALSE.
                      RES(1)=0
                 ENDIF
            ENDIF
*   Assign the result.
            IF(IOBJ.EQ.1)THEN
                 VGAS(J)=RES(1)
            ELSEIF(IOBJ.EQ.2)THEN
                 MGAS(J)=RES(1)
            ELSEIF(IOBJ.EQ.3)THEN
                 DGAS(J)=RES(1)
            ELSEIF(IOBJ.EQ.4)THEN
                 IF(RES(1).GT.0)THEN
                      AGAS(J)=LOG(RES(1))
                 ELSE
                      AGAS(J)=-30.0
                 ENDIF
            ELSEIF(IOBJ.EQ.6)THEN
                 IF(RES(1).GT.0)THEN
                      BGAS(J)=LOG(RES(1))
                 ELSE
                      BGAS(J)=-30.0
                 ENDIF
            ELSEIF(IOBJ.EQ.12)THEN
                 IF(RES(1).GT.0)THEN
                      HGAS(J)=LOG(RES(1))
                 ELSE
                      HGAS(J)=-30.0
                 ENDIF
            ELSEIF(IOBJ.EQ.7)THEN
                 WGAS(J)=PI*RES(1)/180
            ELSEIF(IOBJ.EQ.8)THEN
                 OGAS(J)=RES(1)
            ELSE
                 PRINT *,' ###### GASADD ERROR   : Unidentified'//
     -                ' field; program bug - please report.'
                 OK=.FALSE.
            ENDIF
*   Next point.
40          CONTINUE
       ENDIF
*** Check the error flag and set the GASOK bit accordingly.
       CALL ALGERR
       IF(OK)THEN
            GASOK(IOBJ)=.TRUE.
       ELSE
            PRINT *,' !!!!!! GASADD WARNING : In total ',NERR,
     -           ' type or arithmetic errors were found; entry'//
     -           ' deleted.'
            GASOK(IOBJ)=.FALSE.
       ENDIF
*** If a formula was used, delete the entry point.
       IF(IENTRY.GT.0)CALL ALGCLR(IENTRY)
*** Next item.
10     CONTINUE
*** Print the error messages.
       CALL INPERR
       END
