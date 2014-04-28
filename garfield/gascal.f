CDECK  ID>, GASCAL.
       SUBROUTINE GASCAL(INSTR,IFAIL)
*-----------------------------------------------------------------------
*   GASCAL - Processes gas related procedure calls.
*   (Last changed on 20/ 1/09.)
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
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
       INTEGER INS(MXINS,4),ALGENT(MXALGE,10),MODREG(MXCONS:MXREG),
     -      ISYNCH,IINS0,ICONS0,ARGREF(MXARG,2),MODARG(MXARG),
     -      NREG,NCONS,NINS,NERR,NRES,NALGE,IENTRL,NAERR(100)
       REAL REG(MXCONS:MXREG),ARG(MXARG),EXPMAX
       PARAMETER(EXPMAX=40.0)
       LOGICAL EXEC(MXINS),LIGUND,LINUND
       COMMON /ALGDAT/ REG,ARG,MODARG,ARGREF,INS,MODREG,ALGENT,
     -      NREG,NCONS,NINS,NERR,NAERR,
     -      NRES,NALGE,IENTRL,ISYNCH,IINS0,ICONS0,EXEC,LIGUND,LINUND
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
       CHARACTER*(MXINCH) STRING
       INTEGER INPCMX,IFAIL,IFAIL1,IFAIL3,INSTR,IPROC,NARG,ISIZ(MXMDIM),
     -      IREP,ISEP,IREF(8),ISLOT(8),NDAT,MATSLT,I,J,NC,IAUX,STRLEN
       REAL GASVEL,GASMOB,GASDFT,GASTWN,GASATT,GASDFL,GASLOR,GASDIS,
     -      GASVT1,GASVT2,COV(3,3),EXVECT(MXEXG),IOVECT(MXIOG)
       EXTERNAL INPCMX,MATSLT,GASVEL,GASMOB,GASDFT,GASTWN,GASATT,
     -      GASDFL,GASLOR,GASVT1,GASVT2,GASDIS,STRLEN
*** Assume the CALL will fail.
       IFAIL=1
*** Verify that gas initialisation has been done.
       IF(.NOT.GASSET)THEN
            PRINT *,' !!!!!! GASCAL WARNING : Gas data not available'//
     -           ' ; procedure not executed.'
            RETURN
       ENDIF
*** Some easy reference variables.
       NARG=INS(INSTR,3)
       IPROC=INS(INSTR,1)
*** Get gas availability flags.
       IF(IPROC.EQ.-201)THEN
*   Check arguments.
            IF(NARG.NE.2.OR.MODARG(1).NE.1.OR.ARGREF(2,1).GE.2)THEN
                 PRINT *,' !!!!!! GASCAL WARNING : Incorrect'//
     -                ' argument list for GAS_AVAILABILITY.'
                 RETURN
            ENDIF
*   Clear the return argument.
            CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
*   Get hold of the item requested.
            CALL STRBUF('READ',NINT(ARG(1)),STRING,NC,IFAIL1)
*   Convert to upper case.
            CALL CLTOU(STRING(1:NC))
*   Drift velocity || E.
            IF(INPCMX(STRING(1:NC),'DR#IFT-VEL#OCITY')+
     -           INPCMX(STRING(1:NC),'VEL#OCITY-E').NE.0)THEN
                 IF(GASOK(1))THEN
                      ARG(2)=1
                 ELSE
                      ARG(2)=0
                 ENDIF
                 MODARG(2)=3
*   Drift velocity || Btrans.
            ELSEIF(INPCMX(STRING(1:NC),'VEL#OCITY-BTR#ANSVERSAL')+
     -           INPCMX(STRING(1:NC),'VEL#OCITY-BTR#ANSVERSE').NE.0)THEN
                 IF(GASOK(9))THEN
                      ARG(2)=1
                 ELSE
                      ARG(2)=0
                 ENDIF
                 MODARG(2)=3
*   Drift velocity || ExB.
            ELSEIF(INPCMX(STRING(1:NC),'VEL#OCITY-EXB').NE.0)THEN
                 IF(GASOK(10))THEN
                      ARG(2)=1
                 ELSE
                      ARG(2)=0
                 ENDIF
                 MODARG(2)=3
*   Ion mobility.
            ELSEIF(INPCMX(STRING(1:NC),'ION-MOB#ILITY').NE.0)THEN
                 IF(GASOK(2))THEN
                      ARG(2)=1
                 ELSE
                      ARG(2)=0
                 ENDIF
                 MODARG(2)=3
*   Longitudinal diffusion.
            ELSEIF(INPCMX(STRING(1:NC),
     -           'LONG#ITUDINAL-DIFF#USION-#COEFFICIENT')+
     -           INPCMX(STRING(1:NC),
     -           'DIFF#USION-#COEFFICIENT').NE.0)THEN
                 IF(GASOK(3))THEN
                      ARG(2)=1
                 ELSE
                      ARG(2)=0
                 ENDIF
                 MODARG(2)=3
*   Transverse diffusion.
            ELSEIF(INPCMX(STRING(1:NC),
     -           'TRANS#VERSE-DIFF#USION-#COEFFICIENT').NE.0)THEN
                 IF(GASOK(8))THEN
                      ARG(2)=1
                 ELSE
                      ARG(2)=0
                 ENDIF
                 MODARG(2)=3
*   Diffusion tensor.
            ELSEIF(INPCMX(STRING(1:NC),'DIFF#USION-TENS#OR').NE.0)THEN
                 IF(GASOK(11))THEN
                      ARG(2)=1
                 ELSE
                      ARG(2)=0
                 ENDIF
                 MODARG(2)=3
*   Townsend coefficient.
            ELSEIF(INPCMX(STRING(1:NC),
     -           'TOWN#SEND-#COEFFICIENT').NE.0)THEN
                 IF(GASOK(4))THEN
                      ARG(2)=1
                 ELSE
                      ARG(2)=0
                 ENDIF
                 MODARG(2)=3
*   Clustering data.
            ELSEIF(INPCMX(STRING(1:NC),
     -           'CLUS#TERING-DATA').NE.0)THEN
                 IF(GASOK(5))THEN
                      ARG(2)=1
                 ELSE
                      ARG(2)=0
                 ENDIF
                 MODARG(2)=3
*   Attachment coefficient.
            ELSEIF(INPCMX(STRING(1:NC),
     -           'ATT#ACHMENT-#COEFFICIENT').NE.0)THEN
                 IF(GASOK(6))THEN
                      ARG(2)=1
                 ELSE
                      ARG(2)=0
                 ENDIF
                 MODARG(2)=3
*   Ion dissociation coefficient.
            ELSEIF(INPCMX(STRING(1:NC),
     -           'ION-DISS#OCIATION-#COEFFICIENT').NE.0)THEN
                 IF(GASOK(12))THEN
                      ARG(2)=1
                 ELSE
                      ARG(2)=0
                 ENDIF
                 MODARG(2)=3
*   Lorentz angle.
            ELSEIF(INPCMX(STRING(1:NC),
     -           'LOR#ENTZ-#ANGLE').NE.0)THEN
                 IF(GASOK(7))THEN
                      ARG(2)=1
                 ELSE
                      ARG(2)=0
                 ENDIF
                 MODARG(2)=3
*   Unknown item.
            ELSE
                 PRINT *,' !!!!!! GASCAL WARNING : '//
     -                     STRING(1:NC)//' is not a known gas item.'
                 ARG(2)=0
                 MODARG(2)=0
            ENDIF
*** Get gas data.
       ELSEIF(IPROC.EQ.-202)THEN
*   Check arguments.
            IF(NARG.LT.1.OR.NARG.GT.3.OR.
     -           (NARG.GE.1.AND.ARGREF(1,1).GE.2).OR.
     -           (NARG.GE.2.AND.ARGREF(2,1).GE.2).OR.
     -           (NARG.GE.3.AND.ARGREF(3,1).GE.2))THEN
                 PRINT *,' !!!!!! GASCAL WARNING : Incorrect'//
     -                ' argument list for GET_GAS_DATA.'
                 RETURN
            ENDIF
*   Clear the storage.
            IF(NARG.GE.1)CALL ALGREU(NINT(ARG(1)),MODARG(1),ARGREF(1,1))
            IF(NARG.GE.2)CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
            IF(NARG.GE.3)CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
*   Store pressure.
            IF(NARG.GE.1)THEN
                 ARG(1)=PGAS
                 MODARG(1)=2
            ENDIF
*   Store temperature.
            IF(NARG.GE.2)THEN
                 ARG(2)=TGAS
                 MODARG(2)=2
            ENDIF
*   Store identifier.
            IF(NARG.GE.3)THEN
                 CALL STRBUF('STORE',IAUX,GASID,STRLEN(GASID),IFAIL3)
                 ARG(3)=REAL(IAUX)
                 MODARG(3)=1
            ENDIF
*** Get drift velocity, mobility, diffusion, Townsend, attachment.
       ELSEIF((IPROC.LE.-203.AND.IPROC.GE.-213).OR.IPROC.EQ.-215.OR.
     -      IPROC.EQ.-220.OR.IPROC.EQ.-221)THEN
**  Check arguments.
            IF((IPROC.EQ.-220.OR.IPROC.EQ.-221).AND..NOT.(
     -           (NARG.EQ.3.AND.
     -            MODARG(1).EQ.2.AND.
     -            (MODARG(2).EQ.2.OR.MODARG(2).EQ.5).AND.
     -            ARGREF(3,1).LE.1).OR.
     -           (NARG.EQ.5.AND.
     -            MODARG(1).EQ.2.AND.
     -            (MODARG(2).EQ.2.OR.MODARG(2).EQ.5).AND.
     -            (MODARG(3).EQ.2.OR.MODARG(3).EQ.5).AND.
     -            (MODARG(4).EQ.2.OR.MODARG(4).EQ.5).AND.
     -            ARGREF(5,1).LE.1).OR.
     -           (NARG.EQ.8.AND.
     -            MODARG(1).EQ.2.AND.
     -            (MODARG(2).EQ.2.OR.MODARG(2).EQ.5).AND.
     -            (MODARG(3).EQ.2.OR.MODARG(3).EQ.5).AND.
     -            (MODARG(4).EQ.2.OR.MODARG(4).EQ.5).AND.
     -            (MODARG(5).EQ.2.OR.MODARG(5).EQ.5).AND.
     -            (MODARG(6).EQ.2.OR.MODARG(6).EQ.5).AND.
     -            (MODARG(7).EQ.2.OR.MODARG(7).EQ.5).AND.
     -            ARGREF(8,1).LE.1)))THEN
                 PRINT *,' !!!!!! GASCAL WARNING : The procedure has'//
     -                ' as arguments either (id, E, rate),'//
     -                ' (id, Ex, Ey, Ez, rate) or'//
     -                ' (id, Ex, Ey, Ez, Bx, By, Bz, rate); not called.'
                 RETURN
            ELSEIF(IPROC.NE.-220.AND.IPROC.NE.-221.AND..NOT.(
     -           (NARG.EQ.2.AND.
     -           (MODARG(1).EQ.2.OR.MODARG(1).EQ.5).AND.
     -           ARGREF(2,1).LE.1).OR.
     -           (NARG.EQ.4.AND.
     -           (MODARG(1).EQ.2.OR.MODARG(1).EQ.5).AND.
     -           (MODARG(2).EQ.2.OR.MODARG(2).EQ.5).AND.
     -           (MODARG(3).EQ.2.OR.MODARG(3).EQ.5).AND.
     -           ARGREF(4,1).LE.1).OR.
     -           (NARG.EQ.7.AND.
     -           (MODARG(1).EQ.2.OR.MODARG(1).EQ.5).AND.
     -           (MODARG(2).EQ.2.OR.MODARG(2).EQ.5).AND.
     -           (MODARG(3).EQ.2.OR.MODARG(3).EQ.5).AND.
     -           (MODARG(4).EQ.2.OR.MODARG(4).EQ.5).AND.
     -           (MODARG(5).EQ.2.OR.MODARG(5).EQ.5).AND.
     -           (MODARG(6).EQ.2.OR.MODARG(6).EQ.5).AND.
     -           ARGREF(7,1).LE.1)))THEN
                 PRINT *,' !!!!!! GASCAL WARNING : The procedure has'//
     -                ' as input arguments either (E), (Ex, Ey, Ez)'//
     -                ' or (Ex, Ey, Ez, Bx, By, Bz); not called.'
                 RETURN
            ENDIF
            IF(NGAS.LT.1.OR.
     -           (IPROC.EQ.-203.AND..NOT.GASOK(1)).OR.
     -           (IPROC.EQ.-204.AND..NOT.GASOK(2)).OR.
     -           (IPROC.EQ.-205.AND..NOT.GASOK(3)).OR.
     -           (IPROC.EQ.-206.AND..NOT.GASOK(4)).OR.
     -           (IPROC.EQ.-207.AND..NOT.GASOK(6)).OR.
     -           (IPROC.EQ.-208.AND..NOT.GASOK(7)).OR.
     -           (IPROC.EQ.-209.AND..NOT.GASOK(8)).OR.
     -           (IPROC.EQ.-210.AND.(.NOT.(GASOK(1).OR.
     -                         GASOK(9).OR.GASOK(10)))).OR.
     -           (IPROC.EQ.-211.AND..NOT.GASOK(9)).OR.
     -           (IPROC.EQ.-212.AND..NOT.GASOK(10)).OR.
     -           (IPROC.EQ.-213.AND..NOT.GASOK(11)).OR.
     -           (IPROC.EQ.-215.AND..NOT.GASOK(12)).OR.
     -           (IPROC.EQ.-220.AND..NOT.GASOK(15)).OR.
     -           (IPROC.EQ.-221.AND..NOT.GASOK(16)))THEN
                 PRINT *,' !!!!!! GASCAL WARNING : The requested'//
     -                ' gas data is not available.'
                 RETURN
            ENDIF
*   Check level index for ionisations and excitations.
            IF(IPROC.EQ.-220.AND.(NINT(ARG(1)).LT.1.OR.
     -           NINT(ARG(1)).GT.NEXGAS))THEN
                 PRINT *,' !!!!!! GASCAL WARNING : Excitation level'//
     -                ' identifier out of range; not called.'
                 RETURN
            ENDIF
            IF(IPROC.EQ.-221.AND.(NINT(ARG(1)).LT.1.OR.
     -           NINT(ARG(1)).GT.NIOGAS))THEN
                 PRINT *,' !!!!!! GASCAL WARNING : Ionisation level'//
     -                ' identifier out of range; not called.'
                 RETURN
            ENDIF
**  Clear the storage.
            CALL ALGREU(NINT(ARG(NARG)),MODARG(NARG),ARGREF(NARG,1))
**  Are all arguments scalars ?
            IF(((IPROC.EQ.-220.OR.IPROC.EQ.-221).AND.
     -           NARG.EQ.3.AND.MODARG(2).EQ.2).OR.
     -           (IPROC.NE.-220.AND.IPROC.NE.-221.AND.
     -           NARG.EQ.2.AND.MODARG(1).EQ.2))THEN
                 IF(IPROC.EQ.-203)THEN
                      ARG(NARG)=GASVEL(ARG(1),0.0,0.0,0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-204)THEN
                      ARG(NARG)=GASMOB(ARG(1),0.0,0.0,0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-205)THEN
                      ARG(NARG)=GASDFL(ARG(1),0.0,0.0,0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-206)THEN
                      ARG(NARG)=GASTWN(ARG(1),0.0,0.0,0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-207)THEN
                      ARG(NARG)=GASATT(ARG(1),0.0,0.0,0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-208)THEN
                      ARG(NARG)=GASLOR(ARG(1),0.0,0.0,0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-209)THEN
                      ARG(NARG)=GASDFT(ARG(1),0.0,0.0,0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-210)THEN
                      ARG(NARG)=SQRT(
     -                     GASVEL(ARG(1),0.0,0.0,0.0,0.0,0.0)**2+
     -                     GASVT1(ARG(1),0.0,0.0,0.0,0.0,0.0)**2+
     -                     GASVT2(ARG(1),0.0,0.0,0.0,0.0,0.0)**2)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-211)THEN
                      ARG(NARG)=GASVT1(ARG(1),0.0,0.0,0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-212)THEN
                      ARG(NARG)=GASVT2(ARG(1),0.0,0.0,0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-213)THEN
                      CALL GASCOV(ARG(1),0.0,0.0,0.0,0.0,0.0,COV)
                      ISIZ(1)=3
                      ISIZ(2)=3
                      CALL MATADM('ALLOCATE',IREF(1),2,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! GASCAL WARNING : Unable'//
     -                          ' to get space for diffusion tensor.'
                           RETURN
                      ENDIF
                      ISLOT(1)=MATSLT(IREF(1))
                      IF(ISLOT(1).LE.0)THEN
                           PRINT *,' !!!!!! GASCAL WARNING : Unable'//
     -                          ' to locate the diffusion tensor.'
                           RETURN
                      ENDIF
                      DO 100 I=1,3
                      DO 110 J=1,3
                      MVEC(MORG(ISLOT(1))+(I-1)*3+J)=COV(I,J)
110                   CONTINUE
100                   CONTINUE
                      ARG(NARG)=IREF(1)
                      MODARG(NARG)=5
                 ELSEIF(IPROC.EQ.-215)THEN
                      ARG(NARG)=GASDIS(ARG(1),0.0,0.0,0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-220)THEN
                      CALL GASEXR(ARG(2),0.0,0.0,0.0,0.0,0.0,EXVECT)
                      ARG(NARG)=EXVECT(NINT(ARG(1)))
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-221)THEN
                      CALL GASIOR(ARG(2),0.0,0.0,0.0,0.0,0.0,IOVECT)
                      ARG(NARG)=IOVECT(NINT(ARG(1)))
                      MODARG(NARG)=2
                 ELSE
                      PRINT *,' !!!!!! GASCAL WARNING : Incorrect'//
     -                     ' procedure code received; please report.'
                      ARG(NARG)=0
                      MODARG(NARG)=0
                 ENDIF
            ELSEIF(((IPROC.EQ.-220.OR.IPROC.EQ.-221).AND.NARG.EQ.5.AND.
     -           MODARG(2).EQ.2.AND.MODARG(3).EQ.2.AND.
     -           MODARG(4).EQ.2).OR.
     -           (IPROC.NE.-220.AND.IPROC.NE.-221.AND.NARG.EQ.4.AND.
     -           MODARG(1).EQ.2.AND.MODARG(2).EQ.2.AND.
     -           MODARG(3).EQ.2))THEN
                 IF(IPROC.EQ.-203)THEN
                      ARG(NARG)=GASVEL(ARG(1),ARG(2),ARG(3),0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-204)THEN
                      ARG(NARG)=GASMOB(ARG(1),ARG(2),ARG(3),0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-205)THEN
                      ARG(NARG)=GASDFL(ARG(1),ARG(2),ARG(3),0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-206)THEN
                      ARG(NARG)=GASTWN(ARG(1),ARG(2),ARG(3),0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-207)THEN
                      ARG(NARG)=GASATT(ARG(1),ARG(2),ARG(3),0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-208)THEN
                      ARG(NARG)=GASLOR(ARG(1),ARG(2),ARG(3),0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-209)THEN
                      ARG(NARG)=GASDFT(ARG(1),ARG(2),ARG(3),0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-210)THEN
                      ARG(NARG)=SQRT(
     -                     GASVEL(ARG(1),ARG(2),ARG(3),0.0,0.0,0.0)**2+
     -                     GASVT1(ARG(1),ARG(2),ARG(3),0.0,0.0,0.0)**2+
     -                     GASVT2(ARG(1),ARG(2),ARG(3),0.0,0.0,0.0)**2)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-211)THEN
                      ARG(NARG)=GASVT1(ARG(1),ARG(2),ARG(3),0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-212)THEN
                      ARG(NARG)=GASVT2(ARG(1),ARG(2),ARG(3),0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-213)THEN
                      CALL GASCOV(ARG(1),ARG(2),ARG(3),0.0,0.0,0.0,COV)
                      ISIZ(1)=3
                      ISIZ(2)=3
                      CALL MATADM('ALLOCATE',IREF(1),2,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! GASCAL WARNING : Unable'//
     -                          ' to get space for diffusion tensor.'
                           RETURN
                      ENDIF
                      ISLOT(1)=MATSLT(IREF(1))
                      IF(ISLOT(1).LE.0)THEN
                           PRINT *,' !!!!!! GASCAL WARNING : Unable'//
     -                          ' to locate the diffusion tensor.'
                           RETURN
                      ENDIF
                      DO 101 I=1,3
                      DO 111 J=1,3
                      MVEC(MORG(ISLOT(1))+(I-1)*3+J)=COV(I,J)
111                   CONTINUE
101                   CONTINUE
                      ARG(NARG)=IREF(1)
                      MODARG(NARG)=5
                 ELSEIF(IPROC.EQ.-215)THEN
                      ARG(NARG)=GASDIS(ARG(1),ARG(2),ARG(3),0.0,0.0,0.0)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-220)THEN
                      CALL GASEXR(ARG(2),ARG(3),ARG(4),0.,0.,0.,EXVECT)
                      ARG(NARG)=EXVECT(NINT(ARG(1)))
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-221)THEN
                      CALL GASIOR(ARG(2),ARG(3),ARG(4),0.,0.,0.,IOVECT)
                      ARG(NARG)=IOVECT(NINT(ARG(1)))
                      MODARG(NARG)=2
                 ELSE
                      PRINT *,' !!!!!! GASCAL WARNING : Incorrect'//
     -                     ' procedure code received; please report.'
                      ARG(NARG)=0
                      MODARG(NARG)=0
                 ENDIF
            ELSEIF(((IPROC.EQ.-220.OR.IPROC.EQ.-221).AND.NARG.EQ.8.AND.
     -           MODARG(2).EQ.2.AND.MODARG(3).EQ.2.AND.
     -           MODARG(4).EQ.2.AND.MODARG(5).EQ.2.AND.
     -           MODARG(6).EQ.2.AND.MODARG(7).EQ.2).OR.
     -           (IPROC.NE.-220.AND.IPROC.NE.-221.AND.NARG.EQ.7.AND.
     -           MODARG(1).EQ.2.AND.MODARG(2).EQ.2.AND.
     -           MODARG(3).EQ.2.AND.MODARG(4).EQ.2.AND.
     -           MODARG(5).EQ.2.AND.MODARG(6).EQ.2))THEN
                 IF(IPROC.EQ.-203)THEN
                      ARG(NARG)=GASVEL(ARG(1),ARG(2),ARG(3),
     -                     ARG(4),ARG(5),ARG(6))
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-204)THEN
                      ARG(NARG)=GASMOB(ARG(1),ARG(2),ARG(3),
     -                     ARG(4),ARG(5),ARG(6))
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-205)THEN
                      ARG(NARG)=GASDFL(ARG(1),ARG(2),ARG(3),
     -                     ARG(4),ARG(5),ARG(6))
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-206)THEN
                      ARG(NARG)=GASTWN(ARG(1),ARG(2),ARG(3),
     -                     ARG(4),ARG(5),ARG(6))
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-207)THEN
                      ARG(NARG)=GASATT(ARG(1),ARG(2),ARG(3),
     -                     ARG(4),ARG(5),ARG(6))
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-208)THEN
                      ARG(NARG)=GASLOR(ARG(1),ARG(2),ARG(3),
     -                     ARG(4),ARG(5),ARG(6))
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-209)THEN
                      ARG(NARG)=GASDFT(ARG(1),ARG(2),ARG(3),
     -                     ARG(4),ARG(5),ARG(6))
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-210)THEN
                      ARG(NARG)=SQRT(
     -                     GASVEL(ARG(1),ARG(2),ARG(3),
     -                            ARG(4),ARG(5),ARG(6))**2+
     -                     GASVT1(ARG(1),ARG(2),ARG(3),
     -                            ARG(4),ARG(5),ARG(6))**2+
     -                     GASVT2(ARG(1),ARG(2),ARG(3),
     -                            ARG(4),ARG(5),ARG(6))**2)
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-211)THEN
                      ARG(NARG)=GASVT1(ARG(1),ARG(2),ARG(3),
     -                     ARG(4),ARG(5),ARG(6))
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-212)THEN
                      ARG(NARG)=GASVT2(ARG(1),ARG(2),ARG(3),
     -                     ARG(4),ARG(5),ARG(6))
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-213)THEN
                      CALL GASCOV(ARG(1),ARG(2),ARG(3),
     -                     ARG(4),ARG(5),ARG(6),COV)
                      ISIZ(1)=3
                      ISIZ(2)=3
                      CALL MATADM('ALLOCATE',IREF(1),2,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! GASCAL WARNING : Unable'//
     -                          ' to get space for diffusion tensor.'
                           RETURN
                      ENDIF
                      ISLOT(1)=MATSLT(IREF(1))
                      IF(ISLOT(1).LE.0)THEN
                           PRINT *,' !!!!!! GASCAL WARNING : Unable'//
     -                          ' to locate the diffusion tensor.'
                           RETURN
                      ENDIF
                      DO 120 I=1,3
                      DO 130 J=1,3
                      MVEC(MORG(ISLOT(1))+(I-1)*3+J)=COV(I,J)
130                   CONTINUE
120                   CONTINUE
                      ARG(NARG)=IREF(1)
                      MODARG(NARG)=5
                 ELSEIF(IPROC.EQ.-215)THEN
                      ARG(NARG)=GASDIS(ARG(1),ARG(2),ARG(3),
     -                     ARG(4),ARG(5),ARG(6))
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-220)THEN
                      CALL GASEXR(ARG(2),ARG(3),ARG(4),
     -                     ARG(5),ARG(6),ARG(7),EXVECT)
                      ARG(NARG)=EXVECT(NINT(ARG(1)))
                      MODARG(NARG)=2
                 ELSEIF(IPROC.EQ.-221)THEN
                      CALL GASIOR(ARG(2),ARG(3),ARG(4),
     -                     ARG(5),ARG(6),ARG(7),IOVECT)
                      ARG(NARG)=IOVECT(NINT(ARG(1)))
                      MODARG(NARG)=2
                 ELSE
                      PRINT *,' !!!!!! GASCAL WARNING : Incorrect'//
     -                     ' procedure code received; please report.'
                      ARG(NARG)=0
                      MODARG(NARG)=0
                 ENDIF
**  At least one of them is a matrix.
            ELSE
*   Figure out what the dimensions are.
                 NDAT=-1
                 DO 30 I=1,NARG-1
                 IF(I.EQ.1.AND.(IPROC.EQ.-220.OR.IPROC.EQ.-221))GOTO 30
                 IF(MODARG(I).EQ.5)THEN
                      IREF(I)=NINT(ARG(I))
                      ISLOT(I)=MATSLT(IREF(I))
                      IF(ISLOT(I).LE.0)THEN
                           PRINT *,' !!!!!! GASCAL WARNING : Unable'//
     -                          ' locate an input matrix.'
                           RETURN
                      ELSEIF(MMOD(ISLOT(I)).NE.2)THEN
                           PRINT *,' !!!!!! GASCAL WARNING : E or B'//
     -                          ' vector of incorrect type.'
                           RETURN
                      ENDIF
                      IF(NDAT.LT.0)THEN
                           NDAT=MLEN(ISLOT(I))
                      ELSEIF(NDAT.NE.MLEN(ISLOT(I)))THEN
                           PRINT *,' !!!!!! GASCAL WARNING : E and'//
     -                          ' B have inconsistent lengths.'
                           RETURN
                      ENDIF
                 ENDIF
30               CONTINUE
                 IF(NDAT.LT.1)THEN
                      PRINT *,' !!!!!! GASCAL WARNING : Unable'//
     -                     ' to find an E or B vector.'
                      RETURN
                 ENDIF
*   Now book matrices for the missing elements and initialise them.
                 DO 40 I=1,NARG-1
                 IF(I.EQ.1.AND.(IPROC.EQ.-220.OR.IPROC.EQ.-221))GOTO 40
                 IF(MODARG(I).NE.5)THEN
                      ISIZ(1)=NDAT
                      CALL MATADM('ALLOCATE',IREF(I),1,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! GASCAL WARNING : Unable'//
     -                          ' to get a vector replacement.'
                           RETURN
                      ENDIF
                      ISLOT(I)=MATSLT(IREF(I))
                      IF(ISLOT(I).LE.0)THEN
                           PRINT *,' !!!!!! GASCAL WARNING : Unable'//
     -                          ' to locate a vector replacement.'
                           RETURN
                      ENDIF
                      DO 50 J=1,MLEN(ISLOT(I))
                      MVEC(MORG(ISLOT(I))+J)=ARG(I)
50                    CONTINUE
                 ENDIF
40               CONTINUE
*   Allocate an output vector.
                 ISIZ(1)=NDAT
                 CALL MATADM('ALLOCATE',IREF(NARG),1,ISIZ,2,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! GASCAL WARNING : Unable'//
     -                     ' to get an output vector.'
                      RETURN
                 ENDIF
*   And finally locate all vectors.
                 DO 60 I=1,NARG
                 IF(I.EQ.1.AND.(IPROC.EQ.-220.OR.IPROC.EQ.-221))GOTO 60
                 ISLOT(I)=MATSLT(IREF(I))
                 IF(ISLOT(I).LE.0)THEN
                      PRINT *,' !!!!!! GASCAL WARNING : Unable'//
     -                     ' to locate E, B or output.'
                      RETURN
                 ENDIF
60               CONTINUE
*   And compute the data.
                 IF(((IPROC.EQ.-220.OR.IPROC.EQ.-221).AND.
     -                NARG.EQ.8).OR.
     -                (IPROC.NE.-220.AND.IPROC.NE.-221.AND.
     -                NARG.EQ.7))THEN
                      DO 70 I=1,NDAT
                      IF(IPROC.EQ.-203)THEN
                           MVEC(MORG(ISLOT(7))+I)=GASVEL(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),
     -                          MVEC(MORG(ISLOT(4))+I),
     -                          MVEC(MORG(ISLOT(5))+I),
     -                          MVEC(MORG(ISLOT(6))+I))
                      ELSEIF(IPROC.EQ.-204)THEN
                           MVEC(MORG(ISLOT(7))+I)=GASMOB(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),
     -                          MVEC(MORG(ISLOT(4))+I),
     -                          MVEC(MORG(ISLOT(5))+I),
     -                          MVEC(MORG(ISLOT(6))+I))
                      ELSEIF(IPROC.EQ.-205)THEN
                           MVEC(MORG(ISLOT(7))+I)=GASDFL(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),
     -                          MVEC(MORG(ISLOT(4))+I),
     -                          MVEC(MORG(ISLOT(5))+I),
     -                          MVEC(MORG(ISLOT(6))+I))
                      ELSEIF(IPROC.EQ.-206)THEN
                           MVEC(MORG(ISLOT(7))+I)=GASTWN(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),
     -                          MVEC(MORG(ISLOT(4))+I),
     -                          MVEC(MORG(ISLOT(5))+I),
     -                          MVEC(MORG(ISLOT(6))+I))
                      ELSEIF(IPROC.EQ.-207)THEN
                           MVEC(MORG(ISLOT(7))+I)=GASATT(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),
     -                          MVEC(MORG(ISLOT(4))+I),
     -                          MVEC(MORG(ISLOT(5))+I),
     -                          MVEC(MORG(ISLOT(6))+I))
                      ELSEIF(IPROC.EQ.-208)THEN
                           MVEC(MORG(ISLOT(7))+I)=GASLOR(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),
     -                          MVEC(MORG(ISLOT(4))+I),
     -                          MVEC(MORG(ISLOT(5))+I),
     -                          MVEC(MORG(ISLOT(6))+I))
                      ELSEIF(IPROC.EQ.-209)THEN
                           MVEC(MORG(ISLOT(7))+I)=GASDFT(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),
     -                          MVEC(MORG(ISLOT(4))+I),
     -                          MVEC(MORG(ISLOT(5))+I),
     -                          MVEC(MORG(ISLOT(6))+I))
                      ELSEIF(IPROC.EQ.-210)THEN
                           MVEC(MORG(ISLOT(7))+I)=SQRT(
     -                          GASVEL(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),
     -                          MVEC(MORG(ISLOT(4))+I),
     -                          MVEC(MORG(ISLOT(5))+I),
     -                          MVEC(MORG(ISLOT(6))+I))**2+
     -                          GASVT1(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),
     -                          MVEC(MORG(ISLOT(4))+I),
     -                          MVEC(MORG(ISLOT(5))+I),
     -                          MVEC(MORG(ISLOT(6))+I))**2+
     -                          GASVT2(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),
     -                          MVEC(MORG(ISLOT(4))+I),
     -                          MVEC(MORG(ISLOT(5))+I),
     -                          MVEC(MORG(ISLOT(6))+I))**2)
                      ELSEIF(IPROC.EQ.-211)THEN
                           MVEC(MORG(ISLOT(7))+I)=GASVT1(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),
     -                          MVEC(MORG(ISLOT(4))+I),
     -                          MVEC(MORG(ISLOT(5))+I),
     -                          MVEC(MORG(ISLOT(6))+I))
                      ELSEIF(IPROC.EQ.-212)THEN
                           MVEC(MORG(ISLOT(7))+I)=GASVT2(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),
     -                          MVEC(MORG(ISLOT(4))+I),
     -                          MVEC(MORG(ISLOT(5))+I),
     -                          MVEC(MORG(ISLOT(6))+I))
                      ELSEIF(IPROC.EQ.-215)THEN
                           MVEC(MORG(ISLOT(7))+I)=GASDIS(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),
     -                          MVEC(MORG(ISLOT(4))+I),
     -                          MVEC(MORG(ISLOT(5))+I),
     -                          MVEC(MORG(ISLOT(6))+I))
                      ELSEIF(IPROC.EQ.-220)THEN
                           CALL GASEXR(
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),
     -                          MVEC(MORG(ISLOT(4))+I),
     -                          MVEC(MORG(ISLOT(5))+I),
     -                          MVEC(MORG(ISLOT(6))+I),
     -                          MVEC(MORG(ISLOT(7))+I),
     -                          EXVECT)
                           MVEC(MORG(ISLOT(8))+I)=EXVECT(NINT(ARG(1)))
                      ELSEIF(IPROC.EQ.-221)THEN
                           CALL GASIOR(
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),
     -                          MVEC(MORG(ISLOT(4))+I),
     -                          MVEC(MORG(ISLOT(5))+I),
     -                          MVEC(MORG(ISLOT(6))+I),
     -                          MVEC(MORG(ISLOT(7))+I),
     -                          IOVECT)
                           MVEC(MORG(ISLOT(8))+I)=IOVECT(NINT(ARG(1)))
                      ELSE
                           PRINT *,' !!!!!! GASCAL WARNING : Wrong'//
     -                          ' procedure code received; report.'
                           MVEC(MORG(ISLOT(4))+I)=0
                      ENDIF
70                    CONTINUE
                 ELSEIF(((IPROC.EQ.-220.OR.IPROC.EQ.-221).AND.
     -                NARG.EQ.5).OR.
     -                (IPROC.NE.-220.AND.IPROC.NE.-221.AND.
     -                NARG.EQ.4))THEN
                      DO 90 I=1,NDAT
                      IF(IPROC.EQ.-203)THEN
                           MVEC(MORG(ISLOT(4))+I)=GASVEL(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-204)THEN
                           MVEC(MORG(ISLOT(4))+I)=GASMOB(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-205)THEN
                           MVEC(MORG(ISLOT(4))+I)=GASDFL(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-206)THEN
                           MVEC(MORG(ISLOT(4))+I)=GASTWN(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-207)THEN
                           MVEC(MORG(ISLOT(4))+I)=GASATT(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-208)THEN
                           MVEC(MORG(ISLOT(4))+I)=GASLOR(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-209)THEN
                           MVEC(MORG(ISLOT(4))+I)=GASDFT(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-210)THEN
                           MVEC(MORG(ISLOT(4))+I)=SQRT(
     -                          GASVEL(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),0.0,0.0,0.0)**2+
     -                          GASVT1(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),0.0,0.0,0.0)**2+
     -                          GASVT2(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),0.0,0.0,0.0)**2)
                      ELSEIF(IPROC.EQ.-211)THEN
                           MVEC(MORG(ISLOT(4))+I)=GASVT1(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-212)THEN
                           MVEC(MORG(ISLOT(4))+I)=GASVT2(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-215)THEN
                           MVEC(MORG(ISLOT(4))+I)=GASDIS(
     -                          MVEC(MORG(ISLOT(1))+I),
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-220)THEN
                           CALL GASEXR(
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),
     -                          MVEC(MORG(ISLOT(4))+I),0.0,0.0,0.0,
     -                          EXVECT)
                           MVEC(MORG(ISLOT(5))+I)=EXVECT(NINT(ARG(1)))
                      ELSEIF(IPROC.EQ.-221)THEN
                           CALL GASIOR(
     -                          MVEC(MORG(ISLOT(2))+I),
     -                          MVEC(MORG(ISLOT(3))+I),
     -                          MVEC(MORG(ISLOT(4))+I),0.0,0.0,0.0,
     -                          IOVECT)
                           MVEC(MORG(ISLOT(5))+I)=IOVECT(NINT(ARG(1)))
                      ELSE
                           PRINT *,' !!!!!! GASCAL WARNING : Wrong'//
     -                          ' procedure code received; report.'
                           MVEC(MORG(ISLOT(4))+I)=0
                      ENDIF
90                    CONTINUE
                 ELSEIF(((IPROC.EQ.-220.OR.IPROC.EQ.-221).AND.
     -                NARG.EQ.3).OR.
     -                (IPROC.NE.-220.AND.IPROC.NE.-221.AND.
     -                NARG.EQ.2))THEN
                      DO 91 I=1,NDAT
                      IF(IPROC.EQ.-203)THEN
                           MVEC(MORG(ISLOT(2))+I)=GASVEL(
     -                          MVEC(MORG(ISLOT(1))+I),0.0,0.0,
     -                          0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-204)THEN
                           MVEC(MORG(ISLOT(2))+I)=GASMOB(
     -                          MVEC(MORG(ISLOT(1))+I),0.0,0.0,
     -                          0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-205)THEN
                           MVEC(MORG(ISLOT(2))+I)=GASDFL(
     -                          MVEC(MORG(ISLOT(1))+I),0.0,0.0,
     -                          0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-206)THEN
                           MVEC(MORG(ISLOT(2))+I)=GASTWN(
     -                          MVEC(MORG(ISLOT(1))+I),0.0,0.0,
     -                          0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-207)THEN
                           MVEC(MORG(ISLOT(2))+I)=GASATT(
     -                          MVEC(MORG(ISLOT(1))+I),0.0,0.0,
     -                          0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-208)THEN
                           MVEC(MORG(ISLOT(2))+I)=GASLOR(
     -                          MVEC(MORG(ISLOT(1))+I),0.0,0.0,
     -                          0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-209)THEN
                           MVEC(MORG(ISLOT(2))+I)=GASDFT(
     -                          MVEC(MORG(ISLOT(1))+I),0.0,0.0,
     -                          0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-210)THEN
                           MVEC(MORG(ISLOT(2))+I)=SQRT(
     -                          GASVEL(
     -                          MVEC(MORG(ISLOT(1))+I),0.0,0.0,
     -                          0.0,0.0,0.0)**2+
     -                          GASVT1(
     -                          MVEC(MORG(ISLOT(1))+I),0.0,0.0,
     -                          0.0,0.0,0.0)**2+
     -                          GASVT2(
     -                          MVEC(MORG(ISLOT(1))+I),0.0,0.0,
     -                          0.0,0.0,0.0)**2)
                      ELSEIF(IPROC.EQ.-211)THEN
                           MVEC(MORG(ISLOT(2))+I)=GASVT1(
     -                          MVEC(MORG(ISLOT(1))+I),0.0,0.0,
     -                          0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-212)THEN
                           MVEC(MORG(ISLOT(2))+I)=GASVT2(
     -                          MVEC(MORG(ISLOT(1))+I),0.0,0.0,
     -                          0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-215)THEN
                           MVEC(MORG(ISLOT(2))+I)=GASDIS(
     -                          MVEC(MORG(ISLOT(1))+I),0.0,0.0,
     -                          0.0,0.0,0.0)
                      ELSEIF(IPROC.EQ.-220)THEN
                           CALL GASEXR(
     -                          MVEC(MORG(ISLOT(2))+I),0.0,0.0,
     -                          0.0,0.0,0.0,EXVECT)
                           MVEC(MORG(ISLOT(3))+I)=EXVECT(NINT(ARG(1)))
                      ELSEIF(IPROC.EQ.-221)THEN
                           CALL GASIOR(
     -                          MVEC(MORG(ISLOT(2))+I),0.0,0.0,
     -                          0.0,0.0,0.0,IOVECT)
                           MVEC(MORG(ISLOT(3))+I)=IOVECT(NINT(ARG(1)))
                      ELSE
                           PRINT *,' !!!!!! GASCAL WARNING : Wrong'//
     -                          ' procedure code received; report.'
                           MVEC(MORG(ISLOT(2))+I)=0
                      ENDIF
91                    CONTINUE
                 ENDIF
*   Delete temporary matrices.
                 DO 80 I=1,NARG-1
                 IF(I.EQ.1.AND.(IPROC.EQ.-220.OR.IPROC.EQ.-221))GOTO 80
                 IF(MODARG(I).NE.5)THEN
                      ISIZ(1)=NDAT
                      CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)PRINT *,' !!!!!! GASCAL WARNING'//
     -                     ' : Unable to delete a vector replacement.'
                 ENDIF
80               CONTINUE
*   And save the output.
                 ARG(NARG)=IREF(NARG)
                 MODARG(NARG)=5
            ENDIF
*** Get E/p.
       ELSEIF(IPROC.EQ.-214)THEN
*   Check arguments.
            IF(NARG.NE.1.OR.ARGREF(1,1).GE.2)THEN
                 PRINT *,' !!!!!! GASCAL WARNING : Incorrect'//
     -                ' argument list for GET_EP_TABLE.'
                 RETURN
            ELSEIF(NGAS.LT.1)THEN
                 PRINT *,' !!!!!! GASCAL WARNING : No E/p table'//
     -                ' available.'
                 RETURN
            ENDIF
*   Clear the storage.
            CALL ALGREU(NINT(ARG(1)),MODARG(1),ARGREF(1,1))
*   Get a matrix of the proper size.
            ISIZ(1)=NGAS
            CALL MATADM('ALLOCATE',IREP,1,ISIZ,2,IFAIL1)
            ISEP=MATSLT(IREP)
            IF(IFAIL1.NE.0.OR.ISEP.LE.0)THEN
                 PRINT *,' !!!!!! GASCAL WARNING : Unable to obtain'//
     -                ' matrix storage.'
                 RETURN
            ENDIF
*   Copy the contents.
            DO 150 I=1,NGAS
            MVEC(MORG(ISEP)+I)=EGAS(I)
150         CONTINUE
*   And save the output.
            ARG(1)=IREP
            MODARG(1)=5
*** Get excitation level name.
       ELSEIF(IPROC.EQ.-216)THEN
*   Check arguments.
            IF(NARG.LT.2.OR.NARG.GT.3.OR.
     -           MODARG(1).NE.2.OR.ARGREF(2,1).GE.2.OR.
     -           (NARG.GE.3.AND.ARGREF(3,1).GE.2))THEN
                 PRINT *,' !!!!!! GASCAL WARNING : Incorrect'//
     -                ' argument list for EXCITATION_IDENTIFIER.'
                 RETURN
            ELSEIF(.NOT.GASOK(15))THEN
                 PRINT *,' !!!!!! GASCAL WARNING : No excitation'//
     -                ' data available.'
                 RETURN
            ENDIF
*   Clear the storage.
            CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
            IF(NARG.GE.3)
     -           CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
*   Store the string.
            IF(NINT(ARG(1)).LT.1.OR.NINT(ARG(1)).GT.NEXGAS)THEN
                 PRINT *,' !!!!!! GASCAL WARNING : No excitation'//
     -                ' level with reference number ',NINT(ARG(1)),'.'
                 IFAIL=1
                 RETURN
            ELSE
                 CALL STRBUF('STORE',IAUX,DSCEXG(NINT(ARG(1))),
     -                STRLEN(DSCEXG(NINT(ARG(1)))),IFAIL3)
                 IF(IFAIL3.NE.0)PRINT *,' !!!!!! GASCAL WARNING :'//
     -                ' Unable to store excitation level name',
     -                 DSCEXG(NINT(ARG(1)))
                 ARG(2)=REAL(IAUX)
                 MODARG(2)=1
            ENDIF
*   Store the energy.
            IF(NARG.GE.3)THEN
                 ARG(3)=ENEXG(NINT(ARG(1)))
                 MODARG(3)=2
            ENDIF
*** Get ionisation level name.
       ELSEIF(IPROC.EQ.-217)THEN
*   Check arguments.
            IF(NARG.LT.2.OR.NARG.GT.3.OR.
     -           MODARG(1).NE.2.OR.ARGREF(2,1).GE.2.OR.
     -           (NARG.GE.3.AND.ARGREF(3,1).GE.2))THEN
                 PRINT *,' !!!!!! GASCAL WARNING : Incorrect'//
     -                ' argument list for IONISATION_IDENTIFIER.'
                 RETURN
            ELSEIF(.NOT.GASOK(16))THEN
                 PRINT *,' !!!!!! GASCAL WARNING : No ionisation'//
     -                ' data available.'
                 RETURN
            ENDIF
*   Clear the storage.
            CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
            IF(NARG.GE.3)
     -           CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
*   Store the string.
            IF(NINT(ARG(1)).LT.1.OR.NINT(ARG(1)).GT.NIOGAS)THEN
                 PRINT *,' !!!!!! GASCAL WARNING : No ionisation'//
     -                ' level with reference number ',NINT(ARG(1)),'.'
                 IFAIL=1
                 RETURN
            ELSE
                 CALL STRBUF('STORE',IAUX,DSCIOG(NINT(ARG(1))),
     -                STRLEN(DSCIOG(NINT(ARG(1)))),IFAIL3)
                 IF(IFAIL3.NE.0)PRINT *,' !!!!!! GASCAL WARNING :'//
     -                ' Unable to store ionisation level name',
     -                 DSCIOG(NINT(ARG(1)))
                 ARG(2)=REAL(IAUX)
                 MODARG(2)=1
            ENDIF
*   Store the energy.
            IF(NARG.GE.3)THEN
                 ARG(3)=ENIOG(NINT(ARG(1)))
                 MODARG(3)=2
            ENDIF
*** Get cross section name.
       ELSEIF(IPROC.EQ.-218)THEN
*   Check arguments.
            IF(NARG.LT.2.OR.NARG.GT.3.OR.
     -           MODARG(1).NE.2.OR.
     -           (NARG.GE.2.AND.ARGREF(2,1).GE.2).OR.
     -           (NARG.GE.3.AND.ARGREF(3,1).GE.2))THEN
                 PRINT *,' !!!!!! GASCAL WARNING : Incorrect'//
     -                ' argument list for CROSS_SECTION_IDENTIFIER.'
                 RETURN
            ENDIF
*   Clear the storage.
            IF(NARG.GE.2)CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
            IF(NARG.GE.3)CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
*   Store the string.
            IF(NINT(ARG(1)).LT.1.OR.NINT(ARG(1)).GT.NCSGAS)THEN
                 PRINT *,' !!!!!! GASCAL WARNING : No'//
     -                ' cross section with number ',NINT(ARG(1)),'.'
                 IFAIL=1
                 RETURN
            ELSE
                 CALL STRBUF('STORE',IAUX,DSCCSG(NINT(ARG(1))),
     -                STRLEN(DSCCSG(NINT(ARG(1)))),IFAIL3)
                 IF(IFAIL3.NE.0)PRINT *,' !!!!!! GASCAL WARNING :'//
     -                ' Unable to store cross section name '//
     -                 DSCCSG(NINT(ARG(1)))
                 ARG(2)=REAL(IAUX)
                 MODARG(2)=1
            ENDIF
*   Store the kind of cross section.
            IF(NARG.GE.3)THEN
                 IFAIL1=0
                 IF(ICSTYP(NINT(ARG(1))).EQ.1)THEN
                      CALL STRBUF('STORE',IAUX,'Elastic',7,IFAIL1)
                 ELSEIF(ICSTYP(NINT(ARG(1))).EQ.2)THEN
                      CALL STRBUF('STORE',IAUX,'Ionisation',10,IFAIL1)
                 ELSEIF(ICSTYP(NINT(ARG(1))).EQ.3)THEN
                      CALL STRBUF('STORE',IAUX,'Attachment',10,IFAIL1)
                 ELSEIF(ICSTYP(NINT(ARG(1))).EQ.4)THEN
                      CALL STRBUF('STORE',IAUX,'Excitation',10,IFAIL1)
                 ELSEIF(ICSTYP(NINT(ARG(1))).EQ.5)THEN
                      CALL STRBUF('STORE',IAUX,'Super-elastic',13,
     -                     IFAIL1)
                 ELSEIF(ICSTYP(NINT(ARG(1))).EQ.6)THEN
                      CALL STRBUF('STORE',IAUX,'Inelastic',9,IFAIL1)
                 ELSE
                      PRINT *,' !!!!!! GASCAL WARNING : Unknown cross'//
     -                     ' section type; no type stored.'
                      IFAIL1=1
                 ENDIF
                 IF(IFAIL1.EQ.0)THEN
                      ARG(3)=REAL(IAUX)
                      MODARG(3)=1
                 ELSE
                      MODARG(3)=0
                 ENDIF
            ENDIF
*** Return number of levels.
       ELSEIF(IPROC.EQ.-219)THEN
*   Check arguments.
            IF(NARG.GT.2.OR.
     -           (NARG.GE.1.AND.ARGREF(1,1).GE.2).OR.
     -           (NARG.GE.2.AND.ARGREF(2,1).GE.2))THEN
                 PRINT *,' !!!!!! GASCAL WARNING : Incorrect'//
     -                ' argument list for LEVEL_COUNT.'
                 RETURN
            ENDIF
*   Clear the storage.
            DO 140 I=1,NARG
            CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
140         CONTINUE
*   Store the level count.
            ARG(1)=REAL(NEXGAS)
            ARG(2)=REAL(NIOGAS)
            MODARG(1)=2
            MODARG(2)=2
*** Debugging.
       ELSEIF(IPROC.EQ.-222)THEN
            CALL GASLEX(IFAIL1)
*** Unknown gas operation.
       ELSE
            PRINT *,' !!!!!! GASCAL WARNING : Unknown procedure code'//
     -           ' received; nothing done.'
            IFAIL=1
            RETURN
       ENDIF
*** Seems to have worked.
       IFAIL=0
       END
