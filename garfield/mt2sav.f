CDECK  ID>, MT2SAV.
       SUBROUTINE MT2SAV(VAL,NDIM,IDIM,ISIZ,NAME,IFAIL)
*-----------------------------------------------------------------------
*   MT2SAV - Assigns a double precision matrix to a global variable.
*   (Last changed on 26/ 6/96.)
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
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(*) NAME
       DOUBLE PRECISION VAL(*)
       INTEGER IFAIL,JVAR,I,NDIM,ISIZ(*),IDIM(*),MATSLT,MATADR,
     -      IA(MXMDIM),IADDR,JADDR
       EXTERNAL MATSLT,MATADR
*** Tracing and debugging output.
       IF(LIDENT)PRINT *,' /// ROUTINE MT2SAV ///'
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MT2SAV DEBUG   : Storing '',
     -      I3,''-matrix to '',A,''.'')') NDIM,NAME
*** Initial failure flag setting.
       IFAIL=1
*** Make sure that the number of dimensions is reasonable.
       IF(NDIM.GT.MXMDIM.OR.NDIM.LT.1)THEN
            PRINT *,' !!!!!! MT2SAV WARNING : Number of dimensions'//
     -           ' not in the range [1,MXMDIM]; not stored.'
            RETURN
       ENDIF
*** Scan the list of global variables.
       JVAR=0
       DO 100 I=1,NGLB
       IF(GLBVAR(I).EQ.NAME)JVAR=I
100    CONTINUE
*** If it didn't exist, create a new global ...
       IF(JVAR.EQ.0)THEN
*   if there still is space,
            IF(NGLB.LT.MXVAR)THEN
                 NGLB=NGLB+1
                 GLBVAR(NGLB)=NAME
                 JVAR=NGLB
*   otherwise issue a warning.
            ELSE
                 PRINT *,' !!!!!! MT2SAV WARNING : No global variable'//
     -                ' space left for ',NAME,'; matrix not saved.'
                 RETURN
            ENDIF
*** Otherwise re-use an existing global.
       ELSE
            CALL ALGREU(NINT(GLBVAL(JVAR)),GLBMOD(JVAR),0)
       ENDIF
*** Allocate a matrix.
       CALL MATADM('ALLOCATE',IRMAT,NDIM,ISIZ,2,IFAIL1)
*   Check error condition.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! MT2SAV WARNING : Unable to allocate'//
     -           ' space for ',NAME,'; matrix not saved.'
            RETURN
       ENDIF
*   Find location.
       ISMAT=MATSLT(IRMAT)
       IF(ISMAT.LE.0)THEN
            PRINT *,' !!!!!! MT2SAV WARNING : Failure to locate'//
     -           ' the receiving matrix; matrix not stored.'
            RETURN
       ENDIF
*** Copy the array to the matrix, initial address vector.
       DO 10 I=1,NDIM
       IF(ISIZ(I).LE.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MT2SAV DEBUG   :'',
     -           '' Dimension '',I2,'' has length < 1.'')') I
            RETURN
       ENDIF
       IA(I)=1
10     CONTINUE
20     CONTINUE
*   Compute matrix address.
       IADDR=MATADR(ISMAT,IA)
*   Compute Fortran address.
       JADDR=IA(NDIM)-1
       DO 30 I=NDIM-1,1,-1
       JADDR=JADDR*IDIM(I)+IA(I)-1
30     CONTINUE
       JADDR=JADDR+1
*   Copy.
       MVEC(IADDR)=REAL(VAL(JADDR))
*   Update address pointer.
       DO 40 I=1,NDIM
       IF(IA(I).LT.ISIZ(I))THEN
            IA(I)=IA(I)+1
            DO 50 J=1,I-1
            IA(J)=1
50          CONTINUE
            GOTO 20
       ENDIF
40     CONTINUE
*** Assign the number to the global.
       GLBVAL(JVAR)=REAL(IRMAT)
       GLBMOD(JVAR)=5
*** Things seem to have worked.
       IFAIL=0
       END
