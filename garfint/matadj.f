CDECK  ID>, MATADJ.
       SUBROUTINE MATADJ(IREF,NDIM,ISIZ,PAD,IFAIL)
*-----------------------------------------------------------------------
*   MATADJ - Changes the dimensions of a matrix, keeping shape.
*   Variables: IREF        : Reference of matrix
*              ISIZ        : Dimension sizes
*              PAD         : Value for new elements in matrix
*   (Last changed on 12/ 4/96.)
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
       PARAMETER (MXWIRE=   300,MXSW  =   50)
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
       PARAMETER (MXMAP =  5000,MXEPS =   10)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER IREF,IMOD,NDIM,ISIZ(*),IFAIL,ISLOT,ISLOTN,IA(MXMDIM),
     -      MATADR,MATSLT,IADDR,IADDRN
       REAL PAD
       EXTERNAL MATADR,MATSLT
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MATADJ ///'
*** Initial value of the failure flag.
       IFAIL=1
*** Locate the current matrix.
       ISLOT=MATSLT(IREF)
       IF(ISLOT.LE.0)THEN
            PRINT *,' !!!!!! MATADJ WARNING : Matrix to be re-sized'//
     -           ' has not been found.'
            RETURN
       ENDIF
*** Check array dimensions.
       IF(NDIM.NE.MDIM(ISLOT))THEN
            PRINT *,' !!!!!! MATADJ WARNING : Existing matrix has a'//
     -           ' different number of dimensions; not adjusted.'
            RETURN
       ENDIF
*** Allocate space for the new matrix.
       IMOD=MMOD(ISLOT)
       CALL MATADM('ALLOCATE',IREFN,NDIM,ISIZ,IMOD,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! MATADJ WARNING : Unable to allocate'//
     -           ' space for the re-sized matrix ; not re-sized.'
            RETURN
       ENDIF
*** Re-locate the current matrix.
       ISLOT=MATSLT(IREF)
       IF(ISLOT.LE.0)THEN
            PRINT *,' !!!!!! MATADJ WARNING : Matrix to be re-sized'//
     -           ' has not been found.'
            RETURN
       ENDIF
*** Find where the new matrix sits.
       ISLOTN=MATSLT(IREFN)
       IF(ISLOTN.LE.0)THEN
            PRINT *,' !!!!!! MATADJ WARNING : New matrix not found;'//
     -           ' program bug - please report.'
            RETURN
       ENDIF
*** Initialise the new matrix.
       DO 50 I=1,MLEN(ISLOTN)
       MVEC(MORG(ISLOTN)+I)=PAD
50     CONTINUE
*** Initial address vector.
       DO 60 I=1,MDIM(ISLOT)
       IA(I)=1
60     CONTINUE
*   Return here for the next element.
70     CONTINUE
*   Compute addresses in old and new matrix.
       IADDR=MATADR(ISLOT,IA)
       IADDRN=MATADR(ISLOTN,IA)
*   Assign.
       IF(IADDR.GT.0.AND.IADDRN.GT.0)MVEC(IADDRN)=MVEC(IADDR)
*   Increment the address vector.
       DO 80 I=1,MDIM(ISLOT)
       IF(IA(I).LT.MSIZ(ISLOT,I))THEN
            IA(I)=IA(I)+1
            DO 90 J=1,I-1
            IA(J)=1
90          CONTINUE
            GOTO 70
       ENDIF
80     CONTINUE
*** Modify the pointer information.
       MREF(ISLOTN)=MREF(ISLOT)
*** Delete the old matrix.
       MREF(ISLOT)=0
*** Things seem to have worked.
       IFAIL=0
       END
