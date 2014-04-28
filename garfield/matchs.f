CDECK  ID>, MATCHS.
       SUBROUTINE MATCHS(IREF,NDIM,IDIM,PAD,IFAIL)
*-----------------------------------------------------------------------
*   MATCHS - Changes the format of a matrix.
*   (Last changed on 10/ 4/96.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER IREF,IREFN,NDIM,IDIM(*),IFAIL,ISLOT,ISLOTN,MATSLT,IMOD
       REAL PAD
       EXTERNAL MATSLT
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MATCHS ///'
*** Debugging information.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MATCHS DEBUG   : Changing '',
     -      I5,'' to '',I5,'' dimensions, pad='',E12.5)')
     -      IREF,NDIM,PAD
*** Initial value of the failure flag.
       IFAIL=1
*** Check validity of reference.
       IF(IREF.LE.0)THEN
            PRINT *,' !!!!!! MATCHS WARNING : Non-positive reference'//
     -           ' given; matrix not re-shaped.'
            RETURN
       ENDIF
*** Find the mode of the current matrix.
       ISLOT=MATSLT(IREF)
       IF(ISLOT.LE.0)THEN
            PRINT *,' !!!!!! MATCHS WARNING : Matrix to be re-shaped'//
     -           ' has not been found.'
            RETURN
       ENDIF
       IMOD=MMOD(ISLOT)
*** Allocate space for the new matrix.
       CALL MATADM('ALLOCATE',IREFN,NDIM,IDIM,IMOD,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! MATCHS WARNING : Unable to allocate'//
     -           ' space for the re-shaped matrix ; not re-shaped.'
            RETURN
       ENDIF
*** Locate the current matrix.
       ISLOT=MATSLT(IREF)
       IF(ISLOT.LE.0)THEN
            PRINT *,' !!!!!! MATCHS WARNING : Matrix to be re-shaped'//
     -           ' has not been found.'
            RETURN
       ENDIF
*** Find where the new matrix sits.
       ISLOTN=MATSLT(IREFN)
       IF(ISLOTN.LE.0)THEN
            PRINT *,' !!!!!! MATCHS WARNING : New matrix not found;'//
     -           ' program bug - please report.'
            RETURN
       ENDIF
*** Copy the old matrix to the new one.
       DO 60 I=1,MIN(MLEN(ISLOT),MLEN(ISLOTN))
       MVEC(MORG(ISLOTN)+I)=MVEC(MORG(ISLOT)+I)
60     CONTINUE
       DO 70 I=MLEN(ISLOT)+1,MLEN(ISLOTN)
       MVEC(MORG(ISLOTN)+I)=PAD
70     CONTINUE
*** Modify the pointer information.
       MREF(ISLOTN)=MREF(ISLOT)
*** Delete the old matrix.
       MREF(ISLOT)=0
*** Things seem to have worked.
       IFAIL=0
       END
