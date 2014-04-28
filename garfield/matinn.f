CDECK  ID>, MATINN.
       SUBROUTINE MATINN(IRMAT,IRORD,IRPNT,IROUT,IFAIL)
*-----------------------------------------------------------------------
*   MATINN - Interpolates.
*   (Last changed on 12/ 2/07.)
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
       INTEGER IRMAT,IRORD,IRPNT,IROUT,IFAIL,ISMAT,ISORD,ISPNT,ISOUT,I,
     -      J,LORD,ISIZ(MXMDIM),IA(MXMDIM),NPOINT,MATADR,IOFF,IFAIL1
       REAL FINT,XMIN(MXMDIM),XMAX(MXMDIM)
       EXTERNAL FINT,MATADR
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE MATINN ///'
*** Assume that the routine will fail.
       IFAIL=1
*** Look up the matrices a first time.
       ISMAT=0
       ISORD=0
       ISPNT=0
       ISOUT=0
*   Scan the table.
       DO 10 I=1,MXMAT
       IF(MREF(I).EQ.IRMAT)THEN
            ISMAT=I
       ELSEIF(MREF(I).EQ.IRORD)THEN
            ISORD=I
       ELSEIF(MREF(I).EQ.IRPNT)THEN
            ISPNT=I
       ELSEIF(MREF(I).EQ.IROUT)THEN
            ISOUT=I
       ENDIF
       IF(ISMAT.GT.0.AND.ISORD.GT.0.AND.
     -      ISPNT.GT.0.AND.ISOUT.GT.0)GOTO 20
10     CONTINUE
*   Don't insist on the presence of an output matrix.
       IF(ISMAT.GT.0.AND.ISORD.GT.0.AND.ISPNT.GT.0)GOTO 20
*   The others however should exist.
       PRINT *,' !!!!!! MATINN WARNING : Could not find one of the'//
     -      ' matrices; no interpolation.'
       RETURN
20     CONTINUE
*** Interpolation routine FINT is limited to 5 dimensions.
       IF(MDIM(ISMAT).GT.5.OR.MDIM(ISMAT).LT.1)THEN
            PRINT *,' !!!!!! MATINN WARNING : Library interpolation'//
     -           ' routine limited to 1-5 dimensions; nothing done.'
            RETURN
       ENDIF
*** Verify the dimensions.
       LORD=0
       DO 30 I=1,MDIM(ISMAT)
       LORD=LORD+MSIZ(ISMAT,I)
30     CONTINUE
       IF(MDIM(ISMAT).NE.MSIZ(ISPNT,1).OR.
     -      LORD.NE.MSIZ(ISORD,1).OR.MDIM(ISORD).NE.1.OR.
     -      (MDIM(ISPNT).NE.1.AND.MDIM(ISPNT).NE.2))THEN
            PRINT *,' !!!!!! MATINN WARNING : Incompatible dimensions'//
     -           ' of matrix, ordinates and coordinates.'
            RETURN
       ENDIF
*** Take care of the output matrix.
       IF(ISOUT.NE.0)THEN
**  Already exists, check whether the size and shape are OK.
            IF(MDIM(ISOUT).NE.1.OR.
     -           MSIZ(ISOUT,1).LT.MSIZ(ISPNT,2))THEN
*   If not OK, re-shape the matrix.
                 ISIZ(1)=MSIZ(ISPNT,2)
                 CALL MATCHS(IROUT,1,ISIZ,0.0,IFAIL1)
*   Quit if re-shaping failed.
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! MATINN WARNING : Unable to'//
     -                     ' reshape output matrix; no interpolation.'
                      RETURN
                 ENDIF
            ENDIF
**  Output matrix did not exist yet, create one.
       ELSE
            ISIZ(1)=MSIZ(ISPNT,2)
            CALL MATADM('ALLOCATE',IROUT,1,ISIZ,2,IFAIL1)
*   Quit if creating failed.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MATINN WARNING : Unable to'//
     -                ' create an output matrix; no interpolation.'
                 RETURN
            ENDIF
       ENDIF
*** Look up the matrices a second time.
       ISMAT=0
       ISORD=0
       ISPNT=0
       ISOUT=0
*   Scan the table.
       DO 40 I=1,MXMAT
       IF(MREF(I).EQ.IRMAT)THEN
            ISMAT=I
       ELSEIF(MREF(I).EQ.IRORD)THEN
            ISORD=I
       ELSEIF(MREF(I).EQ.IRPNT)THEN
            ISPNT=I
       ELSEIF(MREF(I).EQ.IROUT)THEN
            ISOUT=I
       ENDIF
       IF(ISMAT.GT.0.AND.ISORD.GT.0.AND.
     -      ISPNT.GT.0.AND.ISOUT.GT.0)GOTO 50
40     CONTINUE
*   Now insist on the presence of an output matrix.
       PRINT *,' !!!!!! MATINN WARNING : Could not find one of the'//
     -      ' matrices; no interpolation.'
       RETURN
50     CONTINUE
*** Carry out the actual interpolation, loop over the points.
       IF(MDIM(ISPNT).EQ.2)THEN
            NPOINT=MSIZ(ISPNT,2)
       ELSE
            NPOINT=1
       ENDIF
*   Make a vector of sizes.
       IOFF=0
       DO 110 I=1,MDIM(ISMAT)
       ISIZ(I)=MSIZ(ISMAT,I)
       XMIN(I)=MVEC(MORG(ISORD)+IOFF+1)
       XMAX(I)=MVEC(MORG(ISORD)+IOFF+MSIZ(ISMAT,I))
       IOFF=IOFF+MSIZ(ISMAT,1)
110    CONTINUE
*   Do the actual interpolations.
       DO 100 I=1,NPOINT
       IA(1)=1
       IA(2)=I
       DO 120 J=1,MDIM(ISMAT)
       IF(MVEC(MATADR(ISPNT,IA)+J-1).LT.XMIN(J).OR.
     -      MVEC(MATADR(ISPNT,IA)+J-1).GT.XMAX(J))THEN
            IF(LDEBUG)PRINT *,' ++++++ MATINN DEBUG   : Point ',i,
     -           ' coordinate ',J,' = ',MVEC(MATADR(ISPNT,IA)+J-1),
     -           ' out of range: ',XMIN(J),XMAX(J)
            MVEC(MORG(ISOUT)+I)=0
            GOTO 100
       ENDIF
120    CONTINUE
       MVEC(MORG(ISOUT)+I)=FINT(MDIM(ISMAT),MVEC(MATADR(ISPNT,IA)),
     -      ISIZ,MVEC(MORG(ISORD)+1),MVEC(MORG(ISMAT)+1))
100    CONTINUE
*** Seems to have worked.
       IFAIL=0
       END
