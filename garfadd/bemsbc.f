CDECK  ID>, BEMSBC.
       SUBROUTINE BEMSBC(IPRIM1, NPRIM, PRLIST)
*-----------------------------------------------------------------------
*   BEMSBC - Lists primitives with the same boundary condition. For the
*            time being, boundary conditions and volumes are linked.
*   (Last changed on  5/ 4/10.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER NBEM,IREFB1(MXPLAN),NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,
     -      BEMNEW,BEMINV,BEMSLV
       DOUBLE PRECISION BEMQTH,BEMSTH,BEMSSC,BEMTGT,BEMEPA,BEMEPD
       LOGICAL LBDUMP
       COMMON /BEMDAT/ BEMQTH,BEMSSC,BEMSTH,BEMTGT,BEMEPA,BEMEPD,
     -      IREFB1,NBEM,NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,BEMNEW,
     -      BEMINV,BEMSLV,LBDUMP
       INTEGER IPRIM1, NPRIM, PRLIST(*)
       INTEGER I, NVERTEX, IVOL1, IVOL2, IVOLR1, IVOLR2, IFAIL
       DOUBLE PRECISION XVERT(MXEDGE), YVERT(MXEDGE), ZVERT(MXEDGE),
     -      XNORM, YNORM, ZNORM
*** Initialise count
       NPRIM=0
*** Check the reference primitive number
       IF(IPRIM1.LT.1.OR.IPRIM1.GT.NBEM)THEN
            PRINT *,' !!!!!! BEMSBC WARNING : Reference primitive'//
     -           ' is out of range; no list returned.'
            RETURN
       ENDIF
*** Find the volumes for the primitive.
       CALL BEMBU1('READ',IREFB1(IPRIM1),NVERTEX,XVERT,YVERT,ZVERT,
     -      XNORM,YNORM,ZNORM,
     -      IVOLR2,IVOLR1,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! BEMSBC WARNING : Unable to retrieve the',
     -           ' reference primitive; no list returned.'
            RETURN
       ENDIF
*** Loop over the panels
       DO 10 I=1,NBEM
*   Retrieve
       CALL BEMBU1('READ',IREFB1(I),NVERTEX,XVERT,YVERT,ZVERT,
     -      XNORM,YNORM,ZNORM,
     -      IVOL2,IVOL1,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! BEMSBC WARNING : Did not find primitive ',
     -           I,'; list may be incomplete.'
            GOTO 10
*   If either of the volume numbers matches ...
       ELSEIF((IVOLR1.GT.0.AND.IVOLR1.EQ.IVOL1).OR.
     -        (IVOLR2.GT.0.AND.IVOLR2.EQ.IVOL2).OR.
     -        (IVOLR1.GT.0.AND.IVOLR1.EQ.IVOL2).OR.
     -        (IVOLR2.GT.0.AND.IVOLR2.EQ.IVOL1))THEN
            NPRIM=NPRIM+1
            PRLIST(NPRIM)=I
       ENDIF
10     CONTINUE
       END
