CDECK  ID>, PLAPLT.
       SUBROUTINE PLAPLT
*-----------------------------------------------------------------------
*   PLAPLT - Plots the current set of planes.
*   (Last changed on 11/10/11.)
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
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       DOUBLE PRECISION XPL(MXEDGE+1),YPL(MXEDGE+1),ZPL(MXEDGE+1),
     -      APL,BPL,CPL,DPL
       INTEGER I,J,IVOL,ICOL,IFAIL,NPL,NWORD,INPCMP,NCSTR
       CHARACTER*20 STR
       EXTERNAL INPCMP
*** Identification.
       IF(LIDENT)PRINT *,' /// ROUTINE PLAPLT ///'
*** Open a segment so that we can later on pick out the wires.
C       CALL GCRSG(1)
*** Make the solids detectable.
C       CALL GSDTEC(1,1)
*** Plot the panels, prepare for requesting input if needed.
       IF(LGSTEP)THEN
            WRITE(LUNOUT,'(''  Showing the '',I4,'' panels one at'',
     -           '' the time, hit return or SHOW to proceed.'')') NQ
            CALL INPSWI('TERMINAL')
       ENDIF
*   Loop over the panels.
       DO 10 I=1,NQ
*   Read the panel.
       CALL PLABU2('READ',IQ(I),NPL,XPL,YPL,ZPL,APL,BPL,CPL,DPL,
     -      ICOL,IFAIL)
       IF(IFAIL.NE.0.OR.NPL.LE.2)GOTO 10
*   Set a pick identifier for each solid separately.
C       CALL GSPKID(IVOL)
*   Set the representations.
       IF(ICOL.GE.50.AND.ICOL.LT.50+NPRCOL)THEN
            CALL GRATTS('CONDUCTORS-1','AREA')
       ELSE
            CALL GRATTS('DIELECTRIC-1','AREA')
       ENDIF
*   Set the colour.
       CALL GSFACI(ICOL)
       CALL GSPLCI(ICOL)
*   Add the last point to make a complete loop.
       NPL=NPL+1
       XPL(NPL)=XPL(1)
       YPL(NPL)=YPL(1)
       ZPL(NPL)=ZPL(1)
*   Plot the area.
       CALL GFA2(NPL,XPL,YPL)
       CALL GPL2(NPL,XPL,YPL)
*   Debugging.
       IF(LGSTEP)THEN
            CALL GUWK(1,0)
            CALL OUTFMT(REAL(I),2,STR,NCSTR,'LEFT')
            CALL INPPRM('Panel '//STR(1:NCSTR),'ADD-NOPRINT')
            CALL INPWRD(NWORD)
            CALL INPPRM(' ','BACK-PRINT')
            IF(NWORD.EQ.1.AND.INPCMP(1,'S#HOW')+
     -           INPCMP(1,'Y#ES').NE.0)THEN
                 WRITE(LUNOUT,'(''  Panel '',I3,'': reference='',I4,
     -                '', colour='',I3,'', edges='',I3//
     -                 11X,''x'',13X,''y'',13X,''z'')') I,IQ(I),ICOL,NPL
                 DO 20 J=1,NPL
                 WRITE(LUNOUT,'(3(2X,F12.5))') XPL(J),YPL(J),ZPL(J)
20               CONTINUE
            ELSEIF(NWORD.NE.0)THEN
                 PRINT *,' !!!!!! PLAPLT WARNING : Unknown response ;'//
     -                ' not showing details.'
            ENDIF
       ENDIF
10     CONTINUE
*   Restore input.
       IF(LGSTEP)CALL INPSWI('RESTORE')
*** Close the segment for the solids.
C       CALL GCLSG
*** Optionally also plot the outline.
       IF(LOUTL)THEN
*   Set the representation.
            CALL GRATTS('OUTLINE','POLYLINE')
            DO 1010 IVOL=1,NSOLID
*   cylinders ...
            IF(ISOLTP(IVOL).EQ.1)THEN
C                 CALL PLACYO(IVOL)
*   cylindrical holes ...
             ELSEIF(ISOLTP(IVOL).EQ.2)THEN
                 CALL PLACHO(IVOL)
*   boxes ...
            ELSEIF(ISOLTP(IVOL).EQ.3)THEN
                 CALL PLABXO(IVOL)
*   spheres ...
            ELSEIF(ISOLTP(IVOL).EQ.4)THEN
C                 CALL PLASPO(IVOL)
*   Toblerone ...
            ELSEIF(ISOLTP(IVOL).EQ.5)THEN
                 CALL PLATBO(IVOL)
*   Extrusion ...
            ELSEIF(ISOLTP(IVOL).EQ.6)THEN
C                 CALL PLAEXO(IVOL)
*   other things not known.
            ELSE
                 PRINT *,' !!!!!! PLAPLT WARNING : Asked to plot an'//
     -                ' outline of unknown type ',ISOLTP(IVOL),
     -                '; not plotted.'
            ENDIF
1010        CONTINUE
       ENDIF
       END
