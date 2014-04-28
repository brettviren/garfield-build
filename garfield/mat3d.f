CDECK  ID>, MAT3D.
       SUBROUTINE MAT3D(IREFM,IREFX,IREFY,XTXT,YTXT,ZTXT,TITLE,
     -      PHI,THETA,OPTION)
*-----------------------------------------------------------------------
*   MAT3D  - Plots a surface for a matrix.
*   (Last changed on 13/11/02.)
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
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
       REAL USERX0,USERX1,USERY0,USERY1,FRXMIN,FRXMAX,FRYMIN,FRYMAX,
     -      ARRANG,ARRLEN,BARFRC,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT
       LOGICAL LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,
     -      LWAITA,LWAITB,LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      WKMULT(MXWKLS)
       INTEGER NWK,WKID(MXWKLS),WKCON(MXWKLS),WKFREF(MXWKLS),
     -         WKLUN(MXWKLS),WKSTAT(MXWKLS),WKSREQ(MXWKLS),
     -         NCWKNM(MXWKLS),NCSTMP,IGHIST,IGBAR,NCGKS
       CHARACTER*20 WKNAME(MXWKLS),WKATTR(MXWKLS)
       CHARACTER*80 STAMP
       CHARACTER*(MXNAME) GKSLOG
       COMMON /GRADAT/ USERX0,USERX1,USERY0,USERY1,ARRANG,ARRLEN,
     -      BARFRC,
     -      FRXMIN,FRXMAX,FRYMIN,FRYMAX,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT,
     -      LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,LWAITA,LWAITB,
     -      LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      NWK,WKID,WKCON,WKFREF,WKLUN,WKSTAT,WKSREQ,NCWKNM,NCSTMP,
     -      IGHIST,IGBAR,NCGKS,WKMULT
       COMMON /GRACHR/ WKNAME,WKATTR,STAMP,GKSLOG
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       INTEGER MATSLT,MATADR,NX,NY,MXWS,IFAIL,IX,IY,IA(2),
     -      ISLOTX,ISLOTY,ISLOTM,IREFX,IREFY,IREFM,ISIZ(1)
       REAL PHI,THETA
       CHARACTER*(*) XTXT,YTXT,ZTXT,TITLE,OPTION
       EXTERNAL MATSLT,MATADR
       REAL WS,DUMMY
       PARAMETER(MXWS=MXWIRE)
       COMMON /MATRIX/ WS(MXWS,MXWS),DUMMY(MXWS*MXWS+8*MXWS+6)
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MAT3D ///'
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAT3D  DEBUG   : Plotting'',
     -      '' matrix '',I5,'' axes '',2I5/
     -      26X,''Viewing angles: '',2F10.2,'' degrees''/
     -      26X,''x-Axis label: '',A/26X,''y-Axis label: '',A/
     -      26X,''z-Axis label: '',A/26X,''Title: '',A/
     -      26X,''Options:      '',A/)')
     -      IREFM,IREFX,IREFY,PHI,THETA,XTXT,YTXT,ZTXT,TITLE,OPTION
*** Locate the matrix.
       ISLOTM=MATSLT(IREFM)
       IF(ISLOTM.LE.0)THEN
            PRINT *,' !!!!!! MAT3D  WARNING : Matrix to be plotted'//
     -           ' does not exist; not plotted.'
            RETURN
       ENDIF
       NX=MSIZ(ISLOTM,1)
       NY=MSIZ(ISLOTM,2)
*** Make sure that this matrix has the right dimensions.
       IF(MDIM(ISLOTM).NE.2.OR.
     -      NX.LT.2.OR.NY.LT.2.OR.
     -      NX.GT.MXWS.OR.NY.GT.MXWS)THEN
            PRINT *,' !!!!!! MAT3D  WARNING : The matrix to be'//
     -           ' plotted doesn''t have the right dimensions.'
            RETURN
       ENDIF
*** See whether the coordinates are present.
       ISLOTX=MATSLT(IREFX)
       IF(ISLOTX.GT.0)THEN
            IF(MDIM(ISLOTX).NE.1.OR.MSIZ(ISLOTX,1).NE.NX)THEN
                 PRINT *,' !!!!!! MAT3D  WARNING : x-Coordinate'//
     -                ' vector does not have the right format;'//
     -                ' argument ignored.'
                 ISLOTX=-1
            ENDIF
       ENDIF
*   Create if not existent.
       IF(ISLOTX.LE.0)THEN
            PRINT *,' ------ MAT3D  MESSAGE : x-Range of plot not'//
     -           ' given; set to [1,n].'
            ISIZ(1)=NX
            CALL MATADM('ALLOCATE',IREFX,1,ISIZ,2,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! MAT3D  WARNING : Failed to obtain'//
     -                ' space for the x-coordinate vector; no plot.'
                 RETURN
            ENDIF
       ENDIF
*   Same check for y.
       ISLOTY=MATSLT(IREFY)
       IF(ISLOTY.GT.0)THEN
            IF(MDIM(ISLOTY).NE.1.OR.MSIZ(ISLOTY,1).NE.NY)THEN
                 PRINT *,' !!!!!! MAT3D  WARNING : y-Coordinate'//
     -                ' vector does not have the right format;'//
     -                ' argument ignored.'
                 ISLOTY=-1
            ENDIF
       ENDIF
*   Create if not existent.
       IF(ISLOTY.LE.0)THEN
            PRINT *,' ------ MAT3D  MESSAGE : y-Range of plot not'//
     -           ' given; set to [1,n].'
            ISIZ(1)=NY
            CALL MATADM('ALLOCATE',IREFY,1,ISIZ,2,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! MAT3D  WARNING : Failed to obtain'//
     -                ' space for the x-coordinate vector; no plot.'
                 RETURN
            ENDIF
       ENDIF
*** Recompute all matrix slots.
       ISLOTM=MATSLT(IREFM)
       IF(ISLOTM.LE.0)THEN
            PRINT *,' !!!!!! MAT3D  WARNING : Matrix to be plotted'//
     -           ' does not exist; not plotted.'
            RETURN
       ENDIF
       ISLOTX=MATSLT(IREFX)
       IF(ISLOTX.LE.0)THEN
            PRINT *,' !!!!!! MAT3D  WARNING : x-Coordinate'//
     -           ' vector can not be allocated; no plot.'
            RETURN
       ENDIF
       ISLOTY=MATSLT(IREFY)
       IF(ISLOTY.LE.0)THEN
            PRINT *,' !!!!!! MAT3D  WARNING : y-Coordinate'//
     -           ' vector can not be allocated; no plot.'
            RETURN
       ENDIF
*** Obtain the matrix for surface plotting.
       CALL BOOK('BOOK','MATRIX','MAT3D',IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! MAT3D  WARNING : Unable to obtain'//
     -           ' storage for the surface plot; plot not made.'
            RETURN
       ENDIF
*** Transfer the matrix to the fixed arrays, establish range.
       DO 10 IY=1,NY
       DO 20 IX=1,NX
       IA(1)=IX
       IA(2)=IY
       WS(IX,IY)=MVEC(MATADR(ISLOTM,IA))
20     CONTINUE
10     CONTINUE
*** Make the plot.
       CALL PLASUR(WS,MXWS,MVEC(MORG(ISLOTX)+1),MVEC(MORG(ISLOTY)+1),
     -      NX,NY,PI*PHI/180,PI*THETA/180,XTXT,YTXT,ZTXT,TITLE,OPTION)
*   Record.
       CALL TIMLOG('Making a 3-dimensional plot:            ')
       CALL GRALOG('3-D plot of a matrix.')
*** Release the matrix.
       CALL BOOK('RELEASE','MATRIX','MAT3D',IFAIL)
*** Delete temporary matrices.
       END
