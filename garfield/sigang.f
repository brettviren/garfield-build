CDECK  ID>, SIGANG.
       SUBROUTINE SIGANG(ISOLID,ANGLE,XREF,YREF,ZREF,
     -      XSTART,YSTART,ZSTART)
*-----------------------------------------------------------------------
*   SIGANG - Returns a starting point at angle ANGLE on the surface of
*            volume ISOLID.
*   VARIABLES :
*   (Last changed on 20/ 5/99.)
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
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       DOUBLE PRECISION XREF,YREF,ZREF,R,X0,Y0,Z0,CT,ST,CP,SP,U,V,W
       REAL ANGLE,XSTART,YSTART,ZSTART
       INTEGER ISOLID,IREF
*** Initial points.
       XSTART=REAL(XREF)
       YSTART=REAL(YREF)
       ZSTART=REAL(ZREF)
*** See whether we got a valid solid.
       IF(ISOLID.LT.1.OR.ISOLID.GT.NSOLID)THEN
            PRINT *,' !!!!!! SIGANG WARNING : Invalid solid reference'//
     -           ' received; returning reference point.'
            RETURN
*** If this is not a cylinder, simply return.
       ELSEIF(ISOLTP(ISOLID).NE.1)THEN
            RETURN
*** If a cylinder, process.
       ELSE
*   Starting point in buffer.
            IREF=ISTART(ISOLID)
            IF(IREF.LT.0.OR.IREF+8.GT.MXSBUF)THEN
                 PRINT *,' !!!!!! SIGANG WARNING : Solid address is'//
     -                ' out of range ; returning reference.'
                 RETURN
            ENDIF
*   Extract parameters.
            R =CBUF(IREF+1)
            X0=CBUF(IREF+3)
            Y0=CBUF(IREF+4)
            Z0=CBUF(IREF+5)
            CT=CBUF(IREF+10)
            ST=CBUF(IREF+11)
            CP=CBUF(IREF+12)
            SP=CBUF(IREF+13)
*   Compute thw local W coordinate of reference point.
            W=+CP*ST*(XREF-X0)+SP*ST*(YREF-Y0)+CT*(ZREF-Z0)
*   Compute the U and V coordinates.
            U=R*COS(ANGLE)
            V=R*SIN(ANGLE)
*   Transform to space coordinates.
            XSTART=REAL(X0+CP*CT*U-SP*V+CP*ST*W)
            YSTART=REAL(Y0+SP*CT*U+CP*V+SP*ST*W)
            ZSTART=REAL(Z0   -ST*U        +CT*W)
       ENDIF
       END
