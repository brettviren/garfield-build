CDECK  ID>, PLACHD.
       SUBROUTINE PLACHD(ISOL,NVTX,XVTX,YVTX,ZVTX,XNORM,YNORM,ZNORM,DIS)
*-----------------------------------------------------------------------
*   PLACHD - Returns the discretisation level required for a primitive.
*   (Last changed on  6/ 7/10.)
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
       INTEGER ISOL,NVTX,IREF
       DOUBLE PRECISION XVTX(*), YVTX(*), ZVTX(*), XNORM, YNORM, ZNORM,
     -      CT,ST,CP,SP, UN,VN,WN, U1,V1,W1, DIS
*** Locate the conductor.
       IF(ISOL.LT.1.OR.ISOL.GT.MXSOLI)THEN
            PRINT *,' !!!!!! PLACHD WARNING : Solid reference is out'//
     -           ' of range ; not checked.'
            RETURN
       ENDIF
       IREF=ISTART(ISOL)
       IF(IREF.LT.0.OR.IREF+14.GT.MXSBUF)THEN
            PRINT *,' !!!!!! PLACHD WARNING : Solid address is out'//
     -           ' of range ; not checked.'
            RETURN
       ENDIF
*** Locate the cylinder rotation parameters.
       CT=CBUF(IREF+13)
       ST=CBUF(IREF+14)
       CP=CBUF(IREF+15)
       SP=CBUF(IREF+16)
*** Transform the normal vector to local coordinates.
       UN=+CP*CT*XNORM+SP*CT*YNORM-ST*ZNORM
       VN=-SP   *XNORM+CP*   YNORM
       WN=+CP*ST*XNORM+SP*ST*YNORM+CT*ZNORM
C      print *,' Axis vector: ',un,vn,wn
*** Transform one of the points (first).
       U1=+CP*CT*(XVTX(1)-CBUF(IREF+6))+SP*CT*(YVTX(1)-CBUF(IREF+7))-
     -      ST*(ZVTX(1)-CBUF(IREF+8))
       V1=-SP   *(XVTX(1)-CBUF(IREF+6))+CP*   (YVTX(1)-CBUF(IREF+7))
       W1=+CP*ST*(XVTX(1)-CBUF(IREF+6))+SP*ST*(YVTX(1)-CBUF(IREF+7))+
     -      CT*(ZVTX(1)-CBUF(IREF+8))
C      print *,' Point 1 ',u1,v1,w1
*** Identify the vector.
       IF(WN.GT.MAX(ABS(UN),ABS(VN)))THEN
            DIS=CBUF(IREF+25)
       ELSEIF(WN.LT.-MAX(ABS(UN),ABS(VN)))THEN
            DIS=CBUF(IREF+26)
       ELSEIF(UN*U1+VN*V1+WN*W1.LT.0)THEN
            DIS=CBUF(IREF+27)
       ELSEIF(UN.GT.MAX(ABS(VN),ABS(WN)))THEN
            DIS=CBUF(IREF+21)
       ELSEIF(UN.LT.-MAX(ABS(VN),ABS(WN)))THEN
            DIS=CBUF(IREF+22)
       ELSEIF(VN.GT.MAX(ABS(UN),ABS(WN)))THEN
            DIS=CBUF(IREF+23)
       ELSEIF(VN.LT.-MAX(ABS(UN),ABS(WN)))THEN
            DIS=CBUF(IREF+24)
       ELSE
            PRINT *,' !!!!!! PLACHD WARNING : Found no match for the'//
     -           ' hole panel; returning 1st value.'
            DIS=CBUF(IREF+21)
       ENDIF
*   Debugging.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLACHD DEBUG   :'',
     -      '' Discretisation of solid '',I5,'' (hole)''/
     -      '' Norm vector:    '',3F10.3/
     -      '' First points:   '',3F10.3/
     -      '' Discretisation: '',E12.5)')
     -      ISOL,UN,VN,WN,U1,V1,W1,DIS
       END
