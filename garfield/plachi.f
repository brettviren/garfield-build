CDECK  ID>, PLACHI.
       SUBROUTINE PLACHI(IVOL,XPOS,YPOS,ZPOS,INSIDE)
*-----------------------------------------------------------------------
*   PLACHI - Determines whether a point is located inside a box.
*   (Last changed on  8/ 1/08.)
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
       INTEGER IVOL,IREF
       DOUBLE PRECISION XL,YL,ZL,X0,Y0,Z0,R1,R2,CT,ST,CP,SP,
     -      XPOS,YPOS,ZPOS,U,V,W
       LOGICAL INSIDE
*** Locate the conductor.
       IF(IVOL.LT.1.OR.IVOL.GT.MXSOLI)THEN
            PRINT *,' !!!!!! PLACHI WARNING : Volume reference is out'//
     -           ' of range ; not checked.'
            RETURN
       ENDIF
       IREF=ISTART(IVOL)
       IF(IREF.LT.0.OR.IREF+16.GT.MXSBUF)THEN
            PRINT *,' !!!!!! PLACHI WARNING : Volume address is out'//
     -           ' of range ; not checked.'
            RETURN
       ENDIF
*** Locate the parameters of the surrounding box and of the cylinder.
       R1= CBUF(IREF+1)
       R2= CBUF(IREF+2)
       IF(R1.LE.0.OR.R2.LE.0)THEN
            PRINT *,' !!!!!! PLACHI WARNING : Cylindrical hole ',IVOL,
     -           ' has a non-positive radius; not checked.'
            RETURN
       ENDIF
       XL=ABS(CBUF(IREF+3))
       YL=ABS(CBUF(IREF+4))
       ZL=ABS(CBUF(IREF+5))
       IF(R1.GE.XL.OR.R1.GE.YL.OR.R2.GE.XL.OR.R2.GE.YL.OR.ZL.LE.0)THEN
            PRINT *,' !!!!!! PLACHI WARNING : Radius of cylindrical',
     -           ' hole ',IVOL,' not smaller than the box; not checked.'
            RETURN
       ENDIF
       X0=CBUF(IREF+6)
       Y0=CBUF(IREF+7)
       Z0=CBUF(IREF+8)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLACHI DEBUG   : Checking'',
     -      '' hole from address '',I4/26X,''Centre=      '',3E10.3/
     -      26X,''Half-lengths='',3E10.3/26X,''Radii=       '',2E10.3)')
     -      IREF,X0,Y0,Z0,XL,YL,ZL,R1,R2
*   Shorthand for the rotations.
       CT=CBUF(IREF+13)
       ST=CBUF(IREF+14)
       CP=CBUF(IREF+15)
       SP=CBUF(IREF+16)
*** Transform the point to local coordinates.
       U=+CP*CT*(XPOS-X0)+SP*CT*(YPOS-Y0)-ST*(ZPOS-Z0)
       V=-SP   *(XPOS-X0)+CP*   (YPOS-Y0)
       W=+CP*ST*(XPOS-X0)+SP*ST*(YPOS-Y0)+CT*(ZPOS-Z0)
*** See whether the point is inside.
       IF(ABS(U).GT.XL.OR.ABS(V).GT.YL.OR.ABS(W).GT.ZL.OR.
     -      U**2+V**2.LT.(R1+(W+ZL)*(R2-R1)/(2*ZL))**2)THEN
            INSIDE=.FALSE.
       ELSE
            INSIDE=.TRUE.
       ENDIF
       END
