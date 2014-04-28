CDECK  ID>, PLAEXE.
       SUBROUTINE PLAEXE(IVOL,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX)
*-----------------------------------------------------------------------
*   PLAEXE - Computes an enveloping box of an extrusion.
*   (Last changed on  9/10/11.)
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
       INTEGER IREF,IVOL,I,N
       DOUBLE PRECISION ZL,X0,Y0,Z0,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,
     -      XEMIN,YEMIN,XEMAX,YEMAX
*** Locate the conductor.
       IF(IVOL.LT.1.OR.IVOL.GT.MXSOLI)THEN
            PRINT *,' !!!!!! PLAEXI WARNING : Volume reference is out'//
     -           ' of range ; not checked.'
            RETURN
       ENDIF
       IREF=ISTART(IVOL)
       IF(IREF.LT.0.OR.IREF+9.GT.MXSBUF)THEN
            PRINT *,' !!!!!! PLAEXI WARNING : Volume address is out'//
     -           ' of range ; not checked.'
            RETURN
       ENDIF
*** Locate the extrusion parameters, first the half length in z.
       ZL=ABS(CBUF(IREF+2))
*   Centre.
       X0=CBUF(IREF+3)
       Y0=CBUF(IREF+4)
       Z0=CBUF(IREF+5)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLAEXE DEBUG   :'',
     -      '' Extrusion volume '',I4/26X,
     -      '', Half-length='',E10.3/26X,''Centre=   '',3E10.3)')
     -      IVOL,ZL,X0,Y0,Z0
*   Number of points
       N = NINT(CBUF(IREF+9))
       IF(N.LT.3.OR.N.GT.MXEDGE.OR.IREF+23+2*N.GT.MXSBUF)THEN
            PRINT *,' !!!!!! PLAEXI WARNING : Volume address is out'//
     -           ' of range or N is not valid ; not checked.'
            RETURN
       ENDIF
*** Find range of the curve
       XEMIN=CBUF(IREF+24)
       XEMAX=CBUF(IREF+24)
       YEMIN=CBUF(IREF+25)
       YEMAX=CBUF(IREF+25)
       DO 10 I=2,N
       IF(XEMIN.GT.CBUF(IREF+23+2*I-1))XEMIN=CBUF(IREF+23+2*I-1)
       IF(XEMAX.LT.CBUF(IREF+23+2*I-1))XEMAX=CBUF(IREF+23+2*I-1)
       IF(YEMIN.GT.CBUF(IREF+23+2*I))  YEMIN=CBUF(IREF+23+2*I)
       IF(YEMAX.LT.CBUF(IREF+23+2*I))  YEMAX=CBUF(IREF+23+2*I)
 10    CONTINUE
*** Take the margins wide.
       XMIN=X0-SQRT(MAX(XEMIN,YEMIN,XEMAX,YEMAX)**2+ZL**2)
       YMIN=Y0-SQRT(MAX(XEMIN,YEMIN,XEMAX,YEMAX)**2+ZL**2)
       ZMIN=Z0-SQRT(MAX(XEMIN,YEMIN,XEMAX,YEMAX)**2+ZL**2)
       XMAX=X0+SQRT(MAX(XEMIN,YEMIN,XEMAX,YEMAX)**2+ZL**2)
       YMAX=Y0+SQRT(MAX(XEMIN,YEMIN,XEMAX,YEMAX)**2+ZL**2)
       ZMAX=Z0+SQRT(MAX(XEMIN,YEMIN,XEMAX,YEMAX)**2+ZL**2)
       END
