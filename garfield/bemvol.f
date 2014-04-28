CDECK  ID>, BEMVOL.
       SUBROUTINE BEMVOL(ISOL,ISHAPE,IMAT,EPS,VOLT,CHARGE,IBOUND,IFAIL)
*-----------------------------------------------------------------------
*   BEMVOL - Returns information about a solid.
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
       INTEGER ISOL,ISHAPE,IMAT,IBOUND,IFAIL
       DOUBLE PRECISION EPS,VOLT,CHARGE
*** Solid 0 is the gas.
       IF(ISOL.EQ.0)THEN
            ISHAPE=0
            IMAT=11
            VOLT=0
            CHARGE=0
            EPS=1.0
            IBOUND=0
            IFAIL=0
            RETURN
*** Check solid reference
       ELSEIF(ISOL.LT.1.OR.ISOL.GT.NSOLID)THEN
            PRINT *,' !!!!!! BEMVOL WARNING : Solid reference ',ISOL,
     -           ' out of range; no data returned.'
            IFAIL=1
            RETURN
       ENDIF
*** Return the data: cylinder.
       IF(ISOLTP(ISOL).EQ.1)THEN
            ISHAPE=ISOLTP(ISOL)
            IMAT=ISOLMT(ISOL)
            VOLT=CBUF(ISTART(ISOL)+15)
            EPS=CBUF(ISTART(ISOL)+16)
            IBOUND=NINT(CBUF(ISTART(ISOL)+17))
            CHARGE=CBUF(ISTART(ISOL)+18)
*   Hole.
       ELSEIF(ISOLTP(ISOL).EQ.2)THEN
            ISHAPE=ISOLTP(ISOL)
            IMAT=ISOLMT(ISOL)
            VOLT=CBUF(ISTART(ISOL)+17)
            EPS=CBUF(ISTART(ISOL)+18)
            IBOUND=NINT(CBUF(ISTART(ISOL)+19))
            CHARGE=CBUF(ISTART(ISOL)+20)
*   Box.
       ELSEIF(ISOLTP(ISOL).EQ.3)THEN
            ISHAPE=ISOLTP(ISOL)
            IMAT=ISOLMT(ISOL)
            VOLT=CBUF(ISTART(ISOL)+14)
            EPS=CBUF(ISTART(ISOL)+15)
            IBOUND=NINT(CBUF(ISTART(ISOL)+16))
            CHARGE=CBUF(ISTART(ISOL)+17)
*   Sphere.
       ELSEIF(ISOLTP(ISOL).EQ.4)THEN
            ISHAPE=ISOLTP(ISOL)
            IMAT=ISOLMT(ISOL)
            VOLT=CBUF(ISTART(ISOL)+6)
            EPS=CBUF(ISTART(ISOL)+7)
            IBOUND=NINT(CBUF(ISTART(ISOL)+8))
            CHARGE=CBUF(ISTART(ISOL)+9)
*   Toblerone.
       ELSEIF(ISOLTP(ISOL).EQ.5)THEN
            ISHAPE=ISOLTP(ISOL)
            IMAT=ISOLMT(ISOL)
            VOLT=CBUF(ISTART(ISOL)+15)
            EPS=CBUF(ISTART(ISOL)+16)
            IBOUND=NINT(CBUF(ISTART(ISOL)+17))
            CHARGE=CBUF(ISTART(ISOL)+18)
*   Extrusion.
       ELSEIF(ISOLTP(ISOL).EQ.6)THEN
            ISHAPE=ISOLTP(ISOL)
            IMAT=ISOLMT(ISOL)
            VOLT=CBUF(ISTART(ISOL)+15)
            EPS=CBUF(ISTART(ISOL)+16)
            IBOUND=NINT(CBUF(ISTART(ISOL)+17))
            CHARGE=CBUF(ISTART(ISOL)+18)
*   Anything else.
       ELSE
            PRINT *,' !!!!!! BEMVOL WARNING : Unknown solid shape.'
            IFAIL=1
            RETURN
       ENDIF
*** Seems to have worked.
       IFAIL=0
       END
