CDECK  ID>, BEMSZD.
       SUBROUTINE BEMSZD(IPRIM,DIS,IFAIL)
*-----------------------------------------------------------------------
*   BEMSZD - Determines the discretisation size for a panel.
*            Returns discretisation size in cm, scaled in neBEM
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
       INTEGER NBEM,IREFB1(MXPLAN),NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,
     -      BEMNEW,BEMINV,BEMSLV
       DOUBLE PRECISION BEMQTH,BEMSTH,BEMSSC,BEMTGT,BEMEPA,BEMEPD
       LOGICAL LBDUMP
       COMMON /BEMDAT/ BEMQTH,BEMSSC,BEMSTH,BEMTGT,BEMEPA,BEMEPD,
     -      IREFB1,NBEM,NBEMMN,NBEMMX,NBEMPX,NBEMPY,NBEMPZ,BEMNEW,
     -      BEMINV,BEMSLV,LBDUMP
       INTEGER IPRIM, NVTX, ISOL1, ISOL2, IFAIL, IFAIL1, IWIRE, IVOL
       DOUBLE PRECISION XVTX(MXEDGE), YVTX(MXEDGE), ZVTX(MXEDGE),
     -      XNORM, YNORM, ZNORM, DIS, DIS1, DIS2
*** Default response.
       DIS=0.1
       IFAIL=1
*** See whether this is a wire.
       IF(IPRIM.GT.NBEM)THEN
            IWIRE=0
            DO 10 IVOL=1,NSOLID
            IF(ISOLTP(IVOL).NE.1)GOTO 10
            IF(CBUF(ISTART(IVOL)+9).LT.-0.5)IWIRE=IWIRE+1
            IF(IWIRE.EQ.IPRIM-NBEM)THEN
                 DIS=CBUF(ISTART(IVOL)+21)
                 IFAIL=0
                 RETURN
            ENDIF
10          CONTINUE
            PRINT *,' !!!!!! BEMSZD WARNING : Did not find the'//
     -           ' cylinder corresponding to primitive ',IPRIM
            IFAIL=1
            RETURN
       ENDIF
*** Retrieve the panel to determine the solid and norms.
       CALL BEMBU1('READ',IREFB1(IPRIM),NVTX,XVTX,YVTX,ZVTX,
     -      XNORM,YNORM,ZNORM,
     -      ISOL2,ISOL1,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! BEMSZD WARNING : Unable to retrieve'//
     -           ' primitive ',IPRIM
            IFAIL=1
            RETURN
       ENDIF
*** Work out which type solid 1 is.
       IF(ISOL1.GT.NSOLID)THEN
            PRINT *,' !!!!!! BEMSZD WARNING : Solid 1 reference ',ISOL1,
     -           ' out of range; no data returned.'
            IFAIL=1
            RETURN
*   Does not exist.
       ELSEIF(ISOL1.LT.1)THEN
            DIS1=-1
            GOTO 1000
*   Cylinder.
       ELSEIF(ISOLTP(ISOL1).EQ.1)THEN
            CALL PLACYD(ISOL1,NVTX,XVTX,YVTX,ZVTX,
     -           XNORM,YNORM,ZNORM,DIS1)
*   Hole.
       ELSEIF(ISOLTP(ISOL1).EQ.2)THEN
            CALL PLACHD(ISOL1,NVTX,XVTX,YVTX,ZVTX,
     -           XNORM,YNORM,ZNORM,DIS1)
*   Box.
       ELSEIF(ISOLTP(ISOL1).EQ.3)THEN
            CALL PLABXD(ISOL1,NVTX,XVTX,YVTX,ZVTX,
     -           XNORM,YNORM,ZNORM,DIS1)
*   Sphere.
       ELSEIF(ISOLTP(ISOL1).EQ.4)THEN
            CALL PLASPD(ISOL1,NVTX,XVTX,YVTX,ZVTX,
     -           XNORM,YNORM,ZNORM,DIS1)
*   Toblerone.
       ELSEIF(ISOLTP(ISOL1).EQ.5)THEN
            CALL PLATBD(ISOL1,NVTX,XVTX,YVTX,ZVTX,
     -           XNORM,YNORM,ZNORM,DIS1)
*   Extrusion.
       ELSEIF(ISOLTP(ISOL1).EQ.6)THEN
            CALL PLAEXD(ISOL1,NVTX,XVTX,YVTX,ZVTX,
     -           XNORM,YNORM,ZNORM,DIS1)
*   Anything else.
       ELSE
            PRINT *,' !!!!!! BEMSZD WARNING : Unknown solid 1 shape.'
            IFAIL=1
            RETURN
       ENDIF
C      print *,' Solid 1: Primitive: ',iprim,' nvtx = ',nvtx,
C     -      ' solids: ',isol1, isol2,' Discretisation size: ',dis1
*** Solid 2.
1000   CONTINUE
       IF(ISOL2.GT.NSOLID)THEN
            PRINT *,' !!!!!! BEMSZD WARNING : Solid 2 reference ',ISOL2,
     -           ' out of range; no data returned.'
            IFAIL=1
            RETURN
*   Does not exist.
       ELSEIF(ISOL2.LT.1)THEN
            DIS2=-1
            GOTO 2000
*   Cylinder.
       ELSEIF(ISOLTP(ISOL2).EQ.1)THEN
            CALL PLACYD(ISOL2,NVTX,XVTX,YVTX,ZVTX,
     -           -XNORM,-YNORM,-ZNORM,DIS2)
*   Hole.
       ELSEIF(ISOLTP(ISOL2).EQ.2)THEN
            CALL PLACHD(ISOL2,NVTX,XVTX,YVTX,ZVTX,
     -           -XNORM,-YNORM,-ZNORM,DIS2)
*   Box.
       ELSEIF(ISOLTP(ISOL2).EQ.3)THEN
            CALL PLABXD(ISOL2,NVTX,XVTX,YVTX,ZVTX,
     -           -XNORM,-YNORM,-ZNORM,DIS2)
*   Sphere.
       ELSEIF(ISOLTP(ISOL2).EQ.4)THEN
            CALL PLASPD(ISOL2,NVTX,XVTX,YVTX,ZVTX,
     -           -XNORM,-YNORM,-ZNORM,DIS2)
*   Toblerone.
       ELSEIF(ISOLTP(ISOL2).EQ.5)THEN
            CALL PLATBD(ISOL2,NVTX,XVTX,YVTX,ZVTX,
     -           -XNORM,-YNORM,-ZNORM,DIS2)
*   Extrusion.
       ELSEIF(ISOLTP(ISOL2).EQ.6)THEN
            CALL PLAEXD(ISOL2,NVTX,XVTX,YVTX,ZVTX,
     -           -XNORM,-YNORM,-ZNORM,DIS2)
*   Anything else.
       ELSE
            PRINT *,' !!!!!! BEMSZD WARNING : Unknown solid 2 shape.'
            IFAIL=1
            RETURN
       ENDIF
C      print *,' Solid 2: Primitive: ',iprim,' nvtx = ',nvtx,
C     -      ' solids: ',isol1, isol2,' Discretisation size: ',dis2
*** Combine.
2000   CONTINUE
       IF(DIS1.LT.0.AND.DIS2.LT.0)THEN
            DIS=-1
       ELSEIF(DIS1.LT.0)THEN
            DIS=DIS2
       ELSEIF(DIS2.LT.0)THEN
            DIS=DIS1
       ELSE
            DIS=MIN(DIS1,DIS2)
       ENDIF
C      print *,' Assigned discretisation: ',dis
*** Seems to have worked.
       IFAIL=0
       END
