CDECK  ID>, PLABOX.
       SUBROUTINE PLABOX(XBOX,YBOX,ZBOX,NCUT,XCUT,YCUT,ZCUT,
     -      X0PL,Y0PL,Z0PL,APL,BPL,CPL)
*-----------------------------------------------------------------------
*   PLABOX - Crossings between a box and a plane.
*   (Last changed on  4/ 2/98.)
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
       INTEGER NCUT,IFAIL
       DOUBLE PRECISION XBOX(8),YBOX(8),ZBOX(8),
     -      XCUT(12),YCUT(12),ZCUT(12),
     -      X0PL,Y0PL,Z0PL,APL,BPL,CPL,XC,YC,ZC
*** Initial number of crossings.
       NCUT=0
*** Compute the, at most 6, crossings between plane and box.
       CALL PLALIN(XBOX(1),YBOX(1),ZBOX(1),XBOX(2),YBOX(2),ZBOX(2),
     -      X0PL,Y0PL,Z0PL,APL,BPL,CPL,XC,YC,ZC,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NCUT=NCUT+1
            XCUT(NCUT)=XC
            YCUT(NCUT)=YC
            ZCUT(NCUT)=ZC
       ENDIF
       CALL PLALIN(XBOX(2),YBOX(2),ZBOX(2),XBOX(3),YBOX(3),ZBOX(3),
     -      X0PL,Y0PL,Z0PL,APL,BPL,CPL,XC,YC,ZC,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NCUT=NCUT+1
            XCUT(NCUT)=XC
            YCUT(NCUT)=YC
            ZCUT(NCUT)=ZC
       ENDIF
       CALL PLALIN(XBOX(3),YBOX(3),ZBOX(3),XBOX(4),YBOX(4),ZBOX(4),
     -      X0PL,Y0PL,Z0PL,APL,BPL,CPL,XC,YC,ZC,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NCUT=NCUT+1
            XCUT(NCUT)=XC
            YCUT(NCUT)=YC
            ZCUT(NCUT)=ZC
       ENDIF
       CALL PLALIN(XBOX(4),YBOX(4),ZBOX(4),XBOX(1),YBOX(1),ZBOX(1),
     -      X0PL,Y0PL,Z0PL,APL,BPL,CPL,XC,YC,ZC,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NCUT=NCUT+1
            XCUT(NCUT)=XC
            YCUT(NCUT)=YC
            ZCUT(NCUT)=ZC
       ENDIF
       CALL PLALIN(XBOX(5),YBOX(5),ZBOX(5),XBOX(6),YBOX(6),ZBOX(6),
     -      X0PL,Y0PL,Z0PL,APL,BPL,CPL,XC,YC,ZC,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NCUT=NCUT+1
            XCUT(NCUT)=XC
            YCUT(NCUT)=YC
            ZCUT(NCUT)=ZC
       ENDIF
       CALL PLALIN(XBOX(6),YBOX(6),ZBOX(6),XBOX(7),YBOX(7),ZBOX(7),
     -      X0PL,Y0PL,Z0PL,APL,BPL,CPL,XC,YC,ZC,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NCUT=NCUT+1
            XCUT(NCUT)=XC
            YCUT(NCUT)=YC
            ZCUT(NCUT)=ZC
       ENDIF
       CALL PLALIN(XBOX(7),YBOX(7),ZBOX(7),XBOX(8),YBOX(8),ZBOX(8),
     -      X0PL,Y0PL,Z0PL,APL,BPL,CPL,XC,YC,ZC,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NCUT=NCUT+1
            XCUT(NCUT)=XC
            YCUT(NCUT)=YC
            ZCUT(NCUT)=ZC
       ENDIF
       CALL PLALIN(XBOX(8),YBOX(8),ZBOX(8),XBOX(5),YBOX(5),ZBOX(5),
     -      X0PL,Y0PL,Z0PL,APL,BPL,CPL,XC,YC,ZC,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NCUT=NCUT+1
            XCUT(NCUT)=XC
            YCUT(NCUT)=YC
            ZCUT(NCUT)=ZC
       ENDIF
       CALL PLALIN(XBOX(1),YBOX(1),ZBOX(1),XBOX(5),YBOX(5),ZBOX(5),
     -      X0PL,Y0PL,Z0PL,APL,BPL,CPL,XC,YC,ZC,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NCUT=NCUT+1
            XCUT(NCUT)=XC
            YCUT(NCUT)=YC
            ZCUT(NCUT)=ZC
       ENDIF
       CALL PLALIN(XBOX(2),YBOX(2),ZBOX(2),XBOX(6),YBOX(6),ZBOX(6),
     -      X0PL,Y0PL,Z0PL,APL,BPL,CPL,XC,YC,ZC,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NCUT=NCUT+1
            XCUT(NCUT)=XC
            YCUT(NCUT)=YC
            ZCUT(NCUT)=ZC
       ENDIF
       CALL PLALIN(XBOX(3),YBOX(3),ZBOX(3),XBOX(7),YBOX(7),ZBOX(7),
     -      X0PL,Y0PL,Z0PL,APL,BPL,CPL,XC,YC,ZC,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NCUT=NCUT+1
            XCUT(NCUT)=XC
            YCUT(NCUT)=YC
            ZCUT(NCUT)=ZC
       ENDIF
       CALL PLALIN(XBOX(4),YBOX(4),ZBOX(4),XBOX(8),YBOX(8),ZBOX(8),
     -      X0PL,Y0PL,Z0PL,APL,BPL,CPL,XC,YC,ZC,IFAIL)
       IF(IFAIL.EQ.0)THEN
            NCUT=NCUT+1
            XCUT(NCUT)=XC
            YCUT(NCUT)=YC
            ZCUT(NCUT)=ZC
       ENDIF
*** Eliminate the butterflies.
       CALL BUTFLD(NCUT,XCUT,YCUT,ZCUT)
       END
