CDECK  ID>, PLAPLA.
       SUBROUTINE PLAPLA(APL,BPL,CPL,DPL,VXMIN,VYMIN,VXMAX,VYMAX)
*-----------------------------------------------------------------------
*   PLAPLA - Cross section between a plane and another plane.
*   (Last changed on  8/11/98.)
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
       DOUBLE PRECISION APL,BPL,CPL,DPL,XPL(2),YPL(2),
     -      VXMIN,VXMAX,VYMIN,VYMAX,A,B,C,X1,Y1,X2,Y2
*** See whether the 2 planes are parallel.
       IF(ABS(APL*FPROJ(1,1)+BPL*FPROJ(1,2)+CPL*FPROJ(1,3)).LT.
     -      1D-6*SQRT(APL**2+BPL**2+CPL**2).AND.
     -      ABS(APL*FPROJ(2,1)+BPL*FPROJ(2,2)+CPL*FPROJ(2,3)).LT.
     -      1D-6*SQRT(APL**2+BPL**2+CPL**2))THEN
            RETURN
*** For non-parallel planes, establish crossing points.
       ELSE
*   Intersection equation parameters.
            A=FPROJ(1,1)*APL+FPROJ(1,2)*BPL+FPROJ(1,3)*CPL
            B=FPROJ(2,1)*APL+FPROJ(2,2)*BPL+FPROJ(2,3)*CPL
            C=DPL-FPROJ(3,1)*APL-FPROJ(3,2)*BPL-FPROJ(3,3)*CPL
*   Two points on the line.
            IF(A.EQ.0.AND.B.EQ.0)THEN
                 PRINT *,' !!!!!! PLAPLA WARNING : Unable to compute'//
     -                ' intersect between 2 lines; line not plotted.'
                 RETURN
            ELSEIF(ABS(A).GT.ABS(B))THEN
                 CALL PLACOO(
     -                FPROJ(3,1)+C*FPROJ(1,1)/A,
     -                FPROJ(3,2)+C*FPROJ(1,2)/A,
     -                FPROJ(3,3)+C*FPROJ(1,3)/A,
     -                X1,Y1)
                 CALL PLACOO(
     -                FPROJ(3,1)+FPROJ(2,1)+(C-B)*FPROJ(1,1)/A,
     -                FPROJ(3,2)+FPROJ(2,2)+(C-B)*FPROJ(1,2)/A,
     -                FPROJ(3,3)+FPROJ(2,3)+(C-B)*FPROJ(1,3)/A,
     -                X2,Y2)
            ELSE
                 CALL PLACOO(
     -                FPROJ(3,1)+C*FPROJ(2,1)/B,
     -                FPROJ(3,2)+C*FPROJ(2,2)/B,
     -                FPROJ(3,3)+C*FPROJ(2,3)/B,
     -                X1,Y1)
                 CALL PLACOO(
     -                FPROJ(3,1)+FPROJ(1,1)+(C-A)*FPROJ(2,1)/B,
     -                FPROJ(3,2)+FPROJ(1,2)+(C-A)*FPROJ(2,2)/B,
     -                FPROJ(3,3)+FPROJ(1,3)+(C-A)*FPROJ(2,3)/B,
     -                X2,Y2)
            ENDIF
*   Extend the line to the full area.
            IF(X1.EQ.X2.AND.Y1.EQ.Y2)THEN
                 PRINT *,' !!!!!! PLAPLA WARNING : Intersect line'//
     -                ' is point-like; line not plotted.'
                 RETURN
            ELSEIF(ABS(X1-X2).GT.ABS(Y1-Y2))THEN
                 XPL(1)=VXMIN
                 YPL(1)=Y1+(VXMIN-X1)*(Y2-Y1)/(X2-X1)
                 XPL(2)=VXMAX
                 YPL(2)=Y1+(VXMAX-X1)*(Y2-Y1)/(X2-X1)
            ELSE
                 XPL(1)=X1+(VYMIN-Y1)*(X2-X1)/(Y2-Y1)
                 YPL(1)=VYMIN
                 XPL(2)=X1+(VYMAX-Y1)*(X2-X1)/(Y2-Y1)
                 YPL(2)=VYMAX
            ENDIF
       ENDIF
*** Seems to have worked, plot the line.
       CALL GRLIN2(2,XPL,YPL)
       END
