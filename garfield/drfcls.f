CDECK  ID>, DRFCLS.
       SUBROUTINE DRFCLS
*-----------------------------------------------------------------------
*   DRFCLS - Studies clustering
*   (Last changed on  5/ 9/07.)
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
       INTEGER NNHIST
       PARAMETER(NNHIST=6)
       INTEGER NWORD,INEXT,I,IFAIL1,IFAIL2,IFAIL3,IFAIL4,IFAIL5,IFAIL6,
     -      NITER,NITERR,NCHA(NNHIST),NCHAR,NPAIR,IRSIZE,IRCLUS,IRDELT,
     -      IRRANG,IRETOT,IRECLS,NCLUS,INPCMP,INPTYP,J
       REAL XCLS,YCLS,ZCLS,ECLS,RANGE,XINP0,XINP1,TRALEN,DIST,ETOT,
     -      RANGEH(2,NNHIST),RMINR,RMAXR,EXTRA1
       LOGICAL DONE,LKEEP,AUTO(NNHIST),LHISPL
       EXTERNAL INPCMP,INPTYP
       SAVE NITER,NCHA,LKEEP,LHISPL
       DATA NITER /200/, LKEEP /.FALSE./, LHISPL /.TRUE./
*** Initial binning settings.
       DO 20 I=1,NNHIST
       AUTO(I)=.TRUE.
       RANGEH(1,I)=0
       RANGEH(2,I)=0
       NCHA(I)=100
20     CONTINUE
*** Count words.
       CALL INPNUM(NWORD)
*** Loop over the words.
       INEXT=2
       DO 10 I=2,NWORD
       IF(I.LT.INEXT)GOTO 10
*   Number of iterations.
       IF(INPCMP(I,'ITER#ATIONS')+INPCMP(I,'ITER#ATE').NE.0)THEN
            IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'Has 1 integer argument.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NITERR,NITER)
                 IF(NITERR.GT.0)THEN
                      NITER=NITERR
                 ELSE
                      CALL INPMSG(I+1,'Must be > 0.')
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Number of bins.
       ELSEIF(INPCMP(I,'BIN#S')+INPCMP(I,'CH#ANNELS').NE.0)THEN
            IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'Has 1 integer argument.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NCHAR,100)
                 IF(NCHAR.GT.0.AND.NCHAR.LE.MXCHA)THEN
                      DO 30 J=1,NNHIST
                      NCHA(J)=NCHAR
30                    CONTINUE
                 ELSE
                      CALL INPMSG(I+1,'Not in [1,MXCHA].')
                 ENDIF
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'CL#USTERS-S#IZE-BIN#S')+
     -      INPCMP(I,'CL#USTERS-S#IZE-CH#ANNELS').NE.0)THEN
            IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'Has 1 integer argument.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NCHAR,100)
                 IF(NCHAR.GT.0.AND.NCHAR.LE.MXCHA)THEN
                      NCHA(1)=NCHAR
                 ELSE
                      CALL INPMSG(I+1,'Not in [1,MXCHA].')
                 ENDIF
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'CL#USTERS-C#OUNT-BIN#S')+
     -      INPCMP(I,'CL#USTERS-C#OUNT-CH#ANNELS').NE.0)THEN
            IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'Has 1 integer argument.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NCHAR,100)
                 IF(NCHAR.GT.0.AND.NCHAR.LE.MXCHA)THEN
                      NCHA(2)=NCHAR
                 ELSE
                      CALL INPMSG(I+1,'Not in [1,MXCHA].')
                 ENDIF
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'DEL#TA-R#ANGE-BIN#S')+
     -      INPCMP(I,'DELTA-R#ANGE-CH#ANNELS').NE.0)THEN
            IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'Has 1 integer argument.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NCHAR,100)
                 IF(NCHAR.GT.0.AND.NCHAR.LE.MXCHA)THEN
                      NCHA(3)=NCHAR
                 ELSE
                      CALL INPMSG(I+1,'Not in [1,MXCHA].')
                 ENDIF
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'TR#ACK-R#ANGE-BIN#S')+
     -      INPCMP(I,'TR#ACK-R#ANGE-CH#ANNELS').NE.0)THEN
            IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'Has 1 integer argument.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NCHAR,100)
                 IF(NCHAR.GT.0.AND.NCHAR.LE.MXCHA)THEN
                      NCHA(4)=NCHAR
                 ELSE
                      CALL INPMSG(I+1,'Not in [1,MXCHA].')
                 ENDIF
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'CL#USTERS-E#NERGY-BIN#S')+
     -      INPCMP(I,'CL#USTERS-E#NERGY-CH#ANNELS').NE.0)THEN
            IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'Has 1 integer argument.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NCHAR,100)
                 IF(NCHAR.GT.0.AND.NCHAR.LE.MXCHA)THEN
                      NCHA(5)=NCHAR
                 ELSE
                      CALL INPMSG(I+1,'Not in [1,MXCHA].')
                 ENDIF
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'E#NERGY-L#OSS-BIN#S')+
     -      INPCMP(I,'E#NERGY-L#OSS-CH#ANNELS').NE.0)THEN
            IF(I+1.GT.NWORD.OR.INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'Has 1 integer argument.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NCHAR,100)
                 IF(NCHAR.GT.0.AND.NCHAR.LE.MXCHA)THEN
                      NCHA(6)=NCHAR
                 ELSE
                      CALL INPMSG(I+1,'Not in [1,MXCHA].')
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Ranges of the various histograms.
       ELSEIF(INPCMP(I,'CL#USTERS-S#IZE-RAN#GE').NE.0)THEN
            IF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                 AUTO(1)=.TRUE.
                 INEXT=I+2
            ELSEIF((INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2).OR.
     -           (INPTYP(I+2).NE.1.AND.INPTYP(I+2).NE.2).OR.
     -           I+2.GT.NWORD)THEN
                 CALL INPMSG(I,'Has 2 real arguments.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,RMINR,RANGEH(1,1))
                 CALL INPRDR(I+2,RMAXR,RANGEH(2,1))
                 IF(RMINR.EQ.RMAXR)THEN
                      CALL INPMSG(I,'Zero range not permitted.')
                 ELSE
                      RANGEH(1,1)=RMINR
                      RANGEH(2,1)=RMAXR
                      AUTO(1)=.FALSE.
                 ENDIF
                 INEXT=I+3
            ENDIF
       ELSEIF(INPCMP(I,'CL#USTERS-C#OUNT-RAN#GE').NE.0)THEN
            IF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                 AUTO(2)=.TRUE.
                 INEXT=I+2
            ELSEIF((INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2).OR.
     -           (INPTYP(I+2).NE.1.AND.INPTYP(I+2).NE.2).OR.
     -           I+2.GT.NWORD)THEN
                 CALL INPMSG(I,'Has 2 real arguments.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,RMINR,RANGEH(1,1))
                 CALL INPRDR(I+2,RMAXR,RANGEH(2,1))
                 IF(RMINR.EQ.RMAXR)THEN
                      CALL INPMSG(I,'Zero range not permitted.')
                 ELSE
                      RANGEH(1,2)=RMINR
                      RANGEH(2,2)=RMAXR
                      AUTO(2)=.FALSE.
                 ENDIF
                 INEXT=I+3
            ENDIF
       ELSEIF(INPCMP(I,'DEL#TA-R#ANGE-RAN#GE').NE.0)THEN
            IF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                 AUTO(3)=.TRUE.
                 INEXT=I+2
            ELSEIF((INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2).OR.
     -           (INPTYP(I+2).NE.1.AND.INPTYP(I+2).NE.2).OR.
     -           I+2.GT.NWORD)THEN
                 CALL INPMSG(I,'Has 2 real arguments.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,RMINR,RANGEH(1,1))
                 CALL INPRDR(I+2,RMAXR,RANGEH(2,1))
                 IF(RMINR.EQ.RMAXR)THEN
                      CALL INPMSG(I,'Zero range not permitted.')
                 ELSE
                      RANGEH(1,3)=RMINR
                      RANGEH(2,3)=RMAXR
                      AUTO(3)=.FALSE.
                 ENDIF
                 INEXT=I+3
            ENDIF
       ELSEIF(INPCMP(I,'TR#ACK-R#ANGE-RAN#GE').NE.0)THEN
            IF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                 AUTO(4)=.TRUE.
                 INEXT=I+2
            ELSEIF((INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2).OR.
     -           (INPTYP(I+2).NE.1.AND.INPTYP(I+2).NE.2).OR.
     -           I+2.GT.NWORD)THEN
                 CALL INPMSG(I,'Has 2 real arguments.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,RMINR,RANGEH(1,1))
                 CALL INPRDR(I+2,RMAXR,RANGEH(2,1))
                 IF(RMINR.EQ.RMAXR)THEN
                      CALL INPMSG(I,'Zero range not permitted.')
                 ELSE
                      RANGEH(1,4)=RMINR
                      RANGEH(2,4)=RMAXR
                      AUTO(4)=.FALSE.
                 ENDIF
                 INEXT=I+3
            ENDIF
       ELSEIF(INPCMP(I,'CL#USTERS-E#NERGY-RAN#GE').NE.0)THEN
            IF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                 AUTO(5)=.TRUE.
                 INEXT=I+2
            ELSEIF((INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2).OR.
     -           (INPTYP(I+2).NE.1.AND.INPTYP(I+2).NE.2).OR.
     -           I+2.GT.NWORD)THEN
                 CALL INPMSG(I,'Has 2 real arguments.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,RMINR,RANGEH(1,1))
                 CALL INPRDR(I+2,RMAXR,RANGEH(2,1))
                 IF(RMINR.EQ.RMAXR)THEN
                      CALL INPMSG(I,'Zero range not permitted.')
                 ELSE
                      RANGEH(1,5)=RMINR
                      RANGEH(2,5)=RMAXR
                      AUTO(5)=.FALSE.
                 ENDIF
                 INEXT=I+3
            ENDIF
       ELSEIF(INPCMP(I,'E#NERGY-L#OSS-RAN#GE').NE.0)THEN
            IF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                 AUTO(6)=.TRUE.
                 INEXT=I+2
            ELSEIF((INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2).OR.
     -           (INPTYP(I+2).NE.1.AND.INPTYP(I+2).NE.2).OR.
     -           I+2.GT.NWORD)THEN
                 CALL INPMSG(I,'Has 2 real arguments.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,RMINR,RANGEH(1,1))
                 CALL INPRDR(I+2,RMAXR,RANGEH(2,1))
                 IF(RMINR.EQ.RMAXR)THEN
                      CALL INPMSG(I,'Zero range not permitted.')
                 ELSE
                      RANGEH(1,6)=RMINR
                      RANGEH(2,6)=RMAXR
                      AUTO(6)=.FALSE.
                 ENDIF
                 INEXT=I+3
            ENDIF
*   Keep histograms or not.
       ELSEIF(INPCMP(I,'KEEP-#HISTOGRAMS').NE.0)THEN
            LKEEP=.TRUE.
       ELSEIF(INPCMP(I,'NOKEEP-#HISTOGRAMS').NE.0)THEN
            LKEEP=.FALSE.
*   Plot the histograms or not.
       ELSEIF(INPCMP(I,'PL#OT-#HISTOGRAMS').NE.0)THEN
            LHISPL=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT-#HISTOGRAMS').NE.0)THEN
            LHISPL=.FALSE.
*   Other keywords are not known.
       ELSE
            CALL INPMSG(I,'Not a known keyword.')
       ENDIF
10     CONTINUE
*   Print error messages.
       CALL INPERR
*** Set the progress print.
       CALL PROINT('CLUSTER',1,6)
*** Book histograms.
       CALL HISADM('INTEGER',IRSIZE,NCHA(1),RANGEH(1,1),RANGEH(2,1),
     -      AUTO(1),IFAIL1)
       CALL HISADM('INTEGER',IRCLUS,NCHA(2),RANGEH(1,2),RANGEH(2,2),
     -      AUTO(2),IFAIL2)
       CALL HISADM('ALLOCATE',IRDELT,NCHA(3),RANGEH(1,3),RANGEH(2,3),
     -      AUTO(3),IFAIL3)
       CALL HISADM('ALLOCATE',IRRANG,NCHA(4),RANGEH(1,4),RANGEH(2,4),
     -      AUTO(4),IFAIL4)
       CALL HISADM('ALLOCATE',IRECLS,NCHA(5),RANGEH(1,5),RANGEH(2,5),
     -      AUTO(5),IFAIL5)
       CALL HISADM('ALLOCATE',IRETOT,NCHA(6),RANGEH(1,6),RANGEH(2,6),
     -      AUTO(6),IFAIL6)
       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0.OR.
     -      IFAIL4.NE.0.OR.IFAIL5.NE.0.OR.IFAIL6.NE.0)THEN
            PRINT *,' !!!!!! DRFCLS WARNING : Allocating one or more'//
     -           ' histograms failed; no plots.'
            GOTO 500
       ENDIF
*** Now generate the true sample.
       CALL PROFLD(1,'Tracks',REAL(NITER))
       DO 200 I=1,NITER
       IF(I.EQ.10*(I/10))CALL PROSTA(1,REAL(I))
       NCLUS=0
       RANGE=0
       ETOT=0
*   Prepare track.
       CALL TRACLI
*   Loop over clusters.
210    CONTINUE
*   Generate clusters.
       CALL TRACLS(XCLS,YCLS,ZCLS,ECLS,NPAIR,EXTRA1,DONE,IFAIL1)
*   Check whether done.
       IF(DONE)THEN
            CALL HISENT(IRCLUS,REAL(NCLUS),1.0)
            CALL HISENT(IRRANG,RANGE,1.0)
            CALL HISENT(IRETOT,ETOT,1.0)
            GOTO 200
*   Check error status.
       ELSEIF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! DRFCLS WARNING : Cluster generation'//
     -           ' failed; no plots made.'
            GOTO 500
       ENDIF
*   Enter size in histogram.
       CALL HISENT(IRSIZE,REAL(NPAIR),1.0)
*   Enter energy in histogram.
       CALL HISENT(IRECLS,1E6*ECLS,1.0)
*   Keep range up to date.
       RANGE=MAX(RANGE,SQRT((XCLS-XT0)**2+(YCLS-YT0)**2+
     -      (ZCLS-ZT0)**2))
*   Keep energy up to date.
       ETOT=ETOT+ECLS
*   Compute distance from track.
       TRALEN=(XT1-XT0)**2+(YT1-YT0)**2+(ZT1-ZT0)**2
       IF(TRALEN.LE.0.0)THEN
            DIST=SQRT((XT1-XCLS)**2+(YT1-YCLS)**2+(ZT1-ZCLS)**2)
       ELSE
            XINP0=(XT1-XT0)*(XCLS-XT0)+(YT1-YT0)*(YCLS-YT0)+
     -           (ZT1-ZT0)*(ZCLS-ZT0)
            XINP1=(XT0-XT1)*(XCLS-XT1)+(YT0-YT1)*(YCLS-YT1)+
     -           (ZT0-ZT1)*(ZCLS-ZT1)
            IF(XINP1**2*((XCLS-XT0)**2+(YCLS-YT0)**2+
     -           (ZCLS-ZT0)**2).GT.XINP0**2*((XCLS-XT1)**2+
     -           (YCLS-YT1)**2+(ZCLS-ZT1)**2))THEN
                 DIST=SQRT(MAX(0.0,(XCLS-XT0)**2+(YCLS-YT0)**2+
     -                (ZCLS-ZT0)**2-XINP0**2/TRALEN))
            ELSE
                 DIST=SQRT(MAX(0.0,(XCLS-XT1)**2+(YCLS-YT1)**2+
     -                (ZCLS-ZT1)**2-XINP1**2/TRALEN))
            ENDIF
       ENDIF
       CALL HISENT(IRDELT,DIST,1.0)
*   Increment statistics.
       NCLUS=NCLUS+1
       GOTO 210
200    CONTINUE
       CALL PROEND
*** Plot the histograms.
       IF(LHISPL)THEN
            CALL HISPLT(IRCLUS,'Number of deposits',
     -           'Number of clusters per track',.TRUE.)
            CALL GRNEXT
            CALL HISPLT(IRSIZE,'Number of electrons',
     -           'Number of electrons per cluster',.TRUE.)
            CALL GRNEXT
            CALL HISPLT(IRRANG,'Range [cm]',
     -           'Range of the track',.TRUE.)
            CALL GRNEXT
            CALL HISPLT(IRDELT,'Distance [cm]',
     -           'Distance between cluster and track',.TRUE.)
            CALL GRNEXT
            CALL HISPLT(IRECLS,'Energy [eV]',
     -           'Energy per cluster',.TRUE.)
            CALL GRNEXT
            CALL HISPLT(IRETOT,'Energy [MeV]',
     -           'Total energy loss',.TRUE.)
            CALL GRNEXT
       ENDIF
*** Delete histograms.
500    CONTINUE
       IF(LKEEP)THEN
            CALL HISSAV(IRSIZE,'SIZE',IFAIL1)
            CALL HISSAV(IRCLUS,'CLUSTERS',IFAIL2)
            CALL HISSAV(IRDELT,'DELTA',IFAIL3)
            CALL HISSAV(IRRANG,'RANGE',IFAIL4)
            CALL HISSAV(IRETOT,'DE',IFAIL5)
            CALL HISSAV(IRECLS,'ECLUSTER',IFAIL6)
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0.OR.
     -           IFAIL4.NE.0.OR.IFAIL5.NE.0.OR.IFAIL6.NE.0)THEN
                 PRINT *,' !!!!!! DRFCLS WARNING : Saving one or'//
     -                ' more histograms failed.'
            ELSE
                 PRINT *,' ------ DRFCLS MESSAGE : Histograms saved'//
     -                ' as SIZE, CLUSTERS, DELTA, RANGE, DE and'//
     -                ' ECLUSTER.'
            ENDIF
       ELSE
            CALL HISADM('DELETE',IRSIZE,NCHA,0.0,0.0,.FALSE.,IFAIL1)
            CALL HISADM('DELETE',IRCLUS,NCHA,0.0,0.0,.FALSE.,IFAIL2)
            CALL HISADM('DELETE',IRDELT,NCHA,0.0,0.0,.FALSE.,IFAIL3)
            CALL HISADM('DELETE',IRRANG,NCHA,0.0,0.0,.FALSE.,IFAIL4)
            CALL HISADM('DELETE',IRECLS,NCHA,0.0,0.0,.FALSE.,IFAIL5)
            CALL HISADM('DELETE',IRETOT,NCHA,0.0,0.0,.FALSE.,IFAIL6)
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0.OR.
     -           IFAIL4.NE.0.OR.IFAIL5.NE.0.OR.IFAIL6.NE.0)
     -           PRINT *,' !!!!!! DRFCLS WARNING : Deleting one'//
     -           ' or more histograms failed.'
       ENDIF
       END
