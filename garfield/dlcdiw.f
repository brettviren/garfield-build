CDECK  ID>, DLCDIW.
       SUBROUTINE DLCDIW(COV,XC1,YC1,ZC1,XW1,YW1,DW1,SIGMA,IFAIL)
*-----------------------------------------------------------------------
*   DLCDIW - Integration of the time a cloud needs to reach a wire.
*   (Last changed on  8/11/95.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       DOUBLE PRECISION XU,YU,ZU,TU,XTARG,YTARG,TMC,DMC
       REAL DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX,DTARG,EPSDFI,EPSTWI,
     -      EPSATI,RDF2,DSCMIN,DSCMAX,DTFACT,
     -      DDXMIN,DDXMAX,DDYMIN,DDYMAX,DDZMIN,DDZMAX,EPSDIF,RTRAP,
     -      STMAX,EQTTHR,EQTASP,EQTCLS,QPCHAR
       INTEGER NU,ISTAT,ITARG,MXDIFS,MXTWNS,MXATTS,MDF2,
     -      ISTAT1,ISTAT2,ISTAT3,ISTAT4,ISTAT5,ISTAT6,NMC,MCMETH,
     -      IPTYPE,IPTECH
       LOGICAL LREPSK,LKINK,LSTMAX,LEQSRT,LEQCRS,LEQMRK,LAVPRO
       COMMON /DRFDAT/ XU(MXLIST),YU(MXLIST),ZU(MXLIST),TU(MXLIST),
     -      XTARG,YTARG,TMC,DMC,DTARG,
     -      DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX,
     -      DDXMIN,DDXMAX,DDYMIN,DDYMAX,DDZMIN,DDZMAX,
     -      EQTTHR,EQTASP,EQTCLS,QPCHAR,
     -      RTRAP,STMAX,EPSDIF,EPSDFI,EPSTWI,EPSATI,RDF2,DSCMIN,DSCMAX,
     -      DTFACT,MDF2,
     -      MXDIFS,MXTWNS,MXATTS,
     -      NU,ISTAT,ITARG,
     -      ISTAT1,ISTAT2,ISTAT3,ISTAT4,ISTAT5,ISTAT6,NMC,MCMETH,IPTYPE,
     -      IPTECH,LREPSK,LKINK,LSTMAX,LEQSRT,LEQCRS,LEQMRK,LAVPRO
       DOUBLE PRECISION X(2),COV(2,2),MAT(2,2),XW,YW,DW,XC,YC,ZC,SIGMA,
     -      XC1,YC1,ZC1,C,S,FCENT,FC(3),ST0,ST1,ST2,DGMLT2,SIG1,SIG2,DET
       INTEGER IFAIL,ILOC
       REAL XW1,YW1,DW1
       EXTERNAL DGMLT2,FDIF2N,FDIF2L,FDIF2Q
       COMMON /DF2DAT/ MAT,SIG1,SIG2,DET,XW,YW,DW,XC,YC,ZC,C,S,FCENT
*** Assume the routine will work.
       IFAIL=0
*** Determine a rotation that aligns the cloud with the axes.
       IF(ABS(COV(1,2)+COV(2,1)).LE.1E-8*ABS(COV(2,2)-COV(1,1)))THEN
            C=1
            S=0
       ELSEIF((COV(2,2)-COV(1,1))**2+(COV(1,2)+COV(2,1))**2.GT.0)THEN
            C=SQRT(0.5*(1+(COV(2,2)-COV(1,1))/
     -           SQRT((COV(2,2)-COV(1,1))**2+(COV(1,2)+COV(2,1))**2)))
            S=SIGN(SQRT(1-C**2),COV(1,2)+COV(2,1))
       ELSE
            C=1
            S=0
       ENDIF
*** Rotate the covariance matrix.
       MAT(1,1)=C**2*COV(1,1)-C*S*COV(1,2)-C*S*COV(2,1)+S**2*COV(2,2)
       MAT(1,2)=C**2*COV(2,1)-C*S*COV(2,2)+C*S*COV(1,1)-S**2*COV(1,2)
       MAT(2,1)=C**2*COV(1,2)-C*S*COV(2,2)+C*S*COV(1,1)-S**2*COV(2,1)
       MAT(2,2)=C**2*COV(2,2)+C*S*COV(1,2)+C*S*COV(2,1)+S**2*COV(1,1)
       IF(MAT(1,1).LE.0.0.OR.MAT(2,2).LE.0.0)THEN
            PRINT *,' !!!!!! DLCDIW WARNING : Covariance matrix'//
     -           ' (see below) is 1-dimensional; zero time spread.'
            PRINT *,' Aligned matrix: ',MAT(1,1),MAT(1,2)
            PRINT *,'                 ',MAT(2,1),MAT(2,2)
            print *,' Raw matrix:     ',cov(1,1),cov(1,2)
            print *,'                 ',cov(2,1),cov(2,2)
            print *,' cos/sin:        ',c,s
            print *,' Wire (x,y,d):   ',xw1,yw1,dw1
            print *,' Cloud (x,y,z):  ',xc1,yc1,zc1
            SIGMA=0
            IFAIL=1
            RETURN
       ENDIF
*** Shift wire position to the rotated frame with cloud at (0,0),
       XW=+C*(XW1-XC1)+S*(YW1-YC1)
       YW=-S*(XW1-XC1)+C*(YW1-YC1)
*   simply transfer the wire diameter,
       DW=DW1
*   but keep the original cluster location for speed calculations.
       XC=XC1
       YC=YC1
       ZC=ZC1
*** Prepare correlation and marginal distribution.
       SIG1=SQRT(MAT(1,1))
       SIG2=SQRT(MAT(2,2))
       DET=MAT(2,2)*MAT(1,1)-MAT(1,2)*MAT(2,1)
       IF(DET.EQ.0.0)THEN
            PRINT *,' DLCDIW WARNING : Covariance matrix is singular'//
     -           ' ; time spread set to zero.'
            SIGMA=0
            IFAIL=1
            RETURN
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCDIW DEBUG   : Rotation'',
     -      '' angles: cos='',F10.3,'', sin='',F10.3/25X,
     -      '' Cloud dimensions: ('',E15.8,'','',E15.8,'').'')')
     -      C,S,SIG1,SIG2
*** Compute central velocity.
       CALL DLCVEL(XC,YC,ZC,FC,-1.0,1,ILOC)
       FCENT=SQRT(FC(1)**2+FC(2)**2)
       IF(MDF2.EQ.2.AND.FCENT.LE.0)THEN
            PRINT *,' DLCDIW WARNING : Central velocity is zero;'//
     -           ' time spread set to zero.'
            SIGMA=0
            IFAIL=1
            RETURN
       ENDIF
*** Perform integration.
       ST0=DGMLT2(FDIF2N,-5*SIG2,+5*SIG2,5,6,X)
       ST1=DGMLT2(FDIF2L,-5*SIG2,+5*SIG2,5,6,X)
       ST2=DGMLT2(FDIF2Q,-5*SIG2,+5*SIG2,5,6,X)
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ DLCDIW DEBUG   : S0='',E15.8,
     -           '' S1='',E15.8,'' S2='',E15.8)') ST0,ST1,ST2
       ENDIF
       IF(ST1**2.LE.ST2*ST0)THEN
            SIGMA=SQRT(ST2-ST1**2/ST0)/ST0
       ELSE
            PRINT *,' DLCDIW WARNING : Time variance < 0'//
     -           ' ; time spread set to zero.'
            SIGMA=0
            IFAIL=1
       ENDIF
       END
