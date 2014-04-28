CDECK  ID>, GASSAV.
       SUBROUTINE GASSAV
*-----------------------------------------------------------------------
*   GASSAV - Saves cross section and energy distribution data.
*   (Last changed on 22/ 5/09.)
*-----------------------------------------------------------------------
       implicit none
*   Array dimensions.
       integer mxngas
       parameter(mxngas=6)
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
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
*   Grouped QIN1 ... QIN6 in QINN
       DOUBLE PRECISION QELM,QSUM,QION,QINN,QSATT
       COMMON /MIX1/ QELM(2048),QSUM(2048),QION(mxngas,2048),
     -      QINn(220,2048,mxngas),QSATT(2048)
*   EVECT is originally called E or ES depending on the routine.
       DOUBLE PRECISION Evect,EROOT,QTOT,QREL,QINEL,QEL
       COMMON /MIX2/ Evect(2048),EROOT(2048),QTOT(2048),QREL(2048),
     -      QINEL(2048),QEL(2048)
*   Extensively reduced.
       INTEGER NINn
*           ,LION,LIN1,LIN2,LIN3,LIN4,LIN5,LIN6
*           DOUBLE PRECISION ALION,ALIN1,ALIN2,ALIN3,ALIN4,ALIN5,ALIN6
       COMMON /MIX3/ NINn(mxngas)
*           ,LION(6),LIN1(220),
*     -      LIN2(220),LIN3(220),LIN4(220),LIN5(220),LIN6(220),ALION(6),
*     -      ALIN1(220),ALIN2(220),ALIN3(220),ALIN4(220),ALIN5(220),
*     -      ALIN6(220)
*   Adjusted size of ICOLL
       DOUBLE PRECISION TIME,SPEC,TMAX1,AVE,DEN,XID,X,Y,Z,ST
       INTEGER ICOLL,NNULL,ICOLN
       COMMON/OUTPT/TIME(300),ICOLL(5*mxngas),SPEC(2048),TMAX1,
     -      AVE,DEN,XID,X,Y,Z,ST,NNULL,ICOLN(512)
       INTEGER NGAS,NSTEP,IDBG
       DOUBLE PRECISION EFINAL,ESTEP,AKT,ARY,TEMPC,TORR
       PARAMETER(ARY=13.60569172)
       COMMON/INPT/NGAS,NSTEP,EFINAL,ESTEP,AKT,TEMPC,TORR,IDBG
*   Sometimes IPLAST is called LAST
       DOUBLE PRECISION CF,EIN,TCF,RGAS,WPL
       INTEGER IARRY,IPN,IPLAST,ISIZE
       COMMON/LARGE/CF(2048,512),EIN(512),TCF(2048),IARRY(512),
     -      RGAS(512),IPN(512),WPL(512),IPLAST,ISIZE
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL CSEMAT(MXLIST),CSTMAT(MXLIST),CSAMAT(MXLIST),CSIMAT(MXLIST),
     -      EMAT(MXLIST),FMAT(MXLIST),SCALE,FSUM,ALOSCH
       INTEGER NC,JVAR,I,J,K,NSAMP,ISIZ(1),IDIM(1),IFAIL1,IFAIL2,IFAIL3,
     -      IFAIL4,IFAIL5,IFAIL6,NORM,NDATA
       CHARACTER*20 STR
       PARAMETER(ALOSCH=2.6867775E19)
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE GASSAV ///'
*** Settings.
       NSAMP=2
*** Check that the total number of samples doesn't exceed MXLIST.
       IF(2048/NSAMP.GT.MXLIST)THEN
            PRINT *,' !!!!!! GASSAV WARNING : Number of samples too'//
     -           ' large; reduced.'
            NSAMP=NINT(0.5+2048.0/REAL(MXLIST))
*   and also be sure there is at least 1 sample.
       ELSEIF(2048/NSAMP.LT.1)THEN
            PRINT *,' !!!!!! GASSAV WARNING : Number of samples too'//
     -           ' small; increased to have 1 sample.'
            NSAMP=2048
       ENDIF
*** Figure out which set of variables is still free.
       JVAR=0
10     CONTINUE
       JVAR=JVAR+1
       CALL OUTFMT(REAL(JVAR),2,STR,NC,'LEFT')
       DO 20 I=1,NGLB
       IF(GLBVAR(I).EQ.'CSE_'//STR(1:NC).OR.
     -      GLBVAR(I).EQ.'CST_'//STR(1:NC).OR.
     -      GLBVAR(I).EQ.'CSA_'//STR(1:NC).OR.
     -      GLBVAR(I).EQ.'CSI_'//STR(1:NC).OR.
     -      GLBVAR(I).EQ.'F_'//STR(1:NC).OR.
     -      GLBVAR(I).EQ.'E_'//STR(1:NC))GOTO 10
20     CONTINUE
*** Prepare a sampling vector, initialise the sum of F.
       FSUM=0
       NDATA=0
*   Loop over the samples.
       DO 30 I=0,2047,NSAMP
       NDATA=NDATA+1
*   Initialise.
       EMAT(NDATA)=0
       FMAT(NDATA)=0
       CSTMAT(NDATA)=0
       CSEMAT(NDATA)=0
       CSAMAT(NDATA)=0
       CSIMAT(NDATA)=0
       NORM=0
*   Loop over the sub-samples.
       DO 40 J=1,NSAMP
       IF(I+J.GE.2048)GOTO 40
       NORM=NORM+1
       EMAT(NDATA)=EMAT(NDATA)+EVECT(I+J)
C   Return the same data as Magboltz (RV 22/5/2009)
C       FMAT(NDATA)=FMAT(NDATA)+SPEC(I+J)/TCF(I+J)
       FMAT(NDATA)=FMAT(NDATA)+SPEC(I+J)
       CSTMAT(NDATA)=CSTMAT(NDATA)+QTOT(I+J)
       CSEMAT(NDATA)=CSEMAT(NDATA)+QEL(I+J)
       CSAMAT(NDATA)=CSAMAT(NDATA)+QSATT(I+J)
       DO 50 K=1,NGAS
       CSIMAT(NDATA)=CSIMAT(NDATA)+QION(K,I+J)
50     CONTINUE
40     CONTINUE
*   Normalise by number of samples.
       IF(NORM.GT.0)THEN
            EMAT(NDATA)=EMAT(NDATA)/NORM
            CSTMAT(NDATA)=CSTMAT(NDATA)/(NORM*ALOSCH)
            CSEMAT(NDATA)=CSEMAT(NDATA)/(NORM*ALOSCH)
            CSAMAT(NDATA)=CSAMAT(NDATA)/(NORM*ALOSCH)
            CSIMAT(NDATA)=CSIMAT(NDATA)/(NORM*ALOSCH)
       ELSE
            EMAT(NDATA)=0
            CSTMAT(NDATA)=0
            CSEMAT(NDATA)=0
            CSAMAT(NDATA)=0
            CSIMAT(NDATA)=0
       ENDIF
*   Keep track of the sum of F.
       FSUM=FSUM+FMAT(NDATA)
30     CONTINUE
*** Normalise the energy distribution function.
       SCALE=FSUM*(EMAT(2)-EMAT(1))
       IF(SCALE.LE.0)THEN
            PRINT *,' !!!!!! GASSAV WARNING : Probability scaling'//
     -           ' less or equal 0; probabilities not normalised.'
            SCALE=1
       ENDIF
       DO 70 I=1,NDATA
       FMAT(I)=FMAT(I)/SCALE
70     CONTINUE
*** Save the parameters.
       ISIZ(1)=NDATA
       IDIM(1)=MXLIST
       CALL MATSAV(EMAT,1,IDIM,ISIZ,'E_'//STR(1:NC),IFAIL1)
       CALL MATSAV(FMAT,1,IDIM,ISIZ,'F_'//STR(1:NC),IFAIL2)
       CALL MATSAV(CSTMAT,1,IDIM,ISIZ,'CST_'//STR(1:NC),IFAIL4)
       CALL MATSAV(CSEMAT,1,IDIM,ISIZ,'CSE_'//STR(1:NC),IFAIL3)
       CALL MATSAV(CSAMAT,1,IDIM,ISIZ,'CSA_'//STR(1:NC),IFAIL5)
       CALL MATSAV(CSIMAT,1,IDIM,ISIZ,'CSI_'//STR(1:NC),IFAIL6)
       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0.OR.
     -      IFAIL4.NE.0.OR.IFAIL5.NE.0.OR.IFAIL6.NE.0)THEN
            PRINT *,' !!!!!! GASSAV WARNING : Error while saving one'//
     -           ' or more matrices.'
       ELSE
            WRITE(LUNOUT,'(''  ------ GASSAV MESSAGE : Data saved'',
     -           '' under the following names:''/
     -           26X,''Energy:                   '',A/
     -           26X,''Distribution function:    '',A/
     -           26X,''Total cross section:      '',A/
     -           26X,''Elastic cross section:    '',A/
     -           26X,''Attachment cross section: '',A/
     -           26X,''Ionisation cross section: '',A)')
     -           'E_'//STR(1:NC),'F_'//STR(1:NC),'CST_'//STR(1:NC),
     -           'CSE_'//STR(1:NC),'CSA_'//STR(1:NC),'CSI_'//STR(1:NC)
       ENDIF
       END
