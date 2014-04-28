CDECK  ID>, MAPEPS.
       SUBROUTINE MAPEPS(IFAIL)
*-----------------------------------------------------------------------
*   MAPEPS - Sorts the dielectric constants and the conductivities.
*   (Last changed on 28/ 8/08.)
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
       REAL EXMAP,EYMAP,EZMAP,VMAP,EWXMAP,EWYMAP,EWZMAP,VWMAP,
     -      BXMAP,BYMAP,BZMAP,
     -      XMAP,YMAP,ZMAP,XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,
     -      VMMIN,VMMAX,EPSMAT,EPSSUR,XFMOFF,YFMOFF,ZFMOFF
       INTEGER MATMAP,NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS,
     -      NWMAP
       LOGICAL MAPFLG,LMAPPL,SETAX,SETAY,SETAZ,ELMDGN,LSFDER
       CHARACTER EWSTYP
       CHARACTER*10 MATSRC
       COMMON /FLDMAP/ VMAP(MXMAP,10),VWMAP(MXMAP,10,MXWMAP),
     -      EXMAP(MXMAP,10),EYMAP(MXMAP,10),EZMAP(MXMAP,10),
     -      EWXMAP(MXMAP,10,MXWMAP),EWYMAP(MXMAP,10,MXWMAP),
     -      EWZMAP(MXMAP,10,MXWMAP),
     -      BXMAP(MXMAP,10),BYMAP(MXMAP,10),BZMAP(MXMAP,10),
     -      XMAP(MXMAP,10),YMAP(MXMAP,10),ZMAP(MXMAP,10),
     -      XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,VMMIN,VMMAX,
     -      XFMOFF,YFMOFF,ZFMOFF,
     -      EPSMAT(MXEPS),EPSSUR(MXEPS),MATMAP(MXMAP),
     -      NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS(MXWMAP),NWMAP,
     -      MAPFLG(10+4*MXWMAP),ELMDGN(MXMAP),
     -      LMAPPL,SETAX,SETAY,SETAZ,LSFDER
       COMMON /FLDCHR/ EWSTYP(MXWMAP),MATSRC
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER IND(MXEPS),INEW,I,II,J,K,IFAIL,NDELET,IMAP
       REAL AUX
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPEPS ///'
*** Assume the routine will fail.
       IFAIL=1
*** Sort the epsilons.
       DO 120 I=1,NEPS
       IND(I)=I
120    CONTINUE
C       CALL SORTZV(EPSMAT,IND,NEPS,1,0,0)
       CALL SORTTF(EPSMAT,IND,NEPS)
*** Count deleted elements
       NDELET=0
*** Attribute new numbers to the volumes.
       IMAP=0
       DO 10 I=1,NMAP
*   Search what the old index of this material was.
       DO 20 J=1,NEPS
       IF(IND(J).EQ.MATMAP(I))THEN
            INEW=J
            GOTO 30
       ENDIF
20     CONTINUE
*   Delete the element if it has no valid index
       NDELET=NDELET+1
       GOTO 10
30     CONTINUE
*   Increment the mesh counter
       IMAP=IMAP+1
*   Copy the element.
       MATMAP(IMAP)=INEW
       DO 60 J=1,10
       XMAP(IMAP,J)=XMAP(I,J)
       YMAP(IMAP,J)=YMAP(I,J)
       ZMAP(IMAP,J)=ZMAP(I,J)
       EXMAP(IMAP,J)=EXMAP(I,J)
       EYMAP(IMAP,J)=EYMAP(I,J)
       EZMAP(IMAP,J)=EZMAP(I,J)
       VMAP(IMAP,J) =VMAP(I,J)
       BXMAP(IMAP,J)=BXMAP(I,J)
       BYMAP(IMAP,J)=BYMAP(I,J)
       BZMAP(IMAP,J)=BZMAP(I,J)
       DO 70 K=1,MXWMAP
       EWXMAP(IMAP,J,K)=EWXMAP(I,J,K)
       EWYMAP(IMAP,J,K)=EWYMAP(I,J,K)
       EWZMAP(IMAP,J,K)=EWZMAP(I,J,K)
       VWMAP(IMAP,J,K)=VWMAP(I,J,K)
70     CONTINUE
60     CONTINUE
*   Next element
10     CONTINUE
*** Assign the new number of elements
       NMAP=IMAP
       IF(NDELET.NE.0)PRINT *,' ------ MAPEPS MESSAGE : Eliminated ',
     -      NDELET,' elements without material information.'
*** Sort the epsilons.
       DO 150 I=1,NEPS
*   Find the I'th epsilon.
       II=0
       DO 110 J=I,NEPS
       IF(IND(J).EQ.I)II=J
110    CONTINUE
*   Check we found something.
       IF(II.EQ.0)THEN
            PRINT *,' !!!!!! MAPEPS WARNING : Did not find the'//
     -           ' epsilon with the required index; skipped.'
            GOTO 150
       ENDIF
*   Exchange.
       AUX=EPSMAT(I)
       EPSMAT(I)=EPSMAT(IND(I))
       EPSMAT(IND(I))=AUX
       IND(II)=IND(I)
150    CONTINUE
*** Compute volumes and areas.
       DO 40 I=1,NEPS
       EPSSUR(I)=0
       DO 50 J=1,NMAP
       IF(MATMAP(J).EQ.I)THEN
            IF(MAPTYP.EQ.1.OR.MAPTYP.EQ.2.OR.MAPTYP.EQ.3)THEN
                 EPSSUR(I)=EPSSUR(I)+ABS(
     -                (XMAP(J,3)-XMAP(J,1))*(YMAP(J,2)-YMAP(J,1))-
     -                (YMAP(J,3)-YMAP(J,1))*(XMAP(J,2)-XMAP(J,1)))/2
            ELSEIF(MAPTYP.EQ.4.OR.MAPTYP.EQ.5)THEN
                 EPSSUR(I)=EPSSUR(I)+(ABS((XMAP(I,2)-XMAP(I,1))*
     -                                    (YMAP(I,3)-YMAP(I,1))-
     -                                    (XMAP(I,3)-XMAP(I,1))*
     -                                    (YMAP(I,2)-YMAP(I,1)))+
     -                                ABS((XMAP(I,4)-XMAP(I,1))*
     -                                    (YMAP(I,3)-YMAP(I,1))-
     -                                    (XMAP(I,3)-XMAP(I,1))*
     -                                    (YMAP(I,4)-YMAP(I,1))))/2
            ELSEIF(MAPTYP.EQ.11.OR.MAPTYP.EQ.12.OR.MAPTYP.EQ.13)THEN
                 EPSSUR(I)=EPSSUR(I)+ABS(
     -                (XMAP(J,4)-XMAP(J,1))*(
     -                (YMAP(J,2)-YMAP(J,1))*(ZMAP(J,3)-ZMAP(J,1))-
     -                (YMAP(J,3)-YMAP(J,1))*(ZMAP(J,2)-ZMAP(J,1)))+
     -                (YMAP(J,4)-YMAP(J,1))*(
     -                (ZMAP(J,2)-ZMAP(J,1))*(XMAP(J,3)-XMAP(J,1))-
     -                (ZMAP(J,3)-ZMAP(J,1))*(XMAP(J,2)-XMAP(J,1)))+
     -                (ZMAP(J,4)-ZMAP(J,1))*(
     -                (XMAP(J,2)-XMAP(J,1))*(YMAP(J,3)-YMAP(J,1))-
     -                (XMAP(J,3)-XMAP(J,1))*(YMAP(J,2)-YMAP(J,1))))/6
            ELSEIF(MAPTYP.EQ.14.OR.MAPTYP.EQ.15.OR.MAPTYP.EQ.16)THEN
                 EPSSUR(I)=EPSSUR(I)+ABS(
     -                (XMAP(J,4)-XMAP(J,1))*(
     -                (YMAP(J,2)-YMAP(J,1))*(ZMAP(J,3)-ZMAP(J,1))-
     -                (YMAP(J,3)-YMAP(J,1))*(ZMAP(J,2)-ZMAP(J,1)))+
     -                (YMAP(J,4)-YMAP(J,1))*(
     -                (ZMAP(J,2)-ZMAP(J,1))*(XMAP(J,3)-XMAP(J,1))-
     -                (ZMAP(J,3)-ZMAP(J,1))*(XMAP(J,2)-XMAP(J,1)))+
     -                (ZMAP(J,4)-ZMAP(J,1))*(
     -                (XMAP(J,2)-XMAP(J,1))*(YMAP(J,3)-YMAP(J,1))-
     -                (XMAP(J,3)-XMAP(J,1))*(YMAP(J,2)-YMAP(J,1))))
            ENDIF
       ENDIF
50     CONTINUE
*   Debugging output.
       IF(LDEBUG)THEN
            IF(MATSRC.EQ.'EPSILON')THEN
                 WRITE(LUNOUT,'(''  ++++++ MAPEPS DEBUG   :'',
     -                '' Material '',I3,'': epsilon='',E10.3,'', '',
     -                '' surface '',E10.3)') I,EPSMAT(I),EPSSUR(I)
            ELSE
                 WRITE(LUNOUT,'(''  ++++++ MAPEPS DEBUG   :'',
     -                '' Material '',I3,'': sigma='',E10.3,'' S/m, '',
     -                '' surface '',E10.3)') I,EPSMAT(I),EPSSUR(I)
            ENDIF
       ENDIF
40     CONTINUE
*** Seems to have worked.
       IFAIL=0
       END
