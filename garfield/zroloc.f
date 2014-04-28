CDECK  ID>, ZROLOC.
       SUBROUTINE ZROLOC(XMIN,YMIN,ZXMIN,ZYMIN,ZXMAX,ZYMAX,IFAIL)
*-----------------------------------------------------------------------
*   ZROLOC - Subroutine which tries to locate a zero accurately given
*            a search area. It starts picking points at random, then it
*            continues with the rank 2 Broyden, Fletcher, Goldfarb and
*            Shanno procedure. By changing the ZGAMMA and ZTHETA parms,
*            the DFP method (eg) can be obtained. This routine gives
*            reasonable results for analytic functions only.
*   VARIABLES : (XMIN,YMIN) : Position of the zero.
*   (Last changed on  4/ 4/95.)
*-----------------------------------------------------------------------
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
       LOGICAL ZROSET
       REAL XZ,YZ,PZ,DPMIN,DPMAX,DAMIN,DAMAX,EMIN
       INTEGER NZ,NFC
       COMMON /ZRODAT/ XZ(MXZERO),YZ(MXZERO),PZ(MXZERO),NZ,NFC,
     -                 DPMIN,DPMAX,DAMIN,DAMAX,EMIN,ZROSET
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL H(2,2),HNEW(2,2),P(2),Q(2)
*** Preset some parameters.
       IFAIL=0
*** Perform a random search to find a suitable starting point.
       EOLD=0.0
       DO 300 I=1,100
       XRNDM=ZXMIN+(ZXMAX-ZXMIN)*RNDM(I+1)
       YRNDM=ZYMIN+(ZYMAX-ZYMIN)*RNDM(I-1)
       CALL EFIELD(XRNDM,YRNDM,0.0,EX,EY,EZ,ERNDM,VOLT,0,ILOC)
       NFC=NFC+1
       IF(ERNDM.NE.0.0.AND.(INIT.EQ.0.OR.ERNDM.LT.EOLD))THEN
           XOLD=XRNDM
           YOLD=YRNDM
           EOLD=ERNDM
           INIT=1
       ENDIF
300    CONTINUE
*   Warn if no starting point has been found
       IF(INIT.EQ.0)THEN
           PRINT *,' !!!!!! ZROLOC WARNING : Unable to find a suitable',
     -             ' random starting point'
           IFAIL=1
           RETURN
       ENDIF
       IF(LDEBUG)WRITE(*,'(/26X,''Search start '',2F11.4,
     -      '' (Etot='',E10.3,'').''/)') XOLD,YOLD,EOLD
*** Preset the matrix H to a surely pos. def. unity.
       H(1,1)=1.0
       H(1,2)=0.0
       H(2,1)=0.0
       H(2,2)=1.0
*** Calculate the gradient of the function at the starting point.
       CALL EFIELD(XOLD+1.0E-4*(ABS(XOLD)+1.0),YOLD,0.0,
     -      EX,EY,EZ,ETOTX1,VOLT,0,ILOC)
       CALL EFIELD(XOLD-1.0E-4*(ABS(XOLD)+1.0),YOLD,0.0,
     -      EX,EY,EZ,ETOTX2,VOLT,0,ILOC)
       CALL EFIELD(XOLD,YOLD+1.0E-4*(ABS(YOLD)+1.0),0.0,
     -      EX,EY,EZ,ETOTY1,VOLT,0,ILOC)
       CALL EFIELD(XOLD,YOLD-1.0E-4*(ABS(YOLD)+1.0),0.0,
     -      EX,EY,EZ,ETOTY2,VOLT,0,ILOC)
       NFC=NFC+4
       G1=(ETOTX1-ETOTX2)/(2.0E-4*(ABS(XOLD)+1.0))
       G2=(ETOTY1-ETOTY2)/(2.0E-4*(ABS(YOLD)+1.0))
       IF(G1.EQ.0.0.AND.G2.EQ.0.0)THEN
            IF(LDEBUG)WRITE(*,'(26X,''Starting point is stationary.'')')
            XNEW=XOLD
            YNEW=YOLD
            ENEW=EOLD
            GOTO 500
       ENDIF
*** Begin of the Newton-like search loop.
       NSTEPS=0
310    CONTINUE
       NSTEPS=NSTEPS+1
*   Set a suitable direction for the linear minimisation.
       S1=H(1,1)*G1+H(1,2)*G2
       S2=H(2,1)*G1+H(2,2)*G2
       IF(LDEBUG)WRITE(*,'(26X,''Search direction: '',2F11.4)') -S1,-S2
**  Perform a linear minimisation, first check length of direction,
       IF(S1**2+S2**2.EQ.0.0)THEN
            IF(LDEBUG)WRITE(*,'(26X,''The step size is zero,'',
     -           '' search aborted in step '',I2,''.''/)') NSTEPS
            IFAIL=1
            XNEW=XOLD
            YNEW=YOLD
            ENEW=EOLD
            GOTO 500
       ENDIF
*   copy current estimate to the variables used for linear minimisation,
       XLIN=XOLD
       YLIN=YOLD
       ELIN=EOLD
*   calculate in the neigbourhood of the present best point,
       EPS=0.1*(1.0+SQRT(XLIN**2+YLIN**2))/(S1**2+S2**2)
       CALL EFIELD (XLIN+EPS*S1,YLIN+EPS*S2,0.0,
     -      EX,EY,EZ,ELINP,VOLT,0,ILOC)
       CALL EFIELD (XLIN-EPS*S1,YLIN-EPS*S2,0.0,
     -      EX,EY,EZ,ELINM,VOLT,0,ILOC)
       NFC=NFC+2
*   find a rough estimate for a minimum,
       IF(ELINP+ELINM-2*ELIN.LE.0)THEN
            IF(LDEBUG)WRITE(*,'(26X,''Second derivative is zero, no'',
     -           '' second order guess can be done.'')')
            IF(ELINP.EQ.ELINM)THEN
                 IF(LDEBUG)WRITE(*,'(26X,''First derivative is also 0'',
     -                '' minimum assumed.'')')
                 GOTO 450
            ENDIF
            IF(LDEBUG)WRITE(*,'(26X,''A linear guess is attempted.'')')
            XLIN1=XLIN-S1*ELIN*EPS/(ELINP-ELINM)
            YLIN1=YLIN-S2*ELIN*EPS/(ELINP-ELINM)
       ELSE
            XLIN1=XLIN-S1*(EPS/2)*(ELINP-ELINM)/(ELINP-2*ELIN+ELINM)
            YLIN1=YLIN-S2*(EPS/2)*(ELINP-ELINM)/(ELINP-2*ELIN+ELINM)
       ENDIF
       IF(LDEBUG)WRITE(*,'(26X,''Rough estimate is ('',E10.3,'','',
     -      E10.3,'').'')') XLIN1,YLIN1
*   make sure this point has indeed a amaller E than (XLIN,YLIN),
       NLIN=0
400    CONTINUE
       NLIN=NLIN+1
       IF(NLIN.GT.5)THEN
            IF(LDEBUG)WRITE(*,'(26X,''Maximum number of'',
     -           '' contractions exceeded.'')')
            GOTO 450
       ENDIF
       CALL EFIELD(XLIN1,YLIN1,0.0,EX,EY,EZ,ELIN1,VOLT,0,ILOC)
       PRINT *,' ELIN1=',ELIN1
       NFC=NFC+1
       IF(ELIN1.GT.ELIN)THEN
            XLIN1=(XLIN+XLIN1)/2
            YLIN1=(YLIN+YLIN1)/2
            GOTO 400
       ENDIF
       IF(LDEBUG.AND.NLIN.GT.1)WRITE(*,'(26X,''Rough estimate'',
     -      '' corrected '',I2,'' times.'')') NLIN
*   next set a point 'behind' the minimum,
       NLIN=0
410    CONTINUE
       NLIN=NLIN+1
       IF(NLIN.GT.5)THEN
            IF(LDEBUG)WRITE(*,'(26X,''Maximum number of'',
     -           '' expansions exceeded.'')')
            GOTO 440
       ENDIF
       XLIN2=2*XLIN1-XLIN
       YLIN2=2*YLIN1-YLIN
       CALL EFIELD(XLIN2,YLIN2,0.0,EX,EY,EZ,ELIN2,VOLT,0,ILOC)
       PRINT *,' ELIN2=',ELIN2
       NFC=NFC+1
       IF(ELIN2.LT.ELIN1)THEN
            XLIN1=XLIN2
            YLIN1=YLIN2
            ELIN1=ELIN2
            GOTO 410
       ENDIF
       IF(LDEBUG.AND.NLIN.GT.1)WRITE(*,'(26X,''Over shoot point has'',
     -      '' been corrected '',I2,'' times.'')') NLIN
*   perform a parabolic minimisation: first find improved minimum,
       NPAR=0
420    CONTINUE
       NPAR=NPAR+1
       IF(NPAR.GT.5)THEN
            IF(LDEBUG)WRITE(*,'(26X,''Maximum number of'',
     -           '' parabolic loops exceeded.'')')
            XLIN=XPAR
            YLIN=YPAR
            ELIN=EPAR
            GOTO 450
       ENDIF
       C1=SQRT((XLIN1-XLIN)**2+(YLIN2-YLIN)**2)
       C2=SQRT((XLIN2-XLIN)**2+(YLIN2-YLIN)**2)
       CPAR=0.5*((C1**2-C2**2)*ELIN+C2**2*ELIN1-C1**2*ELIN2)/
     -      ((C1-C2)*ELIN+C2*ELIN1-C1*ELIN2)
       XPAR=XLIN+(XLIN2-XLIN)*(CPAR/C2)
       YPAR=YLIN+(YLIN2-YLIN)*(CPAR/C2)
       CALL EFIELD(XPAR,YPAR,0.0,EX,EY,EZ,EPAR,VOLT,0,ILOC)
       NFC=NFC+1
       IF(EPAR.GT.ELIN1)THEN
            IF(LDEBUG)WRITE(*,'(26X,''Parabolic minimum exceeds'',
     -           '' current minimum.'')')
            XLIN=XLIN1
            YLIN=YLIN1
            ELIN=ELIN1
            GOTO 450
       ENDIF
*   check convergence criteria
       IF(ABS(EPAR-ELIN1).LT.1.0E-3*ELIN1.OR.EPAR.LT.EMIN)THEN
            IF(LDEBUG)WRITE(*,'(26X,''Convergence criteria satisfied'',
     -           '' after '',I2,'' parabolic loops.'')') NPAR
            XLIN=XPAR
            YLIN=YPAR
            ELIN=EPAR
            GOTO 450
       ENDIF
*   shift the data points and perform a new parabolic minimastion,
       IF(CPAR.LT.C1)THEN
            XLIN=XLIN1
            YLIN=YLIN1
            ELIN=ELIN1
       ELSE
            XLIN2=XLIN1
            YLIN2=YLIN1
            ELIN2=ELIN1
       ENDIF
       XLIN1=XPAR
       YLIN1=YPAR
       ELIN1=EPAR
       GOTO 420
*   no convergence: abort the search loop and jump to the end,
440    CONTINUE
       IF(LDEBUG)WRITE(*,'(26X,''The linear search did not converge,'',
     -      '' search aborted.''/)')
       IFAIL=1
       XNEW=XLIN
       YNEW=YLIN
       ENEW=ELIN
       GOTO 500
*   end of linear search loop.
450    CONTINUE
*   make sure the new point is in the right direction.
       IF(S1*(XLIN-XOLD).LE.0.0.AND.S2*(YLIN-YOLD).LE.0.0)THEN
            XNEW=XLIN
            YNEW=YLIN
            ENEW=ELIN
       ELSE
            IF(LDEBUG)WRITE(*,'(26X,''The result of the linear'',
     -           '' minimisation is not accepted''/29X,''because'',
     -           '' CFAC is negative. CFAC is replaced by 1.'')')
            XNEW=XOLD-S1
            YNEW=YOLD-S2
            CALL EFIELD(XNEW,YNEW,0.0,EX,EY,EZ,ENEW,VOLT,0,ILOC)
            NFC=NFC+1
       ENDIF
       IF(LDEBUG)WRITE(*,'(26X,''New estimate '',2F11.4,'' (Etot='',
     -      E10.3,'').''/)') XNEW,YNEW,ENEW
**  Before proceeding further, check whether we are satisfied.
       IF(ABS(EOLD-ENEW).LT.1.0E-4*(ABS(EOLD)+ABS(ENEW)))THEN
            IF(LDEBUG)WRITE(*,'(26X,''Change in E stop criterion'',
     -           '' is satisfied in loop '',I2,''.'')') NSTEPS
            GOTO 500
       ENDIF
       IF(ABS(XOLD-XNEW)*1.0E4.LT.ABS(XOLD)+ABS(XNEW).AND.
     -    ABS(YOLD-YNEW)*1.0E4.LT.ABS(YOLD)+ABS(YNEW))THEN
            IF(LDEBUG)WRITE(*,'(26X,''Position change stop criterion'',
     -           '' is satisfied in loop '',I2,''.'')') NSTEPS
            GOTO 500
       ENDIF
       IF(ENEW.LT.EMIN)THEN
            IF(LDEBUG)WRITE(*,'(26X,''Absolute value of E criterion'',
     -           '' is satisfied in loop '',I2,''.'')') NSTEPS
            GOTO 500
       ENDIF
**  Update H, calculate the gradient of the function at (XNEW,YNEW),
       CALL EFIELD(XNEW+1.0E-4*(ABS(XNEW)+1.0),YNEW,0.0,
     -      EX,EY,EZ,ETOTX1,VOLT,0,ILOC)
       CALL EFIELD(XNEW-1.0E-4*(ABS(XNEW)+1.0),YNEW,0.0,
     -      EX,EY,EZ,ETOTX2,VOLT,0,ILOC)
       CALL EFIELD(XNEW,YNEW+1.0E-4*(ABS(YNEW)+1.0),0.0,
     -      EX,EY,EZ,ETOTY1,VOLT,0,ILOC)
       CALL EFIELD(XNEW,YNEW-1.0E-4*(ABS(YNEW)+1.0),0.0,
     -      EX,EY,EZ,ETOTY2,VOLT,0,ILOC)
       NFC=NFC+4
       G1NEW=(ETOTX1-ETOTX2)/(2.0E-4*(ABS(XNEW)+1.0))
       G2NEW=(ETOTY1-ETOTY2)/(2.0E-4*(ABS(YNEW)+1.0))
       IF(G1NEW.EQ.0.0.AND.G2NEW.EQ.0.0)THEN
            IF(LDEBUG)WRITE(*,'(26X,''Truly stationary point found in'',
     -           '' step '',I2,''.'')') NSTEPS
            GOTO 500
       ENDIF
*   prepare some auxiliary variables,
       P(1)=XNEW-XOLD
       P(2)=YNEW-YOLD
       Q(1)=G1NEW-G1
       Q(2)=G2NEW-G2
       PQ=P(1)*Q(1)+P(2)*Q(2)
       QHQ=Q(1)*(H(1,1)*Q(1)+H(1,2)*Q(2))+Q(2)*(H(2,1)*Q(1)+H(2,2)*Q(2))
*   select ZGAMMA and ZTHETA
       ZGAMMA=1
       ZTHETA=1
*   the update itself.
       DO 360 K=1,2
       DO 350 L=1,2
       HNEW(K,L)=ZGAMMA*H(K,L)+
     -      (1+ZGAMMA*ZTHETA*QHQ/PQ)*P(K)*P(L)/PQ-
     -      ZGAMMA*(1-ZTHETA)*(H(K,1)*Q(1)+H(K,2)*Q(2))*
     -           (Q(1)*H(1,L)+Q(2)*H(2,L))/QHQ-
     -      ZGAMMA*ZTHETA*(P(K)*Q(1)*H(1,L)+P(K)*Q(2)*H(2,L)+
     -           H(K,1)*Q(1)*P(L)+H(K,2)*Q(2)*P(L))/PQ
350    CONTINUE
360    CONTINUE
**  Transfer variables from old to new storage places.
       DO 380 K=1,2
       DO 370 L=1,2
       H(K,L)=HNEW(K,L)
370    CONTINUE
380    CONTINUE
       G1=G1NEW
       G2=G2NEW
       XOLD=XNEW
       YOLD=YNEW
       EOLD=ENEW
       GOTO 310
*** Final printing and checking of the results.
500    CONTINUE
*   Check whether the point lies in the area.
       IF(XNEW.LT.ZXMIN.OR.XNEW.GT.ZXMAX.OR.
     -    YMIN.LT.ZYMIN.OR.YNEW.GT.ZYMAX)THEN
            IF(LDEBUG)WRITE(*,'(26X,''The minimum lies outside the'',
     -           '' area.'')')
            IFAIL=1
*   Perhaps the point lies in the area, has E < EMIN but IFAIL=1.
       ELSEIF(IFAIL.NE.0.AND.ENEW.LT.EMIN)THEN
            IF(LDEBUG)WRITE(*,'(/26X,''Inspite of the failure'',
     -           '' the result is E-acceptable.'')')
            IFAIL=0
       ENDIF
*   Print the end result.
       IF(LDEBUG)WRITE(*,'(26X,''Final (X,Y)  '',2F11.4,'' (Etot='',
     -      E10.3,'').''/26X,''IFAIL for the whole search '',I2,''.'')')
     -      XNEW,YNEW,ENEW,IFAIL
*** Make sure the result is stored in the proper place.
       XMIN=XNEW
       YMIN=YNEW
       END
