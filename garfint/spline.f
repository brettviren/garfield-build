CDECK  ID>, SPLINE.
       SUBROUTINE SPLINE(X,Y,C,N,IFAIL)
*-----------------------------------------------------------------------
*   SPLINE - Routine preparing a cubic spline interpolation through the
*            the points (X(I),Y(I)) I=1,N.
*   VARIABLES : Most of the variables are the same as in the reference,
*               the only major difference being that the indices start
*               at 1 instead of at 0 and that C (program) is M (ref).
*   REFERENCE : Stoer and Bulirsch, Einfuhrung in die numerische
*               Mathematic, I, Heidelberger taschenbucher.
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
       DIMENSION X(MXLIST),Y(MXLIST),Q(MXLIST),U(MXLIST),C(MXLIST)
*** Initialise IFAIL to 0  (success).
       IFAIL=0
*** Reject the trivial case.
       IF(N.LE.1)THEN
            PRINT *,' ###### SPLINE ERROR   : Only ',N,' points on',
     -              ' the spline while a minimum of 2 is required.'
            IFAIL=1
            RETURN
       ENDIF
*** The X's should be all different and in strictly ascending order.
       DO 10 I=1,N-1
       IF(X(I).EQ.X(I+1))THEN
            PRINT *,' ###### SPLINE ERROR   : Two ordinates are equal.'
            IFAIL=1
            RETURN
       ENDIF
       IF(X(I).GT.X(I+1))THEN
            PRINT *,' ###### SPLINE ERROR   : The ordinates are not in',
     -           ' strictly ascending order.'
            IFAIL=1
            RETURN
       ENDIF
10     CONTINUE
*** Define 'boundary values' of ALFA and D.
       ALFA=0
       D=0
*** Solve the set of linear equations determining the C's.
       Q(1)=-ALFA/2.0
       U(1)=D/2.0
       DO 20 K=2,N-1
       ALFA=(X(K+1)-X(K))/(X(K+1)-X(K-1))
       BETA=1.0-ALFA
       D=6.0*((Y(K+1)-Y(K))/(X(K+1)-X(K))-(Y(K)-Y(K-1))/(X(K)-X(K-1)))/
     -      (X(K+1)-X(K-1))
       P=BETA*Q(K-1)+2
       Q(K)=-ALFA/P
       U(K)=(D-BETA*U(K-1))/P
20     CONTINUE
*** Set the C's starting from the last one.
       C(N)=0
       DO 30 K=N-1,1,-1
       C(K)=Q(K)*C(K+1)+U(K)
30     CONTINUE
       END
