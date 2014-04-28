CDECK  ID>, GASMXD.
       SUBROUTINE GASMXD(E,PATH,ELOSS)
*-----------------------------------------------------------------------
*   GASMXD - Returns literature values for the gas mixtures.
*   ORIGIN:     Data taken from a program written by Fabio Sauli and
*               Anna Peisert, apparently based on Schultz & Gresser.
*               Data for Krypton, Argon and ammonia from Wircha.
*   (Last changed on 16/11/93.)
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
*-----------------------------------------------------------------------
*   GMXDAT - Common block for gas mixing.
*   (Last changed on 20/ 2/97.)
*-----------------------------------------------------------------------
       REAL BREAK,FRAC,XLOSCH,EFLD,ESTEP,ECRIT
       INTEGER NBREAK
       COMMON /GMXDAT/ BREAK(MXLIST),FRAC(MXFRAC),XLOSCH,
     -      EFLD,ESTEP,ECRIT,NBREAK
       REAL DIVDIF,ELOSS,SIGMA(MXFRAC),ELVECT(MXFRAC)
       EXTERNAL DIVDIF
*** Gas data.
       REAL ENEON(31),SNEON(31)
       REAL EHEL(37),SHEL(37)
       REAL ENITR(24),SNITR(24)
       DATA ENEON/ 0.03 , 0.04 , 0.05 , 0.06 , 0.07 , 0.08 , 0.09 ,
     -             0.1  , 0.12 , 0.15 , 0.18 , 0.2  , 0.25 , 0.3  ,
     -             0.4  , 0.5  , 0.6  , 0.7  , 0.8  , 0.9  , 1.0  ,
     -             1.2  , 1.5  , 1.8  , 2.0  , 2.5  , 3.0  , 4.0  ,
     -             5.0  , 6.0  , 7.0  /
       DATA SNEON/ 0.469, 0.504, 0.536, 0.566, 0.601, 0.636, 0.669,
     -             0.701, 0.754, 0.828, 0.893, 0.930, 1.018, 1.091,
     -             1.225, 1.321, 1.402, 1.472, 1.528, 1.580, 1.619,
     -             1.685, 1.753, 1.793, 1.815, 1.860, 1.906, 1.984,
     -             2.070, 2.144, 2.213/
       DATA EHEL / 0.008, 0.009, 0.01 , 0.013, 0.017, 0.02 , 0.025,
     -             0.03 , 0.04 , 0.05 , 0.06 , 0.07 , 0.08 , 0.09 ,
     -             0.1  , 0.12 , 0.15 , 0.18 , 0.2  , 0.25 , 0.3  ,
     -             0.4  , 0.5  , 0.6  , 0.7  , 0.8  , 0.9  , 1.0  ,
     -             1.2  , 1.5  , 1.8  , 2.0  , 2.5  , 3.0  , 4.0  ,
     -             5.0  , 6.0  /
       DATA SHEL / 5.18 , 5.19 , 5.21 , 5.26 , 5.31 , 5.35 , 5.41 ,
     -             5.46 , 5.54 , 5.62 , 5.68 , 5.74 , 5.79 , 5.83 ,
     -             5.86 , 5.94 , 6.04 , 6.12 , 6.16 , 6.27 , 6.35 ,
     -             6.49 , 6.59 , 6.66 , 6.73 , 6.77 , 6.82 , 6.85 ,
     -             6.91 , 6.96 , 6.98 , 6.99 , 6.96 , 6.89 , 6.6  ,
     -             6.26 , 6.01 /
       DATA ENITR/0.0016,0.0036,0.0064,0.0103,0.0221, 0.040,0.0651,
     -             0.103, 0.15 , 0.332, 1.0  , 1.2  , 1.4  , 1.6  ,
     -             1.8  , 2.0  , 2.6  , 3.0  , 3.6  , 4.5  ,10.0  ,
     -            20.0  ,35.0  ,40.0  /
       DATA SNITR/ 1.43 , 1.69 , 1.94 , 2.2  , 2.94 , 3.86 , 4.9  ,
     -             6.04 , 7.12 , 9.34 , 9.98 ,10.51 ,11.45 ,12.9  ,
     -            16.95 ,24.01 ,29.88 ,21.63 ,14.66 ,11.52 , 9.51 ,
     -            12.0  ,10.5  ,10.1  /
*** Exceptions.
       IF(E.LE.0.0)THEN
            PATH=0.0
            ELOSS=0.0
            RETURN
       ENDIF
*** Argon.
       IF(FRAC(1).GT.0.0)THEN
            IF(E.LT.0.3)THEN
                 SIGMA(1)=1.46E-17+1.24E-12*(0.3-E)**6.5
            ELSEIF(E.LT.1.15)THEN
                 SIGMA(1)=1.46E-17+1.9E-16*(E-0.3)**2
            ELSEIF(E.LT.11.5)THEN
                 SIGMA(1)=1.52E-15*E/11.5
            ELSE
                 SIGMA(1)=1.52E-15/SQRT(E/11.5)
            ENDIF
            ELVECT(1)=2.746E-5
       ENDIF
*** Methane.
       IF(FRAC(2).GT.0.0)THEN
            IF(E.LE.0.3)THEN
                 SIGMA(2)=5.77E-17/SQRT(E)
            ELSEIF(E.LE.2.0)THEN
                 SIGMA(2)=4.468E-16*E**1.2
            ELSEIF(E.LE.8.)THEN
                 SIGMA(2)=7.258E-16*SQRT(E)
            ELSE
                 SIGMA(2)=2.05E-15/SQRT(E/8)
            ENDIF
            IF(E.LT.0.36)THEN
                 ELVECT(2)=8.35E-5+5.E-17/SIGMA(2)
            ELSE
                 ELVECT(2)=8.35E-5+1.8E-17/(E*SIGMA(2))
            ENDIF
       ENDIF
*** Neon.
       IF(FRAC(3).GT.0.0)THEN
            IF(E.LT.7)THEN
                 SIGMA(3)=1.0E-16*DIVDIF(SNEON,ENEON,31,E,2)
            ELSE
                 SIGMA(3)=1.0E-16*DIVDIF(SNEON,ENEON,31,E,1)
            ENDIF
            ELVECT(3)=5.44E-5
       ENDIF
*** Isobutane.
       IF(FRAC(4).GT.0.0)THEN
            IF(E.LT.0.20)THEN
                 SIGMA(4)=0.72E-15*SQRT(0.20/E)
            ELSEIF(E.LT.0.60)THEN
                 SIGMA(4)=0.72E-15*(E/0.20)**0.3347
            ELSEIF(E.LT.8.00)THEN
                 SIGMA(4)=(1.04E-15*(8-E)+4.8E-15*(E-0.60))/7.4
            ELSE
                 SIGMA(4)=4.8E-15/SQRT(E/8)
            ENDIF
            IF(E.LT.0.36)THEN
                 ELVECT(4)=8.E-17/SIGMA(4)+1.89E-5
            ELSE
                 ELVECT(4)=1.89E-5+2.88E-17/(E*SIGMA(4))
            ENDIF
       ENDIF
*** CO2.
       IF(FRAC(5).GT.0.0)THEN
            IF(E.LT.0.2)THEN
                 SIGMA(5)=1.7E-15*(1/E)**0.5
            ELSEIF(E.LT.1.32)THEN
                 SIGMA(5)=7.6E-16/E
            ELSEIF(E.LT.3.25)THEN
                 SIGMA(5)=1.E-15-3.66E-16*(3.25-E)**0.222
            ELSEIF(E.LT.4.2)THEN
                 SIGMA(5)=1.53E-15-5.9E-16*(4.2-E)**2.09
            ELSEIF(E.LT.6.0)THEN
                 SIGMA(5)=1.53E-15-4.84E-16*(E-4.2)**0.528
            ELSEIF(E.LT.25.)THEN
                 SIGMA(5)=1.6E-15-1.75E-18*(25-E)**2.05
            ELSE
                 SIGMA(5)=1.6E-15-2.94E-18*(E-25)**1.3
            ENDIF
            IF(E.LT.0.2)THEN
                 ELVECT(5)=2.493E-5+4E-16/SIGMA(5)
            ELSE
                 ELVECT(5)=2.493E-5+0.35E-16/(E**0.25*SIGMA(5))
            ENDIF
       ENDIF
*** Helium.
       IF(FRAC(6).GT.0.0)THEN
            SIGMA(6)=1.0E-16*DIVDIF(SHEL,EHEL,37,E,2)
            ELVECT(6)=27.4E-5
       ENDIF
*** Ethane.
       IF(FRAC(7).GT.0.0)THEN
            IF(E.LT.0.025)THEN
                 SIGMA(7)=25.E-16*(0.025/E)**0.7005
            ELSEIF(E.LT.0.035)THEN
                 SIGMA(7)=3.5E-16*(0.035/E)**5.844
            ELSEIF(E.LT.0.07)THEN
                 SIGMA(7)=1.9E-16*(0.07/E)**0.881
            ELSEIF(E.LT.0.09)THEN
                 SIGMA(7)=1.8E-16*(0.09/E)**0.215
            ELSEIF(E.LT.0.2)THEN
                 SIGMA(7)=1.8E-16*(E/0.09)**1.147
            ELSEIF(E.LT.0.3)THEN
                 SIGMA(7)=4.5E-16*(E/0.2)**0.583
            ELSEIF(E.LT.0.6)THEN
                 SIGMA(7)=5.7E-16*(E/0.3)**0.811
            ELSEIF(E.LT.1.)THEN
                 SIGMA(7)=10.E-16*(E/0.6)**0.514
            ELSE
                 SIGMA(7)=13.E-16
            ENDIF
            IF(E.LT.0.36)THEN
                 ELVECT(7)=3.648E-5+6.E-17/SIGMA(7)
            ELSE
                 ELVECT(7)=3.648E-5+2.16E-17/(E*SIGMA(7))
            ENDIF
       ENDIF
*** Nitrogen.
       IF(FRAC(8).GT.0.0)THEN
            SIGMA(8)=1.0E-16*DIVDIF(SNITR,ENITR,24,E,2)
            IF(E.LT.1.3)THEN
                 ELVECT(8)=3.5E-19/SIGMA(8)+3.90E-5
            ELSEIF(E.LT.1.4)THEN
                 ELVECT(8)=2.0E-18/SIGMA(8)+3.90E-5
            ELSEIF(E.LT.1.5)THEN
                 ELVECT(8)=4.0E-18/SIGMA(8)+3.90E-5
            ELSEIF(E.LT.1.6)THEN
                 ELVECT(8)=8.0E-18/SIGMA(8)+3.90E-5
            ELSEIF(E.LT.1.7)THEN
                 ELVECT(8)=1.0E-17/SIGMA(8)+3.90E-5
            ELSEIF(E.LT.1.8)THEN
                 ELVECT(8)=1.5E-17/SIGMA(8)+3.90E-5
            ELSEIF(E.LT.1.9)THEN
                 ELVECT(8)=2.0E-17/SIGMA(8)+3.90E-5
            ELSEIF(E.LT.2.0)THEN
                 ELVECT(8)=7.0E-17/SIGMA(8)+3.90E-5
            ELSEIF(E.LT.5.0)THEN
                 ELVECT(8)=2.0E-16/SIGMA(8)+3.90E-5
            ELSE
                 ELVECT(8)=1.0E-15/(E*SIGMA(8))+3.90E-5
            ENDIF
       ENDIF
*** Xenon.
       IF(FRAC(9).GT.0.0)THEN
            IF(E.LT.0.010)THEN
                 SIGMA(9)=100.E-16*(0.01/E)**0.176
            ELSEIF(E.LT.0.035)THEN
                 SIGMA(9)=100.E-16*(0.01/E)**0.308
            ELSEIF(E.LT.0.1)THEN
                 SIGMA(9)=68.E-16*(0.035/E)**1.166
            ELSEIF(E.LT.0.18)THEN
                 SIGMA(9)=20.E-16*(0.1/E)**1.179
            ELSEIF(E.LT.0.5)THEN
                 SIGMA(9)=10.E-16*(0.18/E)**1.997
            ELSEIF(E.LT.0.7)THEN
                 SIGMA(9)=1.3E-16*(0.5/E)**0.238
            ELSEIF(E.LT.2.0)THEN
                 SIGMA(9)=1.2E-16*(E/0.70)**2.019
            ELSEIF(E.LT.4.1)THEN
                 SIGMA(9)=10.E-16*(E/2.)**1.823
            ELSEIF(E.LT.10.0)THEN
                 SIGMA(9)=37.E-16*(10/E)**0.69
            ELSE
                 SIGMA(9)=37.0E-16*(10/E)**0.69
            ENDIF
            ELVECT(9)=8.29E-6
       ENDIF
*** Methylal.
       IF(FRAC(10).GT.0.0)THEN
            IF(E.LE.2.0)THEN
                 SIGMA(10)=1.1E-15*SQRT(2/E)
            ELSEIF(E.LE.4.0)THEN
                 SIGMA(10)=(1.1E-15*(10-E)+1.8E-15*(E-2))/8
            ELSE
                 SIGMA(10)=1.275E-15*(E/4)**0.22
            ENDIF
            IF(E.LE.0.36)THEN
                 ELVECT(10)=1.444E-5+12.E-17/SIGMA(10)
            ELSE
                 ELVECT(10)=1.444E-5+4.32E-17/(E*SIGMA(10))
            ENDIF
       ENDIF
*** Krypton, Wircha typing mistake corrected.
       IF(FRAC(11).GT.0.0)THEN
            IF(E.LE.0.01)THEN
                 SIGMA(11)=28.0
            ELSEIF(E.LE.0.02)THEN
                 SIGMA(11)=10.0**(0.4763-0.4854*LOG10(E))
            ELSEIF(E.LE.0.04)THEN
                 SIGMA(11)=10.0**(0.2451-0.6215*LOG10(E))
            ELSEIF(E.LE.0.07)THEN
                 SIGMA(11)=10.0**(0.195-0.6571*LOG10(E))
            ELSEIF(E.LE.0.1)THEN
                 SIGMA(11)=10.0**(-0.05-0.8696*LOG10(E))
            ELSEIF(E.LE.0.145)THEN
                 SIGMA(11)=10.0**(-0.2112-1.0307*LOG10(E))
            ELSEIF(E.LE.0.2)THEN
                 SIGMA(11)=10.0**(-0.679-1.5885*LOG10(E))
            ELSEIF(E.LE.0.3)THEN
                 SIGMA(11)=10.0**(-1.2808-2.4497*LOG10(E))
            ELSEIF(E.LE.0.4)THEN
                 SIGMA(11)=10.0**(-0.9284-1.7757*LOG10(E))
            ELSEIF(E.LE.0.5)THEN
                 SIGMA(11)=10.0**(-0.547-0.8171*LOG10(E))
            ELSEIF(E.LE.0.6)THEN
                 SIGMA(11)=10.0**(-0.301)
            ELSEIF(E.LE.0.8)THEN
                 SIGMA(11)=10.0**(-0.2708+0.1363*LOG10(E))
            ELSEIF(E.LE.1.0)THEN
                 SIGMA(11)=10.0**(-0.2007+0.8599*LOG10(E))
            ELSEIF(E.LE.2.0)THEN
                 SIGMA(11)=10.0**(-0.2006+1.8041*LOG10(E))
            ELSEIF(E.LE.3.0)THEN
                 SIGMA(11)=10.0**(-0.2521+1.975*LOG10(E))
            ELSEIF(E.LE.4.0)THEN
                 SIGMA(11)=10.0**(-0.1019+1.6603*LOG10(E))
            ELSEIF(E.LE.5.0)THEN
                 SIGMA(11)=10.0**(+0.1299+1.275*LOG10(E))
            ELSEIF(E.LE.7.0)THEN
                 SIGMA(11)=10.0**(0.28025+1.06004*LOG10(E))
            ELSEIF(E.LE.10.0)THEN
                 SIGMA(11)=10.0**(0.3789+0.9433*LOG10(E))
            ELSE
                 SIGMA(11)=21.0
            ENDIF
            SIGMA(11)=SIGMA(11)*1E-16
            ELVECT(11)=1.309E-5
       ENDIF
*** Ammonia (NH3).
       IF(FRAC(12).GT.0.0)THEN
            IF(E.LE.0.01)THEN
                 SIGMA(12)=1600.
            ELSEIF(E.LE.0.02)THEN
                 SIGMA(12)=10.0**(1.5439-0.83007*LOG10(E))
            ELSEIF(E.LE.0.04)THEN
                 SIGMA(12)=10.0**(1.4135-0.9069*LOG10(E))
            ELSEIF(E.LE.0.1)THEN
                 SIGMA(12)=10.0**(1.0051-1.199*LOG10(E))
            ELSEIF(E.LE.0.2)THEN
                 SIGMA(12)=10.0**(0.7891-1.415*LOG10(E))
            ELSEIF(E.LE.0.4)THEN
                 SIGMA(12)=10.0**(0.9729-1.152*LOG10(E))
            ELSEIF(E.GT.1.0)THEN
                 SIGMA(12)=10.0**(1.0414-0.98*LOG10(E))
            ELSEIF(E.GT.2.0)THEN
                 SIGMA(12)=10.0**(1.0414-1.081*LOG10(E))
            ELSEIF(E.GT.3.0)THEN
                 SIGMA(12)=10.0**(0.716)
            ELSEIF(E.GT.5.0)THEN
                 SIGMA(12)=10.0**(0.3615+0.74289*LOG10(E))
            ELSEIF(E.GT.7.0)THEN
                 SIGMA(12)=10.0**(0.4839+0.5678*LOG10(E))
            ELSEIF(E.GT.10.0)THEN
                 SIGMA(12)=10.0**(0.5404+0.501*LOG10(E))
            ELSE
                 SIGMA(12)=11.0
            ENDIF
            SIGMA(12)=SIGMA(12)*1E-16
            ELVECT(12)=6.442E-5
       ENDIF
*** Test gas.
       IF(FRAC(13).GT.0.0)THEN
            SIGMA(13)=1E-16
            ELVECT(13)=1E-5
       ENDIF
*** Take the sums.
       FRTOT=0.0
       CSTOT=0.0
       PRTOT=0.0
       DO 10 I=1,MXFRAC
       IF(FRAC(I).LE.0.0)GOTO 10
       FRTOT=FRTOT+FRAC(I)
       CSTOT=CSTOT+FRAC(I)*SIGMA(I)
       PRTOT=PRTOT+FRAC(I)*SIGMA(I)*ELVECT(I)
10     CONTINUE
*** Normalise, provided things are not zero.
       IF(FRTOT.NE.0.AND.CSTOT.NE.0)THEN
            CSTOT=CSTOT/FRTOT
            PRTOT=PRTOT/FRTOT
            ELOSS=PRTOT/CSTOT
            PATH=1/(XLOSCH*CSTOT)
       ELSE
            ELOSS=0.0
            PATH=0.0
       ENDIF
       END
