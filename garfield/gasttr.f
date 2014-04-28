CDECK  ID>, GASTTR.
       SUBROUTINE GASTTR(MS,GS,ELOSS)
*-----------------------------------------------------------------------
*   GASTTR - Translates Magboltz state strings.
*   (Last changed on 10/ 6/13.)
*-----------------------------------------------------------------------
       implicit none
       CHARACTER*45 MS
       CHARACTER*(*) GS
       INTEGER IELOSS,IFAIL
       REAL ELOSS
*** Translation already done.
       IF(  MS.EQ.'HYDROGEN-TRIPLET'.OR.
     -      MS.EQ.'HYDROGEN-SINGLET'.OR.
     -      MS.EQ.'HYDROGEN-IONISATION'.OR.
     -      MS.EQ.'DEUTERIUM-TRIPLET'.OR.
     -      MS.EQ.'DEUTERIUM-SINGLET'.OR.
     -      MS.EQ.'DEUTERIUM-IONISATION'.OR.
     -      MS.EQ.'HELIUM-3-TRIPLET'.OR.
     -      MS.EQ.'HELIUM-3-SINGLET'.OR.
     -      MS.EQ.'HELIUM-3-IONISATION'.OR.
     -      MS.EQ.'HELIUM-3-ISOTROPIC-TRIPLET'.OR.
     -      MS.EQ.'HELIUM-3-ISOTROPIC-SINGLET'.OR.
     -      MS.EQ.'HELIUM-3-ISOTROPIC-IONISATION'.OR.
     -      MS.EQ.'HELIUM-4-TRIPLET'.OR.
     -      MS.EQ.'HELIUM-4-SINGLET'.OR.
     -      MS.EQ.'HELIUM-4-IONISATION'.OR.
     -      MS.EQ.'HELIUM-4-ISOTROPIC-TRIPLET'.OR.
     -      MS.EQ.'HELIUM-4-ISOTROPIC-SINGLET'.OR.
     -      MS.EQ.'HELIUM-4-ISOTROPIC-IONISATION'.OR.
     -      MS.EQ.'NEON-1S5'.OR.
     -      MS.EQ.'NEON-1S4'.OR.
     -      MS.EQ.'NEON-1S3'.OR.
     -      MS.EQ.'NEON-1S2'.OR.
     -      MS.EQ.'NEON-2P2-2P10'.OR.
     -      MS.EQ.'NEON-2P1'.OR.
     -      MS.EQ.'NEON-2S'.OR.
     -      MS.EQ.'NEON-3D-3S'.OR.
     -      MS.EQ.'NEON-3P'.OR.
     -      MS.EQ.'NEON-IONISATION'.OR.
     -      MS.EQ.'NEON-ISOTROPIC-1S5'.OR.
     -      MS.EQ.'NEON-ISOTROPIC-1S4'.OR.
     -      MS.EQ.'NEON-ISOTROPIC-1S3'.OR.
     -      MS.EQ.'NEON-ISOTROPIC-1S2'.OR.
     -      MS.EQ.'NEON-ISOTROPIC-2P2-2P10'.OR.
     -      MS.EQ.'NEON-ISOTROPIC-2P1'.OR.
     -      MS.EQ.'NEON-ISOTROPIC-2S'.OR.
     -      MS.EQ.'NEON-ISOTROPIC-3D-3S'.OR.
     -      MS.EQ.'NEON-ISOTROPIC-3P'.OR.
     -      MS.EQ.'NEON-ISOTROPIC-IONISATION'.OR.
     -      MS.EQ.'ARGON-S'.OR.
     -      MS.EQ.'ARGON-P'.OR.
     -      MS.EQ.'ARGON-D'.OR.
     -      MS.EQ.'ARGON-IONISATION'.OR.
     -      MS.EQ.'ARGON-ISOTROPIC-S'.OR.
     -      MS.EQ.'ARGON-ISOTROPIC-P'.OR.
     -      MS.EQ.'ARGON-ISOTROPIC-D'.OR.
     -      MS.EQ.'ARGON-ISOTROPIC-IONISATION'.OR.
     -      MS.EQ.'KRYPTON-S'.OR.
     -      MS.EQ.'KRYPTON-P'.OR.
     -      MS.EQ.'KRYPTON-D-P'.OR.
     -      MS.EQ.'KRYPTON-REST'.OR.
     -      MS.EQ.'KRYPTON-IONISATION'.OR.
     -      MS.EQ.'KRYPTON-ISOTROPIC-S'.OR.
     -      MS.EQ.'KRYPTON-ISOTROPIC-P'.OR.
     -      MS.EQ.'KRYPTON-ISOTROPIC-D-P'.OR.
     -      MS.EQ.'KRYPTON-ISOTROPIC-REST'.OR.
     -      MS.EQ.'KRYPTON-ISOTROPIC-IONISATION'.OR.
     -      MS.EQ.'XENON-1'.OR.
     -      MS.EQ.'XENON-2'.OR.
     -      MS.EQ.'XENON-3'.OR.
     -      MS.EQ.'XENON-4'.OR.
     -      MS.EQ.'XENON-IONISATION'.OR.
     -      MS.EQ.'XENON-ISOTROPIC-1'.OR.
     -      MS.EQ.'XENON-ISOTROPIC-2'.OR.
     -      MS.EQ.'XENON-ISOTROPIC-3'.OR.
     -      MS.EQ.'XENON-ISOTROPIC-4'.OR.
     -      MS.EQ.'XENON-ISOTROPIC-IONISATION'.OR.
     -      MS.EQ.'NITROGEN-TRIPLET-1'.OR.
     -      MS.EQ.'NITROGEN-TRIPLET-3'.OR.
     -      MS.EQ.'NITROGEN-TRIPLET-5'.OR.
     -      MS.EQ.'NITROGEN-SINGLET-2'.OR.
     -      MS.EQ.'NITROGEN-TRIPLET-7'.OR.
     -      MS.EQ.'NITROGEN-TRIPLET-8'.OR.
     -      MS.EQ.'NITROGEN-SINGLET-5'.OR.
     -      MS.EQ.'NITROGEN-IONISATION'.OR.
     -      MS.EQ.'NITROGEN-ISOTROPIC-TRIPLET-1'.OR.
     -      MS.EQ.'NITROGEN-ISOTROPIC-TRIPLET-3'.OR.
     -      MS.EQ.'NITROGEN-ISOTROPIC-TRIPLET-5'.OR.
     -      MS.EQ.'NITROGEN-ISOTROPIC-SINGLET-2'.OR.
     -      MS.EQ.'NITROGEN-ISOTROPIC-TRIPLET-7'.OR.
     -      MS.EQ.'NITROGEN-ISOTROPIC-TRIPLET-8'.OR.
     -      MS.EQ.'NITROGEN-ISOTROPIC-SINGLET-5'.OR.
     -      MS.EQ.'NITROGEN-ISOTROPIC-IONISATION'.OR.
     -      MS.EQ.'OXYGEN-A1'.OR.
     -      MS.EQ.'OXYGEN-B1'.OR.
     -      MS.EQ.'OXYGEN-C1-C3'.OR.
     -      MS.EQ.'OXYGEN-DISSOCIATION-A3'.OR.
     -      MS.EQ.'OXYGEN-DISSOCIATION-B3'.OR.
     -      MS.EQ.'OXYGEN-REST'.OR.
     -      MS.EQ.'OXYGEN-IONISATION'.OR.
     -      MS.EQ.'OZONE-CHAPPUIS'.OR.
     -      MS.EQ.'OZONE-HARTLEY'.OR.
     -      MS.EQ.'OZONE-REST'.OR.
     -      MS.EQ.'OZONE-IONISATION'.OR.
     -      MS.EQ.'FLUORINE-1'.OR.
     -      MS.EQ.'FLUORINE-2'.OR.
     -      MS.EQ.'FLUORINE-3'.OR.
     -      MS.EQ.'FLUORINE-4'.OR.
     -      MS.EQ.'FLUORINE-IONISATION'.OR.
     -      MS.EQ.'CAESIUM-P12'.OR.
     -      MS.EQ.'CAESIUM-P32'.OR.
     -      MS.EQ.'CAESIUM-D32-D52'.OR.
     -      MS.EQ.'CAESIUM-S12'.OR.
     -      MS.EQ.'CAESIUM-REST'.OR.
     -      MS.EQ.'CAESIUM-IONISATION'.OR.
     -      MS.EQ.'MERCURY-DIMER-SUPER'.OR.
     -      MS.EQ.'MERCURY-DIMER'.OR.
     -      MS.EQ.'MERCURY-3P0'.OR.
     -      MS.EQ.'MERCURY-3P1'.OR.
     -      MS.EQ.'MERCURY-3P2'.OR.
     -      MS.EQ.'MERCURY-1P1'.OR.
     -      MS.EQ.'MERCURY-1S0'.OR.
     -      MS.EQ.'MERCURY-REST'.OR.
     -      MS.EQ.'MERCURY-IONISATION'.OR.
     -      MS.EQ.'METHANE-DISSOCIATION-1'.OR.
     -      MS.EQ.'METHANE-DISSOCIATION-2'.OR.
     -      MS.EQ.'METHANE-DISSOCIATION-3'.OR.
     -      MS.EQ.'METHANE-DISSOCIATION-4'.OR.
     -      MS.EQ.'METHANE-IONISATION'.OR.
     -      MS.EQ.'ETHANE-1'.OR.
     -      MS.EQ.'ETHANE-2'.OR.
     -      MS.EQ.'ETHANE-3'.OR.
     -      MS.EQ.'ETHANE-IONISATION'.OR.
     -      MS.EQ.'PROPANE-1'.OR.
     -      MS.EQ.'PROPANE-2'.OR.
     -      MS.EQ.'PROPANE-3'.OR.
     -      MS.EQ.'PROPANE-IONISATION'.OR.
     -      MS.EQ.'ISOBUTANE-1'.OR.
     -      MS.EQ.'ISOBUTANE-2'.OR.
     -      MS.EQ.'ISOBUTANE-3'.OR.
     -      MS.EQ.'ISOBUTANE-IONISATION'.OR.
     -      MS.EQ.'CO2-1'.OR.
     -      MS.EQ.'CO2-2'.OR.
     -      MS.EQ.'CO2-3'.OR.
     -      MS.EQ.'CO2-4'.OR.
     -      MS.EQ.'CO2-5'.OR.
     -      MS.EQ.'CO2-6'.OR.
     -      MS.EQ.'CO2-IONISATION'.OR.
     -      MS.EQ.'METHYLAL-1'.OR.
     -      MS.EQ.'METHYLAL-2'.OR.
     -      MS.EQ.'METHYLAL-IONISATION'.OR.
     -      MS.EQ.'DME-1'.OR.
     -      MS.EQ.'DME-2'.OR.
     -      MS.EQ.'DME-IONISATION'.OR.
     -      MS.EQ.'CF4-DISSOCIATION'.OR.
     -      MS.EQ.'CF4-IONISATION'.OR.
     -      MS.EQ.'H2O-1'.OR.
     -      MS.EQ.'H2O-2'.OR.
     -      MS.EQ.'H2O-3'.OR.
     -      MS.EQ.'H2O-IONISATION'.OR.
     -      MS.EQ.'NITRIC-OXIDE-1'.OR.
     -      MS.EQ.'NITRIC-OXIDE-IONISATION'.OR.
     -      MS.EQ.'BF3-EXCITATION'.OR.
     -      MS.EQ.'BF3-IONISATION')THEN
            GS=MS
            RETURN
*** Look up the string.
       ELSEIF(MS.EQ.'H2 (2001)       EXC TRPLT     ELOSS=  8.85   ')THEN
            GS='HYDROGEN-TRIPLET'
       ELSEIF(MS.EQ.'H2 (2001)       EXC SNGLT     ELOSS= 12.0    ')THEN
            GS='HYDROGEN-SINGLET'
       ELSEIF(MS.EQ.'H2 (2001)       IONISATION    ELOSS= 15.427  ')THEN
            GS='HYDROGEN-IONISATION'
       ELSEIF(MS.EQ.'D2 (1998)       EXC TRPLT     ELOSS=  8.85   ')THEN
            GS='DEUTERIUM-TRIPLET'
       ELSEIF(MS.EQ.'D2 (1998)       EXC SNGLT     ELOSS= 12.0    ')THEN
            GS='DEUTERIUM-SINGLET'
       ELSEIF(MS.EQ.'D2 (1998)       IONISATION    ELOSS= 15.427  ')THEN
            GS='DEUTERIUM-IONISATION'
       ELSEIF(MS.EQ.'He3 (2002)      EXC TRPLT     ELOSS= 19.82   ')THEN
            GS='HELIUM-3-TRIPLET'
       ELSEIF(MS.EQ.'He3 (2002)      EXC SNGLT     ELOSS= 20.61   ')THEN
            GS='HELIUM-3-SINGLET'
       ELSEIF(MS.EQ.'He3 (2002)      IONISATION    ELOSS= 24.587  ')THEN
            GS='HELIUM-3-IONISATION'
       ELSEIF(MS.EQ.'He3 (1992)      EXC TRIPLET   ELOSS= 19.82   ')THEN
            GS='HELIUM-3-ISOTROPIC-TRIPLET'
       ELSEIF(MS.EQ.'He3 (1992)      EXC SINGLET   ELOSS= 20.6    ')THEN
            GS='HELIUM-3-ISOTROPIC-SINGLET'
       ELSEIF(MS.EQ.'He3 (1992)      IONISATION    ELOSS= 24.59   ')THEN
            GS='HELIUM-3-ISOTROPIC-IONISATION'
       ELSEIF(MS.EQ.'He4 (2002)      EXC TRPLT     ELOSS= 19.82   ')THEN
            GS='HELIUM-4-TRIPLET'
       ELSEIF(MS.EQ.'He4 (2002)      EXC SNGLT     ELOSS= 20.61   ')THEN
            GS='HELIUM-4-SINGLET'
       ELSEIF(MS.EQ.'He4 (2002)      IONISATION    ELOSS= 24.587  ')THEN
            GS='HELIUM-4-IONISATION'
       ELSEIF(MS.EQ.'He4 (1997)      EXC TRIPLET   ELOSS= 19.82   ')THEN
            GS='HELIUM-4-ISOTROPIC-TRIPLET'
       ELSEIF(MS.EQ.'He4 (1997)      EXC SINGLET   ELOSS= 20.61   ')THEN
            GS='HELIUM-4-ISOTROPIC-SINGLET'
       ELSEIF(MS.EQ.'He4 (1997)      IONISATION    ELOSS= 24.587  ')THEN
            GS='HELIUM-4-ISOTROPIC-IONISATION'
       ELSEIF(MS.EQ.'Ne (2003 anis.) EXC 1S5       ELOSS= 16.618  ')THEN
            GS='NEON-1S5'
       ELSEIF(MS.EQ.'Ne (2003 anis.) EXC 1S4       ELOSS= 16.670  ')THEN
            GS='NEON-1S4'
       ELSEIF(MS.EQ.'Ne (2003 anis.) EXC 1S3       ELOSS= 16.715  ')THEN
            GS='NEON-1S3'
       ELSEIF(MS.EQ.'Ne (2003 anis.) EXC 1S2       ELOSS= 16.857  ')THEN
            GS='NEON-1S2'
       ELSEIF(MS.EQ.'Ne (2003 anis.) EXCSUM 2P10-2 ELOSS= 18.381  ')THEN
            GS='NEON-2P2-2P10'
       ELSEIF(MS.EQ.'Ne (2003 anis.) EXC 2P1     ) ELOSS= 18.965  ')THEN
            GS='NEON-2P1'
       ELSEIF(MS.EQ.'Ne (2003 anis.) EXC SUM 2S    ELOSS= 19.663  ')THEN
            GS='NEON-2S'
       ELSEIF(MS.EQ.'Ne (2003 anis.) EXC 3D+3S     ELOSS= 20.033  ')THEN
            GS='NEON-3D-3S'
       ELSEIF(MS.EQ.'Ne (2003 anis.) EXC 3P        ELOSS= 20.200  ')THEN
            GS='NEON-3P'
       ELSEIF(MS.EQ.'Ne (2003 anis.) IONISATION    ELOSS= 21.56   ')THEN
            GS='NEON-IONISATION'
       ELSEIF(MS.EQ.'Ne (2003 iso.)  EXC 1S5       ELOSS= 16.618  ')THEN
            GS='NEON-ISOTROPIC-1S5'
       ELSEIF(MS.EQ.'Ne (2003 iso.)  EXC 1S4       ELOSS= 16.670  ')THEN
            GS='NEON-ISOTROPIC-1S4'
       ELSEIF(MS.EQ.'Ne (2003 iso.)  EXC 1S3       ELOSS= 16.715  ')THEN
            GS='NEON-ISOTROPIC-1S3'
       ELSEIF(MS.EQ.'Ne (2003 iso.)  EXC 1S2       ELOSS= 16.857  ')THEN
            GS='NEON-ISOTROPIC-1S2'
       ELSEIF(MS.EQ.'Ne (2003 iso.)  EXCSUM 2P10-2 ELOSS= 18.381  ')THEN
            GS='NEON-ISOTROPIC-2P2-2P10'
       ELSEIF(MS.EQ.'Ne (2003 iso.)  EXC 2P1     ) ELOSS= 18.965  ')THEN
            GS='NEON-ISOTROPIC-2P1'
       ELSEIF(MS.EQ.'Ne (2003 iso.)  EXC SUM 2S    ELOSS= 19.663  ')THEN
            GS='NEON-ISOTROPIC-2S'
       ELSEIF(MS.EQ.'Ne (2003 iso.)  EXC 3D+3S     ELOSS= 20.033  ')THEN
            GS='NEON-ISOTROPIC-3D-3S'
       ELSEIF(MS.EQ.'Ne (2003 iso.)  EXC 3P        ELOSS= 20.200  ')THEN
            GS='NEON-ISOTROPIC-3P'
       ELSEIF(MS.EQ.'Ne (2003 iso.)  IONISATION    ELOSS= 21.56   ')THEN
            GS='NEON-ISOTROPIC-IONISATION'
       ELSEIF(MS.EQ.'Ar (2002 anis.) EXC S-LEVELS  ELOSS= 11.55   ')THEN
            GS='ARGON-S'
       ELSEIF(MS.EQ.'Ar (2002 anis.) EXC P-LEVELS  ELOSS= 13.0    ')THEN
            GS='ARGON-P'
       ELSEIF(MS.EQ.'Ar (2002 anis.) EXC D-LEVELS  ELOSS= 14.0    ')THEN
            GS='ARGON-D'
       ELSEIF(MS.EQ.'Ar (2002 anis.) IONISATION    ELOSS= 15.7    ')THEN
            GS='ARGON-IONISATION'
       ELSEIF(MS.EQ.'Ar (2002 iso.)  EXC S-LEVELS  ELOSS= 11.55   ')THEN
            GS='ARGON-ISOTROPIC-S'
       ELSEIF(MS.EQ.'Ar (2002 iso.)  EXC P-LEVELS  ELOSS= 13.0    ')THEN
            GS='ARGON-ISOTROPIC-P'
       ELSEIF(MS.EQ.'Ar (2002 iso.)  EXC D-LEVELS  ELOSS= 14.O    ')THEN
            GS='ARGON-ISOTROPIC-D'
       ELSEIF(MS.EQ.'Ar (2002 iso.)  IONISATION    ELOSS= 15.7    ')THEN
            GS='ARGON-ISOTROPIC-IONISATION'
       ELSEIF(MS.EQ.'Kr (2002)       EXC S-LEVELS  ELOSS=  9.915  ')THEN
            GS='KRYPTON-S'
       ELSEIF(MS.EQ.'Kr (2002)       EXC P-LEVELS  ELOSS= 11.304  ')THEN
            GS='KRYPTON-P'
       ELSEIF(MS.EQ.'Kr (2002)       EXC D+P-LVLS  ELOSS= 11.998  ')THEN
            GS='KRYPTON-D-P'
       ELSEIF(MS.EQ.'Kr (2002)       EXC HIGHER    ELOSS= 12.75   ')THEN
            GS='KRYPTON-REST'
       ELSEIF(MS.EQ.'Kr (2002)       IONISATION    ELOSS= 13.996  ')THEN
            GS='KRYPTON-IONISATION'
       ELSEIF(MS.EQ.'Kr (2001)       EXC S-LEVELS  ELOSS=  9.915  ')THEN
            GS='KRYPTON-ISOTROPIC-S'
       ELSEIF(MS.EQ.'Kr (2001)       EXC P-LEVELS  ELOSS= 11.304  ')THEN
            GS='KRYPTON-ISOTROPIC-P'
       ELSEIF(MS.EQ.'Kr (2001)       EXC D+P-LVLS  ELOSS= 11.998  ')THEN
            GS='KRYPTON-ISOTROPIC-D-P'
       ELSEIF(MS.EQ.'Kr (2001)       EXC LVLS .GT. EL0SS= 12.75   ')THEN
            GS='KRYPTON-ISOTROPIC-REST'
       ELSEIF(MS.EQ.'Kr (2001)       IONISATION    ELOSS= 13.996  ')THEN
            GS='KRYPTON-ISOTROPIC-IONISATION'
       ELSEIF(MS.EQ.'Xe (2003 anis.) EXC           ELOSS=  8.315  ')THEN
            GS='XENON-1'
       ELSEIF(MS.EQ.'Xe (2003 anis.) EXC           ELOSS=  9.447  ')THEN
            GS='XENON-2'
       ELSEIF(MS.EQ.'Xe (2003 anis.) EXC           ELOSS=  9.917  ')THEN
            GS='XENON-3'
       ELSEIF(MS.EQ.'Xe (2003 anis.) EXC           ELOSS= 11.70   ')THEN
            GS='XENON-4'
       ELSEIF(MS.EQ.'Xe (2003 anis.) IONISATION    ELOSS= 12.13   ')THEN
            GS='XENON-IONISATION'
       ELSEIF(MS.EQ.'Xe (2003)       EXC           ELOSS=  8.315  ')THEN
            GS='XENON-ISOTROPIC-1'
       ELSEIF(MS.EQ.'Xe (2003)       EXC           ELOSS=  9.447  ')THEN
            GS='XENON-ISOTROPIC-2'
       ELSEIF(MS.EQ.'Xe (2003)       EXC           ELOSS=  9.917  ')THEN
            GS='XENON-ISOTROPIC-3'
       ELSEIF(MS.EQ.'Xe (2003)       EXC           ELOSS= 11.70   ')THEN
            GS='XENON-ISOTROPIC-4'
       ELSEIF(MS.EQ.'Xe (2003)       IONISATION    ELOSS= 12.13   ')THEN
            GS='XENON-ISOTROPIC-IONISATION'
       ELSEIF(MS.EQ.'N2 (2004 anis.) EXC TRPLT1    ELOSS=  6.17   ')THEN
            GS='NITROGEN-TRIPLET-1'
       ELSEIF(MS.EQ.'N2 (2004 anis.) EXC TRPLT3    ELOSS=  7.35   ')THEN
            GS='NITROGEN-TRIPLET-3'
       ELSEIF(MS.EQ.'N2 (2004 anis.) EXC TRPLT5    ELOSS=  7.80   ')THEN
            GS='NITROGEN-TRIPLET-5'
       ELSEIF(MS.EQ.'N2 (2004 anis.) EXC SNGLT2    ELOSS=  8.55   ')THEN
            GS='NITROGEN-SINGLET-2'
       ELSEIF(MS.EQ.'N2 (2004 anis.) EXC TRPLT7    ELOSS= 11.03   ')THEN
            GS='NITROGEN-TRIPLET-7'
       ELSEIF(MS.EQ.'N2 (2004 anis.) EXC TRPLT8    ELOSS= 11.87   ')THEN
            GS='NITROGEN-TRIPLET-8'
       ELSEIF(MS.EQ.'N2 (2004 anis.) EXC SNGLT5    ELOSS= 13.0    ')THEN
            GS='NITROGEN-SINGLET-5'
       ELSEIF(MS.EQ.'N2 (2004 anis.) IONISATION    ELOSS= 15.60   ')THEN
            GS='NITROGEN-IONISATION'
       ELSEIF(MS.EQ.'N2 (04 Phelps)  EXC TRPLT1    ELOSS=  6.17   ')THEN
            GS='NITROGEN-ISOTROPIC-TRIPLET-1'
       ELSEIF(MS.EQ.'N2 (04 Phelps)  EXC TRPLT3    ELOSS=  7.35   ')THEN
            GS='NITROGEN-ISOTROPIC-TRIPLET-3'
       ELSEIF(MS.EQ.'N2 (04 Phelps)  EXC TRPLT5    ELOSS=  7.80   ')THEN
            GS='NITROGEN-ISOTROPIC-TRIPLET-5'
       ELSEIF(MS.EQ.'N2 (04 Phelps)  EXC SNGLT2    ELOSS=  8.55   ')THEN
            GS='NITROGEN-ISOTROPIC-SINGLET-2'
       ELSEIF(MS.EQ.'N2 (04 Phelps)  EXC TRPLT7    ELOSS= 11.03   ')THEN
            GS='NITROGEN-ISOTROPIC-TRIPLET-7'
       ELSEIF(MS.EQ.'N2 (04 Phelps)  EXC TRPLT8    ELOSS= 11.87   ')THEN
            GS='NITROGEN-ISOTROPIC-TRIPLET-8'
       ELSEIF(MS.EQ.'N2 (04 Phelps)  EXC SNGLT5    ELOSS= 13.0    ')THEN
            GS='NITROGEN-ISOTROPIC-SINGLET-5'
       ELSEIF(MS.EQ.'N2 (04 Phelps)  IONISATION    ELOSS= 15.60   ')THEN
            GS='NITROGEN-ISOTROPIC-IONISATION'
       ELSEIF(MS.EQ.'O2 (2004)       EXC A1(DEL)G  ELOSS=  0.977  ')THEN
            GS='OXYGEN-A1'
       ELSEIF(MS.EQ.'O2 (2004)       EXC B1(SIG)G  ELOSS=  1.627  ')THEN
            GS='OXYGEN-B1'
       ELSEIF(MS.EQ.'O2 (2004)       EXC C1+C3     ELOSS=  4.50   ')THEN
            GS='OXYGEN-C1-C3'
       ELSEIF(MS.EQ.'O2 (2004)       EXC A3 DISOC  ELOSS=  6.10   ')THEN
            GS='OXYGEN-DISSOCIATION-A3'
       ELSEIF(MS.EQ.'O2 (2004)       EXC B3 DISOC  ELOSS=  8.40   ')THEN
            GS='OXYGEN-DISSOCIATION-B3'
       ELSEIF(MS.EQ.'O2 (2004)       EXC           ELOSS=  9.30   ')THEN
            GS='OXYGEN-REST'
       ELSEIF(MS.EQ.'O2 (2004)       IONISATION    ELOSS= 12.072  ')THEN
            GS='OXYGEN-IONISATION'
       ELSEIF(MS.EQ.'O3 (2002)       EXC CHAPPUIS  ELOSS=  1.50   ')THEN
            GS='OZONE-CHAPPUIS'
       ELSEIF(MS.EQ.'O3 (2002)       EXC HARTLEY   ELOSS=  4.85   ')THEN
            GS='OZONE-HARTLEY'
       ELSEIF(MS.EQ.'O3 (2002)       EXC           ELOSS=  9.00   ')THEN
            GS='OZONE-REST'
       ELSEIF(MS.EQ.'O3 (2002)       IONISATION    ELOSS= 12.75   ')THEN
            GS='OZONE-IONISATION'
       ELSEIF(MS.EQ.'F2 (Morgan)     EXC           ELOSS=  3.16   ')THEN
            GS='FLUORINE-1'
       ELSEIF(MS.EQ.'F2 (Morgan)     EXC           ELOSS=  4.34   ')THEN
            GS='FLUORINE-2'
       ELSEIF(MS.EQ.'F2 (Morgan)     EXC           ELOSS= 11.57   ')THEN
            GS='FLUORINE-3'
       ELSEIF(MS.EQ.'F2 (Morgan)     EXC           ELOSS= 13.08   ')THEN
            GS='FLUORINE-4'
       ELSEIF(MS.EQ.'F2 (Morgan)     IONISATION    ELOSS= 15.69   ')THEN
            GS='FLUORINE-IONISATION'
       ELSEIF(MS.EQ.'Cs (2001)       EXC P1/2      ELOSS=  1.3859 ')THEN
            GS='CAESIUM-P12'
       ELSEIF(MS.EQ.'Cs (2001)       EXC P3/2      ELOSS=  1.4546 ')THEN
            GS='CAESIUM-P32'
       ELSEIF(MS.EQ.'Cs (2001)       EXC D3/2+5/2  ELOSS=  1.7977 ')THEN
            GS='CAESIUM-D32-D52'
       ELSEIF(MS.EQ.'Cs (2001)       EXC S1/2      ELOSS=  2.2981 ')THEN
            GS='CAESIUM-S12'
       ELSEIF(MS.EQ.'Cs (2001)       EXC HIGHER    ELOSS=  2.6986 ')THEN
            GS='CAESIUM-REST'
       ELSEIF(MS.EQ.'Cs (2001)       IONISATION    ELOSS=  3.8926 ')THEN
            GS='CAESIUM-IONISATION'
       ELSEIF(MS.EQ.'Hg2 (2003)      EXC DIMER SUP ELOSS= -0.040  ')THEN
            GS='MERCURY-DIMER-SUPER'
       ELSEIF(MS.EQ.'Hg2 (2003)      EXC DIMER     ELOSS=  0.040  ')THEN
            GS='MERCURY-DIMER'
       ELSEIF(MS.EQ.'Hg2 (2003)      EXC 3P0       ELOSS=  4.667  ')THEN
            GS='MERCURY-3P0'
       ELSEIF(MS.EQ.'Hg2 (2003)      EXC 3P1       ELOSS=  4.887  ')THEN
            GS='MERCURY-3P1'
       ELSEIF(MS.EQ.'Hg2 (2003)      EXC 3P2       ELOSS=  5.461  ')THEN
            GS='MERCURY-3P2'
       ELSEIF(MS.EQ.'Hg2 (2003)      EXC 1P1       ELOSS=  6.704  ')THEN
            GS='MERCURY-1P1'
       ELSEIF(MS.EQ.'Hg2 (2003)      EXC 1S0       ELOSS=  7.926  ')THEN
            GS='MERCURY-1S0'
       ELSEIF(MS.EQ.'Hg2 (2003)      EXC HIGH      ELOSS=  8.60   ')THEN
            GS='MERCURY-REST'
       ELSEIF(MS.EQ.'Hg2 (2003)      IONISATION    ELOSS= 10.4375 ')THEN
            GS='MERCURY-IONISATION'
       ELSEIF(MS.EQ.'CH4 (2004)      EXC DISOCIATN ELOSS=  9.0    ')THEN
            GS='METHANE-DISSOCIATION-1'
       ELSEIF(MS.EQ.'CH4 (2004)      EXC DISOCIATN ELOSS= 10.0    ')THEN
            GS='METHANE-DISSOCIATION-2'
       ELSEIF(MS.EQ.'CH4 (2004)      EXC DISOCIATN ELOSS= 11.0    ')THEN
            GS='METHANE-DISSOCIATION-3'
       ELSEIF(MS.EQ.'CH4 (2004)      EXC DISOCIATN ELOSS= 11.8    ')THEN
            GS='METHANE-DISSOCIATION-4'
       ELSEIF(MS.EQ.'CH4 (2004)      IONISATION    ELOSS= 12.99   ')THEN
            GS='METHANE-IONISATION'
       ELSEIF(MS.EQ.'C2H6 (1999)     EXC           ELOSS=  8.2    ')THEN
            GS='ETHANE-1'
       ELSEIF(MS.EQ.'C2H6 (1999)     EXC           ELOSS= 10.3    ')THEN
            GS='ETHANE-2'
       ELSEIF(MS.EQ.'C2H6 (1999)     EXC           ELOSS= 17.0    ')THEN
            GS='ETHANE-3'
       ELSEIF(MS.EQ.'C2H6 (1999)     IONISATION    ELOSS= 11.52   ')THEN
            GS='ETHANE-IONISATION'
       ELSEIF(MS.EQ.'C3H8 (1999)     EXC           ELOSS=  7.70   ')THEN
            GS='PROPANE-1'
       ELSEIF(MS.EQ.'C3H8 (1999)     EXC           ELOSS= 10.0    ')THEN
            GS='PROPANE-2'
       ELSEIF(MS.EQ.'C3H8 (1999)     EXC           ELOSS= 17.0    ')THEN
            GS='PROPANE-3'
       ELSEIF(MS.EQ.'C3H8 (1999)     IONISATION    ELOSS=10.95    ')THEN
            GS='PROPANE-IONISATION'
       ELSEIF(MS.EQ.'iC4H10 (1999)   EXC           ELOSS=  7.4    ')THEN
            GS='ISOBUTANE-1'
       ELSEIF(MS.EQ.'iC4H10 (1999)   EXC           ELOSS=  9.70   ')THEN
            GS='ISOBUTANE-2'
       ELSEIF(MS.EQ.'iC4H10 (1999)   EXC           ELOSS= 17.0    ')THEN
            GS='ISOBUTANE-3'
       ELSEIF(MS.EQ.'iC4H10 (1999)   IONISATION    ELOSS= 10.67   ')THEN
            GS='ISOBUTANE-IONISATION'
       ELSEIF(MS.EQ.'CO2 (2004 iso.) EXC           ELOSS=  7.900  ')THEN
            GS='CO2-1'
       ELSEIF(MS.EQ.'CO2 (2004 iso.) EXC           ELOSS=  8.900  ')THEN
            GS='CO2-2'
       ELSEIF(MS.EQ.'CO2 (2004 iso.) EXC           ELOSS= 10.500  ')THEN
            GS='CO2-3'
       ELSEIF(MS.EQ.'CO2 (2004 iso.) EXC           ELOSS= 12.200  ')THEN
            GS='CO2-4'
       ELSEIF(MS.EQ.'CO2 (2004 iso.) EXC           ELOSS= 13.200  ')THEN
            GS='CO2-5'
       ELSEIF(MS.EQ.'CO2 (2004 iso.) EXC           ELOSS= 15.000  ')THEN
            GS='CO2-6'
       ELSEIF(MS.EQ.'CO2 (2004 iso.) IONISATION    ELOSS= 13.773  ')THEN
            GS='CO2-IONISATION'
       ELSEIF(MS.EQ.'Methylal (hot)  EXC           ELOSS=  6.3    ')THEN
            GS='METHYLAL-1'
       ELSEIF(MS.EQ.'Methylal (hot)  EXC           ELOSS=  8.3    ')THEN
            GS='METHYLAL-2'
       ELSEIF(MS.EQ.'Methylal (hot)  IONISATION    ELOSS= 10.0    ')THEN
            GS='METHYLAL-IONISATION'
       ELSEIF(MS.EQ.'DME (1998)      EXC           ELOSS=  7.70   ')THEN
            GS='DME-1'
       ELSEIF(MS.EQ.'DME (1998)      EXC           ELOSS=  8.50   ')THEN
            GS='DME-2'
       ELSEIF(MS.EQ.'DME (1998)      IONISATION    ELOSS= 10.04   ')THEN
            GS='DME-IONISATION'
       ELSEIF(MS.EQ.'CF4 (2001)      EXC DISOCIATN ELOSS= 12.5    ')THEN
            GS='CF4-DISSOCIATION'
       ELSEIF(MS.EQ.'CF4 (2001)      IONISATION    ELOSS= 15.90   ')THEN
            GS='CF4-IONISATION'
       ELSEIF(MS.EQ.'H2O (2004)      EXC           ELOSS=  4.20   ')THEN
            GS='H2O-1'
       ELSEIF(MS.EQ.'H2O (2004)      EXC           ELOSS=  7.40   ')THEN
            GS='H2O-2'
       ELSEIF(MS.EQ.'H2O (2004)      EXC           ELOSS= 13.1    ')THEN
            GS='H2O-3'
       ELSEIF(MS.EQ.'H2O (2004)      IONISATION    ELOSS= 12.61   ')THEN
            GS='H2O-IONISATION'
       ELSEIF(MS.EQ.'NO (1995)       EXC           ELOSS=  6.10   ')THEN
            GS='NITRIC-OXIDE-1'
       ELSEIF(MS.EQ.'NO (1995)       IONISATION    ELOSS=  9.2644 ')THEN
            GS='NITRIC-OXIDE-IONISATION'
       ELSEIF(MS.EQ.'N2O (2004)      EXC           ELOSS=  4.06   ')THEN
            GS='NITROUS-OXIDE-1'
       ELSEIF(MS.EQ.'N2O (2004)      EXC           ELOSS=  8.50   ')THEN
            GS='NITROUS-OXIDE-2'
       ELSEIF(MS.EQ.'N2O (2004)      EXC           ELOSS=  9.60   ')THEN
            GS='NITROUS-OXIDE-3'
       ELSEIF(MS.EQ.'N2O (2004)      IONISATION    ELOSS= 12.886  ')THEN
            GS='NITROUS-OXIDE-IONISATION'
       ELSEIF(MS.EQ.'BF3 (2001)      EXC           ELOSS= 10.0    ')THEN
            GS='BF3-EXCITATION'
       ELSEIF(MS.EQ.'BF3 (2001)      IONISATION    ELOSS= 15.56   ')THEN
            GS='BF3-IONISATION'
*   If not found, then use the Magboltz string directly.
       ELSE
            PRINT *,' !!!!!! GASTTR WARNING : Met an unknown state: '//
     -           MS//'; not translated - please report.'
            GS=MS
       ENDIF
*** Pick up the energy.
       IELOSS=INDEX(MS,'ELOSS=')
       IF(IELOSS.LE.0)THEN
            PRINT *,' !!!!!! GASTTR WARNING : Did not find an ELOSS'//
     -           ' field for level '//MS//'; set to -1 eV.'
            ELOSS=-1.0
       ELSEIF(IELOSS+6.GT.LEN(MS))THEN
            PRINT *,' !!!!!! GASTTR WARNING : Did not find an energy'//
     -           ' threshold for level '//MS//'; set to -1 eV.'
            ELOSS=-1.0
       ELSE
            CALL INPRRC(MS(IELOSS+6:),ELOSS,-1.0,IFAIL)
C            READ(MS(IELOSS+6:),'(F10.3)') ELOSS
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! GASTTR WARNING : Did not find an'//
     -                ' energy threshold for level '//MS//
     -                '; set to -1 eV.'
                 ELOSS=-1.0
            ENDIF
       ENDIF
       END
