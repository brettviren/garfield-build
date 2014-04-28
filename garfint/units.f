CDECK  ID>, UNITS.
       SUBROUTINE UNITS(XIN,UIN,XOUT,UOUT,IFAIL)
*-----------------------------------------------------------------------
*   UNITS - Converts units.
*   Ref: http://www.bipm.org/utils/common/pdf/si_brochure_8_en.pdf
*   (Last changed on 23/ 9/09.)
*-----------------------------------------------------------------------
       implicit none
       REAL XIN,XOUT
       CHARACTER*(*) UIN,UOUT
       INTEGER IFAIL,INPCMX
       EXTERNAL INPCMX
*** Preset the output and failure flag.
       XOUT=0
       IFAIL=1
*** If this is a pressure unit.
       IF(INPCMX(UIN,'ATM#OSPHERE')+INPCMX(UIN,'BAR')+
     -      INPCMX(UIN,'MBAR')+INPCMX(UIN,'M#ILLI-BAR')+
     -      INPCMX(UIN,'TORR#ICELLI')+INPCMX(UIN,'MM-HG')+
     -      INPCMX(UIN,'INCH-HG')+INPCMX(UIN,'PA#SCAL')+
     -      INPCMX(UIN,'HPA#SCAL')+INPCMX(UIN,'H#ECTO-PA#SCAL')+
     -      INPCMX(UIN,'KPA#SCAL')+INPCMX(UIN,'K#ILO-PA#SCAL')+
     -      INPCMX(UIN,'N/M2').NE.0)THEN
*   Convert all incoming units to atmospheres.
            IF(INPCMX(UIN,'ATM#OSPHERE').NE.0)THEN
                 XOUT=XIN/1
            ELSEIF(INPCMX(UIN,'BAR').NE.0)THEN
                 XOUT=XIN/1.01325
            ELSEIF(INPCMX(UIN,'TORR#ICELLI')+
     -           INPCMX(UIN,'MM-HG').NE.0)THEN
                 XOUT=XIN/760
            ELSEIF(INPCMX(UIN,'INCH-HG').NE.0)THEN
                 XOUT=XIN/29.9213
            ELSEIF(INPCMX(UIN,'PA#SCAL')+INPCMX(UIN,'N/M2').NE.0)THEN
                 XOUT=XIN/101325
            ELSEIF(INPCMX(UIN,'HPA#SCAL')+
     -           INPCMX(UIN,'H#ECTO-PA#SCAL')+
     -           INPCMX(UIN,'MBAR')+
     -           INPCMX(UIN,'M#ILLI-BAR').NE.0)THEN
                 XOUT=XIN/1013.25
            ELSEIF(INPCMX(UIN,'KPA#SCAL')+
     -           INPCMX(UIN,'K#ILO-PA#SCAL').NE.0)THEN
                 XOUT=XIN/101.325
            ELSE
                 PRINT *,' !!!!!! UNITS  WARNING : Incoming unit ',
     -                UIN,' not recognised.'
                 XOUT=0
                 IFAIL=1
                 RETURN
            ENDIF
*   Convert atmospheres to the desired unit.
            IF(INPCMX(UOUT,'ATM#OSPHERE').NE.0)THEN
                 XOUT=XOUT*1
            ELSEIF(INPCMX(UOUT,'BAR').NE.0)THEN
                 XOUT=XOUT*1.01325
            ELSEIF(INPCMX(UOUT,'TORR#ICELLI')+
     -           INPCMX(UOUT,'MM-HG').NE.0)THEN
                 XOUT=XOUT*760
            ELSEIF(INPCMX(UOUT,'INCH-HG').NE.0)THEN
                 XOUT=XOUT*29.9213
            ELSEIF(INPCMX(UOUT,'PA#SCAL')+INPCMX(UOUT,'N/M2').NE.0)THEN
                 XOUT=XOUT*101325
            ELSEIF(INPCMX(UOUT,'HPA#SCAL')+
     -           INPCMX(UOUT,'H#ECTO-PA#SCAL')+
     -           INPCMX(UOUT,'MBAR')+
     -           INPCMX(UOUT,'M#ILLI-BAR').NE.0)THEN
                 XOUT=XOUT*1013.25
            ELSEIF(INPCMX(UOUT,'KPA#SCAL')+
     -           INPCMX(UOUT,'K#ILO-PA#SCAL').NE.0)THEN
                 XOUT=XOUT*101.325
            ELSE
                 PRINT *,' !!!!!! UNITS  WARNING : Unit mismatch, ',
     -                UIN,' is a pressure while ',UOUT,' is not.'
                 XOUT=0
                 IFAIL=1
                 RETURN
            ENDIF
*** Temperature units.
       ELSEIF(INPCMX(UIN,'K#ELVIN')+INPCMX(UIN,'C#ELSIUS')+
     -      INPCMX(UIN,'F#AHRENHEIT')+INPCMX(UIN,'RA#NKINE')+
     -      INPCMX(UIN,'RE#AUMUR').NE.0)THEN
*   Convert all incoming units to Celsius.
            IF(INPCMX(UIN,'K#ELVIN').NE.0)THEN
                 XOUT=XIN-273.15
            ELSEIF(INPCMX(UIN,'C#ELSIUS').NE.0)THEN
                 XOUT=XIN
            ELSEIF(INPCMX(UIN,'F#AHRENHEIT').NE.0)THEN
                 XOUT=(XIN-32.0)*5.0/9.0
            ELSEIF(INPCMX(UIN,'RA#NKINE').NE.0)THEN
                 XOUT=(XIN-32.0-459.67)*5.0/9.0
            ELSEIF(INPCMX(UIN,'RE#AUMUR').NE.0)THEN
                 XOUT=XIN*5.0/4.0
            ELSE
                 PRINT *,' !!!!!! UNITS  WARNING : Incoming unit ',
     -                UIN,' not recognised.'
                 XOUT=0
                 IFAIL=1
                 RETURN
            ENDIF
*   Convert Celsius to the desired unit.
            IF(INPCMX(UOUT,'K#ELVIN').NE.0)THEN
                 XOUT=XOUT+273.15
            ELSEIF(INPCMX(UOUT,'C#ELSIUS').NE.0)THEN
                 XOUT=XOUT
            ELSEIF(INPCMX(UOUT,'F#AHRENHEIT').NE.0)THEN
                 XOUT=XOUT*9.0/5.0+32.0
            ELSEIF(INPCMX(UOUT,'RA#NKINE').NE.0)THEN
                 XOUT=XOUT*9.0/5.0+32.0+459.67
            ELSEIF(INPCMX(UOUT,'RE#AUMUR').NE.0)THEN
                 XOUT=XOUT*4.0/5.0
            ELSE
                 PRINT *,' !!!!!! UNITS  WARNING : Unit mismatch, ',
     -                UIN,' is a temperature while ',UOUT,' is not.'
                 XOUT=0
                 IFAIL=1
                 RETURN
            ENDIF
*** Other units.
       ELSE
            PRINT *,' !!!!!! UNITS  WARNING : Incoming unit ',UIN,
     -           ' is not known.'
            XOUT=0
            IFAIL=1
            RETURN
       ENDIF
*** Seems to have worked.
       IFAIL=0
       END
