CDECK  ID>, GERHND.
       SUBROUTINE GERHND(IERR,IFCT,IFIL)
*-----------------------------------------------------------------------
*   GERHND - Routine which is supposed to handle error conditions in
*            GKS. It outputs an error message to unit 10 and logs.
*   (Last changed on 19/ 3/92.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER IERR,IFCT,IFIL
       IF(IERR.GE.1.AND.IERR.LE.8)THEN
            WRITE(10,'('' ###### GERHND ERROR   : GKS is not in the'',
     -           '' proper state; please report (No '',I1,'').'')') IERR
       ELSEIF(IERR.EQ.21)THEN
            WRITE(10,'('' !!!!!! GERHND WARNING : The connection'',
     -           '' identifier you specified is not valid.'')')
       ELSEIF(IERR.EQ.23)THEN
            WRITE(10,'('' !!!!!! GERHND WARNING : Workstation type'',
     -           '' is not known to GKS; try using another.'')')
       ELSEIF(IERR.EQ.38)THEN
            WRITE(10,'('' !!!!!! GERHND WARNING : Workstation not of'',
     -           '' type INPUT or OUTIN; please report.'')')
       ELSEIF(IERR.EQ.51)THEN
            WRITE(10,'('' !!!!!! GERHND WARNING : Rectangle'',
     -           '' is not valid ; please report.'')')
       ELSEIF(IERR.EQ.78)THEN
            WRITE(10,'('' ###### GERHND ERROR   : Non-positive'',
     -           '' character height requested ; please report.'')')
       ELSEIF(IERR.EQ.92)THEN
            WRITE(10,'('' !!!!!! GERHND WARNING : Colour index is'',
     -           '' less than zero ; program bug - please report.'')')
       ELSEIF(IERR.EQ.93)THEN
            WRITE(10,'('' !!!!!! GERHND WARNING : Colour index is'',
     -           '' invalid ; program bug - please report.'')')
       ELSEIF(IERR.EQ.94)THEN
            WRITE(10,'('' !!!!!! GERHND WARNING : Representation of'',
     -           '' colour index not defined ; please report.'')')
       ELSEIF(IERR.EQ.95)THEN
            WRITE(10,'('' ###### GERHND ERROR   : Representation of'',
     -           '' colour index not predefined ; please report.'')')
       ELSEIF(IERR.EQ.96)THEN
            WRITE(10,'('' !!!!!! GERHND WARNING : Colour intensity'',
     -           '' RBG invalid ; program bug - please report.'')')
       ELSEIF(IERR.EQ.100)THEN
            WRITE(10,'('' ###### GERHND ERROR   : Invalid number of'',
     -           '' points in an output primitive; please report.'')')
       ELSEIF(IERR.EQ.101)THEN
            WRITE(10,'('' !!!!!! GERHND WARNING : Invalid character'',
     -           '' (perhaps a break) in a string ; please ignore.'')')
       ELSEIF(IERR.EQ.120)THEN
            WRITE(10,'('' !!!!!! GERHND WARNING : The segment name'',
     -           '' is not valid (program bug - please report).'')')
       ELSEIF(IERR.EQ.121)THEN
            WRITE(10,'('' !!!!!! GERHND WARNING : Segment name'',
     -           '' already in use (program bug - please report).'')')
       ELSEIF(IERR.EQ.122)THEN
            WRITE(10,'('' !!!!!! GERHND WARNING : The segment does'',
     -           '' not exist (program bug - please report).'')')
       ELSEIF(IERR.EQ.125)THEN
            WRITE(10,'('' !!!!!! GERHND WARNING : The segment is'',
     -           '' still open (program bug - please report).'')')
       ELSEIF(IERR.EQ.144)THEN
            WRITE(10,'('' !!!!!! GERHND WARNING : The prompt echo'',
     -           '' type is not supported by the workstation.'')')
       ELSEIF(IERR.EQ.147)THEN
            WRITE(10,'('' !!!!!! GERHND WARNING : Overflow in the'',
     -           '' input queue; probably of no importance.'')')
       ELSEIF(IERR.EQ.152)THEN
            WRITE(10,'('' !!!!!! GERHND WARNING : The initial value'',
     -           '' is out of range; probably of no importance.'')')
       ELSEIF(IERR.EQ.300)THEN
            WRITE(10,'('' !!!!!! GERHND WARNING : Unimplemented'',
     -           '' feature used; ignore, normal with mGKS.'')')
       ELSE
            WRITE(10,'('' !!!!!! GERHND WARNING : GKS error '',I6,
     -           '' detected; please report.'')') IERR
       ENDIF
       IF(IFCT.EQ.0)THEN
            WRITE(10,'(25X,''Applies to GOPKS (id '',I1,'').'')') IFCT
       ELSEIF(IFCT.EQ.1)THEN
            WRITE(10,'(25X,''Applies to GCLKS (id '',I1,'').'')') IFCT
       ELSEIF(IFCT.EQ.2)THEN
            WRITE(10,'(25X,''Applies to GOPWK (id '',I1,'').'')') IFCT
       ELSEIF(IFCT.EQ.3)THEN
            WRITE(10,'(25X,''Applies to GCLWK (id '',I1,'').'')') IFCT
       ELSEIF(IFCT.EQ.4)THEN
            WRITE(10,'(25X,''Applies to GACWK (id '',I1,'').'')') IFCT
       ELSEIF(IFCT.EQ.5)THEN
            WRITE(10,'(25X,''Applies to GDAWK (id '',I1,'').'')') IFCT
       ELSEIF(IFCT.EQ.6)THEN
            WRITE(10,'(25X,''Applies to GCLRWK (id '',I1,'').'')') IFCT
       ELSEIF(IFCT.EQ.8)THEN
            WRITE(10,'(25X,''Applies to GUWK (id '',I1,'').'')') IFCT
       ELSEIF(IFCT.EQ.12)THEN
            WRITE(10,'(25X,''Applies to GPL (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.13)THEN
            WRITE(10,'(25X,''Applies to GPM (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.14)THEN
            WRITE(10,'(25X,''Applies to GTX (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.15)THEN
            WRITE(10,'(25X,''Applies to GFA (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.19)THEN
            WRITE(10,'(25X,''Applies to GSLN (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.24)THEN
            WRITE(10,'(25X,''Applies to GSMKSC (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.28)THEN
            WRITE(10,'(25X,''Applies to GSCHXP (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.29)THEN
            WRITE(10,'(25X,''Applies to GSCHSP (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.31)THEN
            WRITE(10,'(25X,''Applies to GSCHH (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.33)THEN
            WRITE(10,'(25X,''Applies to GSTXP (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.41)THEN
            WRITE(10,'(25X,''Applies to GSASF (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.48)THEN
            WRITE(10,'(25X,''Applies to GSCR (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.49)THEN
            WRITE(10,'(25X,''Applies to GSWN (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.50)THEN
            WRITE(10,'(25X,''Applies to GSVP (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.56)THEN
            WRITE(10,'(25X,''Applies to GCRSG (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.57)THEN
            WRITE(10,'(25X,''Applies to GCLSG (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.59)THEN
            WRITE(10,'(25X,''Applies to GDSG (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.69)THEN
            WRITE(10,'(25X,''Applies to GINLC (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.70)THEN
            WRITE(10,'(25X,''Applies to GINSK (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.71)THEN
            WRITE(10,'(25X,''Applies to GINVL (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.72)THEN
            WRITE(10,'(25X,''Applies to GINCH (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.73)THEN
            WRITE(10,'(25X,''Applies to GINPK (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.74)THEN
            WRITE(10,'(25X,''Applies to GINST (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.86)THEN
            WRITE(10,'(25X,''Applies to GRQST (id '',I2,'').'')') IFCT
       ELSEIF(IFCT.EQ.107)THEN
            WRITE(10,'(25X,''Applies to GPREC (id '',I3,'').'')') IFCT
       ELSEIF(IFCT.EQ.525)THEN
            WRITE(10,'(25X,''Applies to GQCHXP (id '',I3,'').'')') IFCT
       ELSE
            WRITE(10,'(25X,''Applies to function '',I4,''.'')') IFCT
       ENDIF
C      CALL GERLOG(IERR,IFCT,IFIL)
       END
