CDECK  ID>, GQWKCA.
       SUBROUTINE GQWKCA(IWKID,IERR,ICAT)
*-----------------------------------------------------------------------
*   GQWKCA - Returns the workstation category.
*   (Last changed on  5/ 2/97.)
*-----------------------------------------------------------------------
       INTEGER IWKID,IERR,ICAT
*** Initial values.
       ICAT=0
       IERR=0
*** No output.
       IF(IWKID.EQ.-1)THEN
            ICAT=2
*** Described in higzwindows.dat
       ELSEIF(IWKID.GE.0.AND.IWKID.LE.10)THEN
            ICAT=2
*** Falco.
       ELSEIF(IWKID.GE.7878)THEN
            ICAT=2
*** xterm.
       ELSEIF(IWKID.GE.7879)THEN
            ICAT=2
*** Various PS formats.
       ELSEIF(IWKID.EQ. -111.OR.IWKID.EQ.   -112.OR.
     -      IWKID.EQ.  -3111.OR.IWKID.EQ.  -3112.OR.
     -      IWKID.EQ. -99111.OR.IWKID.EQ. -99112.OR.
     -      IWKID.EQ.-100111.OR.IWKID.EQ.-100112.OR.
     -      IWKID.EQ.-200111.OR.IWKID.EQ.-200112.OR.
     -      IWKID.EQ.-300111.OR.IWKID.EQ.-300112.OR.
     -      IWKID.EQ.-300111.OR.IWKID.EQ.-300112)THEN
            ICAT=4
*** EPS format.
       ELSEIF(IWKID.EQ.-113)THEN
            ICAT=4
*** LaTeX format.
       ELSEIF(IWKID.EQ.-777)THEN
            ICAT=4
*** Other values are not known.
       ELSE
            IERR=1
       ENDIF
       END
