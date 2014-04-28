CDECK  ID>, ARGGET.
       subroutine argget(iarg,string,nc)
*-----------------------------------------------------------------------
*   ARGGET - Returns an argument with its length, for Unix systems only.
*   (Last changed on  4/ 6/92.)
*-----------------------------------------------------------------------
       character*(*) string
       call getarg(iarg,string)
       do i=len(string),1,-1
       if(string(i:i).ne.' ')then
            nc=i
            return
       endif
       enddo
       nc=0
       end
