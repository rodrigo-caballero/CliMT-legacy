#include <misc.h>
      subroutine freemem(address)
C-----------------------------------------------------------------------
C
C Interface routine to free dynamically allocated memory.  
C Replaces automatic arrays on Cray.  Isolates machine-specific code 
C for portable model.
C
C---------------------------Code history--------------------------------
C
C Original version:  J. Rosinski April, 1993
C                    T. Acker, March 1996
C
C-----------------------------------------------------------------------
c
c $Id: freemem.F,v 1.1 2004/09/07 15:27:25 rca Exp $
c $Author: rca $
c
C-----------------------------------------------------------------------
      implicit none
C-----------------------------Arguments---------------------------------
C
C Input
C
      pointer (address,space)   ! Pointer to dynamically allocated space
      real space
C
C--------------------------Local Variables------------------------------
C
#if ( defined CRAY ) || ( defined T3D ) || ( defined T3E )
      integer ier               ! Error return code
#endif
C
#if ( defined SUN ) || ( defined RS6K )
      integer free
      external free
#endif
C-----------------------------------------------------------------------
#if ( defined CRAY ) || ( defined T3D ) || ( defined T3E )
CMIC$ GUARD
      call hpdeallc(address,ier,0)
CMIC$ END GUARD
C
C write error message to std. output
C
      if (ier.ne.0) then
         write(6,*)'FREEMEM: Bad return code = ',ier,
     $             ' from hpdeallc: Abort'
         if (ier.eq.-3) then
            write(6,*) address,' is outside heap bounds'
         else if (ier.eq.-4) then
            write(6,*)'Block is already free'
         else if (ier.eq.-5) then
            write(6,*) address,' is not at the beginning of a block'
         else if (ier.eq.-7) then
            write(6,*)'Control word for the next block overwritten'
         end if
         call endrun
      end if
#endif
#if ( defined RS6K )
      address = free(%val(address))
#endif
#if ( defined SUN )
      address = free(address)
#endif
#if ( defined SGI ) || ( defined HP )
      call free(address)
#endif
C
      return
C
      end


 
