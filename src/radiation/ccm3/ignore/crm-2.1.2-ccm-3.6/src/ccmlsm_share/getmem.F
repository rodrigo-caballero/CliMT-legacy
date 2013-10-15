#include <misc.h>
#ifndef WORDSIZE
#define WORDSIZE 8
#endif
      subroutine getmem(calledby,nwords,address)
C-----------------------------------------------------------------------
C
C Interface routine for dynamic memory allocation.  Replaces automatic
C arrays on Cray.  Isolates machine-specific code for portable model.
C
C---------------------------Code history--------------------------------
C
C Original version:  J. Rosinski April, 1993
C                    T. Acker, March 1996
C
C-----------------------------------------------------------------------
c
c $Id: getmem.F,v 1.1 2004/09/07 15:27:25 rca Exp $
c $Author: rca $
c
C-----------------------------------------------------------------------
      implicit none
C-----------------------------------------------------------------------
#if ( defined SPMD )
#include <hpstats.h>
#endif
C-----------------------------Arguments---------------------------------
C
C Input 
C
      character*(*) calledby
      integer nwords            ! Number of machine words needed
C
C Output
C
      pointer (address,space)   ! Pointer to dynamically allocated space
      real space
C
C--------------------------Local Variables------------------------------
C
#if ( defined CRAY ) || ( defined T3D ) || ( defined T3E )
      integer ier               ! Error return code
#endif
#if ( defined SUN ) || ( defined RS6K )
      integer malloc
      external malloc
#endif
C
C-----------------------------------------------------------------------
C
#if ( defined CRAY ) || ( defined T3D ) || ( defined T3E )
CMIC$ GUARD
      call hpalloc(address,nwords,ier,0)
CMIC$ END GUARD
      if (ier.ne.0) then
         write(6,*)'GETMEM from ',calledby,': Bad return code = ',ier
         if (ier.eq.-1) then
            write(6,*) nwords,' is not between 1 and 2**31'
         else if (ier.eq.-2) then
            write(6,*)'No more memory available'
         end if
         call endrun
      end if
#endif
#if ( defined RS6K )
      address = malloc(%val(WORDSIZE*nwords))
      if (address.eq.0) then
         write(6,*)'GETMEM from ',calledby,' Cannot malloc ',nwords,
     $             ' words'
         call endrun
      end if
#endif
#if ( defined SUN ) || ( defined SGI ) || ( defined HP )
#if ( ! defined SPMD )
CCCC$DIR CRITICAL_SECTION
#endif
      address = malloc(WORDSIZE*nwords)
      if (address.eq.0) then
         write(6,*)'GETMEM from ',calledby,' Cannot malloc ',nwords,
     $             ' words'
         call endrun
      end if
#if ( ! defined SPMD )
CCCC$DIR END_CRITICAL_SECTION
#endif
#endif
#if ( defined SPMD )
      if (chkheap) then
        if (address.lt.smallest) smallest = address
        if (address.gt.biggest) biggest = address
c
c The following computation assumes byte-addressable machine
c
        cursize = (biggest - smallest) + nwords*8
        if (cursize.gt.maxsize) then
          maxsize = cursize
          write(6,*)' New heap maximum =',maxsize,
     $              ' bytes from ',calledby
        end if
      end if
c
c Initialize heap memory to Infinity
c
c      write(6,*)'First,last addresses from getmem:',address,
c     $          address + nwords*8
c      if (mod(address,8).ne.0) then
c        write(6,*)'GETMEM: called by ',calledby,' not on 8-byte bdy'
c        stop
c      end if
c commented out setbits call in SPMD branch...saves 10 sec/day
c on 32-nodes of SPP
c      call setbits(address,nwords)
#endif
C
      return
C
      end

 





