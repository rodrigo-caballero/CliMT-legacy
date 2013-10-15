#include <misc.h>
      integer function intmax(n,ix,inc)
c
c $Id: intmax.F,v 1.2 2004/09/07 02:47:45 rca Exp $
c $Author: rca $
c
      implicit none
      integer n,inc
      integer ix(*)
c
      integer i,mx
c
      mx = ix(1)
      intmax = 1
      do i=1+inc,inc*n,inc
         if (ix(i).gt.mx) then
            mx = ix(i)
            intmax = i
         end if
      end do
      return
      end
 
