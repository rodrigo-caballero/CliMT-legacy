      subroutine qneg3(subnam  ,lat     ,q       )
C-----------------------------------------------------------------------
C
C Check moisture and tracers for minimum value, reset any below
C minimum value to minimum value and return information to allow
C warning message to be printed. The global average is NOT preserved.
C
C---------------------------Code history--------------------------------
C
C Original version:  J. Rosinski
C Standardized:      J. Rosinski, June 1992
C Reviewed:          D. Williamson, August 1992, March 1996
C
C-----------------------------------------------------------------------
c
c $Id: qneg3.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <pmgrid.h>
C------------------------------Commons----------------------------------
#include <comqmin.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      character*8 subnam       ! name of calling routine
      integer lat              ! latitude index
C
C Input/Output arguments
C
      real q(plond,plev,pcnst) ! moisture/tracer field
C
C---------------------------Local workspace-----------------------------
C
      integer indx(plond)      ! array of indices of points < qmin
      integer nval             ! number of points < qmin for 1 level
      integer nvals            ! number of values found < qmin
      integer i,ii,k           ! longitude, level indices
      integer m                ! constituent index
      integer iw,kw            ! i,k indices of worst violator

      logical found            ! true => at least 1 minimum violator found

      real worst               ! biggest violator
C
C-----------------------------------------------------------------------
C
      do m=1,pcnst
        nvals = 0
        found = .false.
        worst = 1.e35
C
C Test all field values for being less than minimum value. Set q = qmin
C for all such points. Trace offenders and identify worst one.
C
        do k=1,plev
          call whenflt(plon,q(1,k,m),1,qmin(m),indx,nval)
          if (nval.gt.0) then
            found = .true.
            nvals = nvals + nval
            do ii=1,nval
              i = indx(ii)
              if (q(i,k,m).lt.worst) then
                worst = q(i,k,m)
                kw = k
                iw = i
              end if
              q(i,k,m) = qmin(m)
            end do
          end if
        end do
        if (found) then
          write(6,9000)subnam,m,lat,nvals,qmin(m),worst,iw,kw
        end if
      end do
C
      return
 9000 format(' QNEG3 from ',a8,':m=',i3,' lat=',i3,
     $       ' Min. mixing ratio violated at ',i4,' points.  Reset to ',
     $       1p,e8.1,' Worst =',e8.1,' at i,k=',i4,i3)
      end
 
