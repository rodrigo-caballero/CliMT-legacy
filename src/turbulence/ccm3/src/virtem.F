      subroutine virtem(t       ,q       ,zvir    ,tv      )
C-----------------------------------------------------------------------
C
C Compute the virtual temperature.
C
C---------------------------Code history--------------------------------
C
C Original version:  B. Boville
C Standardized:      J. Rosinski, June 1992
C Reviewed:          D. Williamson, J. Hack, August 1992
C Reviewed:          D. Williamson, March 1996
C
C-----------------------------------------------------------------------
c
c $Id: virtem.F,v 1.1 2005/01/24 19:21:44 rca Exp $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <params.h>
#include <pmgrid.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real t(plond,plev)       ! temperature
      real q(plond,plev)       ! specific humidity
      real zvir                ! virtual temperature constant
C
C Output arguments
C
      real tv(plond,plev)      ! virtual temperature
C
C---------------------------Local storage-------------------------------
C
      integer i,k              ! longitude and level indexes
C
      do k=1,plev
        do i=1,plon
          tv(i,k) = t(i,k)*(1.0 + zvir*q(i,k))
        end do
      end do
C

      end
 
