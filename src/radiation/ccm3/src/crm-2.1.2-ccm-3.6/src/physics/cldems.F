#include <misc.h>
#include <params.h>
      subroutine cldems(clwp    ,fice    ,rei     ,emis    )
C-----------------------------------------------------------------------
C
C Compute cloud emissivity using cloud liquid water path (g/m**2)
C
C---------------------------Code history--------------------------------
C
C Original version:  J. Kiehl
C Standardized:      J. Rosinski, June 1992
C Reviewed:          J. Hack, J. Kiehl, August 1992
C Reviewed:          J. Kiehl, April 1996
C
C-----------------------------------------------------------------------
c
c $Id: cldems.F,v 1.1 2004/09/07 15:27:41 rca Exp $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <pmgrid.h>
      real kabsl                  ! longwave liquid absorption coeff (m**2/g)
      parameter (kabsl = 0.090361)
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real clwp(plond,plev)       ! cloud liquid water path (g/m**2)
      real rei(plond,plev)        ! ice effective drop size (microns)
      real fice(plond,plev)       ! fractional ice content within cloud
C
C Output arguments
C
      real emis(plond,plev)       ! cloud emissivity (fraction)
C
C---------------------------Local workspace-----------------------------
C
      integer i,k                 ! longitude, level indices
      real kabs                   ! longwave absorption coeff (m**2/g)
      real kabsi                  ! ice absorption coefficient
C
C-----------------------------------------------------------------------
C
      do k=1,plev
        do i=1,plon
          kabsi = 0.005 + 1./rei(i,k)
          kabs = kabsl*(1.-fice(i,k)) + kabsi*fice(i,k)
          emis(i,k) = 1. - exp(-1.66*kabs*clwp(i,k))
        end do
      end do
C
      return
      end
 
