      subroutine tsinti(tmeltx  ,latvapx ,rairx   ,stebolx )
C-----------------------------------------------------------------------
C
C Initialize surface temperature calculation constants
C
C---------------------------Code history--------------------------------
C
C Original version:  B. Briegleb
C Standardized:      L. Buja, Jun 1992, Feb 1996
C Reviewed:          B. Briegleb, J. Hack, Aug 1992, March 1996
C
C-----------------------------------------------------------------------
c
c $Id: tsinti.F,v 1.1 2005/01/24 19:21:44 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <params.h>
#include <pmgrid.h>
C------------------------------Commons----------------------------------
#include <comtsc.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real tmeltx         ! Melting temperature of snow and ice
      real latvapx        ! Latent heat of vaporization
      real rairx          ! Gas constant for dry air
      real stebolx        ! Stefan-Boltzmann constant
C
C-----------------------------------------------------------------------
C
      latice = 3.336e5    ! Latent heat of fusion at 0'C = 3.336e5 J/Kg
      tmelt  = tmeltx
      latvap = latvapx
      rair   = rairx
      stebol = stebolx
      snwedp = 10.0       ! 10:1 Snow:water equivalent depth factor
C
      end
 
