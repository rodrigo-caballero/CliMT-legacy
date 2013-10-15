#include <misc.h>
#include <params.h>
      subroutine zenith(calday  ,clat    , clon   ,coszrs  )
C-----------------------------------------------------------------------
C
C Compute cosine of solar zenith angle for albedo and radiation 
C   computations.
C
C---------------------------Code history--------------------------------
C
C Original version:  J. Rosinski, May 1994
C Standardized:      L. Buja,     Feb 1996
C Reviewed:          J. Kiehl,    Apr 1996    
C 
C-----------------------------------------------------------------------
c
c $Id: zenith.F,v 1.1 2004/09/07 02:47:29 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <pmgrid.h>
C------------------------------Commons----------------------------------
#include <crdcon.h>
C-----------------------------------------------------------------------
#include <comsol.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real calday              ! Calendar day, including fraction
      real clat                ! Current centered latitude (radians)
      real clon(plon)          ! Centered longitude (radians)
C
C Output arguments
C
      real coszrs(plond)       ! Cosine solar zenith angle
C
C---------------------------Local variables-----------------------------
C
      integer i     ! Longitude loop index
      real delta    ! Solar declination angle  in radians
      real eccf     ! Earth orbit eccentricity factor
C
C--------------------------Statement Function---------------------------
#include <orb_cosz.h>
C-----------------------------------------------------------------------
C
      call orb_decl (calday  ,eccen     ,mvelpp  ,lambm0  ,obliqr  ,
     $            delta   ,eccf)
C
C Compute local cosine solar zenith angle,
C
      do i=1,plon
        coszrs(i) = orb_cosz( calday, clat, clon(i), delta )
      end do
C
      return
      end
