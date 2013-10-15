#include <misc.h>
#include <params.h>
      subroutine zmid(pstarln ,pmln    ,rair    ,gravit  ,tv      ,
     $                z       )
C-----------------------------------------------------------------------
C
C Compute the geopotential height *ABOVE THE SURFACE* at layer
C midpoints from the virtual temperatures and pressures.
C
C---------------------------Code history--------------------------------
C
C Original version:  B. Boville, D. Williamson, Jan 1990
C Standardized:      L. Buja, Jun 1992, Feb 1996
C Reviewed:          J. Hack, B. Boville, Aug 1992, Apr 1996
C
C-----------------------------------------------------------------------
c
c $Id: zmid.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <pmgrid.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real pstarln(plond)        ! Log surface pressures
      real pmln(plond,plev)      ! Log midpoint pressures
      real rair                  ! Gas constant for dry air
      real gravit                ! Acceleration of gravity
      real tv(plond,plev)        ! Virtual temperature
C
C Output arguments
C
      real z(plond,plev)         ! Height above surface at midpoints
C
C---------------------------Local variables-----------------------------
C
      integer i,k,l              ! Lon, level, level indices
      real rog                   ! Rair / gravit
C
C-----------------------------------------------------------------------
C
C Diagonal term of hydrostatic equation
C
      rog = rair/gravit
      do k=1,plev-1
        do i=1,plon
          z(i,k) = rog*tv(i,k)*0.5*(pmln(i,k+1) - pmln(i,k))
        end do
      end do
      do i=1,plon
        z(i,plev) = rog*tv(i,plev)*(pstarln(i) - pmln(i,plev))
      end do
C
C Bottom level term of hydrostatic equation
C
      do  k=1,plev-1
        do i=1,plon
          z(i,k) = z(i,k) + rog*tv(i,plev)*(pstarln(i) -
     $                      0.5*(pmln(i,plev-1) + pmln(i,plev)))
        end do
      end do
C
C Interior terms of hydrostatic equation
C
      do k=1,plev-2
        do l=k+1,plev-1
          do i=1,plon
            z(i,k) = z(i,k) + rog*(tv(i,l)) *
     $                        0.5*(pmln(i,l+1) - pmln(i,l-1))
          end do
        end do
      end do
C
      return
      end
 
