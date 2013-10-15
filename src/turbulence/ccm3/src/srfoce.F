      subroutine srfoce(oro     ,ubot    ,vbot    ,tbot    ,qbot    ,
     $                  thbot   ,zbot    ,pmidm1  ,ts      ,qflx    ,
     $                  taux    ,tauy    ,shflx   ,lhflx   ,lwup    )
C-----------------------------------------------------------------------
C
C Compute ocean to atmosphere sensible/latent heat fluxes and 
C stress components
C 
C---------------------------Code history--------------------------------
C
C Original version:      CCM1
C Standardized:          L. Buja, Jun 1992, Feb 1996
C Split by surface type: L. Bath, Dec 1994
C Reviewed:              B. Briegleb, J. Hack, Aug 1992, March 1996
C
C-----------------------------------------------------------------------
c
c $Id: srfoce.F,v 1.1 2005/01/24 19:21:44 rca Exp $
c $Author: rca $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <params.h>
#include <pmgrid.h>
C------------------------------Commons----------------------------------
#include <compbl.h>
C-----------------------------------------------------------------------
#include <comtsc.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real oro(plond)           ! Land/ocean/seaice flag
      real ubot(plond)          ! Bottom level u wind
      real vbot(plond)          ! Bottom level v wind
      real tbot(plond)          ! Bottom level temperature
      real qbot(plond)          ! Bottom level specific humidity
      real thbot(plond)         ! Bottom level potential temperature
      real zbot(plond)          ! Bottom level height above surface
      real pmidm1(plond)        ! Bottom level pressure
      real ts(plond)            ! surface temperature (K)
C
C Output arguments
C
      real qflx(plond,pcnst)    ! Constituent flux (kg/m2/s)
      real taux(plond)          ! X surface stress (N/m2)
      real tauy(plond)          ! Y surface stress (N/m2)
      real shflx(plond)         ! Surface sensible heat flux (J/m2/s)
      real lhflx(plond)         ! Surface latent   heat flux (J/m2/s)
      real lwup(plond)          ! surface longwave up flux (W/m2)
C
C---------------------------Local variables-----------------------------
C
      integer i,ii              ! Longitude indices
      integer m                 ! constituent index
      integer indx(plond)       ! Longitude index array (land)
      integer npts              ! Number of land points

      real ltheat(plond)        ! Latent heat for given sfc conditions
C
C--------------------------Statement Functions--------------------------
C
      logical ocean
      ocean(i) = nint(oro(i)).eq.0
C
C-----------------------------------------------------------------------
C
C Set up index array of ocean surfaces
C
      npts = 0
      do i=1,plon
        if (ocean(i)) then
          npts = npts + 1
          indx(npts) = i
        end if
      end do
      if (npts.eq.0) return
C
C Determine latent heat
C
      do ii=1,npts
         i = indx(ii)
         ltheat(i) = latvap
      end do
C
C Compute surface fluxes, derivatives, and exchange coefficiants
C
      call flxoce(indx    ,npts    ,pmidm1  ,ubot    ,vbot      ,
     $            tbot    ,qbot    ,thbot   ,zbot    ,ts,
     $            ltheat  ,shflx   ,lhflx   ,taux    ,tauy      ,
     $            lwup    )

C
C Evaluate contituent fluxes
C
      do ii=1,npts
         i = indx(ii)
         qflx(i,1) = lhflx(i)/ltheat(i)
      end do
c
c Set non-water constituent fluxes to zero
c
c+++climt only one constituent in climt
c      do m=2,pcnst
c        do ii=1,npts
c          i = indx(ii)
c          qflx(i,m) = 0.
c        end do
c      end do
c---climt
C

      end
