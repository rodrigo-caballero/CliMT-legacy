#include <misc.h>
#include <params.h>
      subroutine radtpl(tnm     ,lwupcgs  ,qnm     ,pnm     ,plh2o   ,
     $                  tplnka  ,s2c      ,s2t     ,w       ,tplnke  ,
     $                  tint    ,tint4    ,tlayr   ,tlayr4  ,pmln    ,
     $                  piln    )
C-----------------------------------------------------------------------
C
C Compute temperatures and path lengths for longwave radiation
C
C---------------------------Code history--------------------------------
C
C Original version:  CCM1
C Standardized:      L. Buja, June 1992
C Reviewed:          J. Kiehl, B. Briegleb, August 1992
C Reviewed:          J. Kiehl, April 1996
C Reviewed:          B. Briegleb, May 1996
C
C-----------------------------------------------------------------------
c
c $Id: radtpl.F,v 1.3 2008/02/02 13:10:48 rca Exp $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <prgrid.h>
C------------------------------Commons----------------------------------
#include <crdcon.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real tnm(plond,plev)   ! Model level temperatures
      real lwupcgs(plond)    ! Surface longwave up flux
      real qnm(plond,plev)   ! Model level specific humidity
      real pnm(plond,plevp)  ! Pressure at model interfaces (dynes/cm2)
      real plh2o(plond,plevp)! Pressure weighted h2o path
C
C Output arguments
C
      real tplnka(plond,plevp)  
! Level temperature from interface temperatures
      real s2c(plond,plevp)  ! H2o continuum path length
      real s2t(plond,plevp)  ! H2o tmp and prs wghtd path length
      real w(plond,plevp)    ! H2o path length
      real tplnke(plond)     ! Equal to tplnka
      real tint(plond,plevp) ! Layer interface temperature
      real tint4(plond,plevp)! Tint to the 4th power
      real tlayr(plond,plevp)! K-1 level temperature
      real tlayr4(plond,plevp)  ! Tlayr to the 4th power
      real pmln(plond,plev)  ! Ln(pmidm1)
      real piln(plond,plevp) ! Ln(pintm1)
C
C---------------------------Local variables-----------------------------
C
      integer i              ! Longitude index
      integer k              ! Level index

      real r296              ! Inverse stand temp for h2o continuum
      real repsil            ! Inver ratio mol weight h2o to dry air
      real dy                ! Thickness of layer for tmp interp
      real dpnm              ! Pressure thickness of layer
      real dpnmsq            ! Prs squared difference across layer
      real rtnm              ! Inverse level temperature
C
C-----------------------------------------------------------------------
C
      r296   = 1./296.
      repsil = 1./epsilo
C
C Set the top and bottom intermediate level temperatures,
C top level planck temperature and top layer temp**4.
C
C Tint is lower interface temperature
C (not available for bottom layer, so use ground temperature)
C
      do i=1,plon
        tint4(i,plevp) = lwupcgs(i)/stebol
        tint(i,plevp)  = sqrt(sqrt(tint4(i,plevp)))
        tplnka(i,1)    = tnm(i,1)
        tint(i,1)      = tplnka(i,1)
        tlayr4(i,1)    = tplnka(i,1)**4
        tint4(i,1)     = tlayr4(i,1)
      end do
C
C Intermediate level temperatures are computed using temperature
C at the full level below less dy*delta t,between the full level
C
      do k=2,plev
        do i=1,plon
          dy = (piln(i,k) - pmln(i,k))/(pmln(i,k-1) - pmln(i,k))
          tint(i,k)  = tnm(i,k) - dy*(tnm(i,k)-tnm(i,k-1))
          tint4(i,k) = tint(i,k)**4
        end do
      end do
C
C Now set the layer temp=full level temperatures and establish a
C planck temperature for absorption (tplnka) which is the average
C the intermediate level temperatures.  Note that tplnka is not
C equal to the full level temperatures.
C
      do k=2,plevp
        do i=1,plon
          tlayr(i,k)  = tnm(i,k-1)
          tlayr4(i,k) = tlayr(i,k)**4
          tplnka(i,k) = .5*(tint(i,k) + tint(i,k-1))
        end do
      end do
C
C Calculate tplank for emissivity calculation.
C Assume isothermal tplnke i.e. all levels=ttop.
C
      do i=1,plon
        tplnke(i)  = tplnka(i,1)
        tlayr(i,1) = tint(i,1)
      end do
C
C Now compute h2o path fields:
C
      do i=1,plon
        s2t(i,1) = plh2o(i,1) * tnm(i,1)
        w(i,1)   = sslp * (plh2o(i,1)*2.) / pnm(i,1)
        rtnm     = 1./tnm(i,1)
        s2c(i,1) = plh2o(i,1)*
     $           exp(1800.*(rtnm - r296))*qnm(i,1)*repsil
      end do
      do k=1,plev
        do i=1,plon
          dpnm       = pnm(i,k+1) - pnm(i,k)
          dpnmsq     = pnm(i,k+1)**2 - pnm(i,k)**2
          rtnm       = 1./tnm(i,k)
          s2t(i,k+1) = s2t(i,k) + rgsslp*dpnmsq*qnm(i,k)*tnm(i,k)
          w(i,k+1)   = w(i,k)   + rga*qnm(i,k)*dpnm
          s2c(i,k+1) = s2c(i,k) + rgsslp*dpnmsq*qnm(i,k)*
     $           exp(1800.*(rtnm - r296))*qnm(i,k)*repsil
        end do
      end do
C 
      return
      end
 
