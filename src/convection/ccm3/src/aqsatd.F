#include <misc.h>
#include <params.h>
      subroutine aqsatd(t       ,p       ,es      ,qs      ,gam     ,
     $                  ii      ,ilen    ,kk      ,kstart  ,kend    )
C-----------------------------------------------------------------------
C
C Utility procedure to look up and return saturation vapor pressure from 
C precomputed table, calculate and return saturation specific humidity 
C (g/g).   Differs from aqsat by also calculating and returning
C  gamma (l/cp)*(d(qsat)/dT)
C Input arrays temperature and pressure (dimensioned ii,kk).
C
C---------------------------Code history--------------------------------
C
C Original version: J. Hack, Feb 1990
C Standardized:     L. Buja, Feb 1996
C Reviewed:         J. Hack, Aug 1992 
C                   J. Hack, P. Rasch, Apr 1996 
C
C-----------------------------------------------------------------------
c
c $Id: aqsatd.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      integer ii            ! I dimension of arrays t, p, es, qs
      integer kk            ! K dimension of arrays t, p, es, qs 
      real t(ii,kk)         ! Temperature
      real p(ii,kk)         ! Pressure
      integer ilen          ! Vector length in I direction
      integer kstart        ! Starting location in K direction
      integer kend          ! Ending location in K direction
C 
C Output arguments
C
      real es(ii,kk)        ! Saturation vapor pressure
      real qs(ii,kk)        ! Saturation specific humidity
      real gam(ii,kk)       ! (l/cp)*(d(qs)/dt)
C
C---------------------------Local workspace-----------------------------
C
      logical lflg          ! True if in temperature transition region
      integer i             ! i index for vector calculations
      integer k             ! k index 
      real omeps            ! 1. - 0.622
      real trinv            ! Reciprocal of ttrice (transition range)
      real tc               ! Temperature (in degrees C)
      real weight           ! Weight for es transition from water to ice
      real hltalt           ! Appropriately modified hlat for T derivatives
      real hlatsb           ! hlat weighted in transition region
      real hlatvp           ! hlat modified for t changes above 273.16
      real tterm            ! Account for d(es)/dT in transition region
      real desdt            ! d(es)/dT
C
C--------------------------Statement functions--------------------------
#include <eslookup.h>
C-----------------------------------------------------------------------
C
      omeps = 1.0 - epsqs
      do k=kstart,kend
        do i=1,ilen
          es(i,k) = estblf(t(i,k))
C
C Saturation specific humidity
C
          qs(i,k) = epsqs*es(i,k)/(p(i,k) - omeps*es(i,k))
C
C The following check is to avoid the generation of negative qs
C values which can occur in the upper stratosphere and mesosphere
C
          qs(i,k) = min(1.0,qs(i,k))
C
          if (qs(i,k) .lt. 0.0) then
            qs(i,k) = 1.0
            es(i,k) = p(i,k)
          end if
        end do
      end do
C
C "generalized" analytic expression for t derivative of es
C accurate to within 1 percent for 173.16 < t < 373.16
C
      trinv = 0.0
      if ((.not. icephs) .or. (ttrice.eq.0.0)) go to 10
      trinv = 1.0/ttrice
C
      do k=kstart,kend
        do i=1,ilen
C
C Weighting of hlat accounts for transition from water to ice
C polynomial expression approximates difference between es over
C water and es over ice from 0 to -ttrice (C) (min of ttrice is
C -40): required for accurate estimate of es derivative in transition
C range from ice to water also accounting for change of hlatv with t
C above 273.16 where constant slope is given by -2369 j/(kg c) =cpv - cw
C
          tc     = t(i,k) - 273.16
          lflg   = (tc.ge.-ttrice .and. tc.lt.0.0)
          weight = min(-tc*trinv,1.0)
          hlatsb = hlatv + weight*hlatf
          hlatvp = hlatv - 2369.0*tc
          if (t(i,k).lt.273.16) then
            hltalt = hlatsb
          else
            hltalt = hlatvp
          end if
          if (lflg) then
            tterm = pcf(1) +
     $              tc*(pcf(2) + tc*(pcf(3) + tc*(pcf(4) + tc*pcf(5))))
          else
            tterm = 0.0
          end if
          desdt    = hltalt*es(i,k)/(rgasv*t(i,k)*t(i,k)) + tterm*trinv
          gam(i,k) = hltalt*qs(i,k)*p(i,k)*desdt/
     $                 (cp*es(i,k)*(p(i,k) - omeps*es(i,k)))
          if(qs(i,k).eq.1.0) gam(i,k) = 0.0
        end do
      end do
C
      go to 20
C
C No icephs or water to ice transition
C
   10 do k=kstart,kend
        do i=1,ilen
C
C Account for change of hlatv with t above 273.16 where
C constant slope is given by -2369 j/(kg c) = cpv - cw
C
          hlatvp = hlatv - 2369.0*(t(i,k)-273.16)
          if (icephs) then
            hlatsb = hlatv + hlatf
          else
            hlatsb = hlatv
          end if
          if (t(i,k).lt.273.16) then
            hltalt = hlatsb
          else
            hltalt = hlatvp
          end if
          desdt    = hltalt*es(i,k)/(rgasv*t(i,k)*t(i,k))
          gam(i,k) = hltalt*qs(i,k)*p(i,k)*desdt/
     $                  (cp*es(i,k)*(p(i,k) - omeps*es(i,k)))
          if (qs(i,k) .eq. 1.0) gam(i,k) = 0.0
        end do
      end do
C
   20 return
      end
 
