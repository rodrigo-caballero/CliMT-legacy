#include <misc.h>
#include <params.h>
      subroutine vqsatd(t       ,p       ,es      ,qs      ,gam      ,
     $                  len     )
C-----------------------------------------------------------------------
C
C Utility procedure to look up and return saturation vapor pressure from 
C precomputed table, calculate and return saturation specific humidity 
C (g/g), and calculate and return gamma (l/cp)*(d(qsat)/dT).  The same
C function as qsatd, but operates on vectors of temperature and pressure
C
C----------------------------Code History-------------------------------
C
C Original version:  J. Hack
C Standardized:      J. Rosinski, June 1992
C                    T. Acker, March 1996
C Reviewed:          J. Hack, August 1992
C
C-----------------------------------------------------------------------
c
c $Id: vqsatd.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      integer len       ! vector length
      real t(len)       ! temperature
      real p(len)       ! pressure
C 
C Output arguments
C
      real es(len)   ! saturation vapor pressure
      real qs(len)   ! saturation specific humidity
      real gam(len)  ! (l/cp)*(d(qs)/dt)
C
C--------------------------Local Variables------------------------------
C
      logical lflg   ! true if in temperature transition region
c
      integer i      ! index for vector calculations
c
      real omeps     ! 1. - 0.622
      real trinv     ! reciprocal of ttrice (transition range)
      real tc        ! temperature (in degrees C)
      real weight    ! weight for es transition from water to ice
      real hltalt    ! appropriately modified hlat for T derivatives  
c
      real hlatsb    ! hlat weighted in transition region
      real hlatvp    ! hlat modified for t changes above 273.16
      real tterm     ! account for d(es)/dT in transition region
      real desdt     ! d(es)/dT
C
C-----------------------------------------------------------------------
#include <eslookup.h>
C-----------------------------------------------------------------------
C
      omeps = 1.0 - epsqs
      do i=1,len
         es(i) = estblf(t(i))
C
C Saturation specific humidity
C
         qs(i) = epsqs*es(i)/(p(i) - omeps*es(i))
C
C The following check is to avoid the generation of negative
C values that can occur in the upper stratosphere and mesosphere
C
         qs(i) = min(1.0,qs(i))
C
         if (qs(i) .lt. 0.0) then
            qs(i) = 1.0
            es(i) = p(i)
         end if
      end do
C
C "generalized" analytic expression for t derivative of es
C accurate to within 1 percent for 173.16 < t < 373.16
C
      trinv = 0.0
      if ((.not. icephs) .or. (ttrice.eq.0.0)) go to 10
      trinv = 1.0/ttrice
      do i=1,len
C
C Weighting of hlat accounts for transition from water to ice
C polynomial expression approximates difference between es over
C water and es over ice from 0 to -ttrice (C) (min of ttrice is
C -40): required for accurate estimate of es derivative in transition 
C range from ice to water also accounting for change of hlatv with t 
C above 273.16 where const slope is given by -2369 j/(kg c) = cpv - cw
C
         tc     = t(i) - 273.16
         lflg   = (tc.ge.-ttrice .and. tc.lt.0.0)
         weight = min(-tc*trinv,1.0)
         hlatsb = hlatv + weight*hlatf
         hlatvp = hlatv - 2369.0*tc
         if (t(i).lt.273.16) then
            hltalt = hlatsb
         else
            hltalt = hlatvp
         end if
         if (lflg) then
            tterm = pcf(1) + tc*(pcf(2) + tc*(pcf(3) + tc*(pcf(4) +
     $              tc*pcf(5))))
         else
            tterm = 0.0
         end if
         desdt  = hltalt*es(i)/(rgasv*t(i)*t(i)) + tterm*trinv
         gam(i) = hltalt*qs(i)*p(i)*desdt/
     $           (cp*es(i)*(p(i) - omeps*es(i)))
         if(qs(i).eq.1.0) gam(i) = 0.0
      end do
      return
C
C No icephs or water to ice transition
C
   10 do i=1,len
C
C Account for change of hlatv with t above 273.16 where
C constant slope is given by -2369 j/(kg c) = cpv - cw
C
         hlatvp = hlatv - 2369.0*(t(i)-273.16)
         if (icephs) then
            hlatsb = hlatv + hlatf
         else
            hlatsb = hlatv
         end if
         if (t(i).lt.273.16) then
            hltalt = hlatsb
         else
            hltalt = hlatvp
         end if
         desdt  = hltalt*es(i)/(rgasv*t(i)*t(i))
         gam(i) = hltalt*qs(i)*p(i)*desdt/
     $            (cp*es(i)*(p(i) - omeps*es(i)))
         if (qs(i) .eq. 1.0) gam(i) = 0.0
      end do
C
      return
C
      end







 
