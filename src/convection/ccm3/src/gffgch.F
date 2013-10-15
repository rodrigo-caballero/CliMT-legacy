#include <misc.h>
#include <params.h>
      subroutine gffgch(t       ,es      ,itype   )
C-----------------------------------------------------------------------
C
C Computes saturation vapor pressure over water and/or over ice using
C Goff & Gratch (1946) relationships.  T (temperature), and itype are
C input parameters, while es (saturation vapor pressure) is an output
C parameter.  The input parameter itype serves two purposes: a value of
C zero indicates that saturation vapor pressures over water are to be
C returned (regardless of temperature), while a value of one indicates
C that saturation vapor pressures over ice should be returned when t is
C less than 273.16 degrees k.  If itype is negative, its absolute value
C is interpreted to define a temperature transition region below 273.16
C degrees k in which the returned saturation vapor pressure is a
C weighted average of the respective ice and water value.  That is, in
C the temperature range 0 => -itype degrees c, the saturation vapor
C pressures are assumed to be a weighted average of the vapor pressure
C over supercooled water and ice (all water at 0 c; all ice at -itype
C c).  Maximum transition range => 40 c
C
C---------------------------Code history--------------------------------
C
C Original version:  J. Hack
C Standardized:      L. Buja, Jun 1992,  Feb 1996
C Reviewed:          J. Hack, G. Taylor, Aug 1992
C                    J. Hack, Feb 1996 
C
C-----------------------------------------------------------------------
c
c $Id: gffgch.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real t          ! Temperature
      integer itype   ! Flag for ice phase and associated transition
C
C Output arguments
C
      real es         ! Saturation vapor pressure
C
C---------------------------Local variables-----------------------------
C
      real e1         ! Intermediate scratch variable for es over water
      real e2         ! Intermediate scratch variable for es over water
      real eswtr      ! Saturation vapor pressure over water
      real f          ! Intermediate scratch variable for es over water
      real f1         ! Intermediate scratch variable for es over water
      real f2         ! Intermediate scratch variable for es over water
      real f3         ! Intermediate scratch variable for es over water
      real f4         ! Intermediate scratch variable for es over water
      real f5         ! Intermediate scratch variable for es over water
      real ps         ! Reference pressure (mb)
      real t0         ! Reference temperature (freezing point of water)
      real term1      ! Intermediate scratch variable for es over ice
      real term2      ! Intermediate scratch variable for es over ice
      real term3      ! Intermediate scratch variable for es over ice
      real tr         ! Transition range for es over water to es over ice
      real ts         ! Reference temperature (boiling point of water)
      real weight     ! Intermediate scratch variable for es transition
      integer itypo   ! Intermediate scratch variable for holding itype
C
C-----------------------------------------------------------------------
C      
C Check on whether there is to be a transition region for es
C
      if (itype.lt.0) then
        tr    = abs(float(itype))
        itypo = itype
        itype = 1
      else
        tr    = 0.0
        itypo = itype
      end if
      if (tr .gt. 40.0) then
        write(6,900) tr
        call endrun                ! Abnormal termination
      end if
C
      if(t .lt. (273.16 - tr) .and. itype.eq.1) go to 10
C
C Water
C
      ps = 1013.246
      ts = 373.16
      e1 = 11.344*(1.0 - t/ts)
      e2 = -3.49149*(ts/t - 1.0)
      f1 = -7.90298*(ts/t - 1.0)
      f2 = 5.02808*log10(ts/t)
      f3 = -1.3816*(10.0**e1 - 1.0)/10000000.0
      f4 = 8.1328*(10.0**e2 - 1.0)/1000.0
      f5 = log10(ps)
      f  = f1 + f2 + f3 + f4 + f5
      es = (10.0**f)*100.0
      eswtr = es
C
      if(t.ge.273.16 .or. itype.eq.0) go to 20
C
C Ice
C
   10 continue
      t0    = 273.16
      term1 = 2.01889049/(t0/t)
      term2 = 3.56654*log(t0/t)
      term3 = 20.947031*(t0/t)
      es    = 575.185606e10*exp(-(term1 + term2 + term3))
C
      if (t.lt.(273.16 - tr)) go to 20
C
C Weighted transition between water and ice
C
      weight = min((273.16 - t)/tr,1.0)
      es = weight*es + (1.0 - weight)*eswtr
C
   20 continue
      itype = itypo
      return
C
  900 format('GFFGCH: FATAL ERROR ******************************',/,
     $       'TRANSITION RANGE FOR WATER TO ICE SATURATION VAPOR',
     $       ' PRESSURE, TR, EXCEEDS MAXIMUM ALLOWABLE VALUE OF',
     $       ' 40.0 DEGREES C',/, ' TR = ',f7.2)
C
      end
 
 
