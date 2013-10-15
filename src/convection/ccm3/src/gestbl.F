#include <misc.h>
#include <params.h>
      subroutine gestbl(tmn     ,tmx     ,trice   ,ip      ,epsil   ,
     $                  latvap  ,latice  ,rh2o    ,cpair   )
C-----------------------------------------------------------------------
C
C Builds saturation vapor pressure table for later lookup procedure.
C Uses Goff & Gratch (1946) relationships to generate the table
C according to a set of free parameters defined below.  Auxiliary
C routines are also included for making rapid estimates (well with 1%)
C of both es and d(es)/dt for the particular table configuration.
C
C---------------------------Code history--------------------------------
C
C Original version:  J. Hack
C Standardized:      L. Buja, Jun 1992,  Feb 1996
C Reviewed:          J. Hack, G. Taylor, Aug 1992
C                    J. Hack, Aug 1992
C
C-----------------------------------------------------------------------
c
c $Id: gestbl.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C--------------------------Parameters-----------------------------------
#include <pmgrid.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real tmn           ! Minimum temperature entry in es lookup table
      real tmx           ! Maximum temperature entry in es lookup table
      real epsil         ! Ratio of h2o to dry air molecular weights
      real trice         ! Transition range from es over range to es over ice
      real latvap        ! Latent heat of vaporization
      real latice        ! Latent heat of fusion
      real rh2o          ! Gas constant for water vapor
      real cpair         ! Specific heat of dry air
C
C---------------------------Local variables-----------------------------
C
      real t             ! Temperature
      integer n          ! Increment counter
      integer lentbl     ! Calculated length of lookup table
      integer itype      ! Ice phase: 0 -> no ice phase
                         !            1 -> ice phase, no transition
                         !           -x -> ice phase, x degree transition
      logical ip         ! Ice phase logical flag
C
C---------------------------Statement function--------------------------
C
#include <eslookup.h>
C
C-----------------------------------------------------------------------
C
C Set es table parameters
C
      tmin   = tmn       ! Minimum temperature entry in table
      tmax   = tmx       ! Maximum temperature entry in table
      ttrice = trice     ! Trans. range from es over h2o to es over ice
      icephs = ip        ! Ice phase (true or false)
C
C Set physical constants required for es calculation
C
      epsqs  = epsil
      hlatv  = latvap
      hlatf  = latice
      rgasv  = rh2o
      cp     = cpair
C
      lentbl = ifix(tmax-tmin+2.000001)
      if (lentbl .gt. plenest) then
         write(6,9000) tmax, tmin, plenest
         call endrun    ! Abnormal termination
      end if
C
C Begin building es table.
C Check whether ice phase requested.
C If so, set appropriate transition range for temperature
C
      if (icephs) then
         if(ttrice.ne.0.0) then
            itype = -ttrice
         else
            itype = 1
         end if
      else
         itype = 0
      end if
C
      t = tmin - 1.0
      do n=1,lentbl
         t = t + 1.0
         call gffgch(t,estbl(n),itype)
      end do
C
      do n=lentbl+1,plenest
         estbl(n) = -99999.0
      end do
C
C Table complete -- Set coefficients for polynomial approximation of
C difference between saturation vapor press over water and saturation
C pressure over ice for -ttrice < t < 0 (degrees C). NOTE: polynomial
C is valid in the range -40 < t < 0 (degrees C).
C
C                  --- Degree 5 approximation ---
C
      pcf(1) =  5.04469588506e-01
      pcf(2) = -5.47288442819e+00
      pcf(3) = -3.67471858735e-01
      pcf(4) = -8.95963532403e-03
      pcf(5) = -7.78053686625e-05
C
C                  --- Degree 6 approximation ---
C
C-----pcf(1) =  7.63285250063e-02
C-----pcf(2) = -5.86048427932e+00
C-----pcf(3) = -4.38660831780e-01
C-----pcf(4) = -1.37898276415e-02
C-----pcf(5) = -2.14444472424e-04
C-----pcf(6) = -1.36639103771e-06
C
      if (masterproc) 
     $   write(6,*)' *** SATURATION VAPOR PRESSURE TABLE COMPLETED ***'
      return
C
 9000 format('GESTBL: FATAL ERROR *********************************',/,
     $     ' TMAX AND TMIN REQUIRE A LARGER DIMENSION ON THE LENGTH',
     $     ' OF THE SATURATION VAPOR PRESSURE TABLE ESTBL(PLENEST)',/,
     $     ' TMAX, TMIN, AND PLENEST => ', 2f7.2, i3)
C 
      end
 
