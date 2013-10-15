c
c $Id: eslookup.h,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c $Author: rca $
c
C
C Common block and statement functions for saturation vapor pressure
C look-up procedure, J. J. Hack, February 1990
C
      integer plenest  ! length of saturation vapor pressure table
      parameter (plenest=250)
C
C Table of saturation vapor pressure values es from tmin degrees
C to tmax+1 degrees k in one degree increments.  ttrice defines the
C transition region where es is a combination of ice & water values
C
      common/comes/estbl(plenest) ,tmin  ,tmax  ,ttrice ,pcf(6) ,
     $             epsqs          ,rgasv ,hlatf ,hlatv  ,cp     ,
     $             icephs
C
      real estbl      ! table values of saturation vapor pressure
      real tmin       ! min temperature (K) for table
      real tmax       ! max temperature (K) for table
      real ttrice     ! transition range from es over H2O to es over ice
      real pcf        ! polynomial coeffs -> es transition water to ice
      real epsqs      ! Ratio of h2o to dry air molecular weights 
      real rgasv      ! Gas constant for water vapor
      real hlatf      ! Latent heat of vaporization
      real hlatv      ! Latent heat of fusion
      real cp         ! specific heat of dry air
      logical icephs  ! false => saturation vapor press over water only
C
C Dummy variables for statement functions
C
      real td         ! dummy variable for function evaluation
      real tlim       ! intermediate variable for es look-up with estbl4
      real estblf     ! statement function es look-up
      real estbl4     ! statement function es look-up
C
C Statement functions used in saturation vapor pressure table lookup
C there are two ways to use these three statement functions.
C For compilers that do a simple in-line expansion:
C => ttemp = tlim(t)
C    es    = estbl4(ttemp)
C
C For compilers that provide real optimization:
C => es    = estblf(t)
C
      tlim(td) = max(min(td,tmax),tmin)
C
      estblf(td) =  (tmin + int(tlim(td)-tmin) - tlim(td) + 1.0)
     $            *estbl(int(tlim(td)-tmin)+1)
     $            -(tmin + int(tlim(td)-tmin) - tlim(td)      )
     $            *estbl(int(tlim(td)-tmin)+2)
C
      estbl4(td) =  (tmin+int(td-tmin)+1.0-td)*estbl(int(td-tmin)+1)
     $            + ( td-(tmin+int(td-tmin)) )*estbl(int(td-tmin)+2)
C
 
