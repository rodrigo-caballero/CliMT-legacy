C
C	Common's to do with solar radiation
C
C	$Id: comsol.h,v 1.1 2004/09/07 15:27:41 rca Exp $
C
C Visible optical depth
C
      real tauvis     ! Visible optical depth

      common /comvis/ tauvis
C
C Solar constant
C
      real scon       ! Solar constant

      common /comsol/ scon
C
C Earth's orbital characteristics
C	
      real eccen       ! Earth's eccentricity factor (unitless) (typically 0 to 0.1)
      real obliq       ! Earth's obliquity angle (degree's) (-90 to +90) (typically 22-26)
      real mvelp       ! Earth's moving vernal equinox at perhelion (degree's) (0 to 360.0)
      integer iyear_AD ! Year (AD) to simulate above earth's orbital parameters for
C
C Orbital information after processed by orbit_params
C
      real
     $     obliqr,     ! Earth's obliquity in radians
     $     lambm0,     ! Mean longitude of perihelion at the 
C                      ! vernal equinox (radians)
     $     mvelpp      ! Earth's moving vernal equinox longitude
C                      ! of perihelion plus pi (radians)
C
      common /comorb/ eccen   , obliq   , mvelp   , obliqr  , 
     $                lambm0  , mvelpp  , iyear_AD

