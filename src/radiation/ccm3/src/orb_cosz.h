C-----------------------------------------------------------------------
C
C csm_share/orb_cosz.h
C
C	Statement function to return the cosine of the solar
C	zenith angle.  Assumes 365.0 days/year.
C
C------------------ Code History ---------------------------------------
C
C Original Author: Erik Kluzek
C Date:            Dec/97
C
C Version information:
C
C CVS: $Id: orb_cosz.h,v 1.3 2004/09/07 02:47:28 rca Exp $
C CVS: $Source: /Users/rca/cvsroot/CliMT/src/radiation/ccm3/src/orb_cosz.h,v $
C CVS: $Name:  $
C
C-----------------------------------------------------------------------
C ------------- Parameters ----------------------------------
      real orb_pi     ! pi, mathematical constant
      parameter( orb_pi = 3.14159265358979323846)
C ------------- Input Arguments -----------------------------
      real orb_jday   ! Julian calender day (1.xx to 365.xx)
      real orb_lat    ! Centered latitude (radians)
      real orb_lon    ! Centered longitude (radians)
      real orb_declin ! Solar declination (radians)
C ------------- Output --------------------------------------
      real orb_cosz   ! Cosine of the solar zenith angle
C ------------- Statement function --------------------------
      orb_cosz( orb_jday, orb_lat, orb_lon, orb_declin ) = 
     $   sin(orb_lat)*sin(orb_declin) - cos(orb_lat)*cos(orb_declin)
     $   *cos(orb_jday*2.0*orb_pi + orb_lon)
C
