C-----------------------------------------------------------------------
C
C	csm_share/orb.h
C
C	Parameters for use with orbital code.
C
C Original Author: Erik Kluzek
C Date:            Oct/97
C
C-----------------------------------------------------------------------
C
C Version information:
C
C CVS: $Id: orb.h,v 1.1 2004/09/07 15:27:32 rca Exp $
C CVS: $Source: /Users/rca/cvsroot/CliMT/src/radiation/ccm3/src/crm-2.1.2-ccm-3.6/src/csm_share/orb.h,v $
C CVS: $Name:  $
C
C-----------------------------------------------------------------------

      real       ORB_ECCEN_MIN      ! minimum value for eccen
      real       ORB_ECCEN_MAX      ! maximum value for eccen
      real       ORB_OBLIQ_MIN      ! minimum value for obliq
      real       ORB_OBLIQ_MAX      ! maximum value for obliq
      real       ORB_MVELP_MIN      ! minimum value for mvelp
      real       ORB_MVELP_MAX      ! maximum value for mvelp
      real       ORB_UNDEF_REAL     ! undefined/unset/invalid value
      real       ORB_UNDEF_INT      ! undefined/unset/invalid value
      real       ORB_DEFAULT        ! flag to use default orbit
      integer    ORB_NOT_YEAR_BASED ! flag to not use input year

      parameter (ORB_ECCEN_MIN  =   0.0      )
      parameter (ORB_ECCEN_MAX  =   0.1      )
      parameter (ORB_OBLIQ_MIN  = -90.0      )
      parameter (ORB_OBLIQ_MAX  = +90.0      )
      parameter (ORB_MVELP_MIN  =   0.0      )
      parameter (ORB_MVELP_MAX  = 360.0      )
      parameter (ORB_UNDEF_REAL = 1.e36      )
      parameter (ORB_DEFAULT    = ORB_UNDEF_REAL )
      parameter (ORB_UNDEF_INT  = 2000000000 )
      parameter (ORB_NOT_YEAR_BASED = ORB_UNDEF_INT )
