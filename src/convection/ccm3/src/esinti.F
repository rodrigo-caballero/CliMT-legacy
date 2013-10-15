#include <misc.h>
#include <params.h>
      subroutine esinti(epslon  ,latvap  ,latice  ,rh2o    ,cpair   )
C-----------------------------------------------------------------------
C
C Initialize es lookup tables 
C
C---------------------------Code history--------------------------------
C
C Original version:  J. Hack
C Standardized:      L. Buja, Jun 1992, Feb 1996
C Reviewed:          J. Hack, G. Taylor, Aug 1992
C                    J. Hack, Feb 1996
C
C-----------------------------------------------------------------------
c
c $Id: esinti.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real epslon          ! Ratio of h2o to dry air molecular weights 
      real latvap          ! Latent heat of vaporization
      real latice          ! Latent heat of fusion
      real rh2o            ! Gas constant for water vapor
      real cpair           ! Specific heat of dry air
C
C---------------------------Local workspace-----------------------------
C
      real tmn             ! Minimum temperature entry in table
      real tmx             ! Maximum temperature entry in table
      real trice           ! Trans range from es over h2o to es over ice
      logical ip           ! Ice phase (true or false)
C
C-----------------------------------------------------------------------
C
C Specify control parameters first
C
      tmn   = 173.16
      tmx   = 375.16
      trice =  20.00
      ip    = .true.
C
C Call gestbl to build saturation vapor pressure table.
C
      call gestbl(tmn     ,tmx     ,trice   ,ip      ,epslon  ,
     $            latvap  ,latice  ,rh2o    ,cpair   )
C
      return
      end
 
