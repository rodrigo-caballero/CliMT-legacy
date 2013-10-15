#include <misc.h>
#include <params.h>
      subroutine vqsat(indx    ,npts    ,t       ,p       ,es      ,
     $                 qs      ,len    )
C-----------------------------------------------------------------------
C
C Utility procedure to look up and return saturation vapor pressure from 
C precomputed table, calculate and return saturation specific humidity 
C (g/g), for input vectors of temperature and pressure (length len)
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
c $Id: vqsat.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      integer len              ! Vector length 
      integer indx(len)        ! Longitude index array (land)
      integer npts             ! Number of land points
c
      real t(len)              ! Temperature
      real p(len)              ! Pressure
C
C Output arguments
C
      real es(len)             ! Saturation vapor pressure
      real qs(len)             ! Saturation specific humidity
C
C--------------------------Local Variables------------------------------
C
      real omeps               ! 1 - 0.622
      integer i,ii             ! Local vector indices
C
C-----------------------------------------------------------------------
#include <eslookup.h>
C-----------------------------------------------------------------------
C
      omeps = 1.0 - epsqs
CDIR$ IVDEP
      do ii=1,npts
         i = indx(ii)
         es(i) = estblf(t(i))
C
C Saturation specific humidity
C
         qs(i) = epsqs*es(i)/(p(i) - omeps*es(i))
C
C The following check is to avoid the generation of negative values
C that can occur in the upper stratosphere and mesosphere
C
         qs(i) = min(1.0,qs(i))
C
         if (qs(i) .lt. 0.0) then
            qs(i) = 1.0
            es(i) = p(i)
         end if
      end do
C
      return
C
      end
 
