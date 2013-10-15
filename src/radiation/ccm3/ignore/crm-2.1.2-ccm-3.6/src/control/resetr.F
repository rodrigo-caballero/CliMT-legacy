#include <misc.h>
#include <params.h>
      subroutine resetr(pa      ,kdim    ,pvalue  )
C-----------------------------------------------------------------------
C
C Reset array pa(kdim) to pvalue
C
C---------------------------Code history--------------------------------
C
C Original version:  CCM1
C Standardized:      L. Bath, Jun 1992
C                    L. Buja, Feb 1996
C
C-----------------------------------------------------------------------
c
c $Id: resetr.F,v 1.1 2004/09/07 15:27:28 rca Exp $
c $Author: rca $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      integer kdim      ! Dimension of array pa
      real pvalue       ! Value to store in pa
C
C Output arguments
C
      real pa(kdim)     ! Array to reset
C
C---------------------------Local variable------------------------------
C
      integer j         ! Loop index
C
C-----------------------------------------------------------------------
C
      do j=1,kdim
        pa(j) = pvalue
      end do
C
      return
      end
 
