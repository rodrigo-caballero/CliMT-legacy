#include <params.h>
      subroutine endrun
C-----------------------------------------------------------------------
C
C Abort the model for abnormal termination
C
C---------------------------Code history--------------------------------
C
C Original version:  L. Bath, Apr 1992
C Standardized:      L. Bath, Jun 1992
C                    L. Buja, Feb 1996
C
C-----------------------------------------------------------------------
c
c $Id: endrun.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c $Author: rca $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C-----------------------------------------------------------------------
C
#if ( defined SPMD )
      integer info

      call pvmfhalt(info)
#endif
#if ( defined OSF1 )
      stop
#else
C     Commenting this out like in blowup.F: MPS
C      call abort
      stop 'endrun:'
#endif
C
      end
