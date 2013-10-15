#include <params.h>
      subroutine blowup
C-----------------------------------------------------------------------
C
C Model is blowing up or something went wrong reading the boundary
C datasets.  Dispose current history tape and quit.  This routine was
C cannibalized from WRAPUP.  It was not possible to simply include the
C history dispose logic in ENDRUN because of potential recursion
C problems.
C
C--------------------------Code history---------------------------------
C
C Original version:  J. Rosinski, Apr 1993
C Standardized:      L. Buja,     Feb 1996
C
C-----------------------------------------------------------------------
c
c $Id: blowup.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c $Author: rca $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C-----------------------------Parameters--------------------------------
#include <pmgrid.h>
#include <pagrid.h>
C-----------------------------Commons-----------------------------------
#include <comhst.h>
C-----------------------------------------------------------------------
#include <commss.h>
C
C-----------------------------Local Variables---------------------------
C
      integer ktape        ! History tape index
C
C-----------------------------------------------------------------------
C
c      do ktape=1,mtapes
c        call savdis(hunit(ktape),nhfil(ktape),cpath(ktape),nswrps,nsmvn,
c     $               irt        ,msscom      ,.false.)
c      end do
#if ( defined OSF1 )
      stop
#else
c+++rca
c      call abort
      stop 'blowup:'
c---rca
#endif

C
      return
      end
