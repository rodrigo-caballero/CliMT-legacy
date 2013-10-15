#include <misc.h>
#include <params.h>
      subroutine aqsat(t       ,p       ,es      ,qs        ,ii      ,
     $                 ilen    ,kk      ,kstart  ,kend      )
C-----------------------------------------------------------------------
C
C Utility procedure to look up and return saturation vapor pressure from
C precomputed table, calculate and return saturation specific humidity
C (g/g),for input arrays of temperature and pressure (dimensioned ii,kk)
C This routine is useful for evaluating only a selected region in the
C vertical.
C 
C---------------------------Code history--------------------------------
C
C Original version: J. Hack, Feb 1990
C Standardized:     L. Buja, Feb 1996
C Reviewed:         J. Hack, Aug 1992 
C                   J. Hack, P. Rasch, Apr 1996
C
C-----------------------------------------------------------------------
c
c $Id: aqsat.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      integer ii             ! I dimension of arrays t, p, es, qs
      integer kk             ! K dimension of arrays t, p, es, qs 
      real t(ii,kk)          ! Temperature
      real p(ii,kk)          ! Pressure
      integer ilen           ! Length of vectors in I direction which 
                             !   are assumed to start at 1
      integer kstart         ! Starting location in K direction
      integer kend           ! Ending location in K direction
C
C Output arguments
C
      real es(ii,kk)         ! Saturation vapor pressure
      real qs(ii,kk)         ! Saturation specific humidity
C
C---------------------------Local workspace-----------------------------
C
      real omeps             ! 1 - 0.622
      integer i, k           ! Indices
C
C-----------------------------------------------------------------------
#include <eslookup.h>
C-----------------------------------------------------------------------
C
      omeps = 1.0 - epsqs
      do k=kstart,kend
        do i=1,ilen
          es(i,k) = estblf(t(i,k))
C
C Saturation specific humidity
C
          qs(i,k) = epsqs*es(i,k)/(p(i,k) - omeps*es(i,k))
C   
C The following check is to avoid the generation of negative values
C that can occur in the upper stratosphere and mesosphere
C
          qs(i,k) = min(1.0,qs(i,k))
C         
          if (qs(i,k) .lt. 0.0) then
            qs(i,k) = 1.0
            es(i,k) = p(i,k)
          end if
        end do
      end do
C
      return
      end
 
