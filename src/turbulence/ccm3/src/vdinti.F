      subroutine vdinti(cpwvx   ,cpairx  ,gravx   ,rairx   )
C-----------------------------------------------------------------------
C
C Initialization of time independent fields for vertical diffusion.
C Call initialization routine for boundary layer scheme.
C
C---------------------------Code history--------------------------------
C
C Original version:  J. Rosinski
C Standardized:      J. Rosinski, June 1992
C Reviewed:          P. Rasch, B. Boville, August 1992
C Reviewed:          P. Rasch, April 1996
C Reviewed:          B. Boville, April 1996
C
C-----------------------------------------------------------------------
c
c $Id: vdinti.F,v 1.1 2005/01/24 19:21:44 rca Exp $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <params.h>
#include <pmgrid.h>
C------------------------------Commons----------------------------------
#include <comvd.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real cpwvx        ! spec. heat of water vapor at const. pressure
      real cpairx       ! specific heat of dry air
      real gravx        ! acceleration due to gravity
      real rairx        ! gas constant for dry air
C
C---------------------------Local workspace-----------------------------
C
      integer k         ! vertical loop index
C
C-----------------------------------------------------------------------
C
C Hard-wired numbers.
C zkmin = minimum k = kneutral*f(ri)
C
      zkmin = 0.01
C
C Set physical constants for vertical diffusion and pbl:
C
      cpair  = cpairx
      gravit = gravx
      rair   = rairx
C
C Derived constants
C ntopfl = top level to which v-diff is applied
C npbl = max number of levels (from bottom) in pbl
C
c
c Limit pbl height to regions below 400 mb
c
C-------       npbl = 0
C-------       do k=plev,1,-1
C-------         if (hypm(k).ge.4.e4) then
C-------           npbl = npbl + 1
C-------         end if
C-------       end do
C-------       npbl = max(npbl,1)
C-------       if (masterproc) then
C-------       write(6,*)'VDINTI: PBL height will be limited to bottom ',npbl,
C-------      $' model levels. Top is ',hypm(plevp-npbl),' pascals'
C-------       end if

c++++rca
      npbl=1
c---rca
      ntopfl = 1
      if (plev.eq.1) ntopfl = 0
      cpvir = cpwvx/cpairx - 1.
C
C Set the square of the mixing lengths.
C
      ml2(1) = 0.
      do k=2,plev
        ml2(k) = 30.0**2
      end do
      ml2(plevp) = 0.
C
C Initialize pbl variables
C
      call pbinti(gravx)
C

      end
 
