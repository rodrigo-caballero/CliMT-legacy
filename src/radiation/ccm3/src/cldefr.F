#include <misc.h>
#include <params.h>
      subroutine cldefr(oro     ,t       ,rel     ,rei     ,fice    ,
     $                  ps      ,pmid    )
C-----------------------------------------------------------------------
C
C Compute cloud drop size
C
C---------------------------Code history--------------------------------
C
C Original version:  J. Kiehl, Jan 1993
C Reviewed:          J. Kiehl, Feb 1996
C Standardized:      L. Buja,  Feb 1996
C Reviewed:          J. Kiehl, Apr 1996
C
C-----------------------------------------------------------------------
c
c $Id: cldefr.F,v 1.3 2008/02/02 13:10:46 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <pmgrid.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real oro(plond)           ! Land/ocean/seaice flag
      real t(plond,plev)        ! Temperature
      real ps(plond)            ! Surface pressure
      real pmid(plond,plev)     ! Midpoint pressures
C
C Output arguments
C
      real rel(plond,plev)      ! Liquid effective drop size (microns)
      real rei(plond,plev)      ! Ice effective drop size (microns)
      real fice(plond,plev)     ! Fractional ice content within cloud
      real pirnge           ! Nrmlzd pres range for ice particle changes
      real picemn          ! Normalized pressure below which rei=reimax
      real rirnge            ! Range of ice radii (reimax - 10 microns)
      real reimax               ! Maximum ice effective radius
      real pnrml                ! Normalized pressure
      real weight               ! Coef. for determining rei as fn of P/PS
C
C---------------------------Local workspace-----------------------------
C
      integer i,k               ! Lon, lev indices
      real rliq                 ! Temporary liquid drop size
C
C--------------------------Statement functions--------------------------
C
      logical land
      land(i) = nint(oro(i)).eq.1
C
C-----------------------------------------------------------------------
C
      do k=1,plev
        do i=1,plon
C
C Define liquid drop radius
C
          if (land(i)) then
            rliq = 5.0 + 5.0*min(1.0,max(0.0,(263.16-t(i,k))*0.05))
          else
            rliq = 10.0
          end if
          rel(i,k) = rliq
C
C Determine rei as function of normalized pressure
C
          reimax   = 30.0
          rirnge   = 20.0 
          pirnge   = 0.4
          picemn   = 0.4
C
          pnrml    = pmid(i,k)/ps(i)
          weight   = max(min((pnrml-picemn)/pirnge,1.0),0.)
          rei(i,k) = reimax - rirnge*weight
C
C Define fractional amount of cloud that is ice
C
C If warmer than -10 degrees C then water phase
C
          if(t(i,k).gt.263.16) fice(i,k) = 0.0
C
C If colder than -10 degrees C but warmer than -30 C mixed phase
C
          if (t(i,k).le.263.16.and.t(i,k).ge.243.16) then
            fice(i,k) =(263.16-t(i,k)) / 20.0
          end if
C
C If colder than -30 degrees C then ice phase
C
          if (t(i,k).lt.243.16) fice(i,k) = 1.0
C
C Turn off ice radiative properties by setting fice = 0.0
C
C+             fice(i,k) = 0.0
C
        end do
      end do
C
      return
      end
 
