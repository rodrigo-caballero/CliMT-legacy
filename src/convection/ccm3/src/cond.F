#include <misc.h>
#include <params.h>
      subroutine cond(tdt     ,pmid    ,pdel    ,t       ,q       ,
     $                qc      ,precl   )
C-----------------------------------------------------------------------
C
C Calculate large scale condensation
C
C---------------------------Code history--------------------------------
C
C Original version:  CCM1
C Standardized:      L. Buja, Jun 1992, Feb 1996
C Reviewed:          J. Hack, G. Taylor, Aug 1992
C                    J. Hack, Feb 1996 
C
C-----------------------------------------------------------------------
c
c $Id: cond.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <pmgrid.h>
C------------------------------Commons----------------------------------
#include <comadj.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real tdt                    ! Physics time step (2 delta t)
      real pmid(plond,plev)       ! Pressure at layer midpoints
      real pdel(plond,plev)       ! Delta p at each model level
C
C Output arguments
C
      real t(plond,plev)          ! Temperature
      real q(plond,plev)          ! Specific humidity
      real qc(plond,plev)         ! Moisture tendency due to rainout
      real precl(plond)           ! Large-scale precipitation rate
C
C---------------------------Local variables-----------------------------
C
      real absqs                  ! Intermediate quantity
      real denom                  ! Intermediate quantity
      real dqsdt                  ! Change of qsat with respect to temp.
      real est(plond,plev)        ! Saturation vapor pressure
      real omeps                  ! 1 - 0.622
      real qsat(plond,plev)       ! Saturation specific humidity
      real rain(plond)            ! Rain (units of kg/m^2 s)
      real rga                    ! Reciprocal gravitatnl acceleration
      real rhm1                   ! RH - saturation RH
      real zqcd(plond)            ! Intermed quantity (several actually)
      real zqdt                   ! Reciprocal of tdt
      real cndwtr(plond,plev)     ! Water condensation rate (kg/m**2/s)
      real ke                     ! `disposable parameter' in evaporation
      real evap                   ! Water evaporation rate
      real relhum                 ! Relative humidity
      real dpovrg                 ! deltap/grav
      integer i                   ! Longitude index
      integer jiter               ! Iteration counter
      integer k                   ! Vertical index
C
C-----------------------------------------------------------------------
C
      rga   = 1./gravit
      zqdt  = 1./tdt
      omeps = 1. - epsilo
C
C First diagnose condensation rate due to stable processes
C Update column T and Q (evaporation process is `time-split')
C Condensation calculation is hard-wired for two iterations
C
      do k=1,plev
        do i=1,plon
          cndwtr(i,k) = 0.0
        end do
      end do
C
      do jiter=1,2
        call aqsat(t       ,pmid    ,est     ,qsat    ,plond   ,
     $             plon    ,plev    ,1       ,plev    )
        do k=1,plev
C
C Calculate condensation-rate and new t- and q-values
C
          do i=1,plon
C
C Use of critical saturation vapor pressure requires coefficient on the
C term omeps*est(i,k) in the next statement (e.g. omeps*est(i,k)*escrit)
C Corresponding changes must also be incorporated into estabv.for (e.g.,
C terms est(i,k) in qsat evaluation become escrit*est(i,k))
C
            denom   = (pmid(i,k) - omeps*est(i,k))*t(i,k)**2
            dqsdt   = clrh2o*qsat(i,k)*pmid(i,k)/denom
            absqs   = abs(qsat(i,k))
            rhm1    = q(i,k)/qsat(i,k) - 1.
            zqcd(i) = max(absqs*rhm1/(1. + cldcp*dqsdt),0.)
            if (q(i,k) .lt. 0.0) zqcd(i) = 0.
            q(i,k)  = q(i,k) - zqcd(i)
            t(i,k)  = t(i,k) + zqcd(i)*cldcp
            cndwtr(i,k) = cndwtr(i,k) + zqcd(i)*pdel(i,k)*rga*zqdt
            qc    (i,k) = qc(i,k)     + zqcd(i)*zqdt
          end do
        end do
      end do
C
C Initialize rain vector (will be updated as rain falls through column)
C
      do i=1,plon
        rain(i) = max(cndwtr(i,1),0.0)
      end do
      call aqsat(t       ,pmid    ,est     ,qsat    ,plond   ,
     $           plon    ,plev    ,1       ,plev    )
C
C Evaporate condensate on the way down (see Sundqvist, 1988: Physically
C Based Modelling ..., pp 433-461, Schlesinger, Ed., Kluwer Academic)
C variable evap has units of 1/s; variable rain has units of kg/m**2/s
C rain is used to accumuluate unevaporated rain water on the way down
C
      ke = 1.0e-5                     ! set in common block in final code
      do k=2,plev
        do i=1,plon
          dpovrg  = pdel(i,k)*rga
          relhum  = q(i,k)/qsat(i,k)
          evap    = max(ke*(1.0 - relhum)*sqrt(rain(i)), 0.0)
          evap    = min(evap, (qsat(i,k)-q(i,k))/tdt)
          evap    = min(rain(i)/dpovrg,evap)
          qc(i,k) = qc(i,k) - evap
          q(i,k)  = q(i,k) + evap*tdt
          t(i,k)  = t(i,k) - evap*tdt*cldcp
          rain(i) = max(rain(i) - evap*dpovrg + cndwtr(i,k),0.0)
        end do
      end do
      do i=1,plon
        precl(i) = rain(i)/rhoh2o
      end do
C
      return
      end
 
