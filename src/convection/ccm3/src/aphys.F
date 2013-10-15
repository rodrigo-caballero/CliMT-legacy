#include <misc.h>
#include <params.h>

      subroutine aphys(nstep   ,lat     ,tdt     ,phis    ,pblht   ,
     $                 tpert   ,pmid    ,pint    ,pdel    ,rpdel   ,
     $                 pmln    ,piln    ,t       ,q       ,ts      ,
     $                 qpert   ,precl   ,precc   ,precsl  ,precsc  ,
     $                 dtcond  ,dqcond  ,cnt     ,cnb     ,qc      ,
     $                 cmfdt   ,cmfdq   ,cmfdqr  ,cmfmc   ,cmfsl   ,
     $                 cmflq   ,zmdt    ,zmdq    )

C-----------------------------------------------------------------------
C
C Adjustment physics:
C Apply dry convection, moist convection, and large-scale condensation
C schemes.  Correct invalid (negative or too small) constituent mixing 
C ratios if necessary and report action in unit 6 output.
C
C---------------------------Code history--------------------------------
C
C Original version:  CCM1
C Standardized:      J. Rosinski,         Jun 1992
C                    L. Buja,             Feb 1996
C Reviewed:          J. Hack, G. Taylor,  Aug 1992
C                    J. Hack, B. Boville, Feb 1996
C
C-----------------------------------------------------------------------
c
c $Id: aphys.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <pmgrid.h>
C------------------------------Commons----------------------------------
#include <comcon.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      integer nstep                 ! Current time step for cmfmca diag.
      integer lat                   ! Latitude index (S->N)
      real tdt                      ! 2 delta t (model time increment)
      real phis(plond)              ! Surface geopotential
      real pblht(plond)             ! Planetary boundary layer height
      real tpert(plond)             ! Thermal temperature excess
      real pmid(plond,plev)         ! Pressures at model levels
      real pint(plond,plevp)        ! Pressure at model interfaces
      real pdel(plond,plev)         ! Delta-p (pressure depth)
      real rpdel(plond,plev)        ! 1./pdel
      real pmln(plond,plev)         ! log(pmid)
      real piln(plond,plevp)        ! log(pint)
C
C Input/Output arguments
C
      real t(plond,plev)            ! Temperature (K)
      real q(plond,plev,pcnst)      ! Constituent mixing ratio field
      real ts(plond)                ! Surface temperature
      real qpert(plond,pcnst)       ! Thermal humidity & constituent excess
C
C Output arguments
C
      real precl(plond)             ! Large-scale precipitation rate
      real precc(plond)             ! Convective-scale preciptn rate
      real precsl(plond)            ! L.S. precipitation rate (snow)
      real precsc(plond)            ! C.S. precipitation rate (snow)
      real dtcond(plond,plev)       ! dT/dt due to convective adjustments
      real dqcond(plond,plev,pcnst) ! dq/dt due to convective adjustments
      real cnt(plond)               ! Top level of convective activity
      real cnb(plond)               ! Bottom level of convective activity
      real qc(plond,plev)           ! dq/dt due to rainout terms
      real cmfdt(plond,plev)        ! dT/dt due to moist convection
      real cmfdq(plond,plev)        ! dq/dt due to moist convection
      real zmdt(plond,plev)         ! Zhang convective temperature tendency
      real zmdq(plond,plev)         ! Zhang convective moisture tendency
      real cmfdqr(plond,plev)       ! dq/dt due to moist convective rainout 
      real cmfmc(plond,plev)        ! Moist convection cloud mass flux
      real cmfsl(plond,plev)        ! Moist convection lw stat energy flux
      real cmflq(plond,plev)        ! Moist convection total water flux
C
C---------------------------Local workspace-----------------------------
C
C Most variables with appended 2 are a second copy of similar quantities
C declared above to accommodate calls to two convection schemes
C
      real tv(plond,plev)           ! Virtual temperature array
      real rtdt                     ! 1./ztodt
      real zi(plond,plevp)          ! Height above sfc in interface
      real zm(plond,plev)           ! Height above sfc in mid-layer
      real precc2(plond)            ! Convective-scale preciptn rate
      real cnt2(plond)              ! Top level of convective activity
      real cnb2(plond)              ! Bottom level of convective activity
      real qc2(plond,plev)          ! dq/dt due to rainout terms
      real cmfdqr2(plond,plev)      ! dq/dt due to moist convective rainout 
      real cmfmc2(plond,plev)       ! Moist convection cloud mass flux
      real cmfsl2(plond,plev)       ! Moist convection lw stat energy flux
      real cmflq2(plond,plev)       ! Moist convection total water flux
      real tpert2(plond)            ! Perturbation T
      real qpert2(plond,pcnst)      ! Perturbation q
      real dlf(plond,plev)          ! Detraining cld H20 from convection
      real pflx(plond,plev)         ! Conv rain flux thru out btm of lev
      integer  i,k,m                ! Lon, lev, constituent indices
C
C-----------------------------------------------------------------------
C
C Zero out precip and convective fields before accumulating terms
C
      call resetr(precl,plond,0.)
      call resetr(precc,plond,0.)
      call resetr(precsl,plond,0.)
      call resetr(precsc,plond,0.)
      call resetr(qc    ,plond*plev,0.)
      call resetr(cmfdt ,plond*plev,0.)
      call resetr(cmfdq ,plond*plev,0.)
      call resetr(zmdt  ,plond*plev,0.)
      call resetr(zmdq  ,plond*plev,0.)
      call resetr(cmfdqr,plond*plev,0.)
      call resetr(cmfmc ,plond*plev,0.)
      call resetr(cmfsl ,plond*plev,0.)
      call resetr(cmflq ,plond*plev,0.)
C
C Store initial temperature, and constituent fields for later computation 
C of changes due to convective processes (dry, moist, and large-scale)
C
      do k=1,plev
        do i=1,plon
          dtcond(i,k) = t(i,k)
          dqcond(i,k,1) = q(i,k,1)
        end do
      end do
      do m=2,pcnst
        do k=1,plev
          do i=1,plon
            dqcond(i,k,m) = q(i,k,m)
          end do
        end do
      end do
C
C Calculate geopotential height for cmfmca (requires virtual temperature)
C
      call virtem(t       ,q       ,zvir    ,tv      )
      call zmid(piln(1,plevp),pmln    ,rair    ,gravit   ,tv      ,
     $          zm      )
      call zint(piln    ,pmln    ,rair    ,gravit  ,tv      ,
     $          zm      ,zi      )      
C
C Dry adjustment
C
      call dadadj(lat     ,pmid    ,pint    ,pdel    ,t       ,
     $            q       )

C
C Since the PBL doesn't pass constituent perturbations, they 
C are zeroed here for input to the moist convection routine
C
      do m=2,pcnst
        do i=1,plon
          qpert(i,m) = 0.0
        end do
      end do
C
C              --------- Moist convection -------------
C
C Begin with Zhang-McFarlane (1996) convection parameterization
C Calls to qneg3 are to guard against problems, but probably unnecessary
C
       call qneg3('APHYS bz' ,lat       ,q(1,1,1) )
       call conv_ccm(t       ,q        ,precc   ,cnt     ,cnb     ,
     $	             pblht   ,zm       ,phis    ,zi      ,zmdq    ,
     $               zmdt    ,pmid     ,pint    ,pdel    ,ts      ,
     $               .5*tdt  ,cmfmc    ,cmfdqr  ,nstep   ,lat     ,
     $               tpert   ,qpert    ,dlf     ,pflx    )
       call qneg3('APHYS az' ,lat,      q(1,1,1) )
C
C Convert mass flux from reported mb/s to kg/m^2/s
C
      do k=1,plev
        do i=1,plon
          cmfmc(i,k) = cmfmc(i,k) * 100./gravit
        end do
      end do
c
c Add production of rain by conv_ccm to qc.  PJR says the vertical loop 
c indices are inconsistent.
c
      do k=2,plev
        do i=1,plon
          qc(i,k) = qc(i,k) + (pflx(i,k)-pflx(i,k-1))*gravit/pdel(i,k)
        end do
      end do
C
      do i=1,plon
        tpert2(i) = 0.
      end do
      do m=1,pcnst
        do i=1,plon
           qpert2(i,m) = qpert(i,m)
        end do
      end do
C
C Call Hack (1994) convection scheme to deal with shallow/mid-level convection
C Begin by zeroing local copies of mass flux, energy fluxes, etc.
C
      call resetr(cmfmc2 ,plond*plev,0.)
      call resetr(cmfdqr2,plond*plev,0.)
      call resetr(cmfsl2 ,plond*plev,0.)
      call resetr(cmflq2 ,plond*plev,0.)
      call resetr(qc2    ,plond*plev,0.)
C
      call cmfmca(lat     ,nstep   ,tdt     ,pmid    ,pdel    ,
     $            rpdel   ,zm      ,tpert2  ,qpert2  ,phis    ,
     $            pblht   ,t       ,q       ,cmfdt   ,cmfdq   ,
     $            cmfmc2  ,cmfdqr2 ,cmfsl2  ,cmflq2  ,precc2  ,
     $            qc2     ,cnt2    ,cnb2    )
c
c Merge shallow/mid-level output with prior results from Zhang-McFarlane
c
      do i=1,plon
        precc(i) = precc(i) + precc2(i)
        if (cnt2(i) .lt. cnt(i)) cnt(i) = cnt2(i)
        if (cnb2(i) .gt. cnb(i)) cnb(i) = cnb2(i)
      end do
c
      do k=1,plev
        do i=1,plon
          cmfmc(i,k)  = cmfmc(i,k)  + cmfmc2(i,k)
          cmfdqr(i,k) = cmfdqr(i,k) + cmfdqr2(i,k)
          cmfsl(i,k)  = cmfsl(i,k)  + cmfsl2(i,k)
          cmflq(i,k)  = cmflq(i,k)  + cmflq2(i,k)
          qc(i,k)     = qc(i,k)     + qc2(i,k)
        end do 
      end do
C
C Large-scale (stable) condensation procedure
C Evaporation of stratiform precipitation is included in this call
C
c      call cond(tdt    ,pmid    ,pdel    ,t       ,q       ,
c     $          qc     ,precl   )
C
C Compute rate of temperature change due to convective processes
C
      rtdt = 1./tdt
      do k=1,plev
        do i=1,plon
          dtcond(i,k) = (t(i,k) - dtcond(i,k))*rtdt
        end do
      end do
C
C Check for constituents < qmin. Set to minimum value and print message
C
      call qneg3('APHYS   ' ,lat ,q(1,1,1))
C
C Compute rate of constituent change due to convective processes
C
      do m=1,pcnst
        do k=1,plev
          do i=1,plon
            dqcond(i,k,m) = (q(i,k,m) - dqcond(i,k,m))*rtdt
          end do
        end do
      end do
C
C determine whether precipitation, prec, is frozen (snow) or not
C
      do i=1,plon
        if (t(i,plev  ).lt.tmelt .and. ts(i).lt.tmelt .and. 
     $      t(i,plev-1).lt.tmelt) then
          precsc(i) = precc(i)
          precsl(i) = precl(i)
        else
          precsc(i) = 0.
          precsl(i) = 0.
        end if
      end do
C
      return
      end
 
