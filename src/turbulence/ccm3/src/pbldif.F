      subroutine pbldif(th      ,q       ,z       ,u       ,v       ,
     $                  t       ,pmid    ,kvf     ,cflx    ,shflx   ,
     $                  taux    ,tauy    ,ustar   ,kvm     ,kvh     ,
     $                  cgh     ,cgq     ,cgs     ,pblh    ,tpert   ,
     $                  qpert   ,ktopbl  ,ktopblmn)
C------------------------------------------------------------------------
C 
C Atmospheric boundary layer computation.
C
C Nonlocal scheme that determines eddy diffusivities based on a
C diagnosed boundary layer height and a turbulent velocity scale;
C also, countergradient effects for heat and moisture, and constituents
C are included, along with temperature and humidity perturbations which 
C measure the strength of convective thermals in the lower part of the 
C atmospheric boundary layer.
C
C For more information, see Holtslag, A.A.M., and B.A. Boville, 1993:
C Local versus Nonlocal Boundary-Layer Diffusion in a Global Climate
C Model. J. Clim., vol. 6., p. 1825--1842.
c
c Updated by Holtslag and Hack to exclude the surface layer from the
c definition of the boundary layer Richardson number. Ri is now defined
c across the outer layer of the pbl (between the top of the surface
c layer and the pbl top) instead of the full pbl (between the surface and
c the pbl top). For simiplicity, the surface layer is assumed to be the
c region below the first model level (otherwise the boundary layer depth 
c determination would require iteration).
C
C---------------------------Code history--------------------------------
C
C Original version:  B. Boville
C Standardized:      J. Rosinski, June 1992
C Reviewed:          B. Boville, P. Rasch, August 1992
C Reviewed:          B. Boville, P. Rasch, April 1996
C
C Modified for boundary layer height diagnosis: Bert Holtslag, june 1994
C >>>>>>>>>  (Use ricr = 0.3 in this formulation)
C
C-----------------------------------------------------------------------
c
c $Id: pbldif.F,v 1.1 2005/01/24 19:21:44 rca Exp $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <params.h>
#include <pmgrid.h>
C------------------------------Commons----------------------------------
#include <compbl.h>
C-----------------------------------------------------------------------
#include <comvd.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real th(plond,plev)          ! potential temperature [K]
      real q(plond,plev,pcnst)     ! specific humidity [kg/kg]
      real z(plond,plev)           ! height above surface [m]
      real u(plond,plev)           ! windspeed x-direction [m/s]
      real v(plond,plev)           ! windspeed y-direction [m/s]
      real t(plond,plev)           ! temperature (used for density)
      real pmid(plond,plev)        ! midpoint pressures
      real kvf(plond,plevp)        ! free atmospheric eddy diffsvty [m2/s]
      real cflx(plond,pcnst)       ! surface constituent flux (kg/m2/s)
      real shflx(plond)            ! surface heat flux (W/m2)
      real taux(plond)             ! surface u stress (N)
      real tauy(plond)             ! surface v stress (N)
C
C Output arguments
C
      real ustar(plond)            ! surface friction velocity [m/s]
      real kvm(plond,plevp)        ! eddy diffusivity for momentum [m2/s]
      real kvh(plond,plevp)        ! eddy diffusivity for heat [m2/s]
      real cgh(plond,plevp)        ! counter-gradient term for heat [K/m]
      real cgq(plond,plevp,pcnst)  ! counter-gradient term for constituents
      real cgs(plond,plevp)        ! counter-gradient star (cg/flux)
      real pblh(plond)             ! boundary-layer height [m]
      real tpert(plond)            ! convective temperature excess
      real qpert(plond)            ! convective humidity excess

      integer ktopbl(plond)        ! index of first midpoint inside pbl
      integer ktopblmn             ! min value of ktopbl
C
C---------------------------Local parameters----------------------------
C
      real tiny                    ! lower bound for wind magnitude
      parameter (tiny=1.e-10)
C
C---------------------------Local workspace-----------------------------
C
      integer i                    ! longitude index
      integer k                    ! level index
      integer m                    ! constituent index

      real heatv(plond)            ! surface virtual heat flux
      real thvsrf(plond)           ! sfc (bottom) level virtual temperature
      real thvref(plond)           ! reference level virtual temperature
      real tkv                     ! model level potential temperature
      real therm(plond)            ! thermal virtual temperature excess
      real phiminv(plond)          ! inverse phi function for momentum
      real phihinv(plond)          ! inverse phi function for heat 
      real wm(plond)               ! turbulent velocity scale for momentum
      real vvk                     ! velocity magnitude squared
      real zm(plond)               ! current level height
      real zp(plond)               ! current level height + one level up
      real khfs(plond)             ! surface kinematic heat flux [mK/s]
      real kqfs(plond,pcnst)       ! sfc kinematic constituent flux [m/s]
      real zmzp                    ! level height halfway between zm and zp
c      real vv2(plond)              ! velocity magnitude squared 
      real rino(plond,plev)        ! bulk Richardson no. from level to ref lev
      real tlv(plond)              ! ref. level pot tmp + tmp excess
      real fak1(plond)             ! k*ustar*pblh
      real fak2(plond)             ! k*wm*pblh
      real fak3(plond)             ! fakn*wstr/wm 
      real pblk(plond)             ! level eddy diffusivity for momentum
      real pr(plond)               ! Prandtl number for eddy diffusivities
      real zl(plond)               ! zmzp / Obukhov length
      real zh(plond)               ! zmzp / pblh
      real zzh(plond)              ! (1-(zmzp/pblh))**2
      real wstr(plond)             ! w*, convective velocity scale
      real rrho(plond)             ! 1./bottom level density (temporary)
      real obklen(plond)           ! Obukhov length
      real ustr                    ! unbounded ustar
      real term                    ! intermediate calculation
      real fac                     ! interpolation factor
      real pblmin                  ! min pbl height due to mechanical mixing

      logical unstbl(plond)        ! pts w/unstbl pbl (positive virtual ht flx)
      logical stblev(plond)        ! stable pbl with levels within pbl
      logical unslev(plond)        ! unstbl pbl with levels within pbl
      logical unssrf(plond)        ! unstb pbl w/lvls within srf pbl lyr
      logical unsout(plond)        ! unstb pbl w/lvls in outer pbl lyr
      logical check(plond)         ! True=>chk if Richardson no.>critcal
C
C Compute kinematic surface fluxes
C
      do i=1,plon
        rrho(i) = rair*t(i,plev)/pmid(i,plev)
        ustr = sqrt(sqrt(taux(i)**2 + tauy(i)**2)*rrho(i))
        ustar(i) = max(ustr,0.01)
        khfs(i) = shflx(i)*rrho(i)/cpair
        kqfs(i,1) = cflx(i,1)*rrho(i)
      end do
c+++climt only 1 constituent in climt
c      do m=2,pcnst
c        do i=1,plon
c          kqfs(i,m)= cflx(i,m)*rrho(i)
c        end do
c      end do
c---climt
C
C Initialize output arrays with free atmosphere values
C
      do k=1,plevp
        do i=1,plon
          kvm(i,k) = kvf(i,k)
          kvh(i,k) = kvf(i,k)
          cgh(i,k) = 0.0
          cgq(i,k,1) = 0.0
          cgs(i,k)   = 0.0
        end do
      end do
c+++climt only 1 constituent in climt
c      do m=2,pcnst
c        do k=1,plevp
c          do i=1,plon
c            cgq(i,k,m) = 0.0
c          end do
c        end do
c      end do
c---climt
C
C Compute various arrays for use later:
C
      do i=1,plon
        thvsrf(i) = th(i,plev)*(1.0 + 0.61*q(i,plev,1))
        heatv(i)  = khfs(i) + 0.61*th(i,plev)*kqfs(i,1)
        wm(i)     = 0.
        therm(i)  = 0.
        qpert(i)  = 0.
        tpert(i)  = 0.
        fak3(i)   = 0.  
        zh(i)     = 0.  
        obklen(i) = -thvsrf(i)*ustar(i)**3/
     $             (g*vk*(heatv(i) + sign(1.e-10,heatv(i))))
      end do
C
C
C >>>> Define first a new factor fac=100 for use in Richarson number
C      Calculate virtual potential temperature first level
C      and initialize pbl height to z1
C
      fac = 100.
C
      do i=1,plon
        thvref(i) = th(i,plev)*(1.0 + 0.61*q(i,plev,1))
        pblh(i) = z(i,plev)
        check(i) = .true.
C
C Initialization of lowest level Ri number 
C (neglected in initial Holtslag implementation)
C
C--      vv2(i) = fac*ustar(i)**2
C--      vv2(i) = max(vv2(i),tiny)
C--      rino(i,plev) = g*(thvsrf(i) - thvref(i))*z(i,plev)/
C--  $                  (thvref(i)*vv2(i))
        rino(i,plev) = 0.0
      end do
C
C PBL height calculation:
C Search for level of pbl. Scan upward until the Richardson number between
C the first level and the current level exceeds the "critical" value.
C
      do k=plev-1,plev-npbl+1,-1
        do i=1,plon
          if (check(i)) then
            vvk = (u(i,k) - u(i,plev))**2 + (v(i,k) - v(i,plev))**2
     $           + fac*ustar(i)**2
            vvk = max(vvk,tiny)
            tkv = th(i,k)*(1. + 0.61*q(i,k,1))
            rino(i,k) = g*(tkv - thvref(i))*(z(i,k)-z(i,plev))
     $                 /(thvref(i)*vvk)
            if(rino(i,k).ge.ricr) then
              pblh(i) = z(i,k+1) + (ricr - rino(i,k+1))/
     $                 (rino(i,k) - rino(i,k+1))*(z(i,k) - z(i,k+1))
              check(i) = .false.
            end if
          end if
        end do
      end do
C
C Set pbl height to maximum value where computation exceeds number of
C layers allowed
C
      do i=1,plon
        if (check(i)) pblh(i) = z(i,plevp-npbl)
      end do
C
C Improve estimate of pbl height for the unstable points.
C Find unstable points (virtual heat flux is positive):
C
      do i=1,plon
        if (heatv(i) .gt. 0.) then
          unstbl(i) = .true.
          check(i) = .true.
        else
          unstbl(i) = .false.
          check(i) = .false.
        end if   
      end do
C
C For the unstable case, compute velocity scale and the
C convective temperature excess:
C
      do i=1,plon
        if (check(i)) then
          phiminv(i) = (1. - binm*pblh(i)/obklen(i))**onet
          wm(i)= ustar(i)*phiminv(i)
          therm(i) = heatv(i)*fak/wm(i)       
C--         rino(i,plev) = -g*therm(i)*z(i,plev)/(thvref(i)*vv2(i))
          rino(i,plev) = 0.0
          tlv(i) = thvref(i) + therm(i)
        end if
      end do
C
C Improve pblh estimate for unstable conditions using the
C convective temperature excess:
C
      do k=plev-1,plev-npbl+1,-1
        do i=1,plon
          if (check(i)) then
            vvk = (u(i,k) - u(i,plev))**2 + (v(i,k) - v(i,plev))**2
     $           + fac*ustar(i)**2
            vvk = max(vvk,tiny)
            tkv = th(i,k)*(1. + 0.61*q(i,k,1))
            rino(i,k) = g*(tkv - tlv(i))*(z(i,k)-z(i,plev))
     $                 /(thvref(i)*vvk)
            if(rino(i,k).ge.ricr) then
              pblh(i) = z(i,k+1) + (ricr - rino(i,k+1))/
     $                 (rino(i,k) - rino(i,k+1))*(z(i,k) - z(i,k+1))
              check(i) = .false.
            end if
          end if
        end do
      end do
C
C Points for which pblh exceeds number of pbl layers allowed;
C set to maximum
C
      do i=1,plon
        if (check(i)) pblh(i) = z(i,plevp-npbl)
      end do
C
C PBL height must be greater than some minimum mechanical mixing depth
C Several investigators have proposed minimum mechanical mixing depth
C relationships as a function of the local friction velocity, u*.  We 
C make use of a linear relationship of the form h = c u* where c=700.
C The scaling arguments that give rise to this relationship most often 
C represent the coefficient c as some constant over the local coriolis
C parameter.  Here we make use of the experimental results of Koracin 
C and Berkowicz (1988) [BLM, Vol 43] for wich they recommend 0.07/f
C where f was evaluated at 39.5 N and 52 N.  Thus we use a typical mid
C latitude value for f so that c = 0.07/f = 700.
C
      do i=1,plon
        pblmin  = 700.0*ustar(i)
        pblh(i) = max(pblh(i),pblmin)
      end do
C
C pblh is now available; do preparation for diffusivity calculation:
C
      do i=1,plon
        pblk(i) = 0.0
        fak1(i) = ustar(i)*pblh(i)*vk
C
C Do additional preparation for unstable cases only, set temperature
C and moisture perturbations depending on stability.
C
        if (unstbl(i)) then
          phiminv(i) = (1. - binm*pblh(i)/obklen(i))**onet
          phihinv(i) = sqrt(1. - binh*pblh(i)/obklen(i))
          wm(i)      = ustar(i)*phiminv(i)
          fak2(i)    = wm(i)*pblh(i)*vk
          wstr(i)    = (heatv(i)*g*pblh(i)/thvref(i))**onet 
          fak3(i)    = fakn*wstr(i)/wm(i)
          tpert(i)   = max(khfs(i)*fak/wm(i),0.)   
          qpert(i)   = max(kqfs(i,1)*fak/wm(i),0.)    
        else
          tpert(i)   = max(khfs(i)*fak/ustar(i),0.) 
          qpert(i)   = max(kqfs(i,1)*fak/ustar(i),0.) 
        end if
      end do
C
C Main level loop to compute the diffusivities and 
C counter-gradient terms:
C
      do 1000 k=plev,plev-npbl+2,-1
C
C Find levels within boundary layer:
C
        do i=1,plon
          unslev(i) = .false.
          stblev(i) = .false.
          zm(i) = z(i,k)
          zp(i) = z(i,k-1)
          if (zkmin.eq.0.0 .and. zp(i).gt.pblh(i)) zp(i) = pblh(i)
          if (zm(i) .lt. pblh(i)) then
            ktopbl(i) = k
            zmzp = 0.5*(zm(i) + zp(i))
            zh(i) = zmzp/pblh(i)
            zl(i) = zmzp/obklen(i)
            zzh(i) = 0.
            if (zh(i).le.1.0) zzh(i) = (1. - zh(i))**2
C
C stblev for points zm < plbh and stable and neutral
C unslev for points zm < plbh and unstable
C
            if (unstbl(i)) then
              unslev(i) = .true.
            else
              stblev(i) = .true.
            end if
          end if
        end do
C
C Stable and neutral points; set diffusivities; counter-gradient
C terms zero for stable case:
C
        do i=1,plon
          if (stblev(i)) then
            if (zl(i).le.1.) then
              pblk(i) = fak1(i)*zh(i)*zzh(i)/(1. + betas*zl(i))
            else
              pblk(i) = fak1(i)*zh(i)*zzh(i)/(betas + zl(i))
            end if
            kvm(i,k) = max(pblk(i),kvf(i,k))
            kvh(i,k) = kvm(i,k)
          end if
        end do
C
C unssrf, unstable within surface layer of pbl
C unsout, unstable within outer   layer of pbl
C
        do i=1,plon
          unssrf(i) = .false.
          unsout(i) = .false.
          if (unslev(i)) then
            if (zh(i).lt.sffrac) then
              unssrf(i) = .true.
            else
              unsout(i) = .true.
            end if
          end if
        end do
C
C Unstable for surface layer; counter-gradient terms zero
C
        do i=1,plon
          if (unssrf(i)) then
            term = (1. - betam*zl(i))**onet
            pblk(i) = fak1(i)*zh(i)*zzh(i)*term
            pr(i) = term/sqrt(1. - betah*zl(i))
          end if
        end do
C
C Unstable for outer layer; counter-gradient terms non-zero:
C
        do i=1,plon
          if (unsout(i)) then
            pblk(i) = fak2(i)*zh(i)*zzh(i)
            cgs(i,k) = fak3(i)/(pblh(i)*wm(i))
            cgh(i,k) = khfs(i)*cgs(i,k)
            pr(i) = phiminv(i)/phihinv(i) + ccon*fak3(i)/fak
            cgq(i,k,1) = kqfs(i,1)*cgs(i,k)
          end if
        end do
c+++climt only 1 constituent in climt
c        do m=2,pcnst
c          do i=1,plon
c            if (unsout(i)) cgq(i,k,m) = kqfs(i,m)*cgs(i,k)
c          end do
c        end do
c---climt
C
C For all unstable layers, set diffusivities
C
        do i=1,plon
          if (unslev(i)) then
            kvm(i,k) = max(pblk(i),kvf(i,k))
            kvh(i,k) = max(pblk(i)/pr(i),kvf(i,k))
          end if
        end do
 1000 continue           ! end of level loop
c+
c Check whether last allowed midpoint is within pbl, determine ktopblmn
c-
      ktopblmn = plev
      k = plev-npbl+1
      do i = 1, plon
         if (z(i,k) .le. pblh(i)) ktopbl(i) = k
         ktopblmn = min(ktopblmn, ktopbl(i))
      end do
C
      end
 
