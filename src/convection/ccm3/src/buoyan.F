#include <misc.h>
#include <params.h>
      subroutine buoyan(q       ,t       ,p       ,z       ,pf      ,
     $                  tp      ,qstp    ,tl      ,rl      ,cape    ,
     $                  pblt    ,lcl     ,lel     ,lon     ,mx      ,
     $                  rd      ,grav    ,cp      ,msg     ,nstep   ,
     $                  lat     ,tpert   ,qpert   )
C-----------------------------------------------------------------------
C This is contributed code not fully standardized by the CCM core group.
C
c the documentation has been enhanced to the degree that we are able
c
C Original version:  G. Zhang and collaborators
C Standardized:      Core group staff, 1994 and 195
C Reviewed:          P. Rasch, April 1996
c-----------------------------------------------------------------------
c  $Id: buoyan.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c-----------------------------------------------------------------------
#include <implicit.h>
C-----------------------------------------------------------------------
#include <pmgrid.h>
C--------------------------Commons--------------------------------------
#include <guang.h>
C-----------------------------------------------------------------------
C
C jul 14/92 - guang jun zhang, m.lazare, n.mcfarlane.  as in
C             previous version buoyan except remove pathalogical
C             cases of "zig-zags" in profiles where lel defined
C             far too high (by use of lelten array, which assumes
C             a maximum of five such crossing points).
C feb 18/91 - guang jun zhang, m.lazare, n.mcfarlane.  previous
C             version buoyan.
C
C input arguments
C
      real q(plond,plev)        ! spec. humidity
      real t(plond,plev)        ! temperature
      real p(plond,plev)        ! pressure
      real z(plond,plev)        ! height
      real pf(plond,plev+1)     ! pressure at interfaces
      real pblt(plond)          ! index of pbl depth
      real tpert(plond)         ! perturbation temperature by pbl processes
      real qpert(plond)         ! perturbation moisture by pbl processes
c
c output arguments
c
      real tp(plond,plev)       ! parcel temperature
      real qstp(plond,plev)     ! saturation mixing ratio of parcel
      real tl(plond)            ! parcel temperature at lcl
      real cape(plond)          ! convective aval. pot. energy.
      integer lcl(plond)        ! 
      integer lel(plond)        ! 
      integer lon(plond)        ! level of onset of deep convection
      integer mx(plond)         ! level of max moist static energy
C
C--------------------------Local Variables------------------------------
C
      real capeten(plond,5)     ! provisional value of cape
      real tv(plond,plev)       ! 
      real tpv(plond,plev)      ! 
      real buoy(plond,plev)

      real a1(plond) 
      real a2(plond) 
      real estp(plond) 
      real pl(plond) 
      real plexp(plond) 
      real hmax(plond) 
      real hmn(plond) 
      real y(plond)

      logical plge600(plond) 
      integer knt(plond) 
      integer lelten(plond,5)

      real cp
      real e
      real grav

      integer i
      integer k
      integer lat
      integer msg
      integer n
      integer nstep

      real rd
      real rl
C
C-----------------------------------------------------------------------
C
      do n = 1,5
        do i = 1,plon
          lelten(i,n) = plev
          capeten(i,n) = 0.
        end do
      end do
C
      do i = 1,plon
        lon(i) = plev
        knt(i) = 0
        lel(i) = plev
        mx(i) = lon(i)
        cape(i) = 0.
        hmax(i) = 0.
      end do
C
C set "launching" level(mx) to be at maximum moist static energy.
C search for this level stops at planetary boundary layer top.
C
      do k = plev,msg + 1,-1
        do i = 1,plon
          hmn(i) = cp*t(i,k) + grav*z(i,k) + rl*q(i,k)
          if (k.ge.nint(pblt(i)) .and. k.le.lon(i) .and.
     $         hmn(i).gt.hmax(i)) then
            hmax(i) = hmn(i)
            mx(i) = k
          end if
        end do
      end do
C
      do i = 1,plon
        lcl(i) = mx(i)
        e = p(i,mx(i))*q(i,mx(i))/ (eps1+q(i,mx(i)))
        tl(i) = 2840./ (3.5*log(t(i,mx(i)))-log(e)-4.805) + 55.
        if (tl(i).lt.t(i,mx(i))) then
          plexp(i) = (1./ (0.2854* (1.-0.28*q(i,mx(i)))))
          pl(i) = p(i,mx(i))* (tl(i)/t(i,mx(i)))**plexp(i)

        else
          tl(i) = t(i,mx(i))
          pl(i) = p(i,mx(i))
        end if
      end do
C
C calculate lifting condensation level (lcl).
C
      do k = plev,msg + 2,-1
        do i = 1,plon
          if (k.le.mx(i) .and. (p(i,k).gt.pl(i).and.
     $         p(i,k-1).le.pl(i))) then
            lcl(i) = k - 1
          end if
        end do
      end do
C
C if lcl is above the nominal level of non-divergence (600 mbs),
C no deep convection is permitted (ensuing calculations
C skipped and cape retains initialized value of zero).
C
      do i = 1,plon
        plge600(i) = pl(i).ge.600.
      end do
C
C initialize parcel properties in sub-cloud layer below lcl.
C
      do k = plev,msg + 1,-1
        do i=1,plon
          if (k.gt.lcl(i) .and. k.le.mx(i) .and. plge600(i)) then
            tv(i,k) = t(i,k)* (1.+1.608*q(i,k))/ (1.+q(i,k))
            qstp(i,k) = q(i,mx(i))
            tp(i,k) = t(i,mx(i))* (p(i,k)/p(i,mx(i)))**
     $                (0.2854* (1.-0.28*q(i,mx(i))))
C
C buoyancy is increased by 0.5 k as in tiedtke
C
c-jjh          tpv (i,k)=tp(i,k)*(1.+1.608*q(i,mx(i)))/
c-jjh     1                     (1.+q(i,mx(i)))
            tpv(i,k) = (tp(i,k)+tpert(i))*
     $                 (1.+1.608*q(i,mx(i)))/ (1.+q(i,mx(i)))
            buoy(i,k) = tpv(i,k) - tv(i,k) + 0.5
          end if
        end do
      end do
C
C define parcel properties at lcl (i.e. level immediately above pl).
C
      do k = plev,msg + 1,-1
        do i=1,plon
          if (k.eq.lcl(i) .and. plge600(i)) then
            tv(i,k) = t(i,k)* (1.+1.608*q(i,k))/ (1.+q(i,k))
            qstp(i,k) = q(i,mx(i))
            tp(i,k) = tl(i)* (p(i,k)/pl(i))**
     $                (0.2854* (1.-0.28*qstp(i,k)))
C              estp(i)  =exp(a-b/tp(i,k))
C use of different formulas for est has about 1 g/kg difference
C in qs at t= 300k, and 0.02 g/kg at t=263k, with the formula
C above giving larger qs.
C
            estp(i) = c1*exp((c2* (tp(i,k)-tfreez))/
     $                ((tp(i,k)-tfreez)+c3))

            qstp(i,k) = eps1*estp(i)/ (p(i,k)-estp(i))
            a1(i) = cp/rl + qstp(i,k)* (1.+qstp(i,k)/eps1)*rl*
     $              eps1/ (rd*tp(i,k)**2)
            a2(i) = .5* (qstp(i,k)* (1.+2./eps1*qstp(i,k))*
     $              (1.+qstp(i,k)/eps1)*eps1**2*rl*rl/
     $              (rd**2*tp(i,k)**4)-qstp(i,k)*
     $              (1.+qstp(i,k)/eps1)*2.*eps1*rl/
     $              (rd*tp(i,k)**3))
            a1(i) = 1./a1(i)
            a2(i) = -a2(i)*a1(i)**3
            y(i) = q(i,mx(i)) - qstp(i,k)
            tp(i,k) = tp(i,k) + a1(i)*y(i) + a2(i)*y(i)**2
c          estp(i)  =exp(a-b/tp(i,k))
            estp(i) = c1*exp((c2* (tp(i,k)-tfreez))/
     $              ((tp(i,k)-tfreez)+c3))

            qstp(i,k) = eps1*estp(i)/ (p(i,k)-estp(i))
C
C buoyancy is increased by 0.5 k in cape calculation.
C dec. 9, 1994
c-jjh          tpv(i,k) =tp(i,k)*(1.+1.608*qstp(i,k))/(1.+q(i,mx(i)))
C
            tpv(i,k) = (tp(i,k)+tpert(i))* (1.+1.608*qstp(i,k))/
     $                 (1.+q(i,mx(i)))
            buoy(i,k) = tpv(i,k) - tv(i,k) + 0.5
          end if
        end do
      end do
C
C main buoyancy calculation.
C
      do k = plev - 1,msg + 1,-1
        do i=1,plon
          if (k.lt.lcl(i) .and. plge600(i)) then
            tv(i,k) = t(i,k)* (1.+1.608*q(i,k))/ (1.+q(i,k))
            qstp(i,k) = qstp(i,k+1)
            tp(i,k) = tp(i,k+1)* (p(i,k)/p(i,k+1))**
     $                (0.2854* (1.-0.28*qstp(i,k)))
c          estp(i) = exp(a-b/tp(i,k))
            estp(i) = c1*exp((c2* (tp(i,k)-tfreez))/
     $                ((tp(i,k)-tfreez)+c3))

            qstp(i,k) = eps1*estp(i)/ (p(i,k)-estp(i))
            a1(i) = cp/rl + qstp(i,k)* (1.+qstp(i,k)/eps1)*rl*
     $              eps1/ (rd*tp(i,k)**2)
            a2(i) = .5* (qstp(i,k)* (1.+2./eps1*qstp(i,k))*
     $              (1.+qstp(i,k)/eps1)*eps1**2*rl*rl/
     $              (rd**2*tp(i,k)**4)-qstp(i,k)*
     $              (1.+qstp(i,k)/eps1)*2.*eps1*rl/
     $              (rd*tp(i,k)**3))
            a1(i) = 1./a1(i)
            a2(i) = -a2(i)*a1(i)**3
            y(i) = qstp(i,k+1) - qstp(i,k)
            tp(i,k) = tp(i,k) + a1(i)*y(i) + a2(i)*y(i)**2
c          estp(i)  =exp(a-b/tp(i,k))
            estp(i) = c1*exp((c2* (tp(i,k)-tfreez))/
     $                ((tp(i,k)-tfreez)+c3))

            qstp(i,k) = eps1*estp(i)/ (p(i,k)-estp(i))
c-jjh          tpv(i,k) =tp(i,k)*(1.+1.608*qstp(i,k))/
cjt            (1.+q(i,mx(i)))
            tpv(i,k) = (tp(i,k)+tpert(i))* (1.+1.608*qstp(i,k))/
     $                 (1.+q(i,mx(i)))
            buoy(i,k) = tpv(i,k) - tv(i,k) + 0.5
          end if
        end do
      end do
C
      do k = msg + 2,plev
        do i = 1,plon
          if (k.lt.lcl(i) .and. plge600(i)) then
            if (buoy(i,k+1).gt.0. .and. buoy(i,k).le.0.) then
              knt(i) = min(5,knt(i) + 1)
              lelten(i,knt(i)) = k
            end if
          end if
        end do
      end do
C
C calculate convective available potential energy (cape).
C
      do n = 1,5
        do k = msg + 1,plev
          do i = 1,plon
            if (plge600(i) .and.k.le.mx(i) .and.k.gt.lelten(i,n)) then
              capeten(i,n) = capeten(i,n) +
     $                       rd*buoy(i,k)*log(pf(i,k+1)/pf(i,k))
            end if
          end do
        end do
      end do
C
C find maximum cape from all possible tentative capes from
C one sounding,
C and use it as the final cape, april 26, 1995
C
      do n = 1,5
        do i = 1,plon
          if (capeten(i,n).gt.cape(i)) then
            cape(i) = capeten(i,n)
            lel(i) = lelten(i,n)
          end if
        end do
      end do
C
C put lower bound on cape for diagnostic purposes.
C
      do i = 1,plon
        cape(i) = max(cape(i), 0.)
      end do
C
      return
      end
 
