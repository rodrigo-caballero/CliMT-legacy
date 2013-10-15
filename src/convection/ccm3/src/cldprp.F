#include <misc.h>
#include <params.h>
      subroutine cldprp(q       ,t       ,u       ,v       ,p       ,
     $                  z       ,s       ,mu      ,eu      ,du      ,
     $                  md      ,ed      ,sd      ,qd      ,ud      ,
     $                  vd      ,mc      ,qu      ,su      ,zf      ,
     $                  qst     ,hmn     ,hsat    ,alpha   ,shat    ,
     $                  ql      ,totpcp  ,totevp  ,cmeg    ,jb      ,
     $                  lel     ,jt      ,jlcl    ,mx      ,j0      ,
     $                  jd      ,rl      ,il1g    ,il2g    ,rd      ,
     $                  grav    ,cp      ,msg     ,nstep   ,lat     ,
     $                  pflx    ,evp     ,cu      ,mu2     ,eu2     ,
     $                  du2     ,md2     ,ed2     )
C------------------------------------------------------------------------------
C This is contributed code not fully standardized by the CCM core group.
C
C this code is very much rougher than virtually anything else in the CCM
C there are debug statements left strewn about and code segments disabled
C these are to facilitate future development. We expect to release a
C cleaner code in a future release
C
c the documentation has been enhanced to the degree that we are able
c
C Original version:  G. Zhang and collaborators
C Standardized:      Core group staff, 1994 and 195
C Reviewed:          P. Rasch, April 1996
C
C**** PLEASE NOTE ****
C
C we are aware of a specific problem in this code 
C (identified by the string ---> PROBLEM ONE)
C during the calculation of the updraft cloud properties,
C rather than adding a perturbation to the updraft temperature of 
C half a degree, (there was an inadvertant addition of cp*0.5) degrees
C or about 500 degrees. (This problem was in the code prior to its 
C contribution to the NCAR effort)

C Fortunately, the erroneous values
C are overwritten later in the code. The problem is quite subtle.
C The erroneous values would persist between cloud base and the lifting 
C condensation level. The addition of the very high perturbation to the updraft
C temperature causes the saturation mixing ratio to be set to zero, 
C and later the lcl to be set to one level above cloud base.
C There are therefore no levels between cloud base and the lcl. Therefore
C all erroneous values are overwritten.

C The only manifestation we are aware of with respect to this problem
C is that the lifting condensation level is constrained to be one level above
C cloud base.

C We discovered the problem after too much had been invested in
C very long integrations (in terms of computer time)
C to allow for a modification and model retuning. It is our expectation that
C this problem will be fixed in the next release of the model.
C
C *********** 
c-----------------------------------------------------------------------
c
c $Id: cldprp.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c
c-----------------------------------------------------------------------
C nov 20/92 - guang jun zhang,m.lazare. now has deeper (more
C             realistic) downdrafts.
C jul 14/92 - guang jun zhang,m.lazare. add shallow mixing
C             formulation.
C nov 21/91 - m.lazare. like previous cldprop except minimum "f"
C                       now 0.0004 instead of 0.001 (more
C                       realistic with more deep).
C may 09/91 - guang jun zhang, m.lazare, n.mcfarlane.
C             original version cldprop.
C------------------------------------------------------------------------------
#include <implicit.h>
C------------------------------------------------------------------------------
#include <pmgrid.h>
C------------------------------------------------------------------------------
#include <guang.h>
C------------------------------------------------------------------------------
C
C Input arguments
C
      real q(plond,plev)        ! spec. humidity of env
      real t(plond,plev)        ! temp of env
      real p(plond,plev)        ! pressure of env
      real z(plond,plev)        ! height of env
      real s(plond,plev)        ! normalized dry static energy of env
      real zf(plond,plevp)      ! height of interfaces
      real u(plond,plev)        ! zonal velocity of env
      real v(plond,plev)        ! merid. velocity of env

      integer jb(plond)         ! updraft base level
      integer lel(plond)        ! updraft launch level
      integer jt(plond)         ! updraft plume top
      integer jlcl(plond)       ! updraft lifting cond level
      integer mx(plond)         ! updraft base level (same is jb)
      integer j0(plond)         ! level where updraft begins detraining
      integer jd(plond)         ! level of downdraft
c
c output
c
      real alpha(plond,plev)    !
      real cmfdqr(plond,plev)   ! rate of production of precip at that layer
      real du(plond,plev)       ! detrainement rate of updraft
      real ed(plond,plev)       ! entrainment rate of downdraft
      real eu(plond,plev)       ! entrainment rate of updraft
      real hmn(plond,plev)      ! moist stat energy of env
      real hsat(plond,plev)     ! sat moist stat energy of env
      real mc(plond,plev)       ! net mass flux
      real md(plond,plev)       ! downdraft mass flux
      real mu(plond,plev)       ! updraft mass flux
      real pflx(plond,plev)     ! precipitation flux thru layer
      real qd(plond,plev)       ! spec humidity of downdraft
      real ql(plond,plev)       ! liq water of updraft
      real qst(plond,plev)      ! saturation spec humidity of env.
      real qu(plond,plev)       ! spec hum of updraft
      real sd(plond,plev)       ! normalized dry stat energy of downdraft
      real shat(plond,plev)     ! interface values of dry stat energy
      real su(plond,plev)       ! normalized dry stat energy of updraft
      real ud(plond,plev)       ! downdraft u
      real vd(plond,plev)       ! downdraft v
c
c     these version of the mass fluxes conserve mass (used in tracer transport)
c
      real mu2(plond,plev)      ! updraft mass flux
      real eu2(plond,plev)      ! updraft entrainment
      real du2(plond,plev)      ! updraft detrainment
      real md2(plond,plev)      ! downdraft mass flux
      real ed2(plond,plev)      ! downdraft entrainment
      real rl                   ! latent heat of vap

      integer il1g              !CORE GROUP REMOVE
      integer il2g              !CORE GROUP REMOVE

      real rd                   ! gas constant for dry air
      real grav                 ! gravity
      real cp                   ! heat capacity of dry air

      integer msg               ! missing moisture vals (always 0)
      integer nstep             ! time step index
      integer lat               ! lat index
C
C Local workspace
C
      real gamma(plond,plev)  
      real dz(plond,plev)  
      real iprm(plond,plev)  
      real hu(plond,plev)  
      real hd(plond,plev)  
      real eps(plond,plev)  
      real f(plond,plev)  
      real k1(plond,plev)  
      real i2(plond,plev)  
      real ihat(plond,plev)  
      real i3(plond,plev)  
      real idag(plond,plev)  
      real i4(plond,plev)  
      real qsthat(plond,plev)  
      real hsthat(plond,plev)  
      real gamhat(plond,plev)  
      real cu(plond,plev)  
      real evp(plond,plev)  
      real cmeg(plond,plev)  
      real qds(plond,plev) 
      real hmin(plond)  
      real expdif(plond)  
      real expnum(plond)  
      real ftemp(plond)  
      real eps0(plond)  
      real rmue(plond)  
      real zuef(plond)  
      real zdef(plond)  
      real epsm(plond)  
      real ratmjb(plond)  
      real est(plond)  
      real totpcp(plond)  
      real totevp(plond)  
      real alfa(plond) 
      real beta
      real c0
      real ql1
      real weight
      real tu
      real estu
      real qstu

      real small
      real mdt  
      real cu2

      integer khighest
      integer klowest  
      integer kount 
      integer i,k

      logical doit(plond)
      logical done(plond)
C
C------------------------------------------------------------------------------
C
      do i = 1,il2g
        ftemp(i) = 0.
        expnum(i) = 0.
        expdif(i) = 0.
      end do
c
cjr Change from msg+1 to 1 to prevent blowup
c
      do k = 1,plev
        do i = 1,il2g
          dz(i,k) = zf(i,k) - zf(i,k+1)
        end do
      end do

c
c initialize many output and work variables to zero
c
      do k = msg + 1,plev
        do i = 1,il2g
          k1(i,k) = 0.
          i2(i,k) = 0.
          i3(i,k) = 0.
          i4(i,k) = 0.
          mu(i,k) = 0.
          f(i,k) = 0.
          eps(i,k) = 0.
          eu(i,k) = 0.
          du(i,k) = 0.
          ql(i,k) = 0.
          cu(i,k) = 0.
          evp(i,k) = 0.
          cmeg(i,k) = 0.
          qds(i,k) = q(i,k)
          md(i,k) = 0.
          ed(i,k) = 0.
          sd(i,k) = s(i,k)
          qd(i,k) = q(i,k)
          ud(i,k) = u(i,k)
          vd(i,k) = v(i,k)
          mc(i,k) = 0.
          qu(i,k) = q(i,k)
          su(i,k) = s(i,k)
c        est(i)=exp(a-b/t(i,k))
          est(i) = c1*exp((c2* (t(i,k)-tfreez))/((t(i,k)-tfreez)+c3))
          qst(i,k) = eps1*est(i)/ (p(i,k)-est(i))
          gamma(i,k) = qst(i,k)*(1. + qst(i,k)/eps1)*eps1*rl/
     $                 (rd*t(i,k)**2)*rl/cp
          hmn(i,k) = cp*t(i,k) + grav*z(i,k) + rl*q(i,k)
          hsat(i,k) = cp*t(i,k) + grav*z(i,k) + rl*qst(i,k)
          hu(i,k) = hmn(i,k)
          hd(i,k) = hmn(i,k)
          mu2(i,k) = 0.
          eu2(i,k) = 0.
          du2(i,k) = 0.
          md2(i,k) = 0.
          ed2(i,k) = 0.
          pflx(i,k) = 0.
          cmfdqr(i,k) = 0.
        end do
      end do
c
cjr Set to zero things which make this routine blow up
c
      do k=1,msg
        do i=1,il2g
          cmfdqr(i,k) = 0.
          mu2(i,k) = 0.
          eu2(i,k) = 0.
          du2(i,k) = 0.
          md2(i,k) = 0.
          ed2(i,k) = 0.
        end do
      end do
c
c interpolate the layer values of qst, hsat and gamma to
c layer interfaces
c
      do i = 1,il2g
        hsthat(i,msg+1) = hsat(i,msg+1)
        qsthat(i,msg+1) = qst(i,msg+1)
        gamhat(i,msg+1) = gamma(i,msg+1)
        totpcp(i) = 0.
        totevp(i) = 0.
      end do
      do k = msg + 2,plev
        do i = 1,il2g
          if (abs(qst(i,k-1)-qst(i,k)).gt.1.E-6) then
            qsthat(i,k) = log(qst(i,k-1)/qst(i,k))*qst(i,k-1)*
     $                    qst(i,k)/ (qst(i,k-1)-qst(i,k))
          else
            qsthat(i,k) = qst(i,k)
          end if
          hsthat(i,k) = cp*shat(i,k) + rl*qsthat(i,k)
          if (abs(gamma(i,k-1)-gamma(i,k)).gt.1.E-6) then
            gamhat(i,k) = log(gamma(i,k-1)/gamma(i,k))*
     $                    gamma(i,k-1)*gamma(i,k)/
     $                    (gamma(i,k-1)-gamma(i,k))
          else
            gamhat(i,k) = gamma(i,k)
          end if
        end do
      end do
C
C initialize cloud top to highest plume top.
C
      do i = 1,il2g
        jt(i) = max(lel(i),4)
        jd(i) = plev
        jlcl(i) = lel(i)
        hmin(i) = 1.E6
      end do
C
C find the level of minimum hsat, where detrainment starts
C
      do k = msg + 1,plev
        do i = 1,il2g
          if (hsat(i,k).le.hmin(i) .and. k.ge.jt(i).and.k.le.jb(i)) then
            hmin(i) = hsat(i,k)
            j0(i) = k
          end if
        end do
      end do
      do i = 1,il2g
        j0(i) = min(j0(i),jb(i)-2)
        j0(i) = max(j0(i),jt(i)+2)
c
c Fix from Guang Zhang to address out of bounds array reference
c
        j0(i) = min(j0(i),plev)
      end do
C
C Initialize certain arrays inside cloud
C
      do k = msg + 1,plev
        do i = 1,il2g
          if (k.ge.jt(i) .and. k.le.jb(i)) then
            hu(i,k) = hmn(i,mx(i)) + cp*0.5
            su(i,k) = s(i,mx(i)) + 0.5
C*** PROBLEM ONE ***
            su(i,k) = s(i,mx(i)) + cp*0.5
          end if
        end do
      end do
C
C *********************************************************
C compute taylor series for approximate eps(z) below
C *********************************************************
C
      do k = plev - 1,msg + 1,-1
        do i = 1,il2g
          if (k.lt.jb(i) .and. k.ge.jt(i)) then
            k1(i,k) = k1(i,k+1) + (hmn(i,mx(i))-hmn(i,k))*dz(i,k)
            ihat(i,k) = 0.5* (k1(i,k+1)+k1(i,k))
            i2(i,k) = i2(i,k+1) + ihat(i,k)*dz(i,k)
            idag(i,k) = 0.5* (i2(i,k+1)+i2(i,k))
            i3(i,k) = i3(i,k+1) + idag(i,k)*dz(i,k)
            iprm(i,k) = 0.5* (i3(i,k+1)+i3(i,k))
            i4(i,k) = i4(i,k+1) + iprm(i,k)*dz(i,k)
          end if
        end do
      end do
C
C re-initialize hmin array for ensuing calculation.
C
      do i = 1,il2g
        hmin(i) = 1.E6
      end do
      do k = msg + 1,plev
        do i = 1,il2g
          if (k.ge.j0(i).and.k.le.jb(i) .and. hmn(i,k).le.hmin(i)) then
            hmin(i) = hmn(i,k)
            expdif(i) = hmn(i,mx(i)) - hmin(i)
          end if
        end do
      end do
C
C *********************************************************
C compute approximate eps(z) using above taylor series
C *********************************************************
C
      do k = msg + 2,plev
        do i = 1,il2g
          expnum(i) = 0.
          ftemp(i) = 0.
          if (k.lt.jt(i) .or. k.ge.jb(i)) then
            k1(i,k) = 0.
            expnum(i) = 0.
          else
            expnum(i) = hmn(i,mx(i)) - (hsat(i,k-1)*(zf(i,k)-z(i,k)) +
     $                  hsat(i,k)* (z(i,k-1)-zf(i,k)))/(z(i,k-1)-z(i,k))
          end if
          if ((expdif(i).gt.100..and.expnum(i).gt.0.) .and.
     $        k1(i,k).gt.expnum(i)*dz(i,k)) then
            ftemp(i) = expnum(i)/k1(i,k)
            f(i,k) = ftemp(i) + i2(i,k)/k1(i,k)*ftemp(i)**2 +
     $                (2.*i2(i,k)**2-k1(i,k)*i3(i,k))/k1(i,k)**2*
     $                ftemp(i)**3 + (-5.*k1(i,k)*i2(i,k)*i3(i,k)+
     $                5.*i2(i,k)**3+k1(i,k)**2*i4(i,k))/
     $                k1(i,k)**3*ftemp(i)**4
            f(i,k) = max(f(i,k),0.)
            f(i,k) = min(f(i,k),0.0002)
          end if
        end do
      end do
      do i = 1,il2g
        if (j0(i).lt.jb(i)) then
          if (f(i,j0(i)).lt.1.E-6 .and. f(i,j0(i)+1).gt.f(i,j0(i)))
     $      j0(i) = j0(i) + 1
        end if
      end do
      do k = msg + 2,plev
        do i = 1,il2g
          if (k.ge.jt(i) .and. k.le.j0(i)) then
            f(i,k) = max(f(i,k),f(i,k-1))
          end if
        end do
      end do
      do i = 1,il2g
        eps0(i) = f(i,j0(i))
        eps(i,jb(i)) = eps0(i)
      end do
      do k=plev,msg+1,-1
        do i=1,il2g
          if (k.ge.j0(i)) then
            if (k.le.jb(i)) eps(i,k) = f(i,j0(i))
          else
            if (k.ge.jt(i)) eps(i,k) = f(i,k)
          end if
        end do
      end do
C
C specify the updraft mass flux mu, entrainment eu, detrainment du
C and moist static energy hu.
C here and below mu, eu,du, md and ed are all normalized by mb
C
      do i = 1,il2g
        if (eps0(i).gt.0.) then
          mu(i,jb(i)) = 1.
          eu(i,jb(i)) = eps0(i)/2.
          mu2(i,jb(i)) = 1.
          eu2(i,jb(i)) = mu(i,jb(i))/dz(i,jb(i))
        end if
      end do
      do k = plev,msg + 1,-1
        do i = 1,il2g
          if (eps0(i).gt.0. .and. k.ge.jt(i) .and. k.lt.jb(i)) then
            zuef(i) = zf(i,k) - zf(i,jb(i))
            rmue(i) = (1./eps0(i))* (exp(eps(i,k+1)*zuef(i))-1.)/zuef(i)
            mu(i,k) = (1./eps0(i))* (exp(eps(i,k)*zuef(i))-1.)/zuef(i)
            eu(i,k) = (rmue(i)-mu(i,k+1))/dz(i,k)
            du(i,k) = (rmue(i)-mu(i,k))/dz(i,k)
            mu2(i,k) = mu(i,k)
            eu2(i,k) = eu(i,k)
            du2(i,k) = du(i,k)
          end if
        end do
      end do
C
      khighest = plevp
      klowest = 1
      do i=1,il2g
        khighest = min(khighest,lel(i))
        klowest = max(klowest,jb(i))
      end do
      do k = klowest-1,khighest,-1
cdir$ ivdep
        do i = 1,il2g
          if (k.le.jb(i)-1 .and. k.ge.lel(i) .and. eps0(i).gt.0.) then
            if (mu(i,k).lt.0.01) then
              hu(i,k) = hu(i,jb(i))
              mu(i,k) = 0.
              mu2(i,k) = mu(i,k)
              eu2(i,k) = 0.
              du2(i,k) = mu2(i,k+1)/dz(i,k)
            else
              hu(i,k) = mu(i,k+1)/mu(i,k)*hu(i,k+1) +
     $                 dz(i,k)/mu(i,k)* (eu(i,k)*hmn(i,k)-
     $                 du(i,k)*hsat(i,k))
            end if
          end if
        end do
      end do
C
C reset cloud top index beginning from two layers above the
C cloud base (i.e. if cloud is only one layer thick, top is not reset
C
      do i=1,il2g
        doit(i) = .true.
      end do
      do k=klowest-2,khighest-1,-1
        do i=1,il2g
          if (doit(i) .and. k.le.jb(i)-2 .and. k.ge.lel(i)-1) then
            if (hu(i,k  ).le.hsthat(i,k) .and.
     $          hu(i,k+1).gt.hsthat(i,k+1) .and. mu(i,k).ge.0.02) then
              if (hu(i,k)-hsthat(i,k).lt.-2000.) then
                jt(i) = k + 1
                doit(i) = .false.
              else
                jt(i) = k
                if (eps0(i).le.0.) doit(i) = .false.
              end if
            else if (hu(i,k).gt.hu(i,jb(i)) .or. mu(i,k).lt.0.01) then
              jt(i) = k + 1
              doit(i) = .false.
            end if
          end if
        end do
      end do
      do k = plev,msg + 1,-1
cdir$ ivdep
        do i = 1,il2g
          if (k.ge.lel(i) .and. k.le.jt(i) .and. eps0(i).gt.0.) then
            mu(i,k) = 0.
            eu(i,k) = 0.
            du(i,k) = 0.
            mu2(i,k) = 0.
            eu2(i,k) = 0.
            du2(i,k) = 0.
            hu(i,k) = hu(i,jb(i))
          end if
          if (k.eq.jt(i) .and. eps0(i).gt.0.) then
            du(i,k) = mu(i,k+1)/dz(i,k)
            du2(i,k) = mu2(i,k+1)/dz(i,k)
            eu2(i,k) = 0.
            mu2(i,k) = 0.
          end if
        end do
      end do
C
C specify downdraft properties (no downdrafts if jd.ge.jb).
C scale down downward mass flux profile so that net flux
C (up-down) at cloud base in not negative.
C
      do i = 1,il2g
C
C in normal downdraft strength run alfa=0.2.  In test4 alfa=0.1
C
        alfa(i) = 0.1
        jt(i) = min(jt(i),jb(i)-1)
        jd(i) = max(j0(i),jt(i)+1)
        jd(i) = min(jd(i),jb(i))
        hd(i,jd(i)) = hmn(i,jd(i)-1)
        ud(i,jd(i)) = u(i,jd(i)-1)
        vd(i,jd(i)) = v(i,jd(i)-1)
        if (jd(i).lt.jb(i) .and. eps0(i).gt.0.) then
          epsm(i) = eps0(i)
c          alfa(i)=2.*epsm(i)*( zf(i,jd(i))-zf(i,jb(i)) )/
c     1         (  exp(2.*epsm(i)*( zf(i,jd(i))-
c               zf(i,jb(i)) ))-1.  )
          md(i,jd(i)) = -alfa(i)*epsm(i)/eps0(i)
          md2(i,jd(i)) = md(i,jd(i))
        end if
      end do
      do k = msg + 1,plev
        do i = 1,il2g
          if ((k.gt.jd(i).and.k.le.jb(i)) .and. eps0(i).gt.0.) then
            zdef(i) = zf(i,jd(i)) - zf(i,k)
            md(i,k) = -alfa(i)/ (2.*eps0(i))*
     $               (exp(2.*epsm(i)*zdef(i))-1.)/zdef(i)
            md2(i,k) = md(i,k)
          end if
        end do
      end do
#define CHECKUPFLUXES
#undef CHECKUPFLUXES
#ifdef CHECKUPFLUXES
      k = plev
      do i = 1,il2g
        res = mu2(i,k)/dz(i,k) - eu2(i,k) + du2(i,k)
        if (res.gt.1.e-10*max(eu2(i,k),du2(i,k))) then
          write (6,*) 'inconsistent mass fluxes ', i,k,lat
          write (6,*) 'mt, mb, eu*dz, du*dz ', mu2(i,k),
     $         0., eu2(i,k), du2(i,k)
          stop
        endif
      end do
      do k = 1,plev-1
        do i = 1,il2g
          res = (mu2(i,k)-mu2(i,k+1))/dz(i,k) - eu2(i,k) + du2(i,k)
          if (res.gt.1.e-10*max(eu2(i,k),du2(i,k))) then
            write (6,*) 'inconsistent mass fluxes ', i,k,lat
            write (6,*) 'mt, mb, eu*dz, du*dz ', mu2(i,k),
     $           mu2(i,k+1), eu2(i,k), du2(i,k)
            stop
          endif
        end do
      end do
#endif
      do k = msg + 1,plev
cdir$ ivdep
        do i = 1,il2g
          if ((k.ge.jt(i).and.k.le.jb(i)) .and. eps0(i).gt.0. .and.
     $         jd(i).lt.jb(i)) then
            ratmjb(i) = min(abs(mu2(i,jb(i))/md2(i,jb(i))),1.)
            md2(i,k) = md2(i,k)*ratmjb(i)
            ratmjb(i) = min(abs(mu(i,jb(i))/md(i,jb(i))),1.)
            md(i,k) = md(i,k)*ratmjb(i)
          end if
        end do
      end do
      do k = msg + 1,plev
         do i = 1,il2g
            if ((k.gt.jd(i).and.k.le.jb(i)) .and. eps0(i).gt.0.) then
               ed(i,k-1) = (md(i,k-1)-md(i,k))/dz(i,k-1)
               hd(i,k) = md(i,k-1)/md(i,k)*hd(i,k-1) -
     $                    dz(i,k-1)/md(i,k)*ed(i,k-1)*hmn(i,k-1)
               ud(i,k) = md(i,k-1)/md(i,k)*ud(i,k-1) -
     $                    dz(i,k-1)/md(i,k)*ed(i,k-1)*u(i,k-1)
               vd(i,k) = md(i,k-1)/md(i,k)*vd(i,k-1) -
     $                    dz(i,k-1)/md(i,k)*ed(i,k-1)*v(i,k-1)
            end if
         end do
      end do
      small = 1.e-20
      do k = msg + 1,plev
        do i = 1,il2g
          if ((k.ge.jt(i).and.k.le.plev) .and. eps0(i).gt.0.) then
c         if ((k.ge.jt(i).and.k.le.jb(i)) .and. eps0(i).gt.0.) then
c         if ((k.gt.jd(i).and.k.le.jb(i)) .and. eps0(i).gt.0.) then
            ed2(i,k-1) = (md2(i,k-1)-md2(i,k))/dz(i,k-1)
c            mdt = min(md2(i,k),-small)
c            hd(i,k) = (md(i,k-1)*hd(i,k-1) -
c     $                 dz(i,k-1)*ed(i,k-1)*hmn(i,k-1))/mdt
          end if
        end do
      end do
C
C calculate updraft and downdraft properties.
C
      do k = msg + 2,plev
        do i = 1,il2g
          if ((k.ge.jd(i).and.k.le.jb(i)) .and. eps0(i).gt.0. .and.
     $        jd(i).lt.jb(i)) then
c         sd(i,k) = shat(i,k)
c    1             +              (hd(i,k)-hsthat(i,k))/
c    2               (cp    *(1.+gamhat(i,k)))
            qds(i,k) = qsthat(i,k) + gamhat(i,k)*(hd(i,k)-hsthat(i,k))/
     $                 (rl*(1. + gamhat(i,k)))
          end if
        end do
      end do
C
      do i = 1,il2g
         done(i) = .false.
      end do
      kount = 0
      do k = plev,msg + 2,-1
        do i = 1,il2g
          if ((.not.done(i) .and. k.gt.jt(i) .and. k.lt.jb(i)) .and. 
     $         eps0(i).gt.0.) then
            su(i,k) = mu(i,k+1)/mu(i,k)*su(i,k+1) +
     $                dz(i,k)/mu(i,k)* (eu(i,k)-du(i,k))*s(i,k)
            qu(i,k) = mu(i,k+1)/mu(i,k)*qu(i,k+1) +
     $                dz(i,k)/mu(i,k)* (eu(i,k)*q(i,k)-
     $                du(i,k)*qst(i,k))
            tu = su(i,k) - grav/cp*zf(i,k)
            estu = c1*exp((c2* (tu-tfreez))/ ((tu-tfreez)+c3))
            qstu = eps1*estu/ ((p(i,k)+p(i,k-1))/2.-estu)
            if (qu(i,k).ge.qstu) then
              jlcl(i) = k
              kount = kount + 1
              done(i) = .true.
            end if
          end if
        end do
        if (kount.ge.il2g) goto 690
      end do
 690  continue
      do k = msg + 2,plev
        do i = 1,il2g
          if (k.eq.jb(i) .and. eps0(i).gt.0.) then
            qu(i,k) = q(i,mx(i))
            su(i,k) = (hu(i,k)-rl*qu(i,k))/cp
          end if
          if ((k.gt.jt(i).and.k.le.jlcl(i)) .and. eps0(i).gt.0.) then
            su(i,k) = shat(i,k) + (hu(i,k)-hsthat(i,k))/
     $               (cp* (1.+gamhat(i,k)))
            qu(i,k) = qsthat(i,k) + gamhat(i,k)*
     $               (hu(i,k)-hsthat(i,k))/
     $               (rl* (1.+gamhat(i,k)))
          end if
        end do
      end do
C
      do k = plev,msg + 2,-1
        do i = 1,il2g
          if (k.ge.jt(i) .and. k.lt.jb(i) .and. eps0(i).gt.0.) then
            cu(i,k) = ((mu(i,k)*su(i,k)-mu(i,k+1)*su(i,k+1))/
     $               dz(i,k)- (eu(i,k)-du(i,k))*s(i,k))/
     $               (rl/cp)
            if (k.eq.jt(i)) cu(i,k) = 0.
c               cu(i,k) = max(0.,cu(i,k))
c               cu2     = max(0.,
c               cu2     = max(-1.e99,
c     $                   +(eu(i,k)*q(i,k) - du(i,k)*qst(i,k))
c     $                   -(mu(i,k)*qu(i,k)-mu(i,k+1)*qu(i,k+1))/dz(i,k)
c     $                   )
c                  
c               if (abs(cu(i,k)-cu2)/(abs(cu(i,k))+abs(cu2)+1.e-50)
c     $              .gt.0.0000001) then
c                  write (6,*) ' inconsistent condensation rates ', 
c     $                 i, k, lat,
c     $                 cu(i,k), cu2, jt(i), jb(i), jlcl(i), lel(i)
c     $                 ,mu(i,k)
c               endif
          end if
        end do
      end do
C
      beta = 0.
      c0 = 2.E-3
      do k = plev,msg + 2,-1
        do i = 1,il2g
          cmfdqr(i,k) = 0.
C this modification is for test3 run, modified on 6/20/1995
ccc        if(t(i,jt(i) ).gt.tfreez)    c0=0.
ccc        if(t(i,jt(i) ).le.tfreez   )    c0=2.e-3
          if (k.ge.jt(i) .and. k.lt.jb(i) .and. eps0(i).gt.0. .and.
     $         mu(i,k).ge.0.0) then
            if (mu(i,k).gt.0.) then
              ql1 = 1./mu(i,k)* (mu(i,k+1)*ql(i,k+1)-
     $             dz(i,k)*du(i,k)*ql(i,k+1)+dz(i,k)*cu(i,k))
              ql(i,k) = ql1/ (1.+dz(i,k)*c0)
            else
              ql(i,k) = 0.
            end if
            totpcp(i) = totpcp(i) + dz(i,k)*(cu(i,k)-du(i,k)*
     $                  (beta*ql(i,k) + (1. - beta)*ql(i,k+1)))
            cmfdqr(i,k) = c0*mu(i,k)*ql(i,k)
          end if
        end do
      end do
C
      do i = 1,il2g
        qd(i,jd(i)) = qds(i,jd(i))
        sd(i,jd(i)) = (hd(i,jd(i)) - rl*qd(i,jd(i)))/cp
#ifdef NEWVER
cv     mod to make my downdraft calculaton match guangs assumption
c      but still conserve energy. For consistency we should probably 
c      also set sd(i,jd(i) = s(i,jd(i)-1)
c      it would also make more sense to me to use qhat and shat
c      but I am trying to minimize changes to guangs algorithm
c
c         qd(i,jd(i)) = q(i,jd(i)-1)
#endif
#undef NEWVER
      end do
C
#undef GUANGSWAY3
#define GUANGSWAY3
#ifdef GUANGSWAY3
      do k = msg + 2,plev
        do i = 1,il2g
          if ((k.ge.jd(i).and.k.lt.jb(i)) .and. eps0(i).gt.0. .and.
     $         jd(i).lt.jb(i)) then
            qd(i,k+1) = qds(i,k+1)
            sd(i,k+1) = (hd(i,k+1)-rl*qd(i,k+1))/cp
            evp(i,k) = -ed(i,k)*q(i,k) +
     $                (md(i,k)*qd(i,k)-md(i,k+1)*qd(i,k+1))/dz(i,k)
            if (k.eq.jd(i)) then
              evp(i,k) = -ed(i,k)*q(i,k) +
     $                  (md(i,k)*q(i,k-1)-md(i,k+1)*qd(i,k+1))/dz(i,k)
            end if
            evp(i,k) = max(evp(i,k),0.)
            totevp(i) = totevp(i) - dz(i,k)*ed(i,k)*q(i,k)
            end if
         end do
      end do
C
      do i = 1,il2g
        totevp(i) = totevp(i) + md(i,jd(i))*q(i,jd(i)-1) -
     $              md(i,jb(i))*qd(i,jb(i))
      end do
C
C no evaporation below cloud base is considered
C
#else
      do k = msg + 2,plev
        do i = 1,il2g
          if (k.ge.jd(i).and.k.lt.jb(i) .and. eps0(i).gt.0.) then
            qd(i,k+1) = qds(i,k+1)
            evp(i,k) = -ed(i,k)*q(i,k) +
     $                (md(i,k)*qd(i,k)-md(i,k+1)*qd(i,k+1))/dz(i,k)
            evp(i,k) = max(evp(i,k),0.)
            mdt = min(md(i,k+1),-small)
            sd(i,k+1) = ((rl/cp*evp(i,k)-ed(i,k)*s(i,k))*dz(i,k) +
     $                    md(i,k)*sd(i,k))/mdt
            totevp(i) = totevp(i) - dz(i,k)*ed(i,k)*q(i,k)
          end if
        end do
      end do
      do i = 1,il2g
c*guang         totevp(i) = totevp(i) + md(i,jd(i))*q(i,jd(i)-1) -
        totevp(i) = totevp(i) + md(i,jd(i))*qd(i,jd(i)) -
     $              md(i,jb(i))*qd(i,jb(i))
      end do
      if (.true.) then
        do i = 1,il2g
          k = jb(i)
          if (eps0(i).gt.0.) then
            evp(i,k) = -ed(i,k)*q(i,k) + (md(i,k)*qd(i,k))/dz(i,k)
            evp(i,k) = max(evp(i,k),0.)
            totevp(i) = totevp(i) - dz(i,k)*ed(i,k)*q(i,k)
          end if
        end do
      endif
#endif
      do i = 1,il2g
        totpcp(i) = max(totpcp(i),0.)
        totevp(i) = max(totevp(i),0.)
      end do
C
      weight = 1.0
      do k = msg + 2,plev
        do i = 1,il2g
          if (totevp(i).gt.0. .and. totpcp(i).gt.0. .and.
     $         k.ge.jd(i) .and. k.le.jb(i)) then
            md(i,k) = md(i,k)*min(1.,weight*totpcp(i)/
     $               (totevp(i)+weight*totpcp(i)))
            ed(i,k) = ed(i,k)*min(1.,weight*totpcp(i)/
     $               (totevp(i)+weight*totpcp(i)))
            evp(i,k) = evp(i,k)*min(1.,
     $                weight*totpcp(i)/ (totevp(i)+
     $                weight*totpcp(i)))
          else
            md(i,k) = 0.
            ed(i,k) = 0.
            evp(i,k) = 0.
          end if
c
c cmeg is the cloud water condensed - rain water evaporated
c cmfdqr  is the cloud water converted to rain - (rain evaporated)
c
          cmeg(i,k) = cu(i,k) - evp(i,k)
          cmfdqr(i,k) = cmfdqr(i,k)-evp(i,k)
        end do
      end do
C
      do k = msg + 2,plev
        do i = 1,il2g
          if (totevp(i).gt.0. .and. totpcp(i).gt.0.) then
            md2(i,k) = md2(i,k)*min(1.,weight*totpcp(i)/
     $                (totevp(i)+weight*totpcp(i)))
            ed2(i,k) = ed2(i,k)*min(1.,weight*totpcp(i)/
     $                (totevp(i)+weight*totpcp(i)))
          else
            md2(i,k) = 0.
            ed2(i,k) = 0.
          end if
        end do
      end do
      do k = 2,plev
        do i = 1,il2g
          pflx(i,k) = pflx(i,k-1) + cmfdqr(i,k)*dz(i,k)
        end do
      end do
      do i = 1,il2g
        if (totevp(i).gt.0. .and. totpcp(i).gt.0.) then
          totevp(i) = totevp(i)*min(1.,
     $               weight*totpcp(i)/(totevp(i) + weight*totpcp(i)))
        else
          totevp(i) = 0.
        end if
      end do
C
      do k = msg + 1,plev
        do i = 1,il2g
          if (k.ge.jt(i) .and. k.le.jb(i)) then
            mc(i,k) = mu(i,k) + md(i,k)
          end if
        end do
      end do
C
      return
      end
 
