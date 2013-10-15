#include <misc.h>
#include <params.h>
      subroutine closure(q       ,t       ,p       ,z       ,s       ,
     $                   tp      ,qs      ,qu      ,su      ,mc      ,
     $                   du      ,mu      ,md      ,qd      ,sd      ,
     $                   alpha   ,qhat    ,shat    ,dp      ,qstp    ,
     $                   zf      ,ql      ,dsubcld ,mb      ,cape    ,
     $                   tl      ,lcl     ,lel     ,jt      ,mx      ,
     $                   il1g    ,il2g    ,rd      ,grav    ,cp      ,
     $                   rl      ,msg     ,capelmt ,nstep   ,lat     )
C-----------------------------------------------------------------------
C
C This is contributed code not fully standardized by the CCM core group.
C
C this code is very much rougher than virtually anything else in the CCM
C We expect to release cleaner code in a future release
C
c the documentation has been enhanced to the degree that we are able
c
C Original version:  G. Zhang and collaborators
C Standardized:      Core group staff, 1994 and 195
C Reviewed:          P. Rasch, April 1996
C-----------------------------------------------------------------------
C
C $Id: closure.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
C
#include <implicit.h>
C-----------------------------------------------------------------------
#include <pmgrid.h>
C--------------------------Commons--------------------------------------
#include <guang.h>
C-----------------------------------------------------------------------
C
C may 09/91 - guang jun zhang, m.lazare, n.mcfarlane.
C
C-----------------------------Arguments---------------------------------
C
      real q(plond,plev)        ! spec humidity
      real t(plond,plev)        ! temperature
      real p(plond,plev)        ! pressure (mb)
      real z(plond,plev)        ! height (m)
      real s(plond,plev)        ! normalized dry static energy 
      real tp(plond,plev)       ! parcel temp
      real qs(plond,plev)       ! sat spec humidity
      real qu(plond,plev)       ! updraft spec. humidity
      real su(plond,plev)       ! normalized dry stat energy of updraft
      real mc(plond,plev)       ! net convective mass flux 
      real du(plond,plev)       ! detrainment from updraft
      real mu(plond,plev)       ! mass flux of updraft
      real md(plond,plev)       ! mass flux of downdraft
      real qd(plond,plev)       ! spec. humidity of downdraft
      real sd(plond,plev)       ! dry static energy of downdraft
      real alpha(plond,plev)
      real qhat(plond,plev)     ! environment spec humidity at interfaces
      real shat(plond,plev)     ! env. normalized dry static energy at intrfcs
      real dp(plond,plev)       ! pressure thickness of layers
      real qstp(plond,plev)     ! spec humidity of parcel
      real zf(plond,plev+1)     ! height of interface levels
      real ql(plond,plev)       ! liquid water mixing ratio

      real mb(plond)            ! cloud base mass flux
      real cape(plond)          ! available pot. energy of column
      real tl(plond)
      real dsubcld(plond)       ! thickness of subcloud layer

      integer lcl(plond)        ! index of lcl
      integer lel(plond)        ! index of launch leve
      integer jt(plond)         ! top of updraft
      integer mx(plond)         ! base of updraft
C
C--------------------------Local variables------------------------------
C
      real dtpdt(plond,plev)
      real dqsdtp(plond,plev)
      real dtmdt(plond,plev)
      real dqmdt(plond,plev)
      real dboydt(plond,plev)
      real thetavp(plond,plev)
      real thetavm(plond,plev)

      real dtbdt(plond),dqbdt(plond),dtldt(plond)
      real beta
      real capelmt
      real cp
      real dadt
      real debdt
      real dltaa
      real eb
      real grav

      integer i
      integer il1g
      integer il2g
      integer k
      integer lat
      integer msg
      integer nstep

      real rd
      real rl
      real tau
      save tau
C
C tau=4800. were used in canadian climate center. however, when it
C is used here in echam3 t42, convection is too weak, thus 
C adjusted to 2400. i.e the e-folding time is 1 hour now.
C
      data tau/7200./
C-----------------------------------------------------------------------
C change of subcloud layer properties due to convection is
C related to cumulus updrafts and downdrafts.
C mc(z)=f(z)*mb, mub=betau*mb, mdb=betad*mb are used
C to define betau, betad and f(z).
C note that this implies all time derivatives are in effect
C time derivatives per unit cloud-base mass flux, i.e. they
C have units of 1/mb instead of 1/sec.
C
      do i = il1g,il2g
         mb(i) = 0.
         eb = p(i,mx(i))*q(i,mx(i))/ (eps1+q(i,mx(i)))
         dtbdt(i) = (1./dsubcld(i))* (mu(i,mx(i))*
     $               (shat(i,mx(i))-su(i,mx(i)))+
     $               md(i,mx(i))* (shat(i,mx(i))-sd(i,mx(i))))
         dqbdt(i) = (1./dsubcld(i))* (mu(i,mx(i))*
     $               (qhat(i,mx(i))-qu(i,mx(i)))+
     $               md(i,mx(i))* (qhat(i,mx(i))-qd(i,mx(i))))
         debdt = eps1*p(i,mx(i))/ (eps1+q(i,mx(i)))**2*dqbdt(i)
         dtldt(i) = -2840.* (3.5/t(i,mx(i))*dtbdt(i)-debdt/eb)/
     $               (3.5*log(t(i,mx(i)))-log(eb)-4.805)**2
      end do
C
C   dtmdt and dqmdt are cumulus heating and drying.
C
      do k = msg + 1,plev
         do i = il1g,il2g
            dtmdt(i,k) = 0.
            dqmdt(i,k) = 0.
         end do
      end do
C
      do k = msg + 1,plev - 1
         do i = il1g,il2g
            if (k.eq.jt(i)) then
               dtmdt(i,k) = (1./dp(i,k))*
     $                       (mu(i,k+1)* (su(i,k+1)-shat(i,k+1)-
     $                       rl/cp*ql(i,k+1))+md(i,k+1)* (sd(i,k+1)-
     $                       shat(i,k+1)))
               dqmdt(i,k) = (1./dp(i,k))*(mu(i,k+1)* (qu(i,k+1)-
     $                       qhat(i,k+1)+ql(i,k+1))+md(i,k+1)*
     $                       (qd(i,k+1)-qhat(i,k+1)))
            end if
         end do
      end do
C
      beta = 0.
      do k = msg + 1,plev - 1
         do i = il1g,il2g
            if (k.gt.jt(i) .and. k.lt.mx(i)) then
               dtmdt(i,k) = (mc(i,k)* (shat(i,k)-s(i,k))+
     $                       mc(i,k+1)* (s(i,k)-shat(i,k+1)))/
     $                       dp(i,k) - rl/cp*du(i,k)*
     $                       (beta*ql(i,k)+ (1-beta)*ql(i,k+1))
c          dqmdt(i,k)=(mc(i,k)*(qhat(i,k)-q(i,k))
c     1                +mc(i,k+1)*(q(i,k)-qhat(i,k+1)))/dp(i,k)
c     2                +du(i,k)*(qs(i,k)-q(i,k))
c     3                +du(i,k)*(beta*ql(i,k)+(1-beta)*ql(i,k+1))

               dqmdt(i,k) = (mu(i,k+1)* (qu(i,k+1)-qhat(i,k+1)+
     $                       cp/rl* (su(i,k+1)-s(i,k)))-
     $                       mu(i,k)* (qu(i,k)-qhat(i,k)+cp/rl* 
     $                       (su(i,k)-s(i,k)))+md(i,k+1)* 
     $                       (qd(i,k+1)-qhat(i,k+1)+cp/rl* 
     $                       (sd(i,k+1)-s(i,k)))-md(i,k)* 
     $                       (qd(i,k)-qhat(i,k)+cp/rl* 
     $                       (sd(i,k)-s(i,k))))/dp(i,k) +
     $                       du(i,k)* (beta*ql(i,k)+
     $                       (1-beta)*ql(i,k+1))
            end if
         end do
      end do
C
      do k = msg + 1,plev
         do i = il1g,il2g
            if (k.ge.lel(i) .and. k.le.lcl(i)) then
               thetavp(i,k) = tp(i,k)* (1000./p(i,k))** (rd/cp)*
     $                         (1.+1.608*qstp(i,k)-q(i,mx(i)))
               thetavm(i,k) = t(i,k)* (1000./p(i,k))** (rd/cp)*
     $                         (1.+0.608*q(i,k))
               dqsdtp(i,k) = qstp(i,k)* (1.+qstp(i,k)/eps1)*eps1*rl/
     $                         (rd*tp(i,k)**2)
C
C dtpdt is the parcel temperature change due to change of
C subcloud layer properties during convection.
C
               dtpdt(i,k) = tp(i,k)/ (1.+
     $                       rl/cp* (dqsdtp(i,k)-qstp(i,k)/tp(i,k)))*
     $                        (dtbdt(i)/t(i,mx(i))+
     $                       rl/cp* (dqbdt(i)/tl(i)-q(i,mx(i))/
     $                       tl(i)**2*dtldt(i)))
C
C dboydt is the integrand of cape change.
C
               dboydt(i,k) = ((dtpdt(i,k)/tp(i,k)+1./ 
     $                        (1.+1.608*qstp(i,k)-q(i,mx(i)))* 
     $                        (1.608 * dqsdtp(i,k) * dtpdt(i,k) -
     $                        dqbdt(i))) - (dtmdt(i,k)/t(i,k)+0.608/ 
     $                        (1.+0.608*q(i,k))*dqmdt(i,k)))*grav*
     $                        thetavp(i,k)/thetavm(i,k)
            end if
         end do
      end do
C
      do k = msg + 1,plev
         do i = il1g,il2g
            if (k.gt.lcl(i) .and. k.lt.mx(i)) then
               thetavp(i,k) = tp(i,k)* (1000./p(i,k))** (rd/cp)*
     $                         (1.+0.608*q(i,mx(i)))
               thetavm(i,k) = t(i,k)* (1000./p(i,k))** (rd/cp)*
     $                         (1.+0.608*q(i,k))
C
C dboydt is the integrand of cape change.
C
               dboydt(i,k) = (dtbdt(i)/t(i,mx(i))+
     $                        0.608/ (1.+0.608*q(i,mx(i)))*dqbdt(i)-
     $                        dtmdt(i,k)/t(i,k)-
     $                        0.608/ (1.+0.608*q(i,k))*dqmdt(i,k))*
     $                        grav*thetavp(i,k)/thetavm(i,k)
            end if
         end do
      end do

C
C buoyant energy change is set to 2/3*excess cape per 3 hours
C
      do i = il1g,il2g
         dadt = 0.
         do k = lel(i),mx(i) - 1
            dadt = dadt + dboydt(i,k)* (zf(i,k)-zf(i,k+1))
         end do
C
         dltaa = -1.* (cape(i)-capelmt)
         if (dadt.ne.0.) mb(i) = max(dltaa/tau/dadt,0.)
      end do
C
      return
      end
 
