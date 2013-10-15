#include <misc.h>
#include <params.h>
      subroutine q1q2(dqdt    ,dsdt    ,q       ,s       ,qs      ,
     $                qu      ,su      ,mc      ,du      ,alpha   ,
     $                qhat    ,shat    ,dp      ,mu      ,md      ,
     $                sd      ,qd      ,ql      ,dsubcld ,qdb     ,
     $                sdb     ,betau   ,betad   ,mb      ,lcl     ,
     $                jt      ,mx      ,dt      ,il1g    ,il2g    ,
     $                cp      ,rl      ,msg     ,nstep   ,lat     ,
     $                dl      ,cu      )
C-----------------------------------------------------------------------
C
C This is contributed code which is not fully standardized.  Both the
C internal documentation and the coding style itself are somewhat
C different from the rest of the standard CCM3 model code.  It is hoped
C that the consistency will be improved in future versions
C
C Reference: Zhang and McFarland, 1995, Atmos. Ocean
C
C PLEASE NOTE:
C
C The form of the equations used in this subroutine differ from that of
C the above reference. The forms are equivalent in the continuous form
C but differ in the descrete form. We have preserved the form as
C supplied by Zhang in order to prevent changes in the simulation that
C would result from a change to a more familiar form.
C
C This code is very much rougher than virtually anything else in the CCM
C there are debug statements left strewn about and code segments
C disabled.  These are to facilitate future development.  We expect to
C release a cleaner code in a future release
C
C---------------------------Code history--------------------------------
C
C Original version:  G. Zhang
C Modified:          G. Zhang, P. Rasch and J. Hack, 1994 and 1995
C Standardized:      CCM Core Group, 1995
C Reviewed:          P. Rasch, April 1996
C
C-----------------------------------------------------------------------
c
c $Id: q1q2.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c
C------------------------------------------------------------------------
#include <pmgrid.h>
C-----------------------------------------------------------------------
C
C jul 14/92 - guang jun zhang, m.lazare.  like previous version
C             q1q2 except modify definitions of qhat and shat to
C             avoid pathological cases where very small gradients
C             exist between adjacent levels .
C feb 18/91 - guang jun zhang, m.lazare, n.mcfarlane.  previous
C             version q1q2.
C
C-----------------------------Arguments---------------------------------
C
      real dqdt(plond,plev)     ! moisture tendency (kg/kg/s)
      real dsdt(plond,plev)     ! heating rate (K/s)
C
      real q(plond,plev)        ! specific humidity (kg/kg)
      real s(plond,plev)        ! dry static energy divided by Cp (K)
      real qs(plond,plev)       ! saturation specific humidity (kg/kg)
      real qu(plond,plev)       ! updraft spec. humidity (kg/kg)
      real su(plond,plev)       ! updraft dry static energy divided by Cp (K)
      real mc(plond,plev)       ! sum of mu and md
      real du(plond,plev)       ! detraining mass from updraft
      real alpha(plond,plev)    !
      real qhat(plond,plev)     ! env. specific humidity at interfaces
      real shat(plond,plev)     ! env. dry static energy divided by Cp (K) at interfaces
      real dp(plond,plev)       ! pressure thickness
      real mu(plond,plev)       ! updraft mass flux
      real md(plond,plev)       ! downdraft mass flux
      real sd(plond,plev)       ! downdraft normalized dry stat energy
      real qd(plond,plev)       ! downdraft mass flux
      real ql(plond,plev)       ! liquid water in updraft
      real dl(plond,plev)       ! detrained liquid water
      real cu(plond,plev)       ! condensation rate in updraft

      real dsubcld(plond)       ! pressure thickness below cloud base
      real qdb(plond)           ! 
      real sdb(plond)           ! 
      real betau(plond)         ! 
      real betad(plond)         ! 
      real mb(plond)            ! mass flux at cloud base

      integer lcl(plond)        !
      integer jt(plond)         ! index of top of updraft
      integer mx(plond)         ! index of base of updraft

C work fields:

      real facq(plond,plev)     ! 
      real facs(plond,plev)     ! 
      real dsds(plond,plev)     ! 
      real dqdq(plond,plev)     ! 

      real rl                   ! latent heat of vap.
C-------------------------------------------------------------------
      rnu = 0.
      do k = msg + 1,plev
         do i = il1g,il2g
            dsdt(i,k) = 0.
            dqdt(i,k) = 0.
            if (k.ge.jt(i) .and. k.lt.plev) then
               sdif = 0.
               if (s(i,k).gt.0. .or. s(i,k+1).gt.
     $             0.) sdif = abs((s(i,k)-s(i,k+1))/
     $             max(s(i,k),s(i,k+1)))
               if (sdif.gt.1.E-6) then
                  dsds(i,k) = s(i,k+1)/ (s(i,k)-s(i,k+1)) -
     $                         log(s(i,k)/s(i,k+1))*
     $                         (s(i,k+1)/ (s(i,k)-s(i,k+1)))**2
               else
                  dsds(i,k) = 0.5
               end if
               qdif = 0.
               if (q(i,k).gt.0. .or. q(i,k+1).gt.
     $             0.) qdif = abs((q(i,k)-q(i,k+1))/
     $             max(q(i,k),q(i,k+1)))
               if (qdif.gt.1.E-6) then
                  dqdq(i,k) = q(i,k+1)/ (q(i,k)-q(i,k+1)) -
     $                         log(q(i,k)/q(i,k+1))*
     $                         (q(i,k+1)/ (q(i,k)-q(i,k+1)))**2
               else
                  dqdq(i,k) = 0.5
               end if
               facs(i,k) = 1./ (1.+mc(i,k+1)*dt/dp(i,k)*rnu*
     $                      dsds(i,k))
               facq(i,k) = 1./ (1.+mc(i,k+1)*dt/dp(i,k)*rnu*
     $                      dqdq(i,k))
            end if
         end do
      end do
C
C this effect is included in test4
C this feature is included on dec. 8, 1994
C following tiedtke, if rh > 80% detrained liquid water has no
C cooling or moistening effect, thus effectively becomes 
C convective precipitation.
C
      do i = il1g,il2g
         if (mb(i).gt.0.) then
            fact = 1.
            if (q(i,jt(i)).gt.0.8*qs(i,jt(i)) .and.
     $          jt(i).lt.plev-3) fact = 0.
            dsdt(i,jt(i)) = facs(i,jt(i))/dp(i,jt(i))*
     $                        (mu(i,jt(i)+1)* (su(i,
     $                        jt(i)+1)-shat(i,jt(i)+1)-
     $                        rl/cp*fact*ql(i,jt(i)+1))+
     $                        md(i,jt(i)+1)* (sd(i,jt(i)+1)-shat(i,
     $                        jt(i)+1)))
            dqdt(i,jt(i)) = facq(i,jt(i))/dp(i,jt(i))*
     $                        (mu(i,jt(i)+1)* (qu(i,
     $                        jt(i)+1)-qhat(i,jt(i)+1)+fact*ql(i,
     $                        jt(i)+1))+md(i,jt(i)+1)*
     $                        (qd(i,jt(i)+1)-qhat(i,jt(i)+1)))
            dl(i,jt(i)) = facq(i,jt(i))/dp(i,jt(i))*
     $                        (mu(i,jt(i)+1)*ql(i,jt(i)+1))
         end if
      end do
C
      beta = 0.
      do k = msg + 1,plev
         do i = il1g,il2g
            if (k.gt.jt(i) .and. k.lt.mx(i) .and. mb(i).gt.0.) then
               fact = 1.
               if (q(i,k).gt.0.8*qs(i,k) .and. k.lt.plev-3) fact = 0.
               shat(i,k) = shat(i,k) + dsdt(i,k-1)*dt*rnu*
     $                      dsds(i,k-1)
               qhat(i,k) = qhat(i,k) + dqdt(i,k-1)*dt*rnu*
     $                      dqdq(i,k-1)
               dsdt(i,k) = facs(i,k)* ((mc(i,k)* (shat(i,k)-
     $                      s(i,k))+mc(i,k+1)* (s(i,k)-
     $                      shat(i,k+1)))/dp(i,k)-rl/cp*fact*du(i,k)*
     $                      (beta*ql(i,k)+ (1-beta)*ql(i,k+1)))
c          dqdt(i,k)=facq(i,k)
c     1               *((mc(i,k)*(qhat(i,k)-q(i,k))
c     2               +mc(i,k+1)*(q(i,k)-qhat(i,k+1)))/dp(i,k)
c     3               +du(i,k)*(qs(i,k)-q(i,k))
c     4         +fact*du(i,k)*(beta*ql(i,k)+(1-beta)*ql(i,k+1)) )

               dqdt(i,k) = facq(i,k)* ((mu(i,k+1)* (qu(i,k+1)-
     $                      qhat(i,k+1)+cp/rl* (su(i,k+1)-s(i,k)))-
     $                      mu(i,k)* (qu(i,k)-qhat(i,k)+cp/rl* 
     $                      (su(i,k)-s(i,k)))+md(i,k+1)* (qd(i,k+1)-
     $                      qhat(i,k+1)+cp/rl* (sd(i,k+1)-s(i,k)))-
     $                      md(i,k)* (qd(i,k)-qhat(i,k)+cp/rl* 
     $                      (sd(i,k)-s(i,k))))/dp(i,k)+fact*du(i,k)* 
     $                      (beta*ql(i,k)+ (1-beta)*ql(i,k+1)))
               dl(i,k) = facq(i,k)*(du(i,k)* 
     $                      (beta*ql(i,k)+ (1-beta)*ql(i,k+1)))
            end if
         end do
      end do
C
      do k = msg + 1,plev             
         do i = il1g,il2g
            if (k.eq.mx(i) .and. mb(i).gt.0.) then
               shat(i,k) = shat(i,k) + dsdt(i,k-1)*dt*rnu*
     $                      dsds(i,k-1)
               qhat(i,k) = qhat(i,k) + dqdt(i,k-1)*dt*rnu*
     $                      dqdq(i,k-1)
               dsdt(i,k) = (1./dsubcld(i))*
     $                      (mu(i,k)* (shat(i,k)-
     $                      su(i,k))+md(i,k)*
     $                      (shat(i,k)-sd(i,k)))
               dqdt(i,k) = (1./dsubcld(i))*
     $                      (mu(i,k)* (qhat(i,k)-
     $                      qu(i,k))+md(i,k)*
     $                      (qhat(i,k)-qd(i,k)))
            else if (k.gt.mx(i) .and. mb(i).gt.0.) then
               dsdt(i,k) = dsdt(i,k-1)
               dqdt(i,k) = dqdt(i,k-1)
            end if
         end do
      end do
C
      return
      end
 
