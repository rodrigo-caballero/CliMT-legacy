#include <misc.h>
#include <params.h>
      subroutine convtran(q     ,mu     ,md      ,du     ,eu     ,
     $                    ed    ,dp     ,dsubcld ,jt     ,mx     ,
     $                    ideep ,il1g   ,il2g    ,nstep  ,lat    ,
     $                    delt    )

C-----------------------------------------------------------------------
C
C Convective transport of trace species
C
C Note that we are still assuming that the tracers are in a moist mixing ratio
C this will change soon
C
C-------------------------Code History----------------------------------
C
C Original version:  P. Rasch, Jan 1996 
C Standardized:      L. Buja,  Feb 1996
C Reviewed:          P. Rasch, Feb 1996      
C 
C-----------------------------------------------------------------------
      implicit none
C--------------------------Commons--------------------------------------
#include <pmgrid.h>
C-----------------------------Arguments---------------------------------
C 
C Input
C
      real mu(plond,plev)       ! Mass flux up
      real md(plond,plev)       ! Mass flux down
      real du(plond,plev)       ! Mass detraining from updraft
      real eu(plond,plev)       ! Mass entraining from updraft
      real ed(plond,plev)       ! Mass entraining from downdraft
      real dp(plond,plev)       ! Delta pressure between interfaces
      real dsubcld(plond)       ! Delta pressure from cloud base to sfc

      integer jt(plond)         ! Index of cloud top for each column
      integer mx(plond)         ! Index of cloud top for each column
      integer ideep(plond)      ! Gathering array
      integer il1g              ! Gathered min lon indices over which to operate
      integer il2g              ! Gathered max lon indices over which to operate
      integer lat               ! Latitude
      integer nstep             ! Time step index

      real delt                 ! Time step

C input/output

      real q(plond,plev,pcnst)  ! Tracer array including moisture

C--------------------------Local Variables------------------------------

      integer i                 ! Work index
      integer k                 ! Work index
      integer kbm               ! Highest altitude index of cloud base
      integer kk                ! Work index
      integer kkp1              ! Work index
      integer km1               ! Work index
      integer kp1               ! Work index
      integer ktm               ! Highest altitude index of cloud top
      integer m                 ! Work index

      real cabv                 ! Mix ratio of constituent above
      real cbel                 ! Mix ratio of constituent below
      real cdifr                ! Normalized diff between cabv and cbel
      real chat(plond,plev)     ! Mix ratio in env at interfaces
      real cond(plond,plev)     ! Mix ratio in downdraft at interfaces
      real const(plond,plev)    ! Gathered tracer array 
      real conu(plond,plev)     ! Mix ratio in updraft at interfaces
      real dcondt(plond,plev)   ! Gathered tend array 
      real small                ! A small number
      real mbsth                ! Threshold for mass fluxes
      real mupdudp              ! A work variable
      real minc                 ! A work variable
      real maxc                 ! A work variable
      real qn                   ! A work variable
      real fluxin               ! A work variable
      real fluxout              ! A work variable
      real netflux              ! A work variable

C-----------------------------------------------------------------------
C
      small = 1.e-36
c mbsth is the threshold below which we treat the mass fluxes as zero (in mb/s)
      mbsth = 1.e-15

c Find the highest level top and bottom levels of convection
      ktm = plev
      kbm = plev
      do i = il1g, il2g
         ktm = min(ktm,jt(i))
         kbm = min(kbm,mx(i))
      end do

c Loop ever each constituent
      do m = 2,pcnst

c Gather up the constituent and set tend to zero
         do k = 1,plev
            do i =il1g,il2g
               const(i,k) = q(ideep(i),k,m)
            end do
         end do

c From now on work only with gathered data

c Interpolate environment tracer values to interfaces
         do k = 1,plev
            km1 = max(1,k-1)
            do i = il1g, il2g
               minc = min(const(i,km1),const(i,k))
               maxc = max(const(i,km1),const(i,k))
               if (minc.lt.0) then
                  cdifr = 0.
               else
                  cdifr = abs(const(i,k)-const(i,km1))/max(maxc,small)
               endif

c If the two layers differ significantly use a geometric averaging
c procedure
               if (cdifr.gt.1.E-6) then
                  cabv = max(const(i,km1),maxc*1.e-12)
                  cbel = max(const(i,k),maxc*1.e-12)
                  chat(i,k) = log(cabv/cbel)
     $                          /(cabv-cbel)
     $                          *cabv*cbel

               else             ! Small diff, so just arithmetic mean
                  chat(i,k) = 0.5* (const(i,k)+const(i,km1))
               end if

c Provisional up and down draft values
               conu(i,k) = chat(i,k)
               cond(i,k) = chat(i,k)

c              provisional tends
               dcondt(i,k) = 0.

            end do
         end do

#ifdef CHECKNEG
         do k = 1,plev
            km1 = max(1,k-1)
            do i = il1g, il2g
               if (chat(i,k).lt.0.) then
                  write (6,*) ' negative chat ', i, k, lat, chat(i,k),
     $                 const(i,km1), const(i,k)
                  stop
               endif
            end do
         end do
#endif


c Do levels adjacent to top and bottom
         k = 2
         km1 = 1
         kk = plev 
         do i = il1g,il2g
            mupdudp = mu(i,kk) + du(i,kk)*dp(i,kk)
            if (mupdudp.gt.mbsth) then
               conu(i,kk) = ( 
     $                       +eu(i,kk)*const(i,kk)*dp(i,kk)
     $                      )/mupdudp
            endif
            if (md(i,k).lt.-mbsth) then
               cond(i,k) =  (  
     $                       -ed(i,km1)*const(i,km1)*dp(i,km1)
     $                      )/md(i,k)
            endif
         end do

c Updraft from bottom to top
         do kk = plev-1,1,-1
            kkp1 = min(plev,kk+1)
            do i = il1g,il2g
               mupdudp = mu(i,kk) + du(i,kk)*dp(i,kk)
               if (mupdudp.gt.mbsth) then
                  conu(i,kk) = (  mu(i,kkp1)*conu(i,kkp1) 
     $                           +eu(i,kk)*const(i,kk)*dp(i,kk)
     $                         )/mupdudp
               endif
            end do
         end do

c Downdraft from top to bottom
         do k = 3,plev
            km1 = max(1,k-1)
            do i = il1g,il2g
               if (md(i,k).lt.-mbsth) then
                  cond(i,k) =  (  md(i,km1)*cond(i,km1) 
     $                           -ed(i,km1)*const(i,km1)*dp(i,km1)
     $                         )/md(i,k)
               endif
            end do
         end do


#ifdef DEBUG
         do k = ktm,plev
            do i = il1g,il2g
c               if (conu(i,k)*mu(i,k).lt.0.) then
               if (conu(i,k).lt.0.) then
                  write (6,*) ' warning negativue cu ', 
     $                 i, k, lat, m, conu(i,k), mu(i,k)
               endif
c               if (cond(i,k)*md(i,k).gt.0.) then
               if (cond(i,k).lt.0.) then
                  write (6,*) ' warning negativue cd ', 
     $                 i, k, lat, m, cond(i,k)
                  km1 = max(1,k-1)
                  write (6,*) 'mda, cda, eda, cone, mdb ',
     $                 md(i,km1),cond(i,km1) ,
     $                         -ed(i,km1),const(i,km1),md(i,k) 
               endif
            end do
         end do
#endif

         do k = ktm,plev
            km1 = max(1,k-1)
            kp1 = min(plev,k+1)
            do i = il1g,il2g

c version 1 hard to check for roundoff errors
c               dcondt(i,k) = 
c     $                  +(+mu(i,kp1)* (conu(i,kp1)-chat(i,kp1))
c     $                    -mu(i,k)*   (conu(i,k)-chat(i,k))
c     $                    +md(i,kp1)* (cond(i,kp1)-chat(i,kp1))
c     $                    -md(i,k)*   (cond(i,k)-chat(i,k))
c     $                   )/dp(i,k)

c version 2 hard to limit fluxes
c               fluxin =  mu(i,kp1)*conu(i,kp1) + mu(i,k)*chat(i,k) 
c     $                 -(md(i,k)  *cond(i,k)   + md(i,kp1)*chat(i,kp1))
c               fluxout = mu(i,k)*conu(i,k)     + mu(i,kp1)*chat(i,kp1)
c     $                 -(md(i,kp1)*cond(i,kp1) + md(i,k)*chat(i,k))

c version 3 limit fluxes outside convection to mass in appropriate layer
c these limiters are probably only safe for positive definite quantitities
c it assumes that mu and md already satify a courant number limit of 1
               fluxin =  mu(i,kp1)*conu(i,kp1) 
     $                 + mu(i,k)*min(chat(i,k),const(i,km1)) 
     $                 -(md(i,k)  *cond(i,k)   
     $                   + md(i,kp1)*min(chat(i,kp1),const(i,kp1)))
               fluxout = mu(i,k)*conu(i,k)     
     $                  +mu(i,kp1)*min(chat(i,kp1),const(i,k))
     $                 -(md(i,kp1)*cond(i,kp1) 
     $                   + md(i,k)*min(chat(i,k),const(i,k)))

               netflux = fluxin - fluxout
               if (abs(netflux).lt.max(fluxin,fluxout)*1.e-12) then
                  netflux = 0.
               endif
               dcondt(i,k) = netflux/dp(i,k)
            end do
         end do
C
         do k = kbm,plev             
            km1 = max(1,k-1)
            do i = il1g,il2g
               if (k.eq.mx(i)) then

c version 1
c                  dcondt(i,k) = (1./dsubcld(i))*
c     $              (-mu(i,k)*(conu(i,k)-chat(i,k))
c     $               -md(i,k)*(cond(i,k)-chat(i,k))
c     $              )

c version 2
                  fluxin =  mu(i,k)*chat(i,k) - md(i,k)*cond(i,k)
                  fluxout = mu(i,k)*conu(i,k) - md(i,k)*chat(i,k)
c version 3
                  fluxin =  mu(i,k)*min(chat(i,k),const(i,km1))
     $                    - md(i,k)*cond(i,k)
                  fluxout = mu(i,k)*conu(i,k) 
     $                 - md(i,k)*min(chat(i,k),const(i,k))

                  netflux = fluxin - fluxout
                  if (abs(netflux).lt.max(fluxin,fluxout)*1.e-12) then
                     netflux = 0.
                  endif
c                  dcondt(i,k) = netflux/dsubcld(i)
                  dcondt(i,k) = netflux/dp(i,k)
               else if (k.gt.mx(i)) then
c                  dcondt(i,k) = dcondt(i,k-1)
                  dcondt(i,k) = 0.
               end if
            end do
         end do

c Update and scatter data back to full arrays

         do k = 1,plev
            kp1 = min(plev,k+1)
            do i = il1g,il2g
               qn = const(i,k)+dcondt(i,k)*2.*delt
#define DEBUG
#undef DEBUG
#ifdef DEBUG
               if (qn.lt.0) then
                  write (6,*) ' qn less than zero ', i, k, lat, m, qn
                  write (6,*) ' qo, dcondt*2*dt ', q(ideep(i),k,m),
     $                 dcondt(i,k)*2.*delt
                  write (6,*) ' jt, jb ', jt(i), mx(i)
                  write (6,*) ' mu md bel ', mu(i,kp1), md(i,kp1)
                  write (6,*) ' mu md abv ', mu(i,k), md(i,k)
                  write (6,*) ' conu, cond abv ', conu(i,k), cond(i,k)
                  write (6,*) ' conu, cond bel ', conu(i,kp1),
     $                                            cond(i,kp1)
                  write (6,*) ' chat abv, bel ', chat(i,k), chat(i,kp1)
                  write (6,*) ' dp ', dp(i,k)
               fluxin =  mu(i,kp1)*conu(i,kp1) + mu(i,k)*chat(i,k) 
     $                 -(md(i,k)  *cond(i,k)   + md(i,kp1)*chat(i,kp1))
               fluxout = mu(i,k)*conu(i,k)     + mu(i,kp1)*chat(i,kp1)
     $                 -(md(i,kp1)*cond(i,kp1) + md(i,k)*chat(i,k))
               netflux = fluxin - fluxout
               write (6,*) ' fluxin, fluxout, netflux ', fluxin, 
     $              fluxout, netflux
               write (6,*) ' term1 ',
     $                    +mu(i,kp1)* (conu(i,kp1)-chat(i,kp1))/dp(i,k)
               write (6,*) ' term2 ',
     $                    -mu(i,k)*   (conu(i,k)-chat(i,k))/dp(i,k)
               write (6,*) ' term3 ',
     $                    +md(i,kp1)* (cond(i,kp1)-chat(i,kp1))/dp(i,k)
               write (6,*) ' term4 ',
     $                    -md(i,k)*   (cond(i,k)-chat(i,k))/dp(i,k)
               stop
               endif
#endif
#undef DEBUG
               q(ideep(i),k,m) = qn

            end do
         end do
         
      end do                    ! m = 2,pcnst
C
      return
      end
 
