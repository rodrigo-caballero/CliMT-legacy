      subroutine vdiff(lat     ,um1     ,vm1     ,tm1     ,qm1     ,
     $                 pmidm1  ,pintm1  ,rpdel   ,rpdeli  ,ztodt   ,
     $                 thm     ,zm      ,taux    ,tauy    ,shflx   ,
     $                 cflx    ,up1     ,vp1     ,thp     ,qp1     ,
     $                 pblh    ,ustar   ,kvh     ,kvm     ,tpert   ,
     $                 qpert   ,cgs     )
C-----------------------------------------------------------------------
C
C Driver routine to compute vertical diffusion of momentum,
C moisture, trace constituents and potential temperature.
C
C Free atmosphere diffusivities are computed first; then modified
C by the boundary layer scheme; then passed to individual
C parameterizations mvdiff, qvdiff
C
C The free atmosphere diffusivities are based on standard mixing length 
C forms for the neutral diffusivity multiplied by functns of Richardson 
C number. K = l^2 * |dV/dz| * f(Ri). The same functions are used for 
C momentum, potential temperature, and constitutents.
C The stable Richardson num function (Ri>0) is taken from Holtslag and 
C Beljaars (1989), ECMWF proceedings. f = 1 / (1 + 10*Ri*(1 + 8*Ri))
C The unstable Richardson number function (Ri<0) is taken from  CCM1.
C f = sqrt(1 - 18*Ri)
C
C---------------------------Code history--------------------------------
C
C Original version:  CCM1
C Standardized:      J. Rosinski, June 1992
C Reviewed:          P. Rasch, B. Boville, August 1992
C Reviewed:          P. Rasch, March 1996
C Reviewed:          B. Boville, April 1996
C
C-----------------------------------------------------------------------
c
c $Id: vdiff.F,v 1.1 2005/01/24 19:21:44 rca Exp $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <params.h>
#include <pmgrid.h>
C------------------------------Commons----------------------------------
#include <comvd.h>
C-----------------------------------------------------------------------
#include <comqmin.h>
C------------------------------Arguments--------------------------------C
C Input arguments
C
      integer lat                 ! latitude index

      real um1(plond,plev)        ! u wind input
      real vm1(plond,plev)        ! v wind input
      real tm1(plond,plev)        ! temperature input
      real qm1(plond,plev,pcnst)  ! moisture and trace constituent input
      real pmidm1(plond,plev)     ! midpoint pressures
      real pintm1(plond,plevp)    ! interface pressures
      real rpdel(plond,plev)      ! 1./pdel  (thickness bet interfaces)
      real rpdeli(plond,plev)     ! 1./pdeli (thickness bet midpoints)
      real ztodt                  ! 2 delta-t
      real thm(plond,plev)        ! potential temperature
      real zm(plond,plev)         ! midpoint geoptl height above sfc
      real taux(plond)            ! x surface stress (n)
      real tauy(plond)            ! y surface stress (n)
      real shflx(plond)           ! surface sensible heat flux (w/m2)
      real cflx(plond,pcnst)      ! surface constituent flux (kg/m2/s)
C
C Output arguments
C
      real up1(plond,plev)        ! u-wind after vertical diffusion
      real vp1(plond,plev)        ! v-wind after vertical diffusion
      real thp(plond,plev)        ! pot temp after vert. diffusion
      real qp1(plond,plev,pcnst)  ! moist, tracers after vert. diff
      real pblh(plond)            ! planetary boundary layer height
      real ustar(plond)           ! surface friction velocity
      real kvh(plond,plevp)       ! coefficient for heat and tracers
      real kvm(plond,plevp)       ! coefficient for momentum
      real tpert(plond)           ! convective temperature excess
      real qpert(plond)           ! convective humidity excess
      real cgs(plond,plevp)       ! counter-grad star (cg/flux)
C
C---------------------------Local workspace-----------------------------
C
      real cah(plond,plev)        ! -upper diag for heat and constituts
      real cam(plond,plev)        ! -upper diagonal for momentum
      real cch(plond,plev)        ! -lower diag for heat and constits
      real ccm(plond,plev)        ! -lower diagonal for momentum
      real cgh(plond,plevp)       ! countergradient term for heat
      real cgq(plond,plevp,pcnst) ! countergrad term for constituent
      real dvdz2                  ! (du/dz)**2 + (dv/dz)**2
      real dz                     ! delta z between midpoints
      real fstab                  ! stable f(ri)
      real funst                  ! unstable f(ri)
      real kvf(plond,plevp)       ! free atmosphere kv at interfaces
      real rinub                  ! richardson no=(g/theta)(dtheta/dz)/
C                                                 (du/dz**2+dv/dz**2)
      real sstab                  ! static stability = g/th  * dth/dz
      real potbar(plond,plevp)    ! pintm1(k)/(.5*(tm1(k)+tm1(k-1))
      real tmp1(plond)            ! temporary storage
      real tmp2                   ! temporary storage
      real rcpair                 ! 1./cpair
      real ztodtgor               ! ztodt*gravit/rair
      real gorsq                  ! (gravit/rair)**2
      real dubot(plond)           ! lowest layer u change from stress
      real dvbot(plond)           ! lowest layer v change from stress
      real dtbot(plond)           ! lowest layer t change from heat flx
      real dqbot(plond,pcnst)     ! lowest layer q change from const flx
      real thx(plond,plev)        ! temperature input + counter gradient
      real thv(plond,plev)        ! virtual potential temperature
      real qmx(plond,plev,pcnst)  ! constituents input + counter grad
      real zeh(plond,plev)        ! term in tri-diag. matrix system (T & Q)
      real zem(plond,plev)        ! term in tri-diag. matrix system (momentum)
      real termh(plond,plev)      ! 1./(1. + cah(k) + cch(k) - cch(k)*zeh(k-1))
      real termm(plond,plev)      ! 1./(1. + cam(k) + ccm(k) - ccm(k)*zem(k-1))
      real kvn                    ! neutral Kv
      real ksx(plond)             ! effective surface drag factor (x)
      real ksy(plond)             ! effective surface drag factor (y)
      real sufac(plond)           ! lowest layer u implicit stress factor
      real svfac(plond)           ! lowest layer v implicit stress factor

      integer indx(plon)          ! array of indices of potential q<0
      integer ilogic(plon)        ! 1 => adjust vertical profile
      integer nval                ! num of values which meet criteria
      integer ii                  ! longitude index of found points
      integer i                   ! longitude index
      integer k                   ! vertical index
      integer m                   ! constituent index
      integer ktopbl(plond)       ! index of first midpoint inside pbl
      integer ktopblmn            ! min value of ktopbl
C
C-----------------------------------------------------------------------
C
C Convert the surface fluxes to lowest level tendencies.
C Stresses are converted to effective drag coefficients if these are >0
C
      rcpair = 1./cpair
      do i=1,plon
        tmp1(i) = ztodt*gravit*rpdel(i,plev)

        ksx(i) = -taux(i) / um1(i,plev)
        if (ksx(i) .gt. 0.) then
           sufac(i) = tmp1(i) * ksx(i)
           dubot(i) = 0.
        else
           ksx(i)   = 0.
           sufac(i) = 0.
           dubot(i) = tmp1(i) * taux(i)
        end if

        ksy(i) = -tauy(i) / vm1(i,plev)
        if (ksy(i) .gt. 0.) then
           svfac(i) = tmp1(i) * ksy(i)
           dvbot(i) = 0.
        else
           ksy(i)   = 0.
           svfac(i) = 0.
           dvbot(i) = tmp1(i) * tauy(i)
        end if

        dqbot(i,1)   = cflx(i,1)*tmp1(i)
        dtbot(i)     = shflx(i)*tmp1(i)*rcpair
        kvf(i,plevp) = 0.0
      end do
c+++climt only 1 constituent in climt
c      do m=2,pcnst
c        do i=1,plon
c          dqbot(i,m) = cflx(i,m)*tmp1(i)
c        end do
c      end do
c---climt

C
C Set the vertical diffusion coefficient above the top diffusion level
C
      do k=1,ntopfl
        do i=1,plon
          kvf(i,k) = 0.0
        end do
      end do
C
C Compute virtual potential temperature for use in static stability 
C calculation.  0.61 is 1. - R(water vapor)/R(dry air).  Use 0.61 instead
C of a computed variable in order to obtain an identical simulation to
C Case 414.
C
      call virtem(thm     ,qm1     ,0.61    ,thv     )
C
C Compute the free atmosphere vertical diffusion coefficients
C kvh = kvq = kvm. 
C
      do k=ntopfl,plev-1
        do i=1,plon
C
C Vertical shear squared, min value of (delta v)**2 prevents zero shear.
C
          dvdz2 = (um1(i,k)-um1(i,k+1))**2 + (vm1(i,k)-vm1(i,k+1))**2
          dvdz2 = max(dvdz2,1.e-36)
          dz    = zm(i,k) - zm(i,k+1)
          dvdz2 = dvdz2/(dz**2)
C
C Static stability (use virtual potential temperature)
C
          sstab = gravit*2.0*( thv(i,k) - thv(i,k+1))/
     $                       ((thv(i,k) + thv(i,k+1))*dz)
C
C Richardson number, stable and unstable modifying functions
C
          rinub = sstab/dvdz2
          fstab = 1.0/(1.0 + 10.0*rinub*(1.0 + 8.0*rinub))
          funst = max(1. - 18.*rinub,0.)
C
C Select the appropriate function of the richardson number
C
          if (rinub.lt.0.0) fstab = sqrt(funst)
C
C Neutral diffusion coefficient
C compute mixing length (z), where z is the interface height estimated
C with an 8 km scale height.
C
          kvn = ml2(k)*sqrt(dvdz2)
C
C Full diffusion coefficient (modified by f(ri)),
C
          kvf(i,k+1) = max(zkmin,kvn*fstab)
        end do
      end do
C
C Determine the boundary layer kvh (=kvq), kvm, 
C counter gradient terms (cgh, cgq, cgs)
C boundary layer height (pblh) and 
C the perturbation temperature and moisture (tpert and qpert)
C The free atmosphere kv is returned above the boundary layer top.
C
      call pbldif(thm     ,qm1     ,zm      ,um1     ,vm1     ,
     $            tm1     ,pmidm1  ,kvf     ,cflx    ,shflx   ,
     $            taux    ,tauy    ,ustar   ,kvm     ,kvh     ,
     $            cgh     ,cgq     ,cgs     ,pblh    ,tpert   ,
     $            qpert   ,ktopbl  ,ktopblmn)

C
C Add the counter grad terms to potential temp, specific humidity
C and other constituents in the bdry layer. Note, ktopblmn gives the 
c minimum vertical index of the first midpoint within the boundary layer.
C
C first set values above boundary layer
C
      do k=1,ktopblmn-2
        do i=1,plon
          thx(i,k)   = thm(i,k)
          qmx(i,k,1) = qm1(i,k,1)
        end do
c+++climt only 1 constituent in climt
c        do m=2,pcnst
c          do i=1,plon
c            qmx(i,k,m) = qm1(i,k,m)
c          end do
c        end do
c---climt
      end do
      do k=2,plev
        do i=1,plon
          potbar(i,k) = pintm1(i,k)/(0.5*(tm1(i,k) + tm1(i,k-1)))
        end do
      end do
      do i=1,plon
        potbar(i,plevp) = pintm1(i,plevp)/tm1(i,plev)
      end do
C
C now focus on the boundary layer
C
      ztodtgor = ztodt*gravit/rair
      do k=ktopblmn-1,plev
        do i=1,plon
          tmp1(i) = ztodtgor*rpdel(i,k)
          thx(i,k) = thm(i,k) + tmp1(i)*
     $           (potbar(i,k+1)*kvh(i,k+1)*cgh(i,k+1) -
     $            potbar(i,k  )*kvh(i,k  )*cgh(i,k  ))
          qmx(i,k,1) = qm1(i,k,1) + tmp1(i) *
     $           (potbar(i,k+1)*kvh(i,k+1)*cgq(i,k+1,1) -
     $            potbar(i,k  )*kvh(i,k  )*cgq(i,k  ,1))
        end do
c+++climt only 1 constituent in climt
c        do m=2,pcnst
c          do i=1,plon
c            qmx(i,k,m) = qm1(i,k,m) + tmp1(i)*
c     $           (potbar(i,k+1)*kvh(i,k+1)*cgq(i,k+1,m) -
c     $            potbar(i,k  )*kvh(i,k  )*cgq(i,k  ,m))
c          end do
c        end do
c---climt
      end do
C
C Check for neg q's in each constituent and put the original vertical
C profile back if a neg value is found. A neg value implies that the
C quasi-equilibrium conditions assumed for the countergradient term are
C strongly violated.
C Original code rewritten by Rosinski 7/8/91 to vectorize in longitude.
C
      do m=1,pcnst
        do i=1,plon
          ilogic(i) = 0
        end do
        do k=ktopblmn-1,plev
          do i=1,plon
            if (qmx(i,k,m).lt.qmincg(m)) ilogic(i) = 1
          end do
        end do
C
C Find long indices of those columns for which negatives were found
C
        call wheneq(plon,ilogic,1,1,indx,nval)
C
C Replace those columns with original values
C
        if (nval.gt.0) then
          do k=ktopblmn-1,plev
            do ii=1,nval
              i=indx(ii)
              qmx(i,k,m) = qm1(i,k,m)
            end do
          end do
        end if
      end do
C
C Determine superdiagonal (ca(k)) and subdiagonal (cc(k)) coeffs of the 
C tridiagonal diffusion matrix. the diagonal elements are a combination of 
C ca and cc; they are not explicitly provided to the solver
C
      gorsq = (gravit/rair)**2
      do k=ntopfl,plev-1
        do i=1,plon
          tmp2 = ztodt*gorsq*rpdeli(i,k)*(potbar(i,k+1)**2)
          cah(i,k  ) = kvh(i,k+1)*tmp2*rpdel(i,k  )
          cam(i,k  ) = kvm(i,k+1)*tmp2*rpdel(i,k  )
          cch(i,k+1) = kvh(i,k+1)*tmp2*rpdel(i,k+1)
          ccm(i,k+1) = kvm(i,k+1)*tmp2*rpdel(i,k+1)
        end do
      end do
C
C The last element of the upper diagonal is zero.
C
      do i=1,plon
        cah(i,plev) = 0.
        cam(i,plev) = 0.
      end do
C
C Calculate e(k) for heat & momentum vertical diffusion.  This term is 
C required in solution of tridiagonal matrix defined by implicit diffusion eqn.
C
      do i=1,plon
        termh(i,ntopfl) = 1./(1. + cah(i,ntopfl))
        termm(i,ntopfl) = 1./(1. + cam(i,ntopfl))
        zeh(i,ntopfl) = cah(i,ntopfl)*termh(i,ntopfl)
        zem(i,ntopfl) = cam(i,ntopfl)*termm(i,ntopfl)
      end do
      do k=ntopfl+1,plev-1
        do i=1,plon
          termh(i,k) = 1./
     $           (1. + cah(i,k) + cch(i,k) - cch(i,k)*zeh(i,k-1))
          termm(i,k) = 1./
     $           (1. + cam(i,k) + ccm(i,k) - ccm(i,k)*zem(i,k-1))
          zeh(i,k) = cah(i,k)*termh(i,k)
          zem(i,k) = cam(i,k)*termm(i,k)
        end do
      end do
C
C Diffuse momentum
C
      call mvdiff(um1     ,vm1     ,dubot   ,dvbot   ,
     $            sufac   ,svfac   ,ccm     ,
     $            zem     ,termm   ,up1     ,vp1     )
c+
c Determine the difference between the implicit stress and the
c externally specified stress. Apply over boundary layer depth.
c-
c      print *, 'vdiff: lat=, ktopblmn', lat, ktopblmn
      do i = 1, plon
         if (ksx(i) .gt. 0.) sufac(i) = (taux(i) + ksx(i)*up1(i,plev))
     $        * ztodt * gravit / (pintm1(i,plevp) - pintm1(i,ktopbl(i)))
         if (ksy(i) .gt. 0.) svfac(i) = (tauy(i) + ksy(i)*vp1(i,plev))
     $        * ztodt * gravit / (pintm1(i,plevp) - pintm1(i,ktopbl(i)))
c        print 1, i, lat
c     $        , taux(i), -ksx(i)*up1(i,plev)
c     $        , sufac(i)*86400./ztodt, ksx(i), um1(i,plev),up1(i,plev)
c    1 format (1x,i4,i4, 1P, 4E20.10, 2f10.4)
      end do

      do k = plev, ktopblmn, -1
         do i = 1, plon
            if (k .ge. ktopbl(i)) then
               up1(i,k) = up1(i,k) + sufac(i)
               vp1(i,k) = vp1(i,k) + svfac(i)
            end if
         end do
      end do
C
C Diffuse constituents
C
      call qvdiff(pcnst   ,qmx     ,dqbot   ,cch     ,zeh     ,
     $            termh   ,qp1     )
C
C Identify and correct constituents exceeding user defined bounds
C 
      call qneg3('VDIFF   ',lat,qp1(1,1,1))
C
C Diffuse potential temperature
C
      call qvdiff(1       ,thx     ,dtbot   ,cch     ,zeh     ,
     $            termh   ,thp     )
C

      end
 
