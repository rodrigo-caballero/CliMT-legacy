#include <misc.h>
#include <params.h>
      subroutine radded(coszrs  ,trayoslp,pflx    ,abh2o   ,abo3    ,
     $                  abco2   ,abo2    ,uh2o    ,uo3     ,uco2    ,
     $                  uo2     ,tauxcl  ,wcl     ,gcl     ,fcl     ,
     $                  tauxci  ,wci     ,gci     ,fci     ,tauxar  ,
     $                  wa      ,ga      ,fa      ,nloop   ,is      ,
     $                  ie      ,rdir    ,rdif    ,tdir    ,tdif    ,
     $                  explay  ,exptdn  ,rdndif  ,tottrn  )
C-----------------------------------------------------------------------
C
C Computes layer reflectivities and transmissivities, from the top down
C to the surface using the delta-Eddington solutions for each layer;
C adds layers from top down to surface as well.
C
C If total transmission to the interface above a particular layer is
C less than trmin, then no further delta-Eddington solutions are
C evaluated for layers below
C
C For more details , see Briegleb, Bruce P., 1992: Delta-Eddington
C Approximation for Solar Radiation in the NCAR Community Climate Model,
C Journal of Geophysical Research, Vol 97, D7, pp7603-7612).
C
C---------------------------Code history--------------------------------
C
C Original version:  B. Briegleb
C Standardized:      J. Rosinski, June 1992
C Reviewed:          J. Kiehl, B. Briegleb, August 1992
C Reviewed:          J. Kiehl, April 1996
C Reviewed:          B. Briegleb, May 1996
C
C-----------------------------------------------------------------------
c
c $Id: radded.F,v 1.1 2004/09/07 15:27:42 rca Exp $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <prgrid.h>
C-----------------------------------------------------------------------
C
C Minimum total transmission below which no layer computation are done:
C
      real trmin                  ! Minimum total transmission allowed
      real wray                   ! Rayleigh single scatter albedo
      real gray                   ! Rayleigh asymetry parameter
      real fray                   ! Rayleigh forward scattered fraction

      parameter (trmin = 1.e-3)
      parameter (wray = 0.999999)
      parameter (gray = 0.0)
      parameter (fray = 0.1)
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real coszrs(plond)          ! Cosine zenith angle
      real trayoslp               ! Tray/sslp
      real pflx(plond,0:plevp)    ! Interface pressure
      real abh2o                  ! Absorption coefficiant for h2o
      real abo3                   ! Absorption coefficiant for o3
      real abco2                  ! Absorption coefficiant for co2
      real abo2                   ! Absorption coefficiant for o2
      real uh2o(plond,0:plev)     ! Layer absorber amount of h2o
      real uo3(plond,0:plev)      ! Layer absorber amount of  o3
      real uco2(plond,0:plev)     ! Layer absorber amount of co2
      real uo2(plond,0:plev)      ! Layer absorber amount of  o2
      real tauxcl(plond,0:plev)   ! Cloud extinction optical depth (liquid)
      real wcl(plond,0:plev)      ! Cloud single scattering albedo (liquid)
      real gcl(plond,0:plev)      ! Cloud asymmetry parameter (liquid)
      real fcl(plond,0:plev)      ! Cloud forward scattered fraction (liquid)
      real tauxci(plond,0:plev)   ! Cloud extinction optical depth (ice)
      real wci(plond,0:plev)      ! Cloud single scattering albedo (ice)
      real gci(plond,0:plev)      ! Cloud asymmetry parameter (ice)
      real fci(plond,0:plev)      ! Cloud forward scattered fraction (ice)
      real tauxar(plond,0:plev)   ! Aerosol extinction optical depth
      real wa(plond,0:plev)       ! Aerosol single scattering albedo
      real ga(plond,0:plev)       ! Aerosol asymmetry parameter
      real fa(plond,0:plev)       ! Aerosol forward scattered fraction

      integer nloop               ! Number of loops (1 or 2)
      integer is(2)               ! Starting index for 1 or 2 loops
      integer ie(2)               ! Ending index for 1 or 2 loops
C
C Input/Output arguments
C
C Following variables are defined for each layer; 0 refers to extra
C layer above top of model:
C
      real rdir(plond,0:plev)     ! Layer reflectivity to direct rad
      real rdif(plond,0:plev)     ! Layer refflectivity to diffuse rad
      real tdir(plond,0:plev)     ! Layer transmission to direct rad
      real tdif(plond,0:plev)     ! Layer transmission to diffuse rad
      real explay(plond,0:plev)   ! Solar beam exp transm for layer
C
C (Note that the following variables are defined on interfaces, with the
C  index k referring to the top interface of the kth layer:
C  exptdn,rdndif,tottrn; for example, tottrn(k=5) refers to the total
C  transmission to the top interface of the 5th layer; plevp refers to
C  the earth surface)
C
      real rdndif(plond,0:plevp)  ! Added dif ref for layers above
      real exptdn(plond,0:plevp)  ! Solar beam exp down transm from top
      real tottrn(plond,0:plevp)  ! Total transmission for layers above
C
C---------------------------Local variables-----------------------------
C
      integer i,ii          ! Longitude indices
      integer k             ! Level index
      integer nn            ! Index of longitude loops (max=nloop)
      integer nval          ! Number of long values satisfying criteria
      integer index(plond)  ! Array of longitude indices

      real taugab(plond)    ! Layer total gas absorption optical depth
      real tauray(plond)    ! Layer rayleigh optical depth
      real taucsc           ! Layer cloud scattering optical depth
      real tautot           ! Total layer optical depth
      real wtot             ! Total layer single scatter albedo
      real gtot             ! Total layer asymmetry parameter
      real ftot             ! Total layer forward scatter fraction
      real wtau             !  rayleigh layer scattering optical depth
      real wt               !  layer total single scattering albedo
      real ts               !  layer scaled extinction optical depth
      real ws               !  layer scaled single scattering albedo
      real gs               !  layer scaled asymmetry parameter
      real rdenom           !  mulitiple scattering term
      real rdirexp          !  layer direct ref times exp transmission
      real tdnmexp          !  total transmission minus exp transmission
C
C---------------------------Statement functions-------------------------
C
C Statement functions and other local variables
C
      real alpha            ! Term in direct reflect and transmissivity
      real gamma            ! Term in direct reflect and transmissivity
      real el               ! Term in alpha,gamma,n,u
      real taus             ! Scaled extinction optical depth
      real omgs             ! Scaled single particle scattering albedo
      real asys             ! Scaled asymmetry parameter
      real u                ! Term in diffuse reflect and transmissivity
      real n                ! Term in diffuse reflect and transmissivity
      real lm               ! Temporary for el
      real ne               ! Temporary for n
      real w                ! Dummy argument for statement function
      real uu               ! Dummy argument for statement function
      real g                ! Dummy argument for statement function
      real e                ! Dummy argument for statement function
      real f                ! Dummy argument for statement function
      real t                ! Dummy argument for statement function
      real et               ! Dummy argument for statement function
C
C Intermediate terms for delta-eddington solution
C
      real alp              ! Temporary for alpha
      real gam              ! Temporary for gamma
      real ue               ! Temporary for u
      real arg              ! Exponential argument
      real extins           ! Extinction
      real amg              ! Alp - gam
      real apg              ! Alp + gam
C
      alpha(w,uu,g,e) = .75*w*uu*((1. + g*(1-w))/(1. - e*e*uu*uu))
      gamma(w,uu,g,e) = .50*w*((3.*g*(1.-w)*uu*uu + 1.)/(1.-e*e*uu*uu))
      el(w,g)         = sqrt(3.*(1-w)*(1. - w*g))
      taus(w,f,t)     = (1. - w*f)*t
      omgs(w,f)       = (1. - f)*w/(1. - w*f)
      asys(g,f)       = (g - f)/(1. - f)
      u(w,g,e)        = 1.5*(1. - w*g)/e
      n(uu,et)        = ((uu+1.)*(uu+1.)/et ) - ((uu-1.)*(uu-1.)*et)
C
C-----------------------------------------------------------------------
C
C Initialize all total transmission values to 0, so that nighttime 
C values from previous computations are not used:
C
      call resetr(tottrn,plond*plevp,0.)
C
C Compute total direct beam transmission, total transmission, and
C reflectivity for diffuse radiation (from below) for all layers above
C each interface by starting from the top and adding layers down:
C
C For the extra layer above model top:
C
      do nn=1,nloop
        do i=is(nn),ie(nn)
          tauray(i) = trayoslp*(pflx(i,1)-pflx(i,0))
          taugab(i) = abh2o*uh2o(i,0) + abo3*uo3(i,0) +
     $                abco2*uco2(i,0) + abo2*uo2(i,0)
          tautot  = tauxcl(i,0) + tauxci(i,0) + tauray(i) + taugab(i)
     $                          + tauxar(i,0)
          taucsc  = tauxcl(i,0)*wcl(i,0)+tauxci(i,0)*wci(i,0)
     $                          + tauxar(i,0)*wa(i,0)
          wtau    = wray*tauray(i) 
          wt      = wtau + taucsc
          wtot = wt/tautot
          gtot = (wtau*gray + gcl(i,0)*tauxcl(i,0)*wcl(i,0) +
     $                        gci(i,0)*tauxci(i,0)*wci(i,0) +
     $                        ga(i,0) *tauxar(i,0)*wa(i,0))/wt
          ftot = (wtau*fray + fcl(i,0)*tauxcl(i,0)*wcl(i,0) +
     $                        fci(i,0)*tauxci(i,0)*wci(i,0) +
     $                        fa(i,0) *tauxar(i,0)*wa(i,0))/wt
          ts   = taus(wtot,ftot,tautot)
          ws   = omgs(wtot,ftot)
          gs   = asys(gtot,ftot)
          lm   = el(ws,gs)
          alp  = alpha(ws,coszrs(i),gs,lm)
          gam  = gamma(ws,coszrs(i),gs,lm)
          ue   = u(ws,gs,lm)
C
C Limit argument of exponential to 25, in case lm*ts very large:
C
          arg  = min(lm*ts,25.)
          extins = exp(-arg)
          ne = n(ue,extins)
          rdif(i,0) = (ue+1.)*(ue-1.)*(1./extins - extins)/ne
          tdif(i,0) = 4.*ue/ne
C
C Limit argument of exponential to 25, in case coszrs is very small:
C
          arg       = min(ts/coszrs(i),25.)
          explay(i,0) = exp(-arg)
          apg = alp + gam
          amg = alp - gam
          rdir(i,0) = amg*(tdif(i,0)*explay(i,0) - 1.) + apg*rdif(i,0)
          tdir(i,0) = apg*tdif(i,0) +
     $                (amg*rdif(i,0) - (apg-1.))*explay(i,0)
C
C Under rare conditions, reflectivies and transmissivities can be
C negative; zero out any negative values
C
          rdir(i,0) = max(rdir(i,0),0.0)
          tdir(i,0) = max(tdir(i,0),0.0)
          rdif(i,0) = max(rdif(i,0),0.0)
          tdif(i,0) = max(tdif(i,0),0.0)
C
C Initialize top interface of extra layer:
C
          exptdn(i,0) =   1.0
          rdndif(i,0) =   0.0
          tottrn(i,0) =   1.0
          rdndif(i,1) = rdif(i,0)
          tottrn(i,1) = tdir(i,0)
        end do
      end do
C
C Now, continue down one layer at a time; if the total transmission to
C the interface just above a given layer is less than trmin, then no
C delta-eddington computation for that layer is done:
C
      do 400 k=1,plev
C
C Initialize current layer properties to zero; only if total
C transmission to the top interface of the current layer exceeds the
C minimum, will these values be computed below:
C
        do nn=1,nloop
          do i=is(nn),ie(nn)
            rdir(i,k)   =  0.0
            rdif(i,k)   =  0.0
            tdir(i,k)   =  0.0
            tdif(i,k)   =  0.0
            explay(i,k) =  0.0
C
C Calculates the solar beam transmission, total transmission, and
C reflectivity for diffuse radiation from below at the top of the
C current layer:
C
            exptdn(i,k) = exptdn(i,k-1)*explay(i,k-1)
            rdenom      = 1./(1. - rdif(i,k-1)*rdndif(i,k-1))
            rdirexp     = rdir(i,k-1)*exptdn(i,k-1)
            tdnmexp     = tottrn(i,k-1) - exptdn(i,k-1)
            tottrn(i,k) = exptdn(i,k-1)*tdir(i,k-1) + tdif(i,k-1)*
     $                   (tdnmexp + rdndif(i,k-1)*rdirexp)*rdenom
            rdndif(i,k) = rdif(i,k-1)  +
     $                (rdndif(i,k-1)*tdif(i,k-1))*(tdif(i,k-1)*rdenom)
            end do
         end do
C
C Compute next layer delta-eddington solution only if total transmission
C of radiation to the interface just above the layer exceeds trmin.
C
         call whenfgt(plon,tottrn(1,k),1,trmin,index,nval)
         if(nval.gt.0) then
CDIR$ IVDEP
           do ii=1,nval
             i=index(ii)
             tauray(i) = trayoslp*(pflx(i,k+1)-pflx(i,k))
             taugab(i) = abh2o*uh2o(i,k) + abo3*uo3(i,k) +
     $                   abco2*uco2(i,k) + abo2*uo2(i,k)
             tautot = tauxcl(i,k) + tauxci(i,k) + 
     $                tauray(i) + taugab(i) + tauxar(i,k)
             taucsc    = tauxcl(i,k)*wcl(i,k) + tauxci(i,k)*wci(i,k)
     $                          + tauxar(i,k)*wa(i,k)
             wtau      = wray*tauray(i)
             wt        = wtau + taucsc
             wtot   = wt/tautot
             gtot   = (wtau*gray + gcl(i,k)*wcl(i,k)*tauxcl(i,k)
     $                 + gci(i,k)*wci(i,k)*tauxci(i,k)
     $                 + ga(i,k) *wa(i,k) *tauxar(i,k))/wt
             ftot   = (wtau*fray + fcl(i,k)*wcl(i,k)*tauxcl(i,k)
     $                 + fci(i,k)*wci(i,k)*tauxci(i,k)
     $                 + fa(i,k) *wa(i,k) *tauxar(i,k))/wt
             ts   = taus(wtot,ftot,tautot)
             ws   = omgs(wtot,ftot)
             gs   = asys(gtot,ftot)
             lm   = el(ws,gs)
             alp  = alpha(ws,coszrs(i),gs,lm)
             gam  = gamma(ws,coszrs(i),gs,lm)
             ue   = u(ws,gs,lm)
C
C Limit argument of exponential to 25, in case lm very large:
C
             arg  = min(lm*ts,25.)
             extins = exp(-arg)
             ne = n(ue,extins)
             rdif(i,k) = (ue+1.)*(ue-1.)*(1./extins - extins)/ne
             tdif(i,k)   =   4.*ue/ne
C
C Limit argument of exponential to 25, in case coszrs is very small:
C
             arg       = min(ts/coszrs(i),25.)
             explay(i,k) = exp(-arg)
             apg = alp + gam
             amg = alp - gam
             rdir(i,k) = amg*(tdif(i,k)*explay(i,k) - 1.) +
     $                   apg*rdif(i,k)
             tdir(i,k) = apg*tdif(i,k) +
     $                   (amg*rdif(i,k) - (apg-1.))*explay(i,k)
C
C Under rare conditions, reflectivies and transmissivities can be
C negative; zero out any negative values
C
             rdir(i,k) = max(rdir(i,k),0.0)
             tdir(i,k) = max(tdir(i,k),0.0)
             rdif(i,k) = max(rdif(i,k),0.0)
             tdif(i,k) = max(tdif(i,k),0.0)
           end do
         end if
  400 continue
C
C Compute total direct beam transmission, total transmission, and
C reflectivity for diffuse radiation (from below) for all layers
C above the surface:
C
      k = plevp
      do nn=1,nloop
        do i=is(nn),ie(nn)
          exptdn(i,k) = exptdn(i,k-1)*explay(i,k-1)
          rdenom = 1./(1. - rdif(i,k-1)*rdndif(i,k-1))
          rdirexp = rdir(i,k-1)*exptdn(i,k-1)
          tdnmexp = tottrn(i,k-1) - exptdn(i,k-1)
          tottrn(i,k) = exptdn(i,k-1)*tdir(i,k-1) + tdif(i,k-1)*
     $                 (tdnmexp + rdndif(i,k-1)*rdirexp)*rdenom
          rdndif(i,k) = rdif(i,k-1)  +
     $             (rdndif(i,k-1)*tdif(i,k-1))*(tdif(i,k-1)*rdenom)
         end do
      end do
C
      return
      end

 
