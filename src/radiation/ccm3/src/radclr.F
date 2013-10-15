#include <misc.h>
#include <params.h>
      subroutine radclr(coszrs  ,trayoslp,pflx    ,abh2o   ,abo3    ,
     $                  abco2   ,abo2    ,uth2o   ,uto3    ,utco2   ,
     $                  uto2    ,tauaer  ,waer    ,gaer    ,faer    ,
     $                  nloop   ,is      ,ie      ,rdir    ,rdif    ,
     $                  tdir    ,tdif    ,explay  ,exptdn  ,rdndif  ,
     $                  tottrn  )
C-----------------------------------------------------------------------
C
C Delta-Eddington solution for special clear sky computation
C
C Computes total reflectivities and transmissivities for two atmospheric
C layers: an overlying purely ozone absorbing layer, and the rest of the
C column below.
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
c $Id: radclr.F,v 1.2 2004/09/07 02:47:45 rca Exp $
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
C
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
      real uth2o(plond)           ! Total column absorber amount of h2o
      real uto3(plond)            ! Total column absorber amount of  o3
      real utco2(plond)           ! Total column absorber amount of co2
      real uto2(plond)            ! Total column absorber amount of  o2
      real tauaer(plond)          ! Total column aerosol extinction
      real waer(plond)            ! Aerosol single scattering albedo
      real gaer(plond)            ! Aerosol asymmetry parameter
      real faer(plond)            ! Aerosol forward scattering fraction

      integer nloop               ! Number of loops (1 or 2)
      integer is(2)               ! Starting index for 1 or 2 loops
      integer ie(2)               ! Ending index for 1 or 2 loops
C
C Input/Output arguments
C
C Following variables are defined for each layer; note, we use layer 0 
C to refer to the entire atmospheric column:
C
      real rdir(plond,0:plev)     ! Layer reflectivity to direct rad
      real rdif(plond,0:plev)     ! Layer refflectivity to diffuse rad
      real tdir(plond,0:plev)     ! Layer transmission to direct rad
      real tdif(plond,0:plev)     ! Layer transmission to diffuse rad
      real explay(plond,0:plev)   ! Solar beam exp transmn for layer
C
C Note that the following variables are defined on interfaces  with
C the index k referring to the top interface of the kth layer:
C exptdn,rdndif,tottrn; for example, tottrn(k=5) refers to the total
C transmission to the top interface of the 5th layer.
C
      real exptdn(plond,0:plevp)  ! Solar beam exp down transmn from top
      real rdndif(plond,0:plevp)  ! Added dif ref for layers above
      real tottrn(plond,0:plevp)  ! Total transmission for layers above
C
C---------------------------Local variables-----------------------------
C
      integer i                   ! Longitude index
      integer k                   ! Level index
      integer nn                  ! Index of longitude loops (max=nloop)
      integer ii                  ! Longitude index
      integer nval                ! Number of long values satisfying criteria
      integer index(plond)        ! Array of longitude indices

      real taugab(plond)          ! Total column gas absorption optical depth
      real tauray(plond)          ! Column rayleigh optical depth
      real tautot                 ! Total column optical depth
      real wtot                   ! Total column single scatter albedo
      real gtot                   ! Total column asymmetry parameter
      real ftot                   ! Total column forward scatter fraction
      real ts                     ! Column scaled extinction optical depth
      real ws                     ! Column scaled single scattering albedo
      real gs                     ! Column scaled asymmetry parameter
      real rdenom                 ! Mulitiple scattering term
      real rdirexp                ! Layer direct ref times exp transmission
      real tdnmexp                ! Total transmission minus exp transmission
C
C---------------------------Statement functions-------------------------
C
C Statement functions for delta-Eddington solution; for detailed
C explanation of individual terms  see the routine 'radded'.
C
      real alpha,gamma,el,taus,omgs,asys,u,n,lm,ne
      real w,uu,g,e,f,t,et
C
C Intermediate terms for delta-Eddington solution
C
      real alp,gam,ue,arg,extins,amg,apg
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
C Initialize all total transmimission values to 0, so that nighttime 
C values from previous computations are not used:
C
      call resetr(tottrn,plond*2,0.)
C
C Compute total direct beam transmission, total transmission, and
C reflectivity for diffuse radiation (from below) for all layers
C above each interface by starting from the top and adding layers
C down:
C
C The top layer is assumed to be a purely absorbing ozone layer, and
C that the mean diffusivity for diffuse transmission is 1.66:
C
      do nn=1,nloop
        do i=is(nn),ie(nn)
          taugab(i) = abo3*uto3(i)
C
C Limit argument of exponential to 25, in case coszrs is very small:
C
          arg         = min(taugab(i)/coszrs(i),25.)
          explay(i,0) = exp(-arg)
          tdir(i,0)   = explay(i,0)
C
C Same limit for diffuse transmission:
C
          arg         = min(1.66*taugab(i),25.)
          tdif(i,0)   = exp(-arg)
          rdir(i,0)   = 0.0
          rdif(i,0)   = 0.0
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
C Now, complete the rest of the column; if the total transmission
C through the top ozone layer is less than trmin, then no
C delta-Eddington computation for the underlying column is done:
C
      do 200 k=1,1
C
C Initialize current layer properties to zero;only if total transmission
C to the top interface of the current layer exceeds the minimum, will
C these values be computed below:
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
     $                    (tdnmexp + rdndif(i,k-1)*rdirexp)*rdenom
            rdndif(i,k) = rdif(i,k-1)  +
     $                (rdndif(i,k-1)*tdif(i,k-1))*(tdif(i,k-1)*rdenom)
          end do
        end do
C
C Compute next layer delta-Eddington solution only if total transmission
C of radiation to the interface just above the layer exceeds trmin.
C
        call whenfgt(plon,tottrn(1,k),1,trmin,index,nval)
        if (nval.gt.0) then
CDIR$ IVDEP
          do ii=1,nval
            i=index(ii)
C
C Remember, no ozone absorption in this layer:
C
            tauray(i) = trayoslp*pflx(i,plevp)
            taugab(i) = abh2o*uth2o(i) + abco2*utco2(i) + abo2*uto2(i)
            tautot    = tauray(i) + taugab(i) + tauaer(i)
            wtot      = (wray*tauray(i) + waer(i)*tauaer(i))/tautot
            gtot      = (gray*wray*tauray(i) +
     $                   gaer(i)*waer(i)*tauaer(i))/(wtot*tautot)
            ftot      = (fray*wray*tauray(i) +
     $                   faer(i)*waer(i)*tauaer(i))/(wtot*tautot)
            ts        = taus(wtot,ftot,tautot)
            ws        = omgs(wtot,ftot)
            gs        = asys(gtot,ftot)
            lm        = el(ws,gs)
            alp       = alpha(ws,coszrs(i),gs,lm)
            gam       = gamma(ws,coszrs(i),gs,lm)
            ue        = u(ws,gs,lm)
C
C Limit argument of exponential to 25, in case lm very large:
C
            arg       = min(lm*ts,25.)
            extins    = exp(-arg)
            ne        = n(ue,extins)
            rdif(i,k) = (ue+1.)*(ue-1.)*(1./extins - extins)/ne
            tdif(i,k) =   4.*ue/ne
C
C Limit argument of exponential to 25, in case coszrs is very small:
C
            arg       = min(ts/coszrs(i),25.)
            explay(i,k) = exp(-arg)
            apg       = alp + gam
            amg       = alp - gam
            rdir(i,k) = amg*(tdif(i,k)*explay(i,k) - 1.) + apg*rdif(i,k)
            tdir(i,k) = apg*tdif(i,k) +
     $                  (amg*rdif(i,k) - (apg-1.))*explay(i,k)
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
  200 continue
C
C Compute total direct beam transmission, total transmission, and
C reflectivity for diffuse radiation (from below) for both layers
C above the surface:
C
      k = 2
      do nn=1,nloop
        do i=is(nn),ie(nn)
          exptdn(i,k) = exptdn(i,k-1)*explay(i,k-1)
          rdenom      = 1./(1. - rdif(i,k-1)*rdndif(i,k-1))
          rdirexp     = rdir(i,k-1)*exptdn(i,k-1)
          tdnmexp     = tottrn(i,k-1) - exptdn(i,k-1)
          tottrn(i,k) = exptdn(i,k-1)*tdir(i,k-1) + tdif(i,k-1)*
     $                  (tdnmexp + rdndif(i,k-1)*rdirexp)*rdenom
          rdndif(i,k) = rdif(i,k-1)  +
     $                  (rdndif(i,k-1)*tdif(i,k-1))*(tdif(i,k-1)*rdenom)
        end do
      end do
C 
      return
      end
 
