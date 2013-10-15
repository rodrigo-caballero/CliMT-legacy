c This is a modified version of the NRC driver, adapted for use with CliMT
c
      program driver

      implicit none 
      include "grid.h"
      include "radinc.h"
      include "radcommon.h"

c+++rca 
c Input
      real p_in(L_LAYERS)  ! [mb] pressure (on GCM levels)
      real t_in(L_LAYERS)  ! [K]  temperature (on GCM levels)
      real q_in(L_LAYERS)  ! [g/g] humidity (on GCM levels)
      real zen             ! [dgr] solar zenith angle
      real solin           ! [W/m2] insolation
      real albi            ! infrared albedo
      real albv            ! visible albedo
      real grav            ! [m/s2] gravit accn
      real gt              ! [K] surface temperature
      real psf             ! [mb] surface pressure
c Output
      real lwflx(L_LAYERS+1)   ! Net infrared flux [W/m2]
      real swflx(L_LAYERS+1)   ! Net visible flux  [W/m2]
      real swhr(L_LAYERS) ! Visible heating rate [K/s]
      real lwhr(L_LAYERS) ! Infrared heating rate [K/s]
c---rca

      real*8 TL(L_LEVELS)
      real*8 sigma(L_LEVELS)
      real*8 PLEV(L_LEVELS+1), TLEV(L_LEVELS)
      real*8 TMID(L_LEVELS), PMID(L_LEVELS)
      real*8 DIFFVT

C  VISUAL

      real*8 DTAUV(L_NLAYRAD,L_NSPECTV,L_NGAUSS)
      real*8 TAUV(L_NLEVRAD,L_NSPECTV,L_NGAUSS)
      real*8 TAUCUMV(L_LEVELS,L_NSPECTV,L_NGAUSS)
      real*8 COSBV(L_NLAYRAD,L_NSPECTV,L_NGAUSS)
      real*8 WBARV(L_NLAYRAD,L_NSPECTV,L_NGAUSS)
      integer ngwv(L_NSPECTV)

C  IR

      real*8 DTAUI(L_NLAYRAD,L_NSPECTI,L_NGAUSS)
      real*8 TAUCUMI(L_LEVELS,L_NSPECTI,L_NGAUSS)
      real*8 COSBI(L_NLAYRAD,L_NSPECTI,L_NGAUSS)
      real*8 WBARI(L_NLAYRAD,L_NSPECTI,L_NGAUSS)
      real*8 dnvflux(L_J,L_I)
      real*8 taugsurfi(L_NSPECTI,L_NGAUSS-1)

C  Water mixing
      real*8 QH2O(L_LEVELS)

      real*8 scaleht,acosz, swhr_strat, lwhr_strat
      real*8 taucum(L_LEVELS), TAUTOT, CONRNU
      real*8 SOL(L_NSPECTV)

      integer NLAYRAD, NLEVRAD, NSPECTI, NSPECTV, NPREF, NTREF
      integer K, L, NW, nn, nnn
      real*8  cmk, rsfi, rsfv, PI, CP
      real*8  ptrop, scalep, rsdist 
      real*8  tstrat, ans
      real*8  fluxid(L_NLAYRAD),fluxvd(L_NLAYRAD)
      real*8  total
      real*8  fdmax, fdmin
      real*8  firmax, firmin, fvmax, fvmin, df

      real*8 FMNETI(L_NLEVRAD), FMNETV(L_NLAYRAD)
      real*8 fluxupi(L_NLAYRAD), fluxdni(L_NLAYRAD), NFLUXTOPI
      real*8 fluxupv(L_NLAYRAD), fluxdnv(L_NLAYRAD), NFLUXTOPV

      real*8 tlimit
      common /tlim1 / tlimit

      real*8 H, dtdz, TS1
      integer ngwi(L_NSPECTI)

!  Testing

      integer ng
      real*4 irheat, sh(L_NLAYRAD), wlv(L_NSPECTV), wlir(L_NSPECTI)
      real*4 heatir(L_NSPECTI,L_NLAYRAD), heatv(L_NSPECTV,L_NLAYRAD)
      real*4 netflux(L_NLAYRAD,17)
      real*4 netfluxir(L_NSPECTI,L_NLAYRAD)
      real*4 netfluxv(L_NSPECTV,L_NLAYRAD)

      common /test01/ netflux, netfluxir, netfluxv
C======================================================================C

c++rca these are only used for printout
      NLAYRAD = L_NLAYRAD
      NLEVRAD = L_NLEVRAD
      NSPECTI = L_NSPECTI
      NSPECTV = L_NSPECTV
      NPREF   = L_NPREF
      NTREF   = L_NTREF
c---rca

      Cmk     = 3.51E+22
      RSFI    = 0.0       ! SURFACE REFLECTANCE FOR IR AKA ALBEDO
      RSFV    = 0.2       ! SURFACE REFLECTANCE FOR VISUAL AKA ALBEDO
c+++rca      ALBI    = 0.00
c+++rca      ALBV    = 0.2
      PI      = abs(acos(-1.))
      CP      = 735.94
      TLIMIT  = 0.2

C     Set up spectral intervals in the Solar (VISUAL) and then IR.
C     Read in the k-coefficients. . .
      call radsetup

      QextREF = Qextv(6)
      ptrop  = 0.00005
      psf    = 6.1
      TAUTOT = 0.3
      CONRNU = 0.03
      scalep = 100.0
      acosz  = 0.5        ! cos solar zenith angle
      solin  = 600.8869179600888 

      do k=1,L_LEVELS
        QH2O(K) = 1.0D-7
      end do

      grav   = 3.72

C  Calculate the sigma values

c+++rca
      open(33,file='plev.dat',status='old')
      do k=1,L_LEVELS
         read(33,*)plev(k)
         sigma(k)=(plev(k)-ptrop)/(psf-ptrop)
      enddo
      close(33)

      open(33,file='plev.layer.dat',status='old')
      do k=1,L_LAYERS
         read(33,*)p_in(k)
      enddo
      close(33)

c+++rca
      do L=1,L_LAYERS
        K=2*L+2
        PLEV(K) = p_in(L)
      end do
      plev(1)=0.
c      plev(2)=p
C     Now interpolate (aka average) to get P at layer boundaries

      do L=1,L_LAYERS-1
        K=2*L+3
        PLEV(K) = 0.5*(PLEV(K-1)+PLEV(K+1))
      end do
      PLEV(L_LEVELS) = PSF
c---rca      

C     Fill cumulative dust optical depth arrays (cum. dust optical
C     depth fromn the top of the atmosphere to the bottom of level K).

      IF(TAUTOT.LE.0.0) THEN
        do K=1,L_LEVELS+1
          tauref(K) = 0.
        end do
      ELSE
        CALL dustprofile(PSF,PTROP,CONRNU,TAUTOT,SIGMA,TAUCUM,TAUREF)
      END IF

C     Fill local arrays with pressures at layer boundaries and
C     mid-points.

c+++rca
C     Pressures at GCM levels
c      PLEV(1) = 0.0
c      PLEV(2) = PTOP
c      PLEV(3) = PTROP
c      DO K=4,L_LEVELS
c        PLEV(K) = SIGMA(K)*(PSF-PTROP)+PTROP
c      END DO
c      PLEV(L_LEVELS+1) = PLEV(L_LEVELS)
c
C  Below  for specified "linear decrease with height" profile

      H      = 11.0
      dtdz   = -2.5
      TS1    = 202.5
      GT     = 240.0
      TSTRAT = TS1
      TL(1)  = TS1
      TL(2)  = TS1
      TL(3)  = TS1

      DO K=4,L_LEVELS-1
        TL(K) = GT - H*dtdz*log(PLEV(K)/PSF)
        if(TL(K).lt.TS1) TL(K) = TS1
      END DO
      TL(L_LEVELS) = GT

C  end "linear decrease with height" profile

C     Temperatures at the GCM pressure levels

      TLEV(1) = TSTRAT
      TLEV(2) = TSTRAT
      TLEV(3) = TSTRAT

C     Fill the values at layer mid-points; we know these, no interpolation
C     necessary

      do L=1,L_LAYERS
        K=2*L+2
        TLEV(K) = TL(K)
      end do

C     Now interpolate (aka average) to get T at layer boundaries

      do L=1,L_LAYERS-1
        K=2*L+3
        TLEV(K) = 0.5*(TLEV(K-1)+TLEV(K+1))
      end do

      TLEV(L_LEVELS) = GT

C     Fill the TMID/PMID arrays used by optci.f and optcv.f

C     TMID and PMID; used to get the index for co2 k-coefficient
C     interpolation

      TMID(1) = TLEV(2)
      TMID(2) = TLEV(2)
      PMID(1) = PLEV(2)
      PMID(2) = PLEV(2)

      DO L=1,L_NLAYRAD-1
        TMID(2*L+1) = TLEV(2*L+1)
        TMID(2*L+2) = TLEV(2*L+1)
        PMID(2*L+1) = PLEV(2*L+1)
        PMID(2*L+2) = PLEV(2*L+1)
      END DO

      TMID(L_LEVELS) = TLEV(L_LEVELS)
      PMID(L_LEVELS) = PLEV(L_LEVELS)

C     And now back to the regular code. . .

C     Calculate solar flux at the current Mars distance

c+++rca get rsdist as ratio of specified insolation to solar flux at 1AU (=1355 W/m2)
      rsdist = 1355./solin
c---rca

      if(acosz.lt.1.e-4) then
        do NW=1,L_NSPECTV
          SOL(NW) = 0.0
        end do
      else
        do NW=1,L_NSPECTV
          SOL(nw) = SOLARF(NW)/RSDIST
        end do
      end if
 
C     Set up, and solve for, the solar (visual) fluxes, if the sun is up
      if(acosz.ge.1.e-4) then
        call OPTCV(DTAUV,TAUV,TAUCUMV,CO2V,TLEV,PLEV,PFGASREF,
     *             TGASREF,Cmk,QEXTV,QSCATV,GV,WBARV,COSBV,
     *             TAURAY,TAUREF,TMID,PMID,NGWV,QH2O,WREFH2O)

        call SFLUXV(DTAUV,TAUV,TAUCUMV,ALBV,DWNV,WBARV,COSBV,
     *              ACOSZ,SOL,GWEIGHT,NFLUXTOPV,FMNETV,
     *              FLUXUPV,FLUXDNV,DIFFVT,FZEROV,NGWV)
      else
        NFLUXTOPV = 0.0
        do L=1,L_NLAYRAD
          FMNETV(L) = 0.0
        end do
      end if

C     Set up, and solve for, the Infrared fluxes
      call OPTCI(DTAUI,TAUCUMI,CO2I,TLEV,PLEV,PFGASREF,TGASREF,
     *           Cmk,QextREF,QEXTI,QSCATI,GI,COSBI,WBARI,TAUREF,
     *           TMID,PMID,NGWI,QH2O,WREFH2O)
 
      call SFLUXI(PLEV,TLEV,DTAUI,TAUCUMI,UBARI,ALBI,WNOI,DWNI,
     *            COSBI,WBARI,GWEIGHT,NFLUXTOPI,FMNETI,
     *            fluxupi,fluxdni,FZEROI,NGWI)

C------ Output for IDL

C-------       open(60,file='My_driver_data')
C------- 
C------- C     Upward and downward flux
C------- 
C-------       firmax = FLUXUPI(1)
C-------       firmin = FLUXUPI(1)
C-------       fvmax  = FLUXUPV(1)
C-------       fvmin  = FLUXUPV(1)
C-------      
C-------       do L=1,L_NLAYRAD
C-------         if(FLUXUPI(L).gt.firmax) firmax = FLUXUPI(L) 
C-------         if(FLUXUPI(L).lt.firmin) firmin = FLUXUPI(L) 
C-------         if(FLUXUPV(L).gt.fvmax)  fvmax  = FLUXUPV(L) 
C-------         if(FLUXUPV(L).lt.fvmin)  fvmin  = FLUXUPV(L) 
C-------       end do
C------- 
C-------       df = 0.05*(firmax-firmin)
C-------       firmax = firmax+df
C-------       firmin = firmin-df
C-------       
C-------       df = 0.05*(fvmax-fvmin)
C-------       fvmax = fvmax+df
C-------       fvmin = fvmin-df
C-------       
C-------       write(60,'(i4)') NLAYRAD
C-------       write(60,'(1pe15.5,3(2x,1pe15.5))') firmax, firmin, fvmax, fvmin
C------- 
C-------       do L=1,L_NLAYRAD
C------- c       scaleht = max(0.0D0,log(psf/plev(2*L+1)))
C-------         write(60,'(1pe15.5,2(2x,1pe15.5))') plev(2*L+1), FLUXUPV(L),
C-------      *                                      FLUXUPI(L)
C-------       end do
C------- 
C------- C     Downward fluxes
C------- 
C-------       firmax = FLUXDNI(1)
C-------       firmin = FLUXDNI(1)
C-------       fvmax  = FLUXDNV(1)
C-------       fvmin  = FLUXDNV(1)
C-------      
C-------       do L=1,L_NLAYRAD
C-------         if(FLUXDNI(L).gt.firmax) firmax = FLUXDNI(L) 
C-------         if(FLUXDNI(L).lt.firmin) firmin = FLUXDNI(L) 
C-------         if(FLUXDNV(L).gt.fvmax)  fvmax  = FLUXDNV(L) 
C-------         if(FLUXDNV(L).lt.fvmin)  fvmin  = FLUXDNV(L) 
C-------       end do
C------- 
C-------       df = 0.05*(firmax-firmin)
C-------       firmax = firmax+df
C-------       firmin = firmin-df
C-------       
C-------       df = 0.05*(fvmax-fvmin)
C-------       fvmax = fvmax+df
C-------       fvmin = fvmin-df
C-------       
C-------       write(60,'(1pe15.5,3(2x,1pe15.5))') firmax, firmin, fvmax, fvmin
C------- 
C-------       do L=1,L_NLAYRAD
C-------         write(60,'(1pe15.8,2(2x,1pe15.8))') plev(2*L+1), FLUXDNV(L),
C-------      *                                      FLUXDNI(L)
C-------       end do
C------- 
C------- C     Net flux and flux divergence (as well as T-profile)
C------- 
C-------       firmax = FMNETI(1)
C-------       firmin = FMNETI(1)
C-------       fvmax  = FMNETV(1)
C-------       fvmin  = FMNETV(1)
C-------      
C-------       do L=1,L_NLAYRAD
C-------         if(FMNETI(L).gt.firmax) firmax = FMNETI(L) 
C-------         if(FMNETI(L).lt.firmin) firmin = FMNETI(L) 
C-------         if(FMNETV(L).gt.fvmax)  fvmax  = FMNETV(L) 
C-------         if(FMNETV(L).lt.fvmin)  fvmin  = FMNETV(L) 
C-------       end do
C------- 
C-------       df = 0.05*(firmax-firmin)
C-------       firmax = firmax+df
C-------       firmin = firmin-df
C-------       
C-------       df = 0.05*(fvmax-fvmin)
C-------       fvmax = fvmax+df
C-------       fvmin = fvmin-df
C-------       
C-------       write(60,'(i4)') NLAYRAD
C-------       write(60,'(f10.4,3(2x,f10.4))') acosz, albv, ans, 
C-------      *           taucum(L_LEVELS)
C-------       write(60,'(1pe15.5,3(2x,1pe15.5))') firmax, firmin, fvmax, fvmin
C-------       do L=1,L_NLAYRAD-1
C-------         write(60,'(1pe15.5,2(2x,1pe17.7))') plev(2*L+1), FMNETV(L),
C-------      *                              FMNETI(L)
C-------       end do
C-------       L = L_NLAYRAD
C-------       scaleht = 0.0
C-------       write(60,'(1pe15.5,2(2x,1pe15.5))') psf, FMNETV(L),
C-------      *                              FMNETI(L)

      print*,'lev pres  sw lw'
      do k=1,L_nlayrad
         print'(i3,10f10.3)',k,plev(2*k+1),fmnetv(k),fmneti(k)
      enddo


C     STRATOSPHERIC heating

      fluxvd(1)  = FMNETV(1)-NFLUXTOPV
      swhr_strat = fluxvd(1)*88775.0*grav/
     *                      (cp*scalep*PLEV(3))
      lwhr_strat = (FMNETI(1)-NFLUXTOPI)*88775.0*grav/
     *                      (cp*scalep*PLEV(3))

C     Heating on lower levels
c+++rca heating in K/s instead of K/sol      
      do L=2,L_NLAYRAD
        fluxvd(L)  = FMNETV(L)-FMNETV(L-1)
        fluxid(L)  = FMNETI(L)-FMNETI(L-1)
        swhr(L-1) = fluxvd(L)*grav/(cp*scalep*
     *                (PLEV(2*L+1)-PLEV(2*L-1)))
        lwhr(L-1)  = fluxid(L)*grav/(cp*scalep*
     *                (PLEV(2*L+1)-PLEV(2*L-1)))
c        vheating(L) = fluxvd(L)*88775.0*grav/(cp*scalep*
c     *                (PLEV(2*L+1)-PLEV(2*L-1)))
c        iheating(L)  = fluxid(L)*88775.0*grav/(cp*scalep*
c     *                (PLEV(2*L+1)-PLEV(2*L-1)))
      end do

c output fluxes
      do k=1,L_NLAYRAD
      swflx(k)=FMNETV(K)
      lwflx(k)=FMNETI(K)
      enddo
c---rca

      print*,'lev pres  swhr lwhr'
      do k=1,L_layers
         print'(i3,10f10.3)',k,plev(2*k+2),swhr(k)*88775.,lwhr(k)*88775.
      enddo

      end
