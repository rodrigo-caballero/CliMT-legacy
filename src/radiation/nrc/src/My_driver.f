      program driver

C     GCM 2.0  VERSION 1.1  JAN 2003

      implicit none 
      include "grid.h"
      include "radinc.h"
      include "radcommon.h"

      real*8 dsig(L_LAYERS), TL(L_LEVELS)
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

      real*8 scaleht
      real*8 taucum(L_LEVELS), TAUTOT, CONRNU
      real*8 SOL(L_NSPECTV)

      integer gcmlayers
      integer NLAYRAD, NLEVRAD, NSPECTI, NSPECTV, NPREF, NTREF
      integer K, L, NW, nn, nnn
      real*8  cmk, rsfi, rsfv, albi, albv, PI, CP
      real*8  ptrop, scalep, rsdist, acosz, grav, gt, tinf, psf
      real*8  tstrat, ans
      real*8  fluxid(L_NLAYRAD),fluxvd(L_NLAYRAD)
      real*8  heating(L_NLAYRAD), total
      real*8  fdmax, fdmin
      real*8  firmax, firmin, fvmax, fvmin, df
      real*4  tstrato, psfo, gto, coszo, alspo, tautoto

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

C 24 Layers (Standard 20+4)
      DATA  DSIG    / 0.0001237, 0.0002186, 0.0003877, 0.0006877,
     *                0.0012199, 0.0021622, 0.0038572, 0.0057287,
     *                0.0094287, 0.0155443, 0.0256262, 0.0422609,
     *                0.0696418, 0.1148966, 0.1530633, 0.1572269,
     *                0.1352191, 0.1212412, 0.0718975, 0.0435678,
     *                0.014,     0.008 ,    0.003 ,    0.001      /

C 30-layer

c     DATA  DSIG    / 0.000005, 0.000009, 0.000014, 0.000031,
c    *                0.000047, 0.000081, 0.000149, 0.000242,
c    *                0.000417, 0.000720, 0.001244, 0.002147,
c    *                0.003699, 0.005728, 0.009428, 0.015542,
c    *                0.025622, 0.042254, 0.069632, 0.114879,
c    *                0.153040, 0.157203, 0.135199, 0.121223,
c    *                0.071883, 0.043562,
c    *                0.014, 0.008 , 0.003 , 0.001 /
C
C======================================================================C

      NLAYRAD = L_NLAYRAD
      NLEVRAD = L_NLEVRAD
      NSPECTI = L_NSPECTI
      NSPECTV = L_NSPECTV
      NPREF   = L_NPREF
      NTREF   = L_NTREF

      Cmk     = 3.51E+22
      RSFI    = 0.0  ! SURFACE REFLECTANCE FOR IR AKA ALBEDO
      RSFV    = 0.2  ! SURFACE REFLECTANCE FOR VISUAL AKA ALBEDO
      ALBI    = 0.04
      ALBI    = 0.00
      ALBV    = 0.24
      ALBV    = 0.2
      PI      = 3.14159265358979D0
      CP      = 735.94
      TLIMIT  = 0.2

C     Set up spectral intervals in the Solar (VISUAL) and then IR.
C     Read in the k-coefficients. . .

      call radsetup

      QextREF = Qextv(6)

      ptrop  = 0.00005
c     ptop   = ptrop*0.5
      psf    = 6.1
      TAUTOT = 0.3
      CONRNU = 0.03
      scalep = 100.0

      acosz  = 0.500000000

      write(6,'("PTOP   = ",1pe10.3)') PTOP
      write(6,'("PTROP  = ",1pe10.3)') PTROP
      write(6,'("PSF    = ",f10.3)') psf
      write(6,'("COSZ   = ",f10.3)') ACOSZ
      write(6,'("ALSP   = ",f10.3)') albv
      write(6,'("TAUTOT = ",f10.3)') tautot

      do k=1,L_LEVELS
        QH2O(K) = 1.0D-7
      end do
 
c     rsdist = 2.428        ! Ls =   0   SCOSZ = 557
c     rsdist = 2.745        ! Ls =  90   SCOSZ = 493
c     rsdist = 2.147        ! Ls = 180   SCOSZ = 630
c     rsdist = 1.927        ! Ls = 270   SCOSZ = 702
c     rsdist = 2.255        !SOLAR FLUX AT MARS:   601.330
C                            used for tests with GCM 1-D model

      rsdist = 2.255

      grav   = 3.72
      gcmlayers = L_LAYERS
      
      tstrat = 200.0
      
      do k=1,L_LEVELS
        tl(K) = TSTRAT
      end do

c     include "temperature_G6"
c     include "temperature_G7"
c     include "temp_test_2001"
      
      gt     = TL(L_LEVELS)
      tinf   = TL(L_LEVELS)

      psfo    = Psf
      tstrato = tstrat
      gto     = gt

C  Calculate the sigma values

      sigma(3) = 0.0
      do L=1,L_LAYERS
        K = 2*L+3
        sigma(K) = sigma(K-2)+DSIG(L)
      end do

      do K=4,L_LEVELS-1,2
        sigma(K) = 0.5*(SIGMA(K+1)+SIGMA(K-1))
      end do

C     Fill cumulative dust optical depth arrays (cum. dust optical
C     depth fromn the top of the atmosphere to the bottom of level K).

      IF(TAUTOT.LE.0.0) THEN
        do K=1,L_LEVELS+1
          tauref(K) = 0.0
        end do
      ELSE
        CALL dustprofile(PSF,PTROP,CONRNU,TAUTOT,SIGMA,TAUCUM,TAUREF)
      END IF

C     These files were created with CONRNU = 0.03, 30-layers
c     include "taucum_0.3"
c     include "taucum_0.45"
c     include "taucum_1.0"
c     include "taucum_5.0"
c     include "taucum_5.0_001"
c     include "taucum_5.0_00000001"

C     Fill local arrays with pressures at layer boundaries and
C     mid-points.

C     Pressures at GCM levels

      PLEV(1) = 0.0
      PLEV(2) = PTOP
      PLEV(3) = PTROP
      DO K=4,L_LEVELS
        PLEV(K) = SIGMA(K)*(PSF-PTROP)+PTROP
      END DO
      PLEV(L_LEVELS+1) = PLEV(L_LEVELS)

C  Below
C  for specified "linear decrease with height" profile

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

C     Calculate solar flux at the current mars distance

      ans = 0.0
      if(acosz.lt.1.0e-4) then
        do NW=1,L_NSPECTV
          SOL(NW) = 0.0
        end do
      else
        do NW=1,L_NSPECTV
          SOL(nw) = SOLARF(NW)/RSDIST
          ans     = ans+sol(NW)
        end do
      end if

      write(6,'("SOLAR FLUX AT MARS:  ",f8.3)') ANS 
 
C     Set up, and solve for, the solar (visual) fluxes, if the sun
C     is up

      if(acosz.ge.1.0e-4) then
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

c       do L=1,L_NLAYRAD
c         write(6,'("FMNETV(",i2,") = ",f12.4)') L, FMNETV(L)
c       end do
c       write(6,'("FLUXDNV     = ",f10.3)') FLUXDNV(L_NLAYRAD)
C     Set up, and solve for, the Infrared fluxes

      call OPTCI(DTAUI,TAUCUMI,CO2I,TLEV,PLEV,PFGASREF,TGASREF,
     *           Cmk,QextREF,QEXTI,QSCATI,GI,COSBI,WBARI,TAUREF,
     *           TMID,PMID,NGWI,QH2O,WREFH2O)
 
      call SFLUXI(PLEV,TLEV,DTAUI,TAUCUMI,UBARI,ALBI,WNOI,DWNI,
     *            COSBI,WBARI,GWEIGHT,NFLUXTOPI,FMNETI,
     *            fluxupi,fluxdni,FZEROI,NGWI)

c     write(6,'("FLUXUPI = ",f12.4)') FLUXUPI(L_NLAYRAD)

C     Output for IDL

      open(60,file='driver_data')
c     write(60,'("G-1")')

C     Upward and downward flux

      firmax = FLUXUPI(1)
      firmin = FLUXUPI(1)
      fvmax  = FLUXUPV(1)
      fvmin  = FLUXUPV(1)
     
      do L=1,L_NLAYRAD
        if(FLUXUPI(L).gt.firmax) firmax = FLUXUPI(L) 
        if(FLUXUPI(L).lt.firmin) firmin = FLUXUPI(L) 
        if(FLUXUPV(L).gt.fvmax)  fvmax  = FLUXUPV(L) 
        if(FLUXUPV(L).lt.fvmin)  fvmin  = FLUXUPV(L) 
      end do

      df = 0.05*(firmax-firmin)
      firmax = firmax+df
      firmin = firmin-df
      
      df = 0.05*(fvmax-fvmin)
      fvmax = fvmax+df
      fvmin = fvmin-df
      
      write(60,'(i4)') NLAYRAD
      write(60,'(1pe15.5,3(2x,1pe15.5))') firmax, firmin, fvmax, fvmin

      do L=1,L_NLAYRAD
c       scaleht = max(0.0D0,log(psf/plev(2*L+1)))
        write(60,'(1pe15.5,2(2x,1pe15.5))') plev(2*L+1), FLUXUPV(L),
     *                                      FLUXUPI(L)
      end do

C     Downward fluxes

      firmax = FLUXDNI(1)
      firmin = FLUXDNI(1)
      fvmax  = FLUXDNV(1)
      fvmin  = FLUXDNV(1)
     
      do L=1,L_NLAYRAD
        if(FLUXDNI(L).gt.firmax) firmax = FLUXDNI(L) 
        if(FLUXDNI(L).lt.firmin) firmin = FLUXDNI(L) 
        if(FLUXDNV(L).gt.fvmax)  fvmax  = FLUXDNV(L) 
        if(FLUXDNV(L).lt.fvmin)  fvmin  = FLUXDNV(L) 
      end do

      df = 0.05*(firmax-firmin)
      firmax = firmax+df
      firmin = firmin-df
      
      df = 0.05*(fvmax-fvmin)
      fvmax = fvmax+df
      fvmin = fvmin-df
      
      write(60,'(1pe15.5,3(2x,1pe15.5))') firmax, firmin, fvmax, fvmin

      do L=1,L_NLAYRAD
c       scaleht = max(0.0D0,log(psf/plev(2*L+1)))
        write(60,'(1pe15.8,2(2x,1pe15.8))') plev(2*L+1), FLUXDNV(L),
     *                                      FLUXDNI(L)
      end do

C     Net flux and flux divergence (as well as T-profile)

      firmax = FMNETI(1)
      firmin = FMNETI(1)
      fvmax  = FMNETV(1)
      fvmin  = FMNETV(1)
     
      do L=1,L_NLAYRAD
        if(FMNETI(L).gt.firmax) firmax = FMNETI(L) 
        if(FMNETI(L).lt.firmin) firmin = FMNETI(L) 
        if(FMNETV(L).gt.fvmax)  fvmax  = FMNETV(L) 
        if(FMNETV(L).lt.fvmin)  fvmin  = FMNETV(L) 
      end do

      df = 0.05*(firmax-firmin)
      firmax = firmax+df
      firmin = firmin-df
      
      df = 0.05*(fvmax-fvmin)
      fvmax = fvmax+df
      fvmin = fvmin-df
      
      write(60,'(i4)') NLAYRAD
      write(60,'(f10.4,3(2x,f10.4))') acosz, albv, ans, 
     *           taucum(L_LEVELS)
      write(60,'(1pe15.5,3(2x,1pe15.5))') firmax, firmin, fvmax, fvmin
      do L=1,L_NLAYRAD-1
c       scaleht = max(0.0D0,log(psf/plev(2*L+1)))
        write(60,'(1pe15.5,2(2x,1pe17.7))') plev(2*L+1), FMNETV(L),
     *                              FMNETI(L)
c       write(6,'(f12.5,3x,f12.5)') -FMNETV(L)/ans, scaleht
      end do
c     write(6,'("FMNETV = ",f10.3)') FMNETV(L_NLAYRAD-1)/ans

      L = L_NLAYRAD
      scaleht = 0.0
      write(60,'(1pe15.5,2(2x,1pe15.5))') psf, FMNETV(L),
     *                              FMNETI(L)

C     UPDATE THE STRATOSPHERE

      fluxvd(1)  = FMNETV(1)-NFLUXTOPV
      heating(1) = fluxvd(1)*88775.0*grav/
     *                      (cp*scalep*PLEV(3))
      fluxid(1) = (FMNETI(1)-NFLUXTOPI)*88775.0*grav/
     *                      (cp*scalep*PLEV(3))

      fdmax = -10000.0
      fdmin =  10000.0
      
      do L=2,L_NLAYRAD
        fluxvd(L)  = FMNETV(L)-FMNETV(L-1)
        heating(L) = fluxvd(L)*88775.0*grav/(cp*scalep*
     *                (PLEV(2*L+1)-PLEV(2*L-1)))
        fluxid(L)  = FMNETI(L)-FMNETI(L-1)
        fluxid(L)  = fluxid(L)*88775.0*grav/(cp*scalep*
     *                (PLEV(2*L+1)-PLEV(2*L-1)))
        if(heating(L) .GT. fdmax) fdmax = heating(L)
        if(fluxid(L) .GT. fdmax) fdmax = fluxid(L)
        if(heating(L) .LT. fdmin) fdmin = heating(L)
        if(fluxid(L) .LT. fdmin) fdmin = fluxid(L)
      end do

      fdmax = min(fdmax,1000.0D0)
      fdmin = max(fdmin,-1000.0D0)

      df = 0.05*(fdmax-fdmin)
      fdmax = fdmax+df
      fdmin = fdmin-df

      write(60,'(1pe10.3,3x,1pe10.3)') fdmax, fdmin
      do L=2,L_NLAYRAD
c       scaleht = max(0.0D0,log(psf/plev(2*L)))
        total   = HEATING(L)+FLUXID(L)
        write(60,'(1pe15.7,4(1x,1pe13.5))') plev(2*L), HEATING(L), 
     *             FLUXID(L), TOTAL, TLEV(2*L)
      end do

      coszo   = acosz
      alspo   = albv
      tautoto = taucum(L_LEVELS)

      write(60,'(f7.2,f10.3)') GT, PSF

      end
