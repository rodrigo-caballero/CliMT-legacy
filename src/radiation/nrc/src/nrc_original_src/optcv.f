      SUBROUTINE OPTCV(DTAUV,TAUV,TAUCUMV,CO2V,TLEV,PLEV,PFGASREF,
     *                 TGASREF,Cmk,QEXTV,QSCATV,GV,WBARV,COSBV,
     *                 TAURAY,TAUREF,TMID,PMID,NGWV,QH2O,WREFH2O)

C  GCM2.0  Feb 2003
C
C THIS SUBROUTINE SETS THE OPTICAL CONSTANTS IN THE VISUAL  
C IT CALCUALTES FOR EACH LAYER, FOR EACH SPECRAL INTERVAL IN THE VISUAL
C LAYER: WBAR, DTAU, COSBAR
C LEVEL: TAU
C
C TAUV(L,NW,NG) is the cumulative optical depth at the top of radiation code
C layer L. NW is spectral wavelength interval, ng the Gauss point index.
C
C     TLEV(L) - Temperature at the layer boundary
C     PLEV(L) - Pressure at the layer boundary (i.e. level)
C     CO2V(NT,NPS,NW,NG) - Visual CO2 k-coefficients 
C
C----------------------------------------------------------------------C

      implicit none
      include "grid.h"
      include "radinc.h"

      real*8 DTAUV(L_NLAYRAD,L_NSPECTV,L_NGAUSS)
      real*8 DTAUKV(L_LEVELS+1,L_NSPECTV,L_NGAUSS)
      real*8 TAUV(L_NLEVRAD,L_NSPECTV,L_NGAUSS)
      real*8 TAUCUMV(L_LEVELS,L_NSPECTV,L_NGAUSS)
      real*8 TLEV(L_LEVELS), PLEV(L_LEVELS+1)
      real*8 TMID(L_LEVELS), PMID(L_LEVELS)
      real*8 CO2V(L_NTREF,L_PINT,L_REFH2O,L_NSPECTV,L_NGAUSS)
      real*8 TGASREF(L_NTREF)
      real*8 PFGASREF(L_PINT)
      real*8 QEXTV(L_NSPECTV), QSCATV(L_NSPECTV)
      real*8 COSBV(L_NLAYRAD,L_NSPECTV,L_NGAUSS)
      real*8 WBARV(L_NLAYRAD,L_NSPECTV,L_NGAUSS)
      real*8 TAURAY(L_NSPECTV), TAUREF(L_LEVELS+1)
      real*8 GV(L_NSPECTV)

      integer L, NW, NG, K, NG1(L_NSPECTV), LK
      integer MT(L_LEVELS), MP(L_LEVELS), NP(L_LEVELS)
      real*8  Cmk, ANS, TAUREFL, TAUGAS
      real*8  TRAY(L_LEVELS,L_NSPECTV)
      real*8  TAEROS(L_LEVELS,L_NSPECTV)
      real*8  DPR(L_LEVELS), U(L_LEVELS), TAUCLD
      real*8  LCOEF(4), LKCOEF(L_LEVELS,4)

      real*8 taugsurf(L_NSPECTV,L_NGAUSS-1), TRAYAER

C  Tlimit:  If the CO2 optical depth (top to the surface) is less than
C  this value, we place that Gauss-point into the "zeros" channel.
C  Set in driver, passed via common

      real*8 tlimit
      common /tlim1 / tlimit

      integer ngwv(L_NSPECTV)

C  Reference wavelength is (now) bin #6 - put into qextref
      real*8 QextREF

C  Water mixing ratio stuff

      real*8 QH2O(L_LEVELS), WREFH2O(L_REFH2O), WRATIO(L_LEVELS)
      real*8 KCOEF(4)
      integer nh2o(L_LEVELS)

C======================================================================C

      QextREF = Qextv(L_NREFV)

C  Determine the total gas opacity throughout the column, for each
C  spectral interval, NW, and each Gauss point, NG.
C  Calculate the continuum opacities, i.e., those that do not depend on
C  NG, the Gauss index.

      DO NG=1,L_NGAUSS-1
        do NW=1,L_NSPECTV
          TAUGSURF(NW,NG) = 0.0D0
        end do
      end do
     
      do K=2,L_LEVELS
        DPR(k) = PLEV(K)-PLEV(K-1)
        U(k)   = Cmk*DPR(k)
   
        call tpindex(PMID(K),TMID(K),QH2O(K),pfgasref,tgasref,WREFH2O,
     *               LCOEF,MT(K),MP(K),NH2O(K),WRATIO(K))

        do LK=1,4
          LKCOEF(K,LK) = LCOEF(LK)
        end do

        DO NW=1,L_NSPECTV
          TRAY(K,NW)   = TAURAY(NW)*DPR(K)
          TAEROS(K,NW) = TAUREF(K)*Qextv(NW)/QextREF
        END DO
      end do

C  TAUCLD = is cloud opacity, zero until further notice
C  TRAYAER is Tau RAYleigh scattering, plus AERosol opacity

      TAUCLD = 0.0D0
      do NW=1,L_NSPECTV
        ngwv(nw) = L_NGAUSS

C  Now fill in the "clear" part of the spectrum (NG = L_NGAUSS)
C  Which holds continuum opacity only

        do K=2,L_LEVELS
           DTAUKV(K,nw,L_NGAUSS) = TAEROS(K,NW)+TRAY(K,NW) + TAUCLD
        end do

        do ng=L_NGAUSS-1,1,-1
          do K=2,L_LEVELS

C           NOW COMPUTE TAUGAS

C  Interpolate between water mixing ratios
C  WRATIO = 0.0 if the requested water amount is equal to, or outside the
C  the range of water amount data.

            KCOEF(1) = CO2V(MT(K),MP(K),NH2O(K),NW,NG) + WRATIO(K)*
     *                (CO2V(MT(K),MP(K),NH2O(K)+1,NW,NG) - 
     *                 CO2V(MT(K),MP(K),NH2O(K),NW,NG))

            KCOEF(2) = CO2V(MT(K),MP(K)+1,NH2O(K),NW,NG) + WRATIO(K)*
     *                (CO2V(MT(K),MP(K)+1,NH2O(K)+1,NW,NG) - 
     *                 CO2V(MT(K),MP(K)+1,NH2O(K),NW,NG))

            KCOEF(3) = CO2V(MT(K)+1,MP(K)+1,NH2O(K),NW,NG) + WRATIO(K)*
     *                (CO2V(MT(K)+1,MP(K)+1,NH2O(K)+1,NW,NG) - 
     *                 CO2V(MT(K)+1,MP(K)+1,NH2O(K),NW,NG))

            KCOEF(4) = CO2V(MT(K)+1,MP(K),NH2O(K),NW,NG) + WRATIO(K)*
     *                (CO2V(MT(K)+1,MP(K),NH2O(K)+1,NW,NG) - 
     *                 CO2V(MT(K)+1,MP(K),NH2O(K),NW,NG))

C  Interpolate the CO2 k-coefficients to the requested T,P

      
            ANS = LKCOEF(K,1)*KCOEF(1) + LKCOEF(K,2)*KCOEF(2) +
     *            LKCOEF(K,3)*KCOEF(3) + LKCOEF(K,4)*KCOEF(4)

            TAUGAS          = U(k)*ANS
            TAUGSURF(NW,NG) = TAUGSURF(NW,NG) + TAUGAS
            DTAUKV(K,nw,ng) = TAUGAS + TAUCLD + TRAY(K,NW) +
     *                        TAEROS(K,NW)
          end do
          if(TAUGSURF(NW,NG) .LT. TLIMIT) THEN
            goto 10
          else
            NGWV(NW) = NG
          end if

        end do
   10   continue
      end do

C  Now the full treatment for the layers, where besides the opacity
C  we need to calculate the scattering albedo and asymmetry factors
C  for each layer

      DO NW=1,L_NSPECTV

C  First, the special "clear" channel
 
        NG = L_NGAUSS
        DO L=1,L_LAYERS
          K              = 2*L+1
          TAUREFL        = (TAUREF(K)+TAUREF(K+1))/QextREF
          DTAUV(L,nw,ng) = DTAUKV(K,NW,NG)+DTAUKV(K+1,NW,NG)
          COSBV(L,NW,NG) = (GV(NW)*Qscatv(NW)*TAUREFL)/
     *                     (TRAY(K,NW)+TRAY(K+1,NW) + 
     *                     QSCATV(NW)*TAUREFL)
          WBARV(L,nw,ng) = (QSCATV(NW)*TAUREFL + 
     *                     (TRAY(K,NW)+TRAY(K+1,NW))*0.9999)/
     *                      DTAUV(L,nw,ng)
        END DO

C  Special bottom layer

        L              = L_NLAYRAD
        K              = 2*L+1
        TAUREFL        = TAUREF(K)/QextREF
        DTAUV(L,nw,ng) = DTAUKV(K,NW,NG)
        COSBV(L,NW,NG) = (GV(NW)*Qscatv(NW)*TAUREFL)/
     *                   (TRAY(K,NW) + QSCATV(NW)*TAUREFL)
        WBARV(L,nw,ng) = (QSCATV(NW)*TAUREFL + TRAY(K,NW)*0.9999)/
     *                    DTAUV(L,nw,ng)

      END DO

C  . . .Now the other Gauss points, if needed.

       do NW=1,L_NSPECTV
        DO NG=L_NGAUSS-1,NGWV(NW),-1
          DO L=1,L_LAYERS
            K              = 2*L+1
            TAUREFL        = (TAUREF(K)+TAUREF(K+1))/QextREF
            DTAUV(L,nw,ng) = DTAUKV(K,NW,NG)+DTAUKV(K+1,NW,NG)
            COSBV(L,NW,NG) = COSBV(L,NW,L_NGAUSS)
            WBARV(L,nw,ng) = (QSCATV(NW)*TAUREFL + 
     *                       (TRAY(K,NW)+TRAY(K+1,NW))*0.9999)/
     *                       DTAUV(L,nw,ng)
          END DO

C  Special bottom layer

          L              = L_NLAYRAD
          K              = 2*L+1
          TAUREFL        = TAUREF(K)/QextREF
          DTAUV(L,nw,ng) = DTAUKV(K,NW,NG)
          COSBV(L,NW,NG) = (GV(NW)*Qscatv(NW)*TAUREFL)/
     *                     (TRAY(K,NW) + QSCATV(NW)*TAUREFL)
          WBARV(L,nw,ng) = (QSCATV(NW)*TAUREFL + TRAY(K,NW)*0.9999)/
     *                      DTAUV(L,nw,ng)
        END DO

      END DO     ! NW spectral loop

C     TOTAL EXTINCTION OPTICAL DEPTHS

      DO NW=1,L_NSPECTV
        NG = L_NGAUSS
        TAUV(1,NW,NG) = 0.0D0
        DO L=1,L_NLAYRAD
          TAUV(L+1,NW,NG) = TAUV(L,NW,NG)+DTAUV(L,NW,NG)
        END DO

        TAUCUMV(1,NW,NG)=0.0D0
        DO K=2,L_LEVELS
          TAUCUMV(K,NW,NG)=TAUCUMV(K-1,NW,NG)+DTAUKV(K,NW,NG)
        END DO
  
        DO NG=L_NGAUSS-1,NGWV(NW),-1
          TAUV(1,NW,NG)=0.0D0
          DO L=1,L_NLAYRAD
            TAUV(L+1,NW,NG)=TAUV(L,NW,NG)+DTAUV(L,NW,NG)
          END DO

          TAUCUMV(1,NW,NG)=0.0D0
          DO K=2,L_LEVELS
            TAUCUMV(K,NW,NG)=TAUCUMV(K-1,NW,NG)+DTAUKV(K,NW,NG)
          END DO
        END DO
      END DO

      RETURN
      END
