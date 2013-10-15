      SUBROUTINE OPTCI(DTAUI,TAUCUMI,CO2I,TLEV,PLEV,PFGASREF,TGASREF,
     *                 Cmk,QREFV,QEXTI,QSCATI,GI,COSBI,WBARI,TAUREF,
     *                 TMID,PMID,NGWI,QH2O,WREFH2O)

C  GCM2.0  Feb 2003
C
C THIS SUBROUTINE SETS THE OPTICAL CONSTANTS IN THE INFRARED
C IT CALCUALTES FOR EACH LAYER, FOR EACH SPECRAL INTERVAL IN THE IR
C LAYER: WBAR, DTAU, COSBAR
C LEVEL: TAU
C
C Qrefv is the extinction coefficient at the reference (visible) 
C wavelength - 0.67 microns.
C
C TAUI(L,LW) is the cumulative optical depth at level L (or alternatively
C at the *bottom* of layer L), LW is the spectral wavelength interval.
C
C     TLEV(L) - Temperature at the layer boundary (i.e. level)
C     PLEV(L) - Pressure at the layer boundary (i.e. level)
C     CO2_KI(NT,NP,NW,NG) - IR CO2 k-coefficients 
C                           CO2_K(temp,Pres,Waveln,gauss)
C                           currently: CO2_K(7,11,5,17)
C
C----------------------------------------------------------------------C

      implicit none
      include "grid.h"
      include "radinc.h"

      real*8 DTAUI(L_NLAYRAD,L_NSPECTI,L_NGAUSS)
      real*8 DTAUKI(L_LEVELS+1,L_NSPECTI,L_NGAUSS)
      real*8 TAUI(L_NLEVRAD,L_NSPECTI,L_NGAUSS)
      real*8 TAUCUMI(L_LEVELS,L_NSPECTI,L_NGAUSS)
      real*8 TAUGAS, Qrefv
      real*8 TLEV(L_LEVELS), PLEV(L_LEVELS+1)
      real*8 TMID(L_LEVELS), PMID(L_LEVELS)
      real*8 CO2I(L_NTREF,L_PINT,L_REFH2O,L_NSPECTI,L_NGAUSS)
      real*8 TGASREF(L_NTREF)
      real*8 PFGASREF(L_PINT)
      real*8 QEXTI(L_NSPECTI), QSCATI(L_NSPECTI)
      real*8 COSBI(L_NLAYRAD,L_NSPECTI,L_NGAUSS)
      real*8 WBARI(L_NLAYRAD,L_NSPECTI,L_NGAUSS)
      real*8 TAUREF(L_LEVELS+1)
      real*8 GI(L_NSPECTI)

      integer L, NW, NG, K, LK
      integer MT(L_LEVELS), MP(L_LEVELS), NP(L_LEVELS)
      real*8  Cmk, ANS, TAUREFL
      real*8  TAEROS(L_LEVELS,L_NSPECTI)
      real*8  DPR(L_LEVELS), U(L_LEVELS), TAUCLD, TAUAC
      real*8  LCOEF(4), LKCOEF(L_LEVELS,4)

! fraction of zeros in each spectral interval, as a function of T, P

      real*8 dt, tt
      real*8 taugsurf(L_NSPECTI,L_NGAUSS-1)

C  Tlimit:  If the CO2 optical depth (top to the surface) is less than
C  this value, we place that Gauss-point into the "zeros" channel.
C  Set in driver, passed via common

      real*8 tlimit
      common /tlim1 / tlimit

      integer NGWI(L_NSPECTI)

C  Water mixing ratio variables

      real*8  QH2O(L_LEVELS), WREFH2O(L_REFH2O), WRATIO(L_LEVELS)
      real*8  KCOEF(4)
      integer NH2O(L_LEVELS)

C======================================================================C

C  Determine the total gas opacity throughout the column, for each
C  spectral interval, NW, and each Gauss point, NG.

      DO NG=1,L_NGAUSS-1
        do NW=1,L_NSPECTI
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

        DO NW=1,L_NSPECTI
          TAEROS(K,NW) = TAUREF(K)*Qexti(NW)/Qrefv
        END DO
      end do

C  TAUCLD = is cloud opacity, zero until further notice

      TAUCLD = 0.0

      do NW=1,L_NSPECTI
        ngwi(NW) = L_NGAUSS

C  Now fill in the "clear" part of the spectrum (NG = L_NGAUSS)
C  Which holds continuum opacity only

        do K=2,L_LEVELS
          DTAUKI(K,NW,L_NGAUSS) = TAEROS(K,NW) + TAUCLD
        end do

        do ng=L_NGAUSS-1,1,-1
          do K=2,L_LEVELS

C           NOW COMPUTE TAUGAS

C  Interpolate between water mixing ratios
C  WRATIO = 0.0 if the requested water amount is equal to, or outside the
C  the range of water amount data.

            KCOEF(1) = CO2I(MT(K),MP(K),NH2O(K),NW,NG) + WRATIO(K)*
     *                (CO2I(MT(K),MP(K),NH2O(K)+1,NW,NG) -
     *                 CO2I(MT(K),MP(K),NH2O(K),NW,NG))

            KCOEF(2) = CO2I(MT(K),MP(K)+1,NH2O(K),NW,NG) + WRATIO(K)*
     *                (CO2I(MT(K),MP(K)+1,NH2O(K)+1,NW,NG) -
     *                 CO2I(MT(K),MP(K)+1,NH2O(K),NW,NG))

            KCOEF(3) = CO2I(MT(K)+1,MP(K)+1,NH2O(K),NW,NG) + WRATIO(K)*
     *                (CO2I(MT(K)+1,MP(K)+1,NH2O(K)+1,NW,NG) -
     *                 CO2I(MT(K)+1,MP(K)+1,NH2O(K),NW,NG))

            KCOEF(4) = CO2I(MT(K)+1,MP(K),NH2O(K),NW,NG) + WRATIO(K)*
     *                (CO2I(MT(K)+1,MP(K),NH2O(K)+1,NW,NG) -
     *                 CO2I(MT(K)+1,MP(K),NH2O(K),NW,NG))

C  Interpolate the CO2 k-coefficients to the requested T,P


            ANS = LKCOEF(K,1)*KCOEF(1) + LKCOEF(K,2)*KCOEF(2) +
     *            LKCOEF(K,3)*KCOEF(3) + LKCOEF(K,4)*KCOEF(4)


            TAUGAS          = U(k)*ANS
            TAUGSURF(NW,NG) = TAUGSURF(NW,NG) + TAUGAS
            DTAUKI(K,nw,ng) = TAUGAS+TAEROS(K,NW)+TAUCLD
          end do

          if(TAUGSURF(NW,NG) .LT. TLIMIT) THEN
            goto 10
          else
            NGWI(NW) = NG
          end if

        end do
   10   continue

      end do

C  Now the full treatment for the layers, where besides the opacity
C  we need to calculate the scattering albedo and asymmetry factors
C  for each layer

      DO NW=1,L_NSPECTI

C  First, the special "clear" channel

        NG = L_NGAUSS

        DO L=1,L_NLAYRAD
          K              = 2*L+1
          TAUREFL        = (TAUREF(K)+TAUREF(K+1))/Qrefv
          DTAUI(L,nw,ng) = DTAUKI(K,NW,NG)+DTAUKI(K+1,NW,NG)
          if(DTAUI(L,NW,NG) .GT. 1.0E-9) then
            WBARI(L,nw,ng) = (QSCATI(NW)*TAUREFL)/DTAUI(L,NW,NG)
          else
            WBARI(L,nw,ng) = 0.0D0
            DTAUI(L,NW,NG) = 1.0E-9
          endif

          TAUAC = TAEROS(K,NW) + TAUCLD
          if(TAUAC .GT. 0.0) then
            cosbi(L,NW,NG) = GI(NW)           !change formula to add clouds
          else
            cosbi(L,NW,NG) = 0.0D0
          end if

        END DO

      END DO

C  . . .Now the other Gauss points, if needed.

      DO NW=1,L_NSPECTI
        DO NG=L_NGAUSS-1,NGWI(NW),-1

          DO L=1,L_NLAYRAD
            K              = 2*L+1
            TAUREFL        = (TAUREF(K)+TAUREF(K+1))/Qrefv
            DTAUI(L,nw,ng) = DTAUKI(K,NW,NG)+DTAUKI(K+1,NW,NG)
            if(DTAUI(L,NW,NG) .GT. 1.0E-9) then
              WBARI(L,nw,ng) = (QSCATI(NW)*TAUREFL)/DTAUI(L,NW,NG)
            else
              WBARI(L,nw,ng) = 0.0D0
              DTAUI(L,NW,NG) = 1.0E-9
            endif

            cosbi(L,NW,NG) = cosbi(L,NW,L_NGAUSS)
          END DO

        END DO

      END DO     ! NW spectral loop

C     TOTAL EXTINCTION OPTICAL DEPTHS

      DO NW=1,L_NSPECTI
        NG = L_NGAUSS
        TAUI(1,NW,NG) = 0.0D0
        DO L=1,L_NLAYRAD
          TAUI(L+1,NW,NG) = TAUI(L,NW,NG)+DTAUI(L,NW,NG)
        END DO

        TAUCUMI(1,NW,NG)=0.0D0
        DO K=2,L_LEVELS
          TAUCUMI(K,NW,NG)=TAUCUMI(K-1,NW,NG)+DTAUKI(K,NW,NG)
        END DO

        DO NG=L_NGAUSS-1,NGWI(NW),-1

            TAUI(1,NW,NG)=0.0D0
            DO L=1,L_NLAYRAD
              TAUI(L+1,NW,NG)=TAUI(L,NW,NG)+DTAUI(L,NW,NG)
            END DO

            TAUCUMI(1,NW,NG)=0.0D0
            DO K=2,L_LEVELS
              TAUCUMI(K,NW,NG)=TAUCUMI(K-1,NW,NG)+DTAUKI(K,NW,NG)
            END DO

        END DO
      END DO

      RETURN
      END
