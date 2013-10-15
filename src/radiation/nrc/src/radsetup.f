      subroutine radsetup

C  GCM2.0  Feb 2003
C
C     PURPOSE:
C        Bundle the new radiation code setup subroutines and call
C     this one subroutine from main, where the three included files
C     are also listed.  Quantities are passed between this driver
C     and the radiation code via common (in radcommon.h).
C
C----------------------------------------------------------------------C

      implicit none

      include "grid.h"
      include "radinc.h"
      include "radcommon.h"

!  These are for the Gauss-split 0.95 case

      DATA GWEIGHT  / 4.8083554740D-02, 1.0563099137D-01,
     *                1.4901065679D-01, 1.7227479710D-01,
     *                1.7227479710D-01, 1.4901065679D-01,
     *                1.0563099137D-01, 4.8083554740D-02,
     *                2.5307134073D-03, 5.5595258613D-03,
     *                7.8426661469D-03, 9.0670945845D-03,
     *                9.0670945845D-03, 7.8426661469D-03,
     *                5.5595258613D-03, 2.5307134073D-03,  0.0D0 /

      DATA UBARI    / 0.5  /

C  These are for the CO2+H2O k-coefficients

      DATA WREFCO2 / 9.999999D-1, 9.99999D-1, 9.9999D-1, 9.999D-1,
     *               9.99D-1, 9.9D-1, 9.0D-1, 8.0D-1, 7.0D-1, 6.0D-1 /

      DATA WREFH2O / 1.0D-7, 1.0D-6, 1.0D-5, 1.0D-4, 1.0D-3, 1.0D-2,
     *               1.0D-1, 2.0D-1, 3.0D-1, 4.0D-1                  /

      REAL*8 FACTOR
      INTEGER NW

C======================================================================C

      call setspv(WNOV,DWNV,WAVEV,SOLARF,TAURAY)
      call setspi(WNOI,DWNI,WAVEI)
      call setrad(TGASREF,PFGASREF,CO2V,CO2I,QEXTV,QSCATV,WV,GV,
     *                  QEXTI,QSCATI,WI,GI,FZEROI,FZEROV)

C  Scale IR opacities (Qexti and Qscati) such that 
C  TAU(0.67 micron)/TAU(9 micron) = VIS2IR, which nominally is 2.

      QextREF = Qextv(L_NREFV)
      VIS2IR  = 2.75D0

      factor  = Qextv(6)/(VIS2IR*Qexti(4))

      DO NW=1,L_NSPECTI
        Qexti(NW)  = Qexti(NW)*factor
        Qscati(NW) = Qscati(NW)*factor
      END DO

      PTOP = 10.0**PFGASREF(1)
      
      return
      end
