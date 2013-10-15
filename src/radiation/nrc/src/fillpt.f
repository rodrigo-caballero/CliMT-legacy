      subroutine fillpt(ptop,pl,p,ptrop,tg,tstrat,tl,plev,tlev,pmid,
     *                  tmid)
C     GCM 2.0  VERSION 1.1  JAN 2003
C  Put the T & P GCM arrays onto the NRC grid:  PLEV, PMID, TLEV, TMID
C  Sept 2002
C
C  PMID and TMID are the pressure and temperature at the GCM layer 
C  mid-points.  PLEV and TLEV are the pressures and temperatures at
C  the GCM layer boundaries, i.e. at GCM levels.

      implicit none

      include "grid.h"
     
      integer K, L
      real*8 PTOP, PL(L_LEVELS), PLEV(L_LEVELS+1), PMID(L_LEVELS)
      real*8 TSTRAT, TL(L_LEVELS), TLEV(L_LEVELS+1), TMID(L_LEVELS)
      real*8 P, PTROP, TG

C======================================================================C

C  Fill the new radiation code variables.
C  PLEV and TLEV are the pressure and tempertures on a vertical grid
C  that the new radiation code uses.

      PLEV(1) = PTOP*1.0E-5
      PLEV(2) = PTOP
      DO K=3,L_LEVELS
        PLEV(K) = PL(K)
      END DO
      PLEV(L_LEVELS+1) = PLEV(L_LEVELS)

      DO K=1,3
        TLEV(K) = TSTRAT
      END DO

      DO K=4,L_LEVELS-1,2
        TLEV(K) = TL(K)
      END DO

      DO K=5,L_LEVELS-2,2
        TLEV(K) = TLEV(K+1) + (TLEV(K-1)-TLEV(K+1))*
     *            DLOG(PLEV(K)/PLEV(K+1))/
     *            DLOG(PLEV(K-1)/PLEV(K+1))
      END DO

C  Temperature of the bottom level is the ground temperature.

      IF(TG.GT.0.0) then
        TLEV(L_LEVELS) = TG
      ELSE
        TLEV(L_LEVELS) =  3182.48/(23.3494-LOG(P+PTROP))
      ENDIF

C  Fill the PMID & TMID arrays used by OPTCI and OPTCV subroutines.
C  TMID and PMID used to get the index for CO2 k-coefficient interpolation.

      TMID(1) = TLEV(2)
      TMID(2) = TLEV(2)
      PMID(1) = PLEV(1)
      PMID(2) = PLEV(2)

      DO L=1,L_LAYERS
        TMID(2*L+1) = TLEV(2*L+1)
        TMID(2*L+2) = TLEV(2*L+1)
        PMID(2*L+1) = PLEV(2*L+1)
        PMID(2*L+2) = PLEV(2*L+1)
      END DO

      TMID(L_LEVELS) = TLEV(L_LEVELS)
      PMID(L_LEVELS) = PLEV(L_LEVELS)

      RETURN
      END
