      subroutine dustprofile(PSF,PTROP,CONRNU,TAUTOT,SIGMA,TAUCUM,
     *                       TAUREF)

C Bob's updates 9/17/99 
C Reference the dust optical depth to PSF (The surface pressure, mbar),
C and modify the way the dust mixing ratio is calculated to more accurately 
C reflect the pressure-optical depth relationship.
C GCM2.0  Sept 2002
C Driver:  Jan 2003 - Modified from GCM 3-D to DRIVER 1-D

      implicit none

      integer JSRCHGT
      external JSRCHGT
      include "grid.h"

      INTEGER NPDST
      PARAMETER (NPDST = 100)

      integer n, k, nstar
      REAL*8  QRDST(NPDST), PRDST(NPDST), TAUDST(NPDST)
      real*8  PSF, PTROP, CONRNU, SIGMA(L_LEVELS), TAUTOT
      real*8  TAUCUM(L_LEVELS), TAUREF(L_LEVELS+1)
      real*8  refpr, pave, sum, qrdst0, pstar, pstar1, pdif1, pdif2

C======================================================================C

C Calculate the Reference Pressure Grid (prdst)

      refpr    = (5.0*psf/ptrop)**(1.0/(float(npdst)-1.0))
      prdst(1) = ptrop
   
      do n=2,npdst
        prdst(n) = refpr*prdst(n-1)
      end do

C Calculate the Mixing Ratio at the Reference Pressure Level

      sum = 0.
      do n = 2,npdst
        if (prdst(n).lt.psf) then
          pave = 0.5*(prdst(n)+prdst(n-1))
          sum  = sum + exp(conrnu*(1.-(psf/pave)))*
     &              (prdst(n)-prdst(n-1))
        end if
        if (prdst(n).ge.psf) go to 10 
      end do

10    continue

      pave = 0.5*(psf+prdst(n-1))
      sum  = sum + exp(conrnu*(1.-(psf/pave)))*(psf-prdst(n-1))

C  GCM1.7  6/28/01   spatially varying dust

      qrdst0 = tautot/sum

C Now calculate the mixing ratio at all other levels

      do n=1,npdst-1

C Region 1: Mixing ratio changes continuously through the layer

        if (psf.gt.prdst(n+1)) then
          pave     = 0.5*(prdst(n+1)+prdst(n))
          qrdst(n) = qrdst0*exp(conrnu*(1.0-(psf/pave)))
        end if

C Region 2: Reference pressure level within this layer. 

        if (psf.le.prdst(n+1).and.psf.ge.prdst(n)) then
          pave     = 0.5*(prdst(n)+psf)
          pdif1    = psf-prdst(n)
          pdif2    = prdst(n+1)-psf
          qrdst(n) = qrdst0*(
     &               exp(conrnu*(1.0-(psf/pave)))*pdif1+pdif2) / 
     &               (prdst(n+1)-prdst(n))
        end if

C Region 3: Mixing ratio constant

        if (psf.lt.prdst(n)) then
          qrdst(n) = qrdst0
        end if

      end do

C Now compute the optical depths (taudst).

      taudst(1) = 0.0

      do n=2,npdst
        taudst(n) = taudst(n-1) + qrdst(n-1)*(prdst(n)-prdst(n-1))
      end do

C  Dust optical depth at the bottom of each sub-layer.

      DO N=1,3
        TAUCUM(N) = 0.0
        TAUREF(N) = 0.0
      END DO

      DO K=4,L_LEVELS
        PSTAR     = (PSF-PTROP)*SIGMA(K)+PTROP
        PSTAR1    = MAX(PSTAR,PRDST(1))
        NSTAR     = MIN0(JSRCHGT(NPDST-1,PRDST,1,PSTAR1)-1,NPDST-1)
        TAUCUM(K) = TAUDST(NSTAR)+(PSTAR1-PRDST(NSTAR))*
     *              (TAUDST(NSTAR+1) - TAUDST(NSTAR))/
     *              (PRDST(NSTAR+1)-PRDST(NSTAR))
        TAUREF(K) = TAUCUM(K) - TAUCUM(K-1)
      END DO

      TAUREF(L_LEVELS+1) = 0.0D0

      RETURN
      END
