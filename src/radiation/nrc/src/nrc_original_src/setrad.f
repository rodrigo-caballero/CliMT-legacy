      subroutine setrad(TGASREF,PFGASREF,CO2V,CO2I,QEXTV,QSCATV,WV,GV,
     *                  QEXTI,QSCATI,WI,GI,FZEROI,FZEROV)

C  GCM2.0  Feb 2003
C
C     PURPOSE:
C        Set up values used by the radiation code, such as the CO2 gas
C     absorption coefficients.  True constants are defined, and the 
C     time-independent quantities used by the radiation code are 
C     calculated. 
C
C     AUTHOR
C        
C
C     UPDATES FOR
C        Jim Pollack
C
C     ENVIRONMENT
C        NAS Cray-YMP      UNICOS       FORTRAN
C
C     REVISION HISTORY
C        See notes from J. Pollack dated 2/20/90 and 3/12/90.
C        Modified Cmk  See note from Jim Pollack dated 1/11/91.
C        Several - See note dated 1/23/91 from Jim Pollack.
C     VERSION 2.0  OCT 2001
C
C     INPUT PARAMETERS
C     DTAU(L,M)      - Dust optical depth of layer L, and for aerosol 
C                      species M.
C     ptrop          - Pressure of the tropopause (mb)
C     SCALEP         - Factor to convert pressures from millibars to
C                      Pascals
C
C     OUTPUT PARAMETERS
C     PTOP       - Pressure at the TOP of the stratosphere ( or
C                  equivalently, at the bottom of the dummy layer.
C                  PTOP = PTROP*SDUMMY
C
C     AEROSOL RADIATIVE OPTICAL CONSTANTS
C     Values are at the wavelenght interval center
C
C     MIE SCATTERING - Size distribution weighted
C     Qextv    - Extinction efficiency - in the visual.
C     Qscatv   - Scattering efficiency - in the visual.
C     WV       - Single scattering albedo - in the visual.
C     GV       - Asymmetry parameter - in the visual.
C
C     Qexti    - Extinction efficiency - in the infrared.
C     Qscati   - Scattering efficiency - in the infrared.
C     WI       - Single scattering albedo - in the infrared.
C     GI       - Asymmetry parameter - in the infrared.
C     
C     CALLED BY
C        RAD
C
C     SUBROUTINES CALLED
C        DMIESS, PLNK
C
C----------------------------------------------------------------------C

      implicit none

      include "grid.h"
      include "radinc.h"

      integer N, NS

      real*8 CO2I(L_NTREF,L_PINT,L_REFH2O,L_NSPECTI,L_NGAUSS)
      real*8 CO2V(L_NTREF,L_PINT,L_REFH2O,L_NSPECTV,L_NGAUSS)
      real*8 PFGASREF(L_PINT)
      real*8 PGASREF(L_NPREF), TGASREF(L_NTREF)

      real*8 qextv(L_NSPECTV), qev1(L_NSPECTV)
      real*8 qscatv(L_NSPECTV), qsv1(L_NSPECTV)
      real*8 wv(L_NSPECTV)
      real*8 gv(L_NSPECTV), gv1(L_NSPECTV)

      real*8 qexti(L_NSPECTI), qei1(L_NSPECTI)
      real*8 qscati(L_NSPECTI), qsi1(L_NSPECTI)
      real*8 wi(L_NSPECTI)
      real*8 gi(L_NSPECTI), gi1(L_NSPECTI)

c     real*8 kvis, kir
      integer nt, np, nw, ng

      real*8 fzeroi(L_NSPECTI)
      real*8 fzerov(L_NSPECTV)

C----------------------------------------------------------------------C

C  Visible dust properties:  Ockert-Bell Planck-weighted values (T=6000K)

C     Qext - Ockert-Bell values (order is increasing waveNUMBER)
C     VISULAL WAVELENGTHS.

      data qev1 / 2.529D0, 2.949D0, 3.209D0, 3.337D0, 3.207D0,
     *            2.938D0, 2.622D0                                   /

C     Qscat - Ockert-Bell values
C     VISUAL wavelengths

      data qsv1 / 2.374D0, 2.637D0, 3.049D0, 3.201D0, 3.045D0, 
     *            2.513D0, 1.623D0                                   /

C     G - Ockert-Bell values
C     VISUAL wavelengths

      data gv1  / 0.635D0, 0.646D0, 0.630D0, 0.630D0, 0.634D0, 
     *            0.700D0, 0.856D0                                   /


C     And now the INFRARED

C     Qext for a modified-gamma distribution, ALPHA=2, GAMMA=0.5,
C     Rm = 0.4 microns, using the Forget optical constants (Nr and Ni).
C     Planck-weighted values (T=215K)
C     INFRARED wavelengths.  (The order is increasing waveNUMBER.)

      data qei1 / 0.193D0, 0.867D0, 1.209D0, 2.173D0, 0.638D0  /

C     Qsca for a modified gamma-distribution, using the Forget
C     optical constants (Nr and Ni).       INFRARED wavelengths

      data qsi1 / 0.027D0, 0.319D0, 0.558D0, 1.136D0, 0.237D0  /

C     g for a modified gamma-distribution, using the Forget
C     optical constants (Nr and Ni).       INFRARED wavelengths

      data gi1  / 0.024D0, 0.127D0, 0.288D0, 0.423D0, 0.548D0  /

C=======================================================================

C     Set the reference pressure and temperature arrays.  These are
C     the pressures and temperatures at which we have k-coefficients.

      pgasref( 1) = 1.0E-6
      pgasref( 2) = 1.0E-5
      pgasref( 3) = 1.0E-4
      pgasref( 4) = 1.0E-3
      pgasref( 5) = 1.0E-2
      pgasref( 6) = 1.0E-1
      pgasref( 7) = 1.0
      pgasref( 8) = 1.0E+1
      pgasref( 9) = 1.0E+2
      pgasref(10) = 1.0E+3
      pgasref(11) = 1.0E+4

      tgasref(1)  =  50.0
      tgasref(2)  = 100.0
      tgasref(3)  = 150.0
      tgasref(4)  = 200.0
      tgasref(5)  = 250.0
      tgasref(6)  = 300.0
      tgasref(7)  = 350.0
 
C     Fill the (VISUAL) arrays Qextv, Qscatv, WV, GV

      DO N=1,L_NSPECTV
        Qextv(n)  = qev1(n)
        Qscatv(n) = qsv1(n)
        IF(Qscatv(n).GE.Qextv(n)) then
          Qscatv(n) = 0.99999*Qextv(n)
        END IF
        WV(n)     = Qscatv(n)/Qextv(n)
        GV(n)     = gv1(n)
      END DO

C     Fill the (INFRARED) arrays Qexti, Qscati, WI, GI

      DO N=1,L_NSPECTI
        Qexti(n)  = qei1(n)
        Qscati(n) = qsi1(n)
        IF(Qscati(n).GE.Qexti(n)) then
          Qscati(n) = 0.99999*Qexti(n)
        END IF
        WI(n)     = Qscati(n)/Qexti(n)
        GI(n)     = gi1(n)
      END DO

C     Get CO2 k coefficients, and interpolate them to the finer
C     pressure grid.

      call laginterp(PGASREF,PFGASREF,CO2I,CO2V,FZEROI,FZEROV)

      return
      end
