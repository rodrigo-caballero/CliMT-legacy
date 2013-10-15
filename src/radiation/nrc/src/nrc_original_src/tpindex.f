      subroutine tpindex(pw,tw,qh2o,pref,tref,wrefh2o,LCOEF,MT,MP,
     *                   NH2O,wratio)

C  GCM2.0  Feb 2003
C
C
C  PURPOSE
C    Get the TI, UI values for a 2-dimensional interpolation
C    based on the following (The interpolation is done in interpco2):
C    Interpolate the CO2 K-coefficients to the current P,T values.
C    The CO2 coefficients are given on a P,T grid:
C    P = {1E-6, 1E-5, 1E-4, 1E-3, 1E-2, 1E-1, 1, 1E+1, 1E+2, 1E+3, 1E+4},
C    T = {50, 100, 150, 200, 250, 300, 350}.
C
C    The interpolation is the usual interpolation in 2-dimensions given
C    in "Numerical Recipes", where the "X" are P, the "Y" are
C    T, and the F(X,Y) are the CO2 K-coefficients.
C
C     The interpolating box is designated as follows:
C
C           (PL,TU)                        (PRR,TU)
C
C                          (TW,PW)
C
C           
C           (PL,TL)                        (PRR,TL)
C
C     PL  - Pressure left
C     PRR - Pressure right
C     TL  - Temperature lower
C     TU  - Temperature upper
C     PW  - Pressure wanted
C     TW  - Temperature wanted
C
C
C  AUTHOR
C      JIM SCHAEFFER     STERLING SOFTWARE  TASK 404  MAR 1998
C
C  FOR
C      BOB HABERLE       PART OF GCM II/RADTRAN UPDATES
C
C  ENVIRONMENT
C      SUN ULTRA-2       SOLARIS 2.5.1   FORTRAN 77
C
C  REVISION HISTORY
C     3-98  JRS  ORIGINAL
C
C  INPUT PARAMETERS
C    PW                 - The pressure to interpolate to
C    TW                 - The temperature to interpolate to
C    Pref(NP)           - The pressure grid array.
C    Tref(NT)           - The temperature grid array.
C    
C  OUTPUT PARAMETERS
C    TI                 - Interpolation term (pressure)
C    UI                 - Interpolation term (temperature)
C    MT                 - Temperature index (bottom left Temperature)
C                         of bounding box
C    MP                 - Pressure index (bottom left pressure)
C                         of bounding box
C
C  CALLED BY
C    SETRAD 
C
C  SUBROUTINES CALLED
C      NONE
C
C----------------------------------------------------------------------C

      implicit none

      include "grid.h"
      include 'radinc.h'

      real*8 Tref(L_NTREF)
      real*8 pref(L_PINT)
      real*8 wrefh2o(L_REFH2O)

      integer MT, MP, N, M, NP, NH2O
      real*8  PW, TW, Qh2o, wratio
      real*8  PWL, LCOEF(4), T, U

C======================================================================C
 
C     Get the upper and lower Temperature-grid indicies that bound the
C     requested temperature.  If the requested temperature is outside
C     the T-grid, set up to extrapolate from the appropriate end.

      IF(TW.LE.TREF(1)) THEN
        MT = 1
        U  = 0.0D0
      ELSE
        do n=1,L_NTREF-1
          if(tw.gt.Tref(n) .and. TW.LE.TREF(N+1)) then
            MT = n
            U = (TW-TREF(MT))/(TREF(MT+1)-TREF(MT))
            goto 10
          end if
        end do

        MT = L_NTREF-1
        U  = 1.0D0

   10   continue
      END IF


C     Get the upper and lower Pressure-grid indicies that bound the
C     requested pressure.  If the requested pressure is outside
C     the P-grid, set up to extrapolate from the appropiate end.

      pwl = log10(pw)

      if(pwl.le.Pref(1)) then
        MP = 1
        T  = 0.0D0
      else
        do n=2,L_PINT-1
          if(pwl.le.Pref(n)) then
            MP = n-1
            T = (PWL-PREF(MP))/(PREF(MP+1)-PREF(MP))
            goto 20
          end if
        end do

        MP = L_PINT-1
        T  = 1.0D0

   20   continue
      end if

C  Fill the interpolation coeficients:

      LCOEF(1) = (1.0-T)*(1.0-U)
      LCOEF(2) = T*(1.0-U)
      LCOEF(3) = T*U
      LCOEF(4) = (1.0-T)*U

C  Get the indicies for water abundance.  There are 10 sets of 
C  k-coefficients with differing amounts of water vs. CO2.

      IF(Qh2o.le.WREFH2O(1)) then
        NH2O   = 1
        WRATIO = 0.0D0
      ELSEIF(Qh2o.ge.WREFH2O(L_REFH2O)) then
        NH2O   = L_REFH2O - 1
        WRATIO = 1.0D0
      ELSE
        DO N=2,L_REFH2O
          IF(QH2O.GE.WREFH2O(N-1) .and. QH2O.lt.WREFH2O(N)) then
            NH2O   = N-1
            WRATIO = (QH2O - WREFH2O(N-1))/(WREFH2O(N) - WREFH2O(N-1))
            GOTO 30
          END IF
        END DO
      END IF

   30 CONTINUE
  
      return
      end
