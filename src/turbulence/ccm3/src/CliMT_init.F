      subroutine init( cpairx, gravitx, latvapx, rairx, rh2ox, cpwvx)

C-----------------------------------------------------------------------
C
C Initialize constants in vert diff, PBL and ocean surface flux schemes
C
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <params.h>
#include <pmgrid.h>
C------------------------------Commons----------------------------------
#include <comcon.h>
C-----------------------------------------------------------------------
#include <comqmin.h>
C-----------------------------------------------------------------------
C Input
      real cpairx,gravitx,latvapx,rairx,rh2ox,cpwvx
C-----------------------------------------------------------------------
C Local
      integer m
C
C Initialize COMCON (master set of physical constants)
C
C Original values from CCM3.6
      Cpair  = 1004.64
      gravit = 9.80616
      latvap = 2.5104e06
      latice = 3.336e5
      omega  = 7.292e-5
      rair   = 287.04
      rearth = 6.37122e+6
      rh2o   = 4.61e2
      rhoh2o = 1.e3
      stebol = 5.67e-8
      tmelt  = 273.16
      cpwv   = 1810.

C Externally specified values
      cpair  = cpairx
      gravit = gravitx
      latvap = latvapx
      rair   = rairx
      rh2o   = rh2ox
      cpwv   = cpwvx
C
C Derived quantities
      epsilo = rair/rh2o  ! (was specified as 0.622 in ccm3.6) 
      cappa  = rair/cpair
      ez     = omega/sqrt(0.375)
      ra     = 1./rearth
      rga    = 1./gravit
      zvir   = rh2o/rair -1.
      cpvir  = cpwv/cpair - 1.

C
C Initialise  COMQMIN
C Set the minimum mixing ratio for the water vapor and 
C counter-gradient term.  
C
      qmin(1) = 1.e-12 
c      do m=2,pcnst
c         qmin(m) = 0.0
c      end do

      qmincg(1) = 0.
c      do m=2,pcnst
c         qmincg(m) = qmin(m)
c      end do

c Initialise vert diff and PBL
      call vdinti (cpwv    ,cpair   ,gravit  ,rair    )

c Initialise ocean surface flux
      call tsinti (tmelt   ,latvap  ,rair    ,stebol  )

      end
