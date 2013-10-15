#include <misc.h>
#include <params.h>
      subroutine mfinti(rair    ,cpair   ,gravit  ,latvap  ,rhowtr  )
C-----------------------------------------------------------------------
C
C Initialize moist convective mass flux procedure common block, cmfmca 
C 
C----------------------------Code History-------------------------------
C
C Original version:  J. Hack
C Standardized:      J. Rosinski, June 1992
C Reviewed:          J. Hack, G. Taylor, August 1992
C
C-----------------------------------------------------------------------
c
c $Id: mfinti.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c
#include <implicit.h>
C-----------------------------------------------------------------------
#include <pmgrid.h>
C------------------------------Commons----------------------------------
#include <comcmf.h>
C-----------------------------------------------------------------------
c+++rca
c#include <comhyb.h>
c---rca
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real rair              ! gas constant for dry air
      real cpair             ! specific heat of dry air
      real gravit            ! acceleration due to gravity
      real latvap            ! latent heat of vaporization
      real rhowtr            ! density of liquid water (STP)
      integer k              ! vertical level index
C
C-----------------------------------------------------------------------
C
C Initialize physical constants for moist convective mass flux procedure
C
      cp     = cpair         ! specific heat of dry air     
      hlat   = latvap        ! latent heat of vaporization  
      grav   = gravit        ! gravitational constant       
      rgas   = rair          ! gas constant for dry air
      rhoh2o = rhowtr        ! density of liquid water (STP)
C
C Initialize free parameters for moist convective mass flux procedure
C
      c0     = 1.0e-4        ! rain water autoconversion coeff (1/m)
      dzmin  = 0.0           ! minimum cloud depth to precipitate (m)
      betamn = 0.10          ! minimum overshoot parameter
      cmftau = 3600.         ! characteristic adjustment time scale
c
c Limit convection to regions below 40 mb
c
c+++rca
C-------       if (hypi(1) .ge. 4.e3) then
C-------         limcnv = 1
C-------       else
C-------         do k=1,plev
C-------           if (hypi(k).lt.4.e3 .and. hypi(k+1).ge.4.e3) then
C-------             limcnv = k
C-------             goto 10
C-------           end if
C-------         end do
C-------         limcnv = plevp
C-------       end if
C-------    10 if (masterproc) then
C-------         write(6,*)'MFINTI: Convection will be capped at intfc ',limcnv,
C-------      $            ' which is ',hypi(limcnv),' pascals'
C-------       end if
      limcnv=1
c---rca

      tpmax  = 1.50          ! maximum acceptable t perturbation (deg C)
      shpmax = 1.50e-3       ! maximum acceptable q perturbation (g/g)
      rlxclm = .true.        ! logical variable to specify that relaxation
c                                time scale should applied to column as 
c                                opposed to triplets individually 
C
C Initialize miscellaneous (frequently used) constants
C
      rhlat  = 1.0/hlat      ! reciprocal latent heat of vaporization  
      rcp    = 1.0/cp        ! reciprocal specific heat of dry air     
      rgrav  = 1.0/grav      ! reciprocal gravitational constant       
C
C Initialize diagnostic location information for moist convection scheme
C
      iloc   = 1             ! longitude point for diagnostic info
      jloc   = 1             ! latitude  point for diagnostic info
      nsloc  = 1             ! nstep value at which to begin diagnostics
C
C Initialize other miscellaneous parameters
C
      tiny   = 1.0e-36       ! arbitrary small number (scalar transport)
      eps    = 1.0e-13       ! convergence criteria (machine dependent) 
C
      return
      end



 
