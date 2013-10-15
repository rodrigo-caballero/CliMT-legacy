#include <misc.h>
#include <params.h>
      subroutine aermix(pint    ,aermmr  ,rh      )
C-----------------------------------------------------------------------
C Set global mean tropospheric aerosol
C
C Specify aerosol mixing ratio and compute relative humidity for later
C adjustment of aerosol optical properties. Aerosol mass mixing ratio 
C is specified so that the column visible aerosol optical depth is a 
C specified global number (tauvis). This means that the actual mixing
C ratio depends on pressure thickness of the lowest three atmospheric 
C layers near the surface.
C
C Optical properties and relative humidity parameterization are from:
C 
C J.T. Kiehl and B.P. Briegleb  "The Relative Roles of Sulfate Aerosols
C and Greenhouse Gases in Climate Forcing"  Science  260  pp311-314
C 16 April 1993
C
C Visible (vis) here means 0.5-0.7 micro-meters
C Forward scattering fraction is taken as asymmetry parameter squared
C
C---------------------------Code history--------------------------------
C
C Original version:  B. Briegleb, Mar 1995
C Standarized:       L. Buja,     Feb 1996
C Reviewed:          B. Briegleb, Mar 1996
C
C-----------------------------------------------------------------------
c
c $Id: aermix.F,v 1.2 2004/09/07 02:47:44 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <pmgrid.h>
C-----------------------------------------------------------------------
#include <ptrrgrid.h>
C-----------------------------------------------------------------------
#include <pagrid.h>
C------------------------------Commons----------------------------------
#include <crdcon.h>
C-----------------------------------------------------------------------
#include <comsol.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real pint(plond,plevrp)   ! Rad level interface press. (dynes/cm2)
C                                    
C Output arguments                   
C                                    
      real aermmr(plond,plevr)  ! Rad level aerosol mass mixing ratio
      real rh(plond,plevr)      ! Rad level relative humidity (fraction)
C
C---------------------------Local variables-----------------------------
C
      integer i      ! Longitude index
      integer k      ! Level index
      integer mxaerl ! Max nmbr aerosol levels counting up from surface
C
      real kaervs    ! Visible extinction coefficiant of aerosol (m2/g)
      real omgvis    ! Visible single scattering albedo
      real gvis      ! Visible scattering asymmetry parameter
      real rhcnst    ! Constant relative humidity factor
C
C Relative humidity factor
C
      real rhfac     ! Multiplication factor for kaer
      real rhpc      ! Level relative humidity in %
      real a0        ! Constant in relative humidity mult factor
      real a1        ! Constant in relative humidity mult factor
      real a2        ! Constant in relative humidity mult factor
      real a3        ! Constant in relative humidity mult factor
C
C--------------------------Data Statements------------------------------
C
      data a0 / -9.2906106183    /
      data a1 /  0.52570211505   /
      data a2 / -0.0089285760691 /
      data a3 /  5.0877212432e-05/
C
      data mxaerl / 3        /
      data kaervs / 5.3012   /
      data omgvis / 0.999999 /
      data gvis   / 0.694889 /
      data rhcnst /  .80     /
C
C-----------------------------------------------------------------------
C
C Set relative humidity and factor; then aerosol amount.
C
      do i=1,plon
        do k=1,plevr
C
          rh(i,k) = rhcnst
C
C Compute relative humidity factor for the extinction coefficiant; this
C factor accounts for the dependence of size distribution on relative
C humidity:
C
          if( rh(i,k) .gt. .90 ) then
            rhfac = 2.8
          else if (rh(i,k) .lt. .60 ) then
            rhfac = 1.0
          else
            rhpc  = 100. * rh(i,k)
            rhfac = (a0 + a1*rhpc + a2*rhpc**2 + a3*rhpc**3)
          endif
C
C Compute aerosol mass mixing ratio for specified levels (1.e4 factor is
C for units conversion of the extinction coefficiant from m2/g to cm2/g)
C
          if( k .ge. plevrp-mxaerl ) then
            aermmr(i,k) = gravit*tauvis /
     $                   ( 1.e4*kaervs*rhfac*(1.-omgvis*gvis*gvis) *
     $                     (pint(i,plevrp)-pint(i,plevrp-mxaerl))    )
          else
            aermmr(i,k) = 0.0
          endif
C
        enddo
      enddo        
C
      return
      end
 
