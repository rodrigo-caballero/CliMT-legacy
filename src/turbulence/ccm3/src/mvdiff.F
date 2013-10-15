      subroutine mvdiff(um1     ,vm1     ,uflx    ,vflx    ,
     $                  sufac   ,svfac   ,cc      ,
     $                  ze      ,term    ,up1     ,vp1     )
C-----------------------------------------------------------------------
C
C Vertical momentum diffusion with explicit surface flux.
C Solve the vertical diffusion equation for momentum.
C Procedure for solution of the implicit equation follows
C Richtmyer and Morton (1967,pp 198-199)
C
C---------------------------Code history--------------------------------
C
C Original version:  CCM1
C Standardized:      J. Rosinski, June 1992
C Reviewed:          P. Rasch, B. Boville, August 1992
C Reviewed:          P. Rasch, April 1996
C Reviewed:          B. Boville, May 1996
C
C-----------------------------------------------------------------------
c
c $Id: mvdiff.F,v 1.1 2005/01/24 19:21:44 rca Exp $
c $Author: rca $
c
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <params.h>
#include <pmgrid.h>
C------------------------------Commons----------------------------------
#include <comvd.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real um1(plond,plev)   ! u wind input
      real vm1(plond,plev)   ! v wind input
      real uflx(plond)       ! sfc u flux into lowest model level
      real vflx(plond)       ! sfc v flux into lowest model level
      real sufac(plond)      ! lowest layer u implicit stress factor
      real svfac(plond)      ! lowest layer v implicit stress factor
      real cc(plond,plev)    ! -lower diag coeff. of tri-diag matrix
      real ze(plond,plev)    ! term in tri-diag. matrix system
      real term(plond,plev)  ! 1./(1. + ca(k) + cc(k) - cc(k)*ze(k-1))
C
C Output arguments
C
      real up1(plond,plev)   ! u wind after diffusion
      real vp1(plond,plev)   ! v wind after diffusion
C
C---------------------------Local workspace-----------------------------
C
      real zfu(plond,plev)   ! terms appearing in soln of tridiag system
      real zfv(plond,plev)   ! terms appearing in soln of tridiag system
      integer i,k            ! longitude,vertical indices
C
C-----------------------------------------------------------------------
C
C Calc fu(k) and fv(k). These are terms required in solution of 
C tridiagonal matrix defined by implicit diffusion eqn.  Note that only 
C levels ntopfl through plev need be solved for. No diffusion is 
C applied above this level.
C
      do i=1,plon
        zfu(i,ntopfl) = um1(i,ntopfl)*term(i,ntopfl)
        zfv(i,ntopfl) = vm1(i,ntopfl)*term(i,ntopfl)
      end do
      do k=ntopfl+1,plev-1
        do i=1,plon
          zfu(i,k) = (um1(i,k) + cc(i,k)*zfu(i,k-1))*term(i,k)
          zfv(i,k) = (vm1(i,k) + cc(i,k)*zfv(i,k-1))*term(i,k)
        end do
      end do
C
C Bottom level: (includes  surface fluxes as either an explicit RHS or
C as an implicit stress)
C
      do i=1,plon
        zfu(i,plev) = (um1(i,plev) + uflx(i) + cc(i,plev)*zfu(i,plev-1))
     $       / (1. + cc(i,plev) + sufac(i) - cc(i,plev)*ze(i,plev-1))
        zfv(i,plev) = (vm1(i,plev) + vflx(i) + cc(i,plev)*zfv(i,plev-1))
     $       / (1. + cc(i,plev) + svfac(i) - cc(i,plev)*ze(i,plev-1))
      end do
C
C Perform back substitution
C
      do i=1,plon
        up1(i,plev) = zfu(i,plev)
        vp1(i,plev) = zfv(i,plev)
      end do
      do k=plev-1,ntopfl,-1
        do i=1,plon
          up1(i,k) = zfu(i,k) + ze(i,k)*up1(i,k+1)
          vp1(i,k) = zfv(i,k) + ze(i,k)*vp1(i,k+1)
        end do
      end do

      end
 
