      subroutine qvdiff(ncnst   ,qm1     ,qflx    ,cc      ,ze      ,
     $                  term    ,qp1     )
C-----------------------------------------------------------------------
C
C Solve vertical diffusion eqtn for constituent with explicit srfc flux.
C Procedure for solution of the implicit equation follows Richtmyer and 
C Morton (1967,pp 198-199).
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
c $Id: qvdiff.F,v 1.1 2005/01/24 19:21:44 rca Exp $
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
      integer ncnst               ! number of constituents being diffused

      real qm1(plond,plev,ncnst)  ! initial constituent
      real qflx(plond,ncnst)      ! sfc q flux into lowest model level
      real cc(plond,plev)         ! -lower diag coeff.of tri-diag matrix
      real ze(plond,plev)         ! term in tri-diag. matrix system
      real term(plond,plev)       ! 1./(1. + ca(k) + cc(k) - cc(k)*ze(k-1))
C
C Output arguments
C
      real qp1(plond,plev,ncnst)  ! final constituent
C
C---------------------------Local workspace-----------------------------
C
      real zfq(plond,plev,pcnst)  ! terms appear in soln of tri-diag sys
      real tmp1d(plond)           ! temporary workspace (1d array)

      integer i,k                 ! longitude, vertical indices
      integer m                   ! constituent index
C
C-----------------------------------------------------------------------
C
C Calculate fq(k).  Terms fq(k) and e(k) are required in solution of 
C tridiagonal matrix defined by implicit diffusion eqn.
C Note that only levels ntopfl through plev need be solved for.
C No vertical diffusion is applied above this level
C
      do m=1,ncnst
        do i=1,plon
          zfq(i,ntopfl,m) = qm1(i,ntopfl,m)*term(i,ntopfl)
        end do
        do k=ntopfl+1,plev-1
          do i=1,plon
            zfq(i,k,m) = (qm1(i,k,m) + cc(i,k)*zfq(i,k-1,m))*term(i,k)
          end do
        end do
      end do
C
C Bottom level: (includes  surface fluxes)
C
      do i=1,plon
        tmp1d(i) = 1./(1. + cc(i,plev) - cc(i,plev)*ze(i,plev-1))
        ze(i,plev) = 0.
      end do
      do m=1,ncnst
        do i=1,plon
          zfq(i,plev,m) = (qm1(i,plev,m) + qflx(i,m) +
     $                     cc(i,plev)*zfq(i,plev-1,m))*tmp1d(i)
        end do
      end do
C
C Perform back substitution
C
      do m=1,ncnst
        do i=1,plon
          qp1(i,plev,m) = zfq(i,plev,m)
        end do
        do k=plev-1,ntopfl,-1
          do i=1,plon
            qp1(i,k,m) = zfq(i,k,m) + ze(i,k)*qp1(i,k+1,m)
          end do
        end do
      end do

      end
 
