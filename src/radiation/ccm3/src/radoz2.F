#include <misc.h>
#include <params.h>
      subroutine radoz2(o3vmr   ,pint    ,plol    ,plos    )
C-----------------------------------------------------------------------
C
C Computes the path length integrals to the model interfaces given the 
C ozone volume mixing ratio
C
C---------------------------Code history--------------------------------
C
C Original version:     CCM1
C Standardized:         J. Rosinski, June 1992
C Reviewed:             J. Kiehl, B. Briegleb, August 1992
C Mixing ratio version: Bruce Biegleb, September 1992
C Reviewed:             J. Kiehl, April 1996
C Reviewed:             B. Briegleb, May 1996
C Top correction:       B. Briegleb, January 1997
C (remove .50 and .25 factors for top interface)
C
C-----------------------------------------------------------------------
c
c $Id: radoz2.F,v 1.2 2004/09/07 02:47:45 rca Exp $
c
#include <implicit.h>
C-----------------------------------------------------------------------
#include <prgrid.h>
C------------------------------Commons----------------------------------
#include <comozp.h>
C------------------------------Input arguments--------------------------
C
      real o3vmr(plond,plev)   ! ozone volume mixing ratio
      real pint(plond,plevp)   ! Model interface pressures
C
C----------------------------Output arguments---------------------------
C
      real plol(plond,plevp)   ! Ozone prs weighted path length (cm)
      real plos(plond,plevp)   ! Ozone path length (cm)
C
C---------------------------Local workspace-----------------------------
C
      integer i                ! longitude index
      integer k                ! level index
C
C-----------------------------------------------------------------------
C
C Evaluate the ozone path length integrals to interfaces; 
C factors of .1 and .01 to convert pressures from cgs to mks:
C
      do i=1,plon
        plos(i,1) = 0.1*cplos*o3vmr(i,1)*pint(i,1)
        plol(i,1) = 0.01*cplol*o3vmr(i,1)*pint(i,1)*pint(i,1)
      end do
      do k=2,plevp
        do i=1,plon
          plos(i,k) = plos(i,k-1) + 0.1*cplos*o3vmr(i,k-1)*
     $                (pint(i,k) - pint(i,k-1))
          plol(i,k) = plol(i,k-1) + 0.01*cplol*o3vmr(i,k-1)*
     $                (pint(i,k)*pint(i,k) - pint(i,k-1)*pint(i,k-1))
        end do
      end do
C
      return
      end
 
