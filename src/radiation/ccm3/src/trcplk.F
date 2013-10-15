#include <misc.h>
#include <params.h>
      subroutine trcplk(tint    ,tlayr   ,tplnke  ,emplnk  ,abplnk1 ,
     $                  abplnk2 )
c----------------------------------------------------------------------
c
c   Calculate Planck factors for absorptivity and emissivity of
c   CH4, N2O, CFC11 and CFC12
c
C   Planck function and derivative evaluated at the band center.
C
C-------------------------Code History----------------------------------
C
C Original version:  J. Kiehl, Nov 1994 
C Standardized:      T. Acker, Feb 1996
C Reviewed:          J. Kiehl, Apr 1996     
C 
c-----------------------------------------------------------------------
c
c $Id: trcplk.F,v 1.2 2004/09/07 02:47:46 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <prgrid.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real tint(plond,plevp)   ! interface temperatures
      real tlayr(plond,plevp)  ! k-1 level temperatures
      real tplnke(plond)       ! Top Layer temperature
c
c output arguments
c
      real emplnk(14,plond)         ! emissivity Planck factor
      real abplnk1(14,plond,plevp)  ! non-nearest layer Plack factor
      real abplnk2(14,plond,plevp)  ! nearest layer factor
c
C--------------------------Local Variables------------------------------
c
      integer wvl                   ! wavelength index
      integer i,k                   ! loop counters
c
      real f1(14)                   ! Planck function factor
      real f2(14)                   !        "
      real f3(14)                   !        "
c
C--------------------------Data Statements------------------------------
c
      data f1 /5.85713e8,7.94950e8,1.47009e9,1.40031e9,1.34853e8,
     $         1.05158e9,3.35370e8,3.99601e8,5.35994e8,8.42955e8,
     $         4.63682e8,5.18944e8,8.83202e8,1.03279e9/
      data f2 /2.02493e11,3.04286e11,6.90698e11,6.47333e11,
     $         2.85744e10,4.41862e11,9.62780e10,1.21618e11,
     $         1.79905e11,3.29029e11,1.48294e11,1.72315e11,
     $         3.50140e11,4.31364e11/
      data f3 /1383.0,1531.0,1879.0,1849.0,848.0,1681.0,
     $         1148.0,1217.0,1343.0,1561.0,1279.0,1328.0,
     $         1586.0,1671.0/
c
C-----------------------------------------------------------------------
c
c Calculate emissivity Planck factor
c
      do wvl = 1,14
         do i = 1,plon
            emplnk(wvl,i) = f1(wvl)/
     $                   (tplnke(i)**4.0*(exp(f3(wvl)/tplnke(i))-1.0))
         end do
      end do
c
c Calculate absorptivity Planck factor for tint and tlayr temperatures
c
      do wvl = 1,14
         do k = 1, plevp
            do i = 1, plon
c
c non-nearlest layer function
c
               abplnk1(wvl,i,k) = (f2(wvl)*exp(f3(wvl)/tint(i,k)))
     $              /(tint(i,k)**5.0*(exp(f3(wvl)/tint(i,k))-1.0)**2.0)
c
c nearest layer function
c
               abplnk2(wvl,i,k) = (f2(wvl)*exp(f3(wvl)/tlayr(i,k)))
     $            /(tlayr(i,k)**5.0*(exp(f3(wvl)/tlayr(i,k))-1.0)**2.0)
            end do
         end do
      end do
c
      return
c
      end
 
