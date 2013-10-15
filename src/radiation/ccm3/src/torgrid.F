#include <misc.h>
#include <params.h>
      subroutine torgrid(pmidm   ,pintm   ,pmlnm   ,pilnm   ,tm      ,
     $                   h2ommrm ,cldm    ,effcldm ,clwpm   ,
     $                   pmid    ,pint    ,pmln    ,piln    ,t       ,
     $                   h2ommr  ,cld     ,effcld  ,clwp    )
C-----------------------------------------------------------------------
C
C Interpolate model arrays to radiation vertical grid.
C
C Currently this is do nothing routine.
C
C-------------------------Code History----------------------------------
C
C Original version:  CCM2
C Standardized:      T. Acker, Feb 1996
C Reviewed:          
C 
C-----------------------------------------------------------------------
c
c $Id: torgrid.F,v 1.2 2004/09/07 02:47:46 rca Exp $
c
C-----------------------------------------------------------------------
#include <implicit.h>
C------------------------------Parameters-------------------------------
#include <pmgrid.h>
C-----------------------------------------------------------------------
#include <ptrrgrid.h>
C------------------------------Argurments--------------------------------
C
C Input arguments
C
      real pmidm(plond,plev)      ! Model level pressures
      real pintm(plond,plevp)     ! Model interface pressures
      real pmlnm(plond,plev)      ! Natural log of pmid
      real pilnm(plond,plevp)     ! Natural log of pint
      real tm(plond,plev)         ! Model level temperatures
      real h2ommrm(plond,plev)    ! Model level specific humidity
      real cldm(plond,plevp)      ! Fractional cloud cover
      real effcldm(plond,plevp)   ! Effective fractional cloud cover
      real clwpm(plond,plev)      ! Cloud liquid water path
C
C Output arguments
C
      real pmid(plond,plevr)      ! Interpolated values of above var. 
      real pint(plond,plevrp)     !   "
      real pmln(plond,plevr)      !   " 
      real piln(plond,plevrp)     !   " 
      real t(plond,plevr)         !   " 
      real h2ommr(plond,plevr)    !   "  
      real cld(plond,plevrp)      !   " 
      real effcld(plond,plevrp)   !   "
      real clwp(plond,plevr)      !   "
C 
C-----------------------------------------------------------------------
C
C Code to interpolate goes here.  Do nothing could be coded as a memory
C transfer, but left out for efficiency considerations.
C
      return
C
      end
 
