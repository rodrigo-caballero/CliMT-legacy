#include <misc.h>
#include <params.h>
      subroutine cainti(cappax  ,cpairx  ,epsilox ,gravitx ,latvapx ,
     $                  rhoh2ox ,clrh2ox )
C-----------------------------------------------------------------------
C
C Initialize convective adjustment common block /comadj/
C Note that the variable nlvdry in comadj is set to its default value in
C preset.for, and updated from namelist input (if necessary) in 
C subroutine DATA
C
C----------------------------Code History-------------------------------
C
C Original version:  J. Hack
C Standardized:      J. Rosinski, June 1992
C Reviewed:          J. Hack, G. Taylor, August 1992
C
C-----------------------------------------------------------------------
c
c $Id: cainti.F,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c
#include <implicit.h>
C-----------------------------------------------------------------------
#include <pmgrid.h>
C-----------------------------------------------------------------------
#include <comadj.h>
C------------------------------Arguments--------------------------------
C
C Input arguments
C
      real cappax            ! R/cp
      real cpairx            ! Specific heat of dry air
      real epsilox           ! Ratio of h2o to dry air molecular weights 
      real gravitx           ! Acceleration due to gravity
      real latvapx           ! Latent heat of vaporization
      real rhoh2ox           ! Density of liquid water
      real clrh2ox           ! Ratio of latvap to water vapor gas constant
C
C-----------------------------------------------------------------------
C
      cappa  = cappax
      cpair  = cpairx
      epsilo = epsilox
      gravit = gravitx
      latvap = latvapx
      rhoh2o = rhoh2ox
      clrh2o = clrh2ox
      cldcp  = latvap/cpair
C
      return
      end
 
