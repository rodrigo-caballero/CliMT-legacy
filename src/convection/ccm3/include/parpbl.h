c
c $Id: parpbl.h,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c $Author: rca $
c
C
C Constants for the surface temperature calculation
C
      real zzocen   ! Ocean aerodynamic roughness length
      real zzsice   ! Sea ice aerodynamic roughness length
      real xkar     ! Von Karman constant
      real hmixmn   ! Minimum boundary layer height
      real ric      ! Critical Richardson number
      real rimax    ! Maximum Richardson number
      real epsi     ! Small factor to prevent exact zeros
      real zref     ! 10m reference height 
      real umin     ! Minimum wind speed at bottom level
C
      parameter ( zzocen =    0.0001  ,
     $            zzsice =    0.0400   ,
     $              xkar =    0.4      ,
     $            hmixmn =  500.0      ,
     $               ric =    3.05     ,
     $             rimax =    0.50*ric ,
     $              epsi =    1.0e-12  ,
     $              zref =   10.0      ,
     $              umin =    1.0      )  
C
