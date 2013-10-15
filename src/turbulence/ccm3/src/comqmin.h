c
c $Id: comqmin.h,v 1.1 2005/01/24 19:21:44 rca Exp $
c $Author: rca $
c
C
C Minimum mass mixing ratio for constituents (water vapor is first)
C
      common /comqmin/ qmin(pcnst),qmincg(pcnst)
C
      real qmin      ! Global minimum constituent concentration
      real qmincg    ! Min. constituent concentration counter-gradient term
C
 
