c
c $Id: albedo.h,v 1.2 2004/09/07 02:47:44 rca Exp $
c $Author: rca $
c
      real snws            ! Snow albedo for 0.2-0.7 micro-meters
      real snwl            ! Snow albedo for 0.7-5.0 micro-meters
      real sices           ! Sea ice albedo for 0.2-0.7 micro-meters
      real sicel           ! Sea ice albedo for 0.7-5.0 micro-meters
      parameter (snws  = 0.95)
      parameter (snwl  = 0.70)
      parameter (sices = 0.70)
      parameter (sicel = 0.50)
C
C Slab ocean model mods
C
      real sicsns          ! Sea-ice snow albedo for 0.2-0.7 micro-meters
      real sicsnl          ! Sea-ice snow albedo for 0.7-5.0 micro-meters
      parameter (sicsns = 0.84)
      parameter (sicsnl = 0.60)
C
      real sicsmn          ! min Sea-ice albedo for 0.2-0.7 micro-meters
      real siclmn          ! min Sea-ice albedo for 0.7-5.0 micro-meters
      parameter (sicsmn = 0.50)
      parameter (siclmn = 0.26)
C
 
