c
c $Id: implicit.h,v 1.1 2005/01/24 19:21:44 rca Exp $
c $Author: rca $
c
      implicit none
#if ( defined RS6000 )
      implicit automatic (a-z)
#endif

