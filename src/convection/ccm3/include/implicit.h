c
c $Id: implicit.h,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c $Author: rca $
c
      implicit none
#if ( defined RS6000 )
      implicit automatic (a-z)
#endif

