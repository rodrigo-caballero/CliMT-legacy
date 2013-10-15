c
c $Id: ptrrgrid.h,v 1.1 2004/09/07 15:27:28 rca Exp $
c $Author: rca $
c
C
C Define radiation vertical grid and buffer length for abs/ems
C out-of-core file
C
      integer plevr    ! Number of vertical levels
      integer plevrp   ! plevr + 1
      integer plngbuf  ! Length of absorptivity/emissivity record
C
      parameter(plevr = PLEVR,
     $          plevrp = plevr + 1,
     $          plngbuf = 512*( ( plond*plevrp*plevrp +
     $                            plond*plevr*4 +
     $                            plond*plevrp )/512 + 1 )  )
C 
 
