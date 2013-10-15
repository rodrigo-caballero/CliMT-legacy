c
c $Id: comctl.h,v 1.2 2004/09/07 02:47:44 rca Exp $
c $Author: rca $
c
C
C Model control variables
C
      common/comctl/itsst   ,nsrest  ,iradsw  ,iradlw  ,iradae  ,
     $              anncyc  ,nlend   ,nlres   ,nlhst   ,lbrnch  ,
     $              aeres   ,ozncyc  ,sstcyc  ,aeregen ,cpuchek ,
     $              incorhst,incorbuf,incorrad,adiabatic,flxave ,
     $              nusr_adv,nusr_nad,
     $              trace_gas, trace_test1,trace_test2, trace_test3,
     $              readtrace,idealdyn

      integer itsst     ! Sea surf. temp. update freq. (iters)
      integer nsrest    ! Restart flag
      integer iradsw    ! Iteration freq. for shortwave radiation
      integer iradlw    ! Iteration freq. for longwave radiation
      integer iradae    ! Iteration freq. for absorptivity/emissivity

      integer nusr_adv  ! Number of user defined advected tracers
      integer nusr_nad  ! Number of user defined non-advected tracers

      logical anncyc    ! true => do annual cycle (otherwise perpetual)
      logical nlend     ! true => end of run
      logical nlres     ! true => continuation run
      logical nlhst     ! true => regeneration run
      logical lbrnch    ! true => branch run
      logical aeres     ! true => a/e data will be stored on restart file
      logical ozncyc    ! true => cycle ozone dataset
      logical sstcyc    ! true => cycle sst dataset
      logical aeregen   ! true => absor/emis part of regeneration data
      logical cpuchek   ! true => check remaining cpu time each writeup
      logical incorhst  ! true => keep history buffer in-core
      logical incorbuf  ! true => keep model buffers in-core
      logical incorrad  ! true => keep abs/ems buffer in-core
      logical adiabatic ! true => no physics
      logical idealdyn  ! true => run "idealized" model configuration
      logical flxave    ! true => send to coupler only on radiation time steps

      logical trace_gas   ! true => turn on greenhouse gas code
      logical trace_test1 ! true => turn on test tracer code with 1 tracer
      logical trace_test2 ! true => turn on test tracer code with 2 tracers
      logical trace_test3 ! true => turn on test tracer code with 3 tracers
      logical readtrace   ! true => obtain initial tracer data from IC file
