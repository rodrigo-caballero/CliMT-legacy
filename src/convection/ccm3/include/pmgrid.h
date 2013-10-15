c
c $Id: pmgrid.h,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c $Author: rca $
c
C
C Grid point resolution parameters
C
      integer plon       ! number of longitudes
      integer plev       ! number of vertical levels
      integer plat       ! number of latitudes
      integer pcnst      ! number of constituents (including water vapor)
      integer pcnste     ! total size of constituents
#if ( CCM_VERS == 3 && CCM_MINOR_VERS >= 6 )  
      integer pnats      ! number of non-advective trace species
#endif
      integer plevmx     ! number of subsurface levels
C
      integer plevp      ! plev + 1
      integer nxpt       ! no.of pts outside active domain of interpolant
      integer jintmx     ! number of extra latitudes in polar region
      integer plond      ! slt extended domain longitude
      integer platd      ! slt extended domain lat.
C
      integer plevd      ! fold plev,pcnst indices into one
      integer i1         ! model starting longitude index
      integer j1         ! model starting latitude index
      integer numbnd     ! no.of latitudes passed N and S of forecast lat
C
      integer beglat     ! beg. index for latitudes owned by a given proc
      integer endlat     ! end. index for latitudes owned by a given proc
      integer beglatex   ! extended grid beglat
      integer endlatex   ! extended grid endlat
      integer numlats    ! number of latitudes owned by a given proc
C
      logical masterproc ! Flag for (iam eq 0)
C
      parameter(plon   = PLON)
      parameter(plev   = PLEV)
      parameter(plat   = PLAT)
      parameter(pcnst  = PCNST)
      parameter(pcnste = 3 + PCNST)
#if ( CCM_VERS == 3 && CCM_MINOR_VERS >= 6 )  
      parameter(pnats  = PNATS)
#endif
      parameter(plevmx = 4)
      parameter(plevp  = plev + 1)
      parameter(nxpt   = 1)
      parameter(jintmx = 1)
cccjt      parameter(plond  = plon + 1 + 2*nxpt)
cccjt      parameter(platd  = plat + 2*nxpt + 2*jintmx)
      parameter(plond  = plon)
      parameter(platd  = plat)
#if ( CCM_VERS == 3 && CCM_MINOR_VERS >= 6 )  
      parameter(plevd  = plev*(3 + pcnst + pnats))
#else
      parameter(plevd  = plev*(3 + pcnst))
#endif
      parameter(i1     = 1 + nxpt)
      parameter(j1     = 1 + nxpt + jintmx)
      parameter(numbnd = nxpt + jintmx)
C
#if ( defined SPMD )
      common/spmdlats/beglat  ,endlat  ,numlats ,beglatex,endlatex 
      common/spmdlats/masterproc
#else
      parameter(beglat   = 1)
      parameter(endlat   = plat)
      parameter(numlats  = plat)
      parameter(beglatex = 1)
      parameter(endlatex = platd)
      parameter(masterproc = .true.)
#endif
C
