c
c $Id: prgrid.h,v 1.1 2004/09/07 15:27:39 rca Exp $
c $Author: rca $
c
C
C Radiation resolution and I/O parameters
C
      integer plon       ! number of longitudes
      integer plev       ! number of vertical levels
      integer plat       ! number of latitudes
      integer pcnst      ! number of constituents (including water vapor)
      integer pnats      ! number of non-advected trace species
      integer plevmx     ! number of subsurface levels
C
      integer plevp      ! plev + 1
      integer nxpt       ! no.of pts outside active domain for interpolant
      integer jintmx     ! number of extra latitudes in polar region
      integer plond      ! slt extended domain longitude
      integer platd      ! slt extended domain lat.
      integer p3d        ! dimensioning construct: num. of 3-d flds in /com3d/
C
      integer plevd      ! fold plev,pcnst indices into one
      integer plngbuf    ! length of absorptivity/emissivity record
C
      integer beglat     ! beg. index for latitudes owned by a given proc
      integer endlat     ! end. index for latitudes owned by a given proc
      integer beglatex   ! extended grid beglat
      integer endlatex   ! extended grid endlat
      integer numlats    ! number of latitudes owned by a given proc
C
      logical masterproc ! Flag for (iam eq 0)
C
      parameter (plon    = PLON) 
      parameter (plev    = PLEVR)
      parameter (plat    = PLAT)
      parameter (pcnst   = PCNST)
      parameter (pnats   = PNATS)
      parameter (plevmx  = 4)
      parameter (plevp   = plev + 1)
      parameter (nxpt    = 1)
      parameter (jintmx  = 1)
c++csz
c      parameter (plond   = plon + 1 + 2*nxpt)
      parameter (plond   = PLON)
c--csz
      parameter (platd   = plat + 2*nxpt + 2*jintmx)
      parameter (p3d     = 3 + pcnst + pnats)
      parameter (plevd   = plev*p3d)
      parameter (plngbuf = 512*((plond*plevp*plevp + plond*plev*4 +
     $                          plond*plevp)/512 + 1))
C
#if ( defined SPMD )
      common/spmdlats/beglat  ,endlat  ,numlats ,beglatex,endlatex 
      common/spmdlats/masterproc
#else              
      parameter (beglat   = 1)
      parameter (endlat   = plat)
      parameter (numlats  = plat)
      parameter (beglatex = 1)
      parameter (endlatex = platd)
      parameter (masterproc = .true.)
#endif



 
