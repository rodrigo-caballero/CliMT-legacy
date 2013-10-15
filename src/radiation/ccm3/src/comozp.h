      integer pozlev
      parameter (pozlev=POZLEV)
!
! Floating point data
!
      common/ozone/ozmixm(plond,pozlev,plat,2),ozmix(plond,pozlev,plat)
      common/ozone/pin(pozlev),cdayozm, cdayozp, cplos, cplol
!
! Pointers to dynamic memory
!
c+++climt  these are not used in CliMT
c      common/ozone/pozlon, pozlat, pdate_oz, psec_oz
c
C-------       pointer (pozlon,ozlon)
C-------       pointer (pozlat,ozlat)
C-------       pointer (pdate_oz,date_oz)
C-------       pointer (psec_oz,sec_oz)
C------- 
C-------       real ozlon(*)      ! Longitude array for bdy dataset values
C-------       real ozlat(*)      ! Latitude array for bdy dataset values
C-------       integer date_oz(*) ! Date on ozone dataset (YYYYMMDD)
C-------       integer sec_oz(*)  ! seconds of date on ozone dataset (0-86399)
C------- 
c---climt
!
! Integers
!
      common/ozone/nm, np, oznid
      common/ozone/lonsiz, levsiz, latsiz, timesiz, np1

      real ozmixm     ! O3 mixing ratios interp. in latitude
      real ozmix      ! O3 mixing ratios interp. in time
      real pin        ! O3 pressure values (pascals)
      real cdayozm    ! Calendar day for prv. month O3 values read in
      real cdayozp    ! Calendar day for nxt. month O3 values read in
      real cplos      ! Const for ozone path length calculation
      real cplol      ! Const for pressure-weighted o3 path length calc.
      
      integer nm,np   ! Array indices for prv., nxt month ozone data
      integer oznid   ! netcdf id for ozone variable
      integer lonsiz  ! size of longitude dimension on ozone dataset
      integer levsiz  ! size of level dimension on ozone dataset
      integer latsiz  ! size of latitude dimension on ozone dataset
      integer timesiz ! size of time dimension on ozone dataset
      integer np1     ! current forward time index of ozone dataset
 
