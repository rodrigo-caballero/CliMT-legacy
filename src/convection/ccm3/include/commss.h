c
c $Id: commss.h,v 1.1.1.1 2004/02/27 05:44:07 rca Exp $
c $Author: rca $
c
C
C Character variables associated with Mass Store pathnames
C
      common/commsc/nhpath  ,nrpath  ,ncdata  ,bndti   ,bndtvs  ,
     $              bndtvo  ,nrfil   ,nsmvn   ,nrmvn   ,msscom  ,
     $              nswrps  ,lcroot  ,datadir
C
      character*72  nhpath   ! MSS pathname for history tapes
      character*72  nrpath   ! MSS pathname for restart files
      character*80  ncdata   ! MSS pathname for initial dataset
      character*80  bndti    ! MSS path for time-inv boundary dataset
C
      character*80  bndtvs   ! MSS path for time-variant sst dataset
      character*80  bndtvo   ! MSS path for time-variant ozone dataset
      character*6   nrfil    ! Current file name for regen dataset
      character*8   nsmvn    ! Virtual volume name for history tapes
C
      character*8   nrmvn    ! Virtual volume name for restart data
      character*80  msscom   ! MSS comment field
      character*8   nswrps   ! MSS write password
      character*40  lcroot   ! Prepend to MSS paths for local disk name
      character*80  datadir  ! local disk directory where requested
                             ! files may be located 
C
C Non-character variables associated with Mass Store pathnames
C
      common/commss/nhpthl  ,nrpthl  ,irt     ,rirt
C
      integer  nhpthl        ! Length of nhpath,history tape pathname
      integer  nrpthl        ! Length of nrpath,restart data pathname
      integer  irt           ! Mass Store retention period, history tapes
      integer  rirt          ! Mass Store retention time, restart data
C








