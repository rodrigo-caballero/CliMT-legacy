
c $Id: comlun.h,v 1.2 2004/09/07 02:47:44 rca Exp $
c $Author: rca $
c
C
C Logical unit numbers and related variables
C
#if ( ! defined PNRG1 )
#define PNRG1 5
#endif
#if ( ! defined PNRG2 )
#define PNRG2 5
#endif
#if ( ! defined PNRG3 )
#define PNRG3 5
#endif
      integer pnrg1     ! maximum number of primary regeneration files
      integer pnrg2     ! maximum number of secondary regeneration files
      integer pnrg3     ! maximum number of history buffer regen files

      parameter (pnrg1 = PNRG1)
      parameter (pnrg2 = PNRG2)
      parameter (pnrg3 = PNRG3)
C
      common/comlun/nsds    ,nrg     ,nrg1(pnrg1)      ,nrg2(pnrg2),
     $              nrg3(pnrg3,ptapes)        ,nra1    ,nrb1    ,
     $              ncid_ini,ncid_oz ,ncid_sst,nabem   ,
     $              nsplit  ,htunits(ptapes)
      common/comlun/rg1lat(pnrg1+1)  ,rg1siz(pnrg1)    ,rg1buf  ,nnrg1,
     $              rg2lat(pnrg2+1)  ,rg2siz(pnrg2)    ,rg2buf  ,nnrg2,
     $              rg3lat(pnrg3+1,ptapes)    ,rg3siz(pnrg3,ptapes)   ,
     $              rg3buf(ptapes)   ,nnrg3(ptapes)    ,mxszrg  ,
     $              nrefrq ,mresfq,lutag(99),rgnht(ptapes)
C
      integer nsds       ! restart dataset unit
      integer nrg        ! master regeneration dataset unit
      integer nrg1       ! primary regeneration dataset units
      integer nrg2       ! secondary regeneration dataset units
      integer nrg3       ! hbuf regeneration dataset units
      integer nra1       ! a work file
      integer nrb1       ! b work file
      integer ncid_ini   ! initial dataset
      integer ncid_oz    ! ozone dataset
      integer ncid_sst   ! sst dataset
      integer nabem      ! absorptivity/emissivity work file
      integer nsplit     ! communication between LINEMS1 and LINEMS2
      integer htunits    ! history tape unit numbers
      integer rg1lat     ! latitude list for primary regen datasets
      integer rg1siz     ! file sizes for preallocation
      integer rg1buf     ! buffer length for assign
      integer nnrg1      ! number of primary regen files written
      integer rg2lat     ! lat list for secondary regen datasets
      integer rg2siz     ! file size for preallocation
      integer rg2buf     ! buffer length for assign
      integer nnrg2      ! number of secondary regen files written
      integer rg3lat     ! latitude list for hbuf regen datasets
      integer rg3siz     ! file sizes for preallocation
      integer rg3buf     ! buffer length for assign
      integer nnrg3      ! number of hbuf regen files written
      integer mxszrg     ! max size of a regen file (megabytes)
      integer nrefrq     ! frequency of regeneration file writes
      integer mresfq     ! frequency of mnthly avg regen file writes
C 
      logical lutag      ! list of flags marking logical units in use
      logical rgnht      ! set true if regen file for a h-tape exists
C
#if ( defined SPMD )
      common/comlun/iualp,iudalp
      integer iualp,iudalp
#endif
      common/comlunc/rg1ext(pnrg1),rg2ext(pnrg2),rg3ext(pnrg3,ptapes)
C
      character*2 rg1ext ! file extension for primary regen files
      character*2 rg2ext ! file extension for secondary regen files
      character*5 rg3ext ! file extension for history buffer regen files
C
 
