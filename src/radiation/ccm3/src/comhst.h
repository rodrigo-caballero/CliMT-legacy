c
c $Id: comhst.h,v 1.2 2004/09/07 02:47:44 rca Exp $
c $Author: rca $
c
C
C Integer and logical variables related to history tapes
C
      integer pichsum  ! Max. value of 4*ichar(character)
      parameter (pichsum=508)
C
      common /comhst/
     $   nhtfrq(ptapes)    ,mfilt(ptapes) ,nlfilt             ,
     $   ndens(ptapes)     ,nflds(ptapes) ,nfils(ptapes)      ,
     $   hunit(ptapes)     ,nrlen(ptapes) ,nplen(ptapes)      ,
     $   sunit             ,stfnum        ,mtapes             ,
     $   nexcl             ,nincl         ,hbufpt(ptapes)     ,
     $   nacs(pflds,plat)  ,iflds(3,pflds),nupnt(pflds,ptapes),
     $   npnt(pflds,ptapes),ndcurf(ptapes),ncdatf(ptapes)     ,
     $   nscurf(ptapes)    ,ncsecf(ptapes),nfldsc(0:pichsum,ptapes),
     $   islocc(0:pichsum,ptapes)         ,hstwr(ptapes)      ,
     $   rstwr             ,nacsav(pflds,plat)
C
      integer nhtfrq   ! Array of write freq of time samples to hist file
      integer mfilt    ! Number of time samples per history file
C
      logical nlfilt   ! Flag for extra time samp on 1st h-file (ktape=1)
      logical hstwr    ! Flag for history writes
      logical rstwr    ! Flag for restart and regeneration writes
C
      integer ndens    ! Array of input packing densities
      integer nflds    ! Array of total no. of fields in each h-file
      integer nfils    ! Array of no. of files on current h-file
      integer hunit    ! History file disk unit no.
      integer nrlen    ! Record length
C
      integer nplen    ! Packed record length,
      integer sunit    ! History buffer SSD unit
      integer stfnum   ! Starting number for history file naming
      integer mtapes   ! Actual number of history files requested 
      integer nexcl    ! Actual number of excluded fields
C
      integer nincl    ! Actual number of included primary file fields
      integer hbufpt   ! Ptrs to start of fields for each file in hbuf
      integer nacs     ! Number of time steps accum. for field in hbuf
      integer nacsav   ! Saved accumulations for restart
      integer iflds    ! Integer portion of master field list
C
      integer nupnt    ! Array of unpacked field pointers
      integer npnt     ! Array of packed field pointers
      integer ndcurf   ! First "current" day for each history file
      integer ncdatf   ! First "current" date for each history file
      integer nscurf   ! First "current" second of day for each h-file
C
      integer ncsecf   ! First "current" second of date for each h-file
      integer nfldsc   ! Number of fields starting with given ichar(1-4)
      integer islocc   ! Index of starting location for each ichar sum
C
C  Character variables related to history tapes
C
      common /comhtc/
     $   nfpath(ptapes)     ,ppath(ptapes)       ,cpath(ptapes)       ,
     $   nhfil(ptapes)      ,ninavg(ptapes)      ,caseid              ,
     $   ctitle             ,fieldn(2,pflds)     ,exclude(pflds)      ,
     $   primary(pflds)     ,aux(pflds,ptapes-1) ,inithist 
C
      character*80 nfpath     ! Array of first pathnames, for header
      character*80 ppath      ! Array of previous pathnames, for header
      character*80 cpath      ! Array of current pathnames
      character*80 nhfil      ! Array of current file names
      character*1  ninavg     ! File fields instantaneous or averaged
C
      character*8  caseid     ! Case identifier
      character*80 ctitle     ! Case title
      character*8  fieldn     ! Character portion of master field list
      character*8  exclude    ! List of fields to rm from primary h-file
      character*8  primary    ! List of fields to add to primary h-file
      character*8  aux        ! Lists of fields for auxiliary files
C
      character*8 inithist    ! If set to 'MONTHLY' or 'YEARLY' then 
                              ! write init conditions aux. file 
 
