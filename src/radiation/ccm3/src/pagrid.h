c
c $Id: pagrid.h,v 1.2 2004/09/07 02:47:45 rca Exp $
c $Author: rca $
c
C
C Model grid point resolution parameters.
C
      integer plnlv     ! Length of multilevel field slice
      integer plndlv    ! Length of multilevel 3-d field slice
      integer pbflnb    ! Length of buffer 1
      integer pbflna    ! Length of buffer 2
      integer pbflnm1   ! Length of buffer m1
C
      integer pflenb    ! Length of buffer 1, padded for unblocked I/O
      integer pflena    ! Length of buffer 2, padded for unblocked I/O
      integer plenalcl  ! Length of buffer 2, needed in SPEGRD
      integer ptifld    ! No. of fields on time-invariant bndary dataset
      integer ptvsfld   ! No. of fields on time-variant boundary dataset
C
      integer ptvofld   ! Number of fields on ozone dataset
      integer plenhi    ! Length of integer header record
      integer plenhc    ! Length of character header record
      integer plenhr    ! Length of real header record
      integer plexbuf   ! Len. of communication buffer for flux coupling
C
      integer ptapes    ! Maximum number of history tapes allowed
      integer pflds     ! Maximum number of fields in all history files
      integer ptileni   ! Length of time-invariant integer header
      integer ptilenc   ! Length of time-invariant character header
      integer ptvoleni  ! Length of ozone integer header
C
      integer ptvolenc  ! Length of ozone character header
      integer ptvsleni  ! Length of time-variant integer header
      integer ptvslenc  ! Length of time-variant character header
      integer plenhis   ! Length of integer header scalars
      integer plenhcs   ! Length of character header scalars
C
      integer ptilenis  ! Length of time-invariant integer scalars
      integer ptilencs  ! Length of time-invariant character scalars
      integer ptolenis  ! Length of ozone integer header scalars
      integer ptolencs  ! Length of ozone character header scalars
      integer ptslenis  ! Length of time-variant integer header scalars
      integer ptslencs  ! Length of time-variant character header scalars
C
      parameter(plnlv=plon*plev,plndlv=plond*plev)
C
C In pbflnb, 9 multi-level fields include the plev levels of plol and
C plos. 2 multi-level fields are pcnst-dependent.
C
      parameter(pbflnb=(7 + 2*pcnst + 1*pnats)*plndlv + (5+pcnst)*plond)
C
C In pbflna, there are 3 multi-level and 3 single-level fields.
C
      parameter(pbflna = (3 + 3*plev)*plond)
      parameter(pbflnm1 = (1 + 2*plev)*plond)
      parameter(pflenb = ((pbflnb + pbflnm1)/512 + 1)*512)
      parameter(pflena = (pbflna/512 + 1)*512)
C
C plenalcl is the buffer size as required in SPEGRD.  
C Only pflena is read/written.
C
      parameter(plenalcl = ((pbflna + 2*plndlv + plond)/512 + 1)*512)
      parameter(plexbuf = (((1 + 6*plev)*plond)/512+1)*512)
      parameter(ptapes = 6)
C
C Maximum total number of fields in all history files
C (primary and auxillary files)
C
      parameter(pflds=1000)
C
C Add 2 extra fields for tvbds     6 April 1995
C
      parameter(ptifld = 11, ptvsfld = 3, ptvofld = 2)
C
C There are 37 scalar words in the integer header and 89 scalar words
C in the character header
C
      parameter(plenhis=37)
      parameter(plenhcs=89)
C
      parameter(plenhi=plenhis+3*pflds)
      parameter(plenhc=plenhcs+2*pflds)
      parameter(plenhr=3*(2*plev + 1) + 2*plat)
      parameter(ptilenis=plenhis) 
      parameter(ptilencs=plenhcs)
C
      parameter(ptileni=ptilenis+3*ptifld) 
      parameter(ptilenc=ptilencs+2*ptifld)
      parameter(ptolenis=plenhis) 
      parameter(ptolencs=plenhcs)
      parameter(ptvoleni=ptolenis+3*ptvofld)
C
      parameter(ptvolenc=ptolencs+2*ptvofld)
      parameter(ptslenis=plenhis) 
      parameter(ptslencs=plenhcs)
      parameter(ptvsleni=ptslenis+3*ptvsfld)
      parameter(ptvslenc=ptslencs+2*ptvsfld)
C
