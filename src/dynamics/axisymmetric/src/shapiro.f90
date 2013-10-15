! Wrappers for the shapiro filter.
! Christian Dieterich Mon May 10 14:06:33 CDT 2004

subroutine shapiro_f1d(out, raw, im, imin, imax, idim, &
  & order, apply, boundary)
! Filter 1D array raw along 1st dimension
  implicit none
  real, dimension(im), intent(out) :: out
  real, dimension(im), intent(in) :: raw
  real, dimension(:), allocatable :: tmp1, tmp2, cof1, cof2
  integer, intent(in) :: im, imin, imax, idim, &
    & order, apply, boundary
  integer :: mord, mapp, mper, mcpx, &
    & ilen, mlen, nlen, i
  out=raw
  if(idim == 1)then
    mord=2**abs(order)
    mapp=apply
    mper=boundary
    mcpx=0
    ilen=imax-imin+1
    mlen=2*ilen
    nlen=2*mord
    allocate (tmp1(mlen), tmp2(mlen), cof1(nlen), cof2(nlen))
    do i=imin,imax
      tmp1(i-imin+1)=raw(i)
    end do
    call shpiro(tmp2, tmp1, cof2, cof1, ilen, mord, mapp, mper, mcpx)
    do i=imin,imax
      out(i)=tmp2(i-imin+1)
    end do
    deallocate (tmp1, tmp2, cof1, cof2)
  end if
  return
end subroutine shapiro_f1d
subroutine shapiro_f2d(out, raw, im, jm, imin, imax, idim, &
  & jmin, jmax, jdim, order, apply, boundary)
! Filter 2D array raw along 1st and 2nd dimension
  implicit none
  real, dimension(im, jm), intent(out) :: out
  real, dimension(im, jm), intent(in) :: raw
  real, dimension(:), allocatable :: tmp1, tmp2, cof1, cof2
  integer, intent(in) :: im, jm, imin, imax, idim, &
    & jmin, jmax, jdim, order, apply, boundary
  integer :: mord, mapp, mper, mcpx, &
    & ilen, jlen, mlen, nlen, i, j, m
  out=raw
  if(idim == 1 .and. jdim == 1)then
    mord=2**abs(order)
    mapp=apply
    mper=boundary
    mcpx=0
    ilen=imax-imin+1
    jlen=jmax-jmin+1
    mlen=2*max(ilen, jlen)
    nlen=2*mord
    allocate (tmp1(mlen), tmp2(mlen), cof1(nlen), cof2(nlen))
    do m=1,mapp
      do j=jmin,jmax
        do i=imin,imax
          tmp1(i-imin+1)=raw(i, j)
        end do
        call shpiro(tmp2, tmp1, cof2, cof1, ilen, mord, 1, mper, mcpx)
        do i=imin,imax
          out(i, j)=tmp2(i-imin+1)
        end do
      end do
      do i=imin,imax
        do j=jmin,jmax
          tmp1(j-jmin+1)=raw(i, j)
        end do
        call shpiro(tmp2, tmp1, cof2, cof1, jlen, mord, 1, mper, mcpx)
        do j=jmin,jmax
          out(i, j)=tmp2(j-jmin+1)
        end do
      end do
    end do
    deallocate (tmp1, tmp2, cof1, cof2)
  else if(idim == 1 .and. jdim /= 1)then
    mord=2**abs(order)
    mapp=apply
    mper=boundary
    mcpx=0
    ilen=imax-imin+1
    mlen=2*ilen
    nlen=2*mord
    allocate (tmp1(mlen), tmp2(mlen), cof1(nlen), cof2(nlen))
    do j=jmin,jmax
      do i=imin,imax
        tmp1(i-imin+1)=raw(i, j)
      end do
      call shpiro(tmp2, tmp1, cof2, cof1, ilen, mord, mapp, mper, mcpx)
      do i=imin,imax
        out(i, j)=tmp2(i-imin+1)
      end do
    end do
    deallocate (tmp1, tmp2, cof1, cof2)
  else if(idim /= 1 .and. jdim == 1)then
    mord=2**abs(order)
    mapp=apply
    mper=boundary
    mcpx=0
    jlen=jmax-jmin+1
    mlen=2*jlen
    nlen=2*mord
    allocate (tmp1(mlen), tmp2(mlen), cof1(nlen), cof2(nlen))
    do i=imin,imax
      do j=jmin,jmax
        tmp1(j-jmin+1)=raw(i, j)
      end do
      call shpiro(tmp2, tmp1, cof2, cof1, jlen, mord, mapp, mper, mcpx)
      do j=jmin,jmax
        out(i, j)=tmp2(j-jmin+1)
      end do
    end do
    deallocate (tmp1, tmp2, cof1, cof2)
  end if
  return
end subroutine shapiro_f2d
subroutine shapiro_f3d(out, raw, im, jm, km, imin, imax, idim, &
  & jmin, jmax, jdim, kmin, kmax, kdim, order, apply, boundary)
! Filter 3D array raw along 1st, 2nd and 3rd dimension
  implicit none
  real, dimension(im, jm, km), intent(out) :: out
  real, dimension(im, jm, km), intent(in) :: raw
  real, dimension(:), allocatable :: tmp1, tmp2, cof1, cof2
  integer, intent(in) :: im, jm, km, imin, imax, idim, &
    & jmin, jmax, jdim, kmin, kmax, kdim, order, apply, boundary
  integer :: mord, mapp, mper, mcpx, &
    & ilen, jlen, klen, mlen, nlen, i, j, k, m
  out=raw
  if(idim == 1 .and. jdim == 1 .and. kdim == 1)then
    mord=2**abs(order)
    mapp=apply
    mper=boundary
    mcpx=0
    ilen=imax-imin+1
    jlen=jmax-jmin+1
    klen=kmax-kmin+1
    mlen=2*max(ilen, jlen, klen)
    nlen=2*mord
    allocate (tmp1(mlen), tmp2(mlen), cof1(nlen), cof2(nlen))
    do m=1,mapp
      do k=kmin,kmax
        do j=jmin,jmax
          do i=imin,imax
            tmp1(i-imin+1)=raw(i, j, k)
          end do
          call shpiro(tmp2, tmp1, cof2, cof1, ilen, mord, 1, mper, mcpx)
          do i=imin,imax
            out(i, j, k)=tmp2(i-imin+1)
          end do
        end do
      end do
      do k=kmin,kmax
        do i=imin,imax
          do j=jmin,jmax
            tmp1(j-jmin+1)=raw(i, j, k)
          end do
          call shpiro(tmp2, tmp1, cof2, cof1, jlen, mord, 1, mper, mcpx)
          do j=jmin,jmax
            out(i, j, k)=tmp2(j-jmin+1)
          end do
        end do
      end do
      do j=jmin,jmax
        do i=imin,imax
          do k=kmin,kmax
            tmp1(k-kmin+1)=raw(i, j, k)
          end do
          call shpiro(tmp2, tmp1, cof2, cof1, klen, mord, 1, mper, mcpx)
          do k=kmin,kmax
            out(i, j, k)=tmp2(k-kmin+1)
          end do
        end do
      end do
    end do
    deallocate (tmp1, tmp2, cof1, cof2)
  else if(idim == 1 .and. jdim == 1 .and. kdim /= 1)then
    mord=2**abs(order)
    mapp=apply
    mper=boundary
    mcpx=0
    ilen=imax-imin+1
    jlen=jmax-jmin+1
    mlen=2*max(ilen, jlen)
    nlen=2*mord
    allocate (tmp1(mlen), tmp2(mlen), cof1(nlen), cof2(nlen))
    do m=1,mapp
      do k=kmin,kmax
        do j=jmin,jmax
          do i=imin,imax
            tmp1(i-imin+1)=raw(i, j, k)
          end do
          call shpiro(tmp2, tmp1, cof2, cof1, ilen, mord, 1, mper, mcpx)
          do i=imin,imax
            out(i, j, k)=tmp2(i-imin+1)
          end do
        end do
        do i=imin,imax
          do j=jmin,jmax
            tmp1(j-jmin+1)=raw(i, j, k)
          end do
          call shpiro(tmp2, tmp1, cof2, cof1, jlen, mord, 1, mper, mcpx)
          do j=jmin,jmax
            out(i, j, k)=tmp2(j-jmin+1)
          end do
        end do
      end do
    end do
    deallocate (tmp1, tmp2, cof1, cof2)
  else if(idim == 1 .and. jdim /= 1 .and. kdim == 1)then
    mord=2**abs(order)
    mapp=apply
    mper=boundary
    mcpx=0
    ilen=imax-imin+1
    klen=kmax-kmin+1
    mlen=2*max(ilen, klen)
    nlen=2*mord
    allocate (tmp1(mlen), tmp2(mlen), cof1(nlen), cof2(nlen))
    do m=1,mapp
      do j=jmin,jmax
        do k=kmin,kmax
          do i=imin,imax
            tmp1(i-imin+1)=raw(i, j, k)
          end do
          call shpiro(tmp2, tmp1, cof2, cof1, ilen, mord, 1, mper, mcpx)
          do i=imin,imax
            out(i, j, k)=tmp2(i-imin+1)
          end do
        end do
        do i=imin,imax
          do k=kmin,kmax
            tmp1(k-kmin+1)=raw(i, j, k)
          end do
          call shpiro(tmp2, tmp1, cof2, cof1, klen, mord, 1, mper, mcpx)
          do k=kmin,kmax
            out(i, j, k)=tmp2(k-kmin+1)
          end do
        end do
      end do
    end do
    deallocate (tmp1, tmp2, cof1, cof2)
  else if(idim /= 1 .and. jdim == 1 .and. kdim == 1)then
    mord=2**abs(order)
    mapp=apply
    mper=boundary
    mcpx=0
    jlen=jmax-jmin+1
    klen=kmax-kmin+1
    mlen=2*max(jlen, klen)
    nlen=2*mord
    allocate (tmp1(mlen), tmp2(mlen), cof1(nlen), cof2(nlen))
    do m=1,mapp
      do i=imin,imax
        do k=kmin,kmax
          do j=jmin,jmax
            tmp1(j-jmin+1)=raw(i, j, k)
          end do
          call shpiro(tmp2, tmp1, cof2, cof1, jlen, mord, 1, mper, mcpx)
          do j=jmin,jmax
            out(i, j, k)=tmp2(j-jmin+1)
          end do
        end do
        do j=jmin,jmax
          do k=kmin,kmax
            tmp1(k-kmin+1)=raw(i, j, k)
          end do
          call shpiro(tmp2, tmp1, cof2, cof1, klen, mord, 1, mper, mcpx)
          do k=kmin,kmax
            out(i, j, k)=tmp2(k-kmin+1)
          end do
        end do
      end do
    end do
    deallocate (tmp1, tmp2, cof1, cof2)
  else if(idim == 1 .and. jdim /= 1 .and. kdim /= 1)then
    mord=2**abs(order)
    mapp=apply
    mper=boundary
    mcpx=0
    ilen=imax-imin+1
    mlen=2*ilen
    nlen=2*mord
    allocate (tmp1(mlen), tmp2(mlen), cof1(nlen), cof2(nlen))
    do k=kmin,kmax
      do j=jmin,jmax
        do i=imin,imax
          tmp1(i-imin+1)=raw(i, j, k)
        end do
        call shpiro(tmp2, tmp1, cof2, cof1, ilen, mord, mapp, mper, mcpx)
        do i=imin,imax
          out(i, j, k)=tmp2(i-imin+1)
        end do
      end do
    end do
    deallocate (tmp1, tmp2, cof1, cof2)
  else if(idim /= 1 .and. jdim == 1 .and. kdim /= 1)then
    mord=2**abs(order)
    mapp=apply
    mper=boundary
    mcpx=0
    jlen=jmax-jmin+1
    mlen=2*jlen
    nlen=2*mord
    allocate (tmp1(mlen), tmp2(mlen), cof1(nlen), cof2(nlen))
    do k=kmin,kmax
      do i=imin,imax
        do j=jmin,jmax
          tmp1(j-jmin+1)=raw(i, j, k)
        end do
        call shpiro(tmp2, tmp1, cof2, cof1, jlen, mord, mapp, mper, mcpx)
        do j=jmin,jmax
          out(i, j, k)=tmp2(j-jmin+1)
        end do
      end do
    end do
    deallocate (tmp1, tmp2, cof1, cof2)
  else if(idim /= 1 .and. jdim /= 1 .and. kdim == 1)then
    mord=2**abs(order)
    mapp=apply
    mper=boundary
    mcpx=0
    klen=kmax-kmin+1
    mlen=2*klen
    nlen=2*mord
    allocate (tmp1(mlen), tmp2(mlen), cof1(nlen), cof2(nlen))
    do j=jmin,jmax
      do i=imin,imax
        do k=kmin,kmax
          tmp1(k-kmin+1)=raw(i, j, k)
        end do
        call shpiro(tmp2, tmp1, cof2, cof1, klen, mord, mapp, mper, mcpx)
        do k=kmin,kmax
          out(i, j, k)=tmp2(k-kmin+1)
        end do
      end do
    end do
    deallocate (tmp1, tmp2, cof1, cof2)
  end if
  return
end subroutine shapiro_f3d

        SUBROUTINE SHPIRO(SMO,DAT,COF,OLD,MDAT,MORD,MAPP,MPER,MCPX)
!  
!  SHAPIRO LOW PASS FILTER,
!  - OF ORDER MORD,
!  - APPLIED MAPP TIMES, 
!  - WITH PERIODIC (MPER>0), 
!         ZERO ( = INPUT BOUNDARY VALUES ARE UNCHANGED ) (MPER=0) 
!      OR ZERO GRADIENT (MPER<0) BOUNDARY CONDITIONS,
!  - FOR COMPLEX (MCPX=1) 
!     OR REAL (MCPX=/=1) INPUT DATA DAT.
!  THE CALLING PROGRAM MUST PROVIDE AT LEAST A 2*MDAT-DIMENSIONAL 
!  REAL OR A MDAT-DIMENSIONAL COMPLEX WORKSPACE SMO, WICH CONTAINS 
!  THE FILTERED DATA ON OUTPUT. ALSO THE INPUT DATA MUST BE DIMENSIONED 
!  AT LEAST WITH 2*MDAT REAL VALUES. ON OUTPUT DAT IS DESTROYED.
!  THE CALLING PROGRAM MUST PROVIDE AT LEAST TWO 2*MORD-DIMENSIONAL 
!  REAL OR MORD-DIMENSIONAL COMPLEX WORKSPACES COF AND OLD.
!  
        COMPLEX SCAL,COF(*),OLD(*),SMO(*),DAT(*)
        IF(MCPX.NE.1)CALL BITSHF(DAT,MDAT)
        DO 100 IDAT=1,MDAT
          SMO(IDAT)=DAT(IDAT)
100     CONTINUE
        IAPP=0
        SCAL=CMPLX(SQRT(.5))
110     IAPP=IAPP+1
        IORD=0
        MCOF=1
        COF(MCOF)=CMPLX(.5)
        GO TO 140
120     IORD=IORD+1
        MCOF=2**(IORD-1)
        SCAL=SCAL*CMPLX((-1.)**IORD)
        JCOF=0
        DO 130 ICOF=1,MCOF,2
          JCOF=JCOF+1
          COF(ICOF)=SCAL*CSQRT(OLD(JCOF))
          COF(ICOF+1)=-COF(ICOF)
130     CONTINUE
140     DO 150 ICOF=1,MCOF
          OLD(ICOF)=COF(ICOF)
150     CONTINUE
        DO 160 ICOF=1,MCOF
          DO 170 IDAT=1,MDAT
            DAT(IDAT)=SMO(IDAT)
170       CONTINUE
          CALL SMOOTH(SMO,MDAT,MPER,1)
          DO 180 IDAT=1,MDAT
            SMO(IDAT)=DAT(IDAT)+COF(ICOF)*SMO(IDAT)/2.
180       CONTINUE
160     CONTINUE
        IF(2*MCOF.LT.MORD)GO TO 120
        IF(IAPP.LT.MAPP)GO TO 110
        IF(MCPX.NE.1)CALL BITSHF(SMO,-MDAT)
        RETURN
        END
        SUBROUTINE SMOOTH(DATA,NDAT,IPER,ICPX)
!  
!  REPLACES A REAL OR COMPLEX DATA STRING BY IT'S SMOOTHED VALUE. 
!  THE CALLING PROGRAM MUST PROVIDE AT LEAST WORKSPACE OF 2*NDAT REAL VALUES.
!  CENTERED THREE-POINT SMOOTHER, WEIGHTED LIKE: DATA(N-1)-2*DATA(N)+DATA(N+1). 
!  FOR ICPX=1 DATA IS ASSUMED TO BE COMPLEX, 
!             ELSE THE DATA POINTS ARE SHIFTED INTO A COMPLEX ARRAY.
!  FOR IPER>0 PERIODIC DIFFERENCING IS DONE AT THE BOUNDARIES, 
!      IPER=0 ZERO BOUNDARY CONDITIONS ARE ASSUMED, 
!      IPER<0 ZERO GRADIENTS AT THE BOUNDARIES ARE PERFORMED.
!  
        COMPLEX BOUND,DATA(*)
        NDATM1=NDAT-1
        IF(ICPX.NE.1)CALL BITSHF(DATA,NDAT)
        BOUND=DATA(1)
        DO 100 N=1,NDATM1
          DATA(N)=DATA(N+1)-DATA(N)
100     CONTINUE
        DATA(NDAT)=BOUND-DATA(NDAT)
        BOUND=DATA(NDAT)
        DATA(NDAT)=DATA(NDAT)-DATA(NDATM1)
        IF(IPER.EQ.0)DATA(NDAT)=0.
        DO 110 N=NDATM1,2,-1
          DATA(N)=DATA(N)-DATA(N-1)
110     CONTINUE
        IF(IPER.EQ.0)BOUND=DATA(1)
        DATA(1)=DATA(1)-BOUND
        IF(IPER.LT.0)THEN
          DATA(1)=DATA(2)
          DATA(NDAT)=DATA(NDATM1)
        END IF
        IF(ICPX.NE.1)CALL BITSHF(DATA,-NDAT)
        RETURN
        END
        SUBROUTINE BITSHF(DATA,NDAT)
!  
!  NDAT>0: SHIFT THE FIRST NDAT BITS FROM A REAL DATA ARRAY 
!          INTO THE FIRST NDAT REAL PARTS OF A COMPLEX ARRAY.
!  NDAT<0: SHIFT THE BITS FROM THE FIRST NDAT REAL PARTS OF A COMPLEX 
!          DATA ARRAY INTO THE FIRST NDAT BITS OF A REAL ARRAY.
!  THE INPUT ARRAY MUST BE DIMENSIONED AT LEAST WITH 2*NDAT BITS.
!  
        COMPLEX DATA(*)
        IF(NDAT.LT.0)GO TO 120
        IF(REAL(NDAT)/2..EQ.REAL(INT(REAL(NDAT)/2.)))THEN
          DO 100 N=NDAT,2,-2
            I=N/2
            DATA(N)=CMPLX(AIMAG(DATA(I)),0.)
            DATA(N-1)=CMPLX(REAL(DATA(I)),0.)
100       CONTINUE
        ELSE
          DO 110 N=NDAT,3,-2
            I=(N-1)/2+1
            DATA(N)=CMPLX(REAL(DATA(I)),0.)
            DATA(N-1)=CMPLX(AIMAG(DATA(I-1)),0.)
110       CONTINUE
          DATA(1)=CMPLX(REAL(DATA(1)),0.)
        END IF
        RETURN
120     JM=-NDAT
        IM=INT(REAL(JM)/2.)
        IF(REAL(JM)/2..NE.REAL(INT(REAL(JM)/2.)))IM=IM+1
        DO 130 I=1,IM
          N=I*2
          DATA(I)=CMPLX(REAL(DATA(N-1)),REAL(DATA(N)))
130     CONTINUE
        RETURN
        END
