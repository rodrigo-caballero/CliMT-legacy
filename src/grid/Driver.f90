integer function get_nlev()
! Makes vertical resolution KM (specified in Makefile) available to Python

  integer, external :: get_km
  get_nlev = get_km()

end function get_nlev

integer function get_nlat()
! Makes latitude resolution JM (specified in Makefile) available to Python

  integer, external :: get_jm
  get_nlat = get_jm()

end function get_nlat

integer function get_nlon()
! Makes longitude resolution IM (specified in Makefile) available to Python

  integer, external :: get_im
  get_nlon = get_im()

end function get_nlon
