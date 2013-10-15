integer function get_km()
! Makes vertical resolution KM (specified in Makefile) available to Python

  get_km = KM
  
end function get_km

integer function get_jm()
! Makes latitude resolution JM (specified in Makefile) available to Python

  get_jm = JM

end function get_jm

integer function get_im()
! Makes longitude resolution IM (specified in Makefile) available to Python

  get_im = IM

end function get_im
