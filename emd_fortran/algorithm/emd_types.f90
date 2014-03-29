module emd_types

  implicit none

  type :: emd_parms

     integer :: max_count      = 10
     real    :: imf_thresh     = 0.005
     real    :: resid_thresh   = 0.01
     logical :: ends_are_peaks = .FALSE.
     real    :: peak_sens      = 0.25

  end type emd_parms

end module emd_types
