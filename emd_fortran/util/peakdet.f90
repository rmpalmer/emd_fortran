module peakdet
  implicit none

contains

  subroutine peaks(data, n &
       ,min_vals, min_locs, max_vals, max_locs &
       ,mncnt, mxcnt &
       ,sens, ends_are_peaks)

    implicit none

    integer, intent(in)             :: n
    real, intent(in), dimension(n)  :: data
    real, intent(out), dimension(n) :: min_vals
    real, intent(out), dimension(n) :: min_locs
    real, intent(out), dimension(n) :: max_vals
    real, intent(out), dimension(n) :: max_locs

    integer, intent(out)            :: mncnt
    integer, intent(out)            :: mxcnt

    real, intent(in), optional :: sens
    logical, intent(in), optional :: ends_are_peaks

    integer :: i = 1
    logical :: lookformax = .TRUE.
    real    :: val = 0.0
    real    :: mn =  0.0
    real    :: mx =  0.0
    real    :: delta = 1.0
    real    :: frac = 0.25
    integer :: mnpos = 1
    integer :: mxpos = 1
    integer :: ierror = 0
    integer :: loop_min = 0
    integer :: loop_max = 0

    logical :: ends_are_peaks_flag

    ends_are_peaks_flag = .FALSE.

    if (present(sens)) frac = sens
    if (present(ends_are_peaks)) ends_are_peaks_flag = ends_are_peaks

    mncnt = 0
    mxcnt = 0

    mx = minval(data)
    mn = maxval(data)
    delta = abs(frac * (mx - mn))
    !write(*,*) 'peaks',mn,mx,dot_product(data(1:n), data(1:n))

    loop_min = 1
    loop_max = n

    if (ends_are_peaks_flag) then
       ! force first point to be used as local max and min
       max_locs(1) = real(1)
       max_vals(1) = data(1)
       mxcnt       = 1
       mx          = data(1)
       mxpos       = 1

       min_locs(1) = real(1)
       min_vals(1) = data(1)
       mncnt       = 1
       mn          = data(1)
       mnpos       = 1

       !write(*,1001) 'maxima',mxpos,real(mxpos),max_vals(mxcnt)
       !write(*,1001) 'minima',mnpos,real(mnpos),min_vals(mncnt)

       loop_min = loop_min + 1
       loop_max = loop_max - 1
    end if

    ! loop over all non-end points (first and last are automatically both min and max)
    do i=loop_min, loop_max ,1

       val = data(i)

       if (val > mx) then
          mx = val
          mxpos = i
       end if

       if (val < mn) then
          mn = val
          mnpos = i
       end if

       if (lookformax) then

          if (val < (mx-delta)) then
             mn = val
             mnpos = i
             lookformax = .FALSE.
             if (abs(max_locs(mxcnt)-real(mxpos)) .lt. 0.1) cycle
             mxcnt = mxcnt + 1
             max_locs(mxcnt) = real(mxpos)
             max_vals(mxcnt) = data(mxpos)
             !write(*,1000) 'maxima',mxpos,real(mxpos),max_vals(mxcnt)
          end if

       else

          if (val > (mn+delta)) then
             mx = val
             mxpos = i
             lookformax = .TRUE.
             if (abs(min_locs(mncnt)-real(mnpos)) .lt. 0.1) cycle
             mncnt = mncnt + 1
             min_locs(mncnt) = real(mnpos)
             min_vals(mncnt) = data(mnpos)
             !write(*,1000) 'minima',mnpos,real(mnpos),min_vals(mncnt)
          end if

       end if

    end do

    if (ends_are_peaks_flag) then

       ! force last point to be used as local max and min
       if (abs(max_locs(mxcnt)-real(n)) .gt. 0.1) then
          mxcnt = mxcnt + 1
          max_locs(mxcnt) = real(n)
          max_vals(mxcnt) = data(n)
       endif

       if (abs(min_locs(mncnt)-real(n)) .gt. 0.1) then
          mncnt = mncnt + 1
          min_locs(mncnt) = real(n)
          min_vals(mncnt) = data(n)
       endif

       !write(*,1001) 'maxima',mxpos,real(mxpos),max_vals(mxcnt)
       !write(*,1001) 'minima',mnpos,real(mnpos),min_vals(mncnt)

    end if

1000 format(1x,'Found ',A,' at ',I6,' ',F5.2,' ',F5.2)
1001 format(1x,'Declared ',A,' at ',I6,' ',F5.2,' ',F5.2)

  end subroutine peaks


end module peakdet
