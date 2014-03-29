module input

  implicit none

contains

  subroutine input_series(fname, x, y, n)

    use emd_types

    implicit none

    integer, parameter :: lu = 18

    character (len=*), intent(in)  :: fname
    real, allocatable, intent(out) :: x(:)
    real, allocatable, intent(out) :: y(:)
    integer, intent(out)           :: n

    integer :: ierror = 0
    real :: abscissa = 0.0
    real :: ordinate = 0.0

    n = 0

    ! open file
    open (unit=lu, file=fname, status='OLD', iostat=ierror)

    errorcheck: if (ierror > 0) then

       write(*,1020) fname

    else

       ! count the number of values
       n = 0
       do
          read (lu,1025, iostat=ierror) abscissa, ordinate
          if ( ierror .ne. 0 ) exit
          n = n + 1
       end do
       write(*,1030) fname, n

       allocate(x(n), stat=ierror)
       allocate(y(n), stat=ierror)

       rewind(unit=lu)
       n = 0
       do
          read (lu, 1025, iostat=ierror) abscissa, ordinate
          if ( ierror .ne. 0 ) exit
          n = n + 1
          x(n) = abscissa
          y(n) = ordinate
       end do

       close(unit=lu)

    endif errorcheck

1020 format (1x,'ERROR: File ',A,' does not exist')
1025 format (F5.2,F5.2)
1030 format (1x,'File ',A,' has ',I8,' lines.')
1040 format (1x,'ERORR: Unable to allocate array')

  end subroutine input_series


end module input

