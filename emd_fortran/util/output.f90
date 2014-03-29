module output

  implicit none

contains

  subroutine output_columns(fname, a, b, c, d, e, n)

    implicit none

    integer, parameter :: lu=18

    character (len=*), intent(in), optional     :: fname
    integer, intent(in)            :: n
    real, intent(in), dimension(n) :: a
    real, intent(in), dimension(n) :: b
    real, intent(in), dimension(n) :: c
    real, intent(in), dimension(n) :: d
    real, intent(in), dimension(n) :: e

    integer :: i
    integer :: ierror

    open(unit=lu, file=fname, status='REPLACE', action='WRITE', iostat=ierror)

    do i=1,n,1
       write(lu,100) i, a(i), b(i), c(i), d(i), e(i)
    end do

    close(unit=lu)

100 format (1x,I5,:,10(',',X,F5.2))

  end subroutine output_columns

  subroutine output_series(fname, x, y, n, extra)

    implicit none

    integer, parameter :: lu = 18
    character (len=*), intent(in), optional     :: fname
    integer, intent(in)               :: n
    real, intent(in),dimension(n)     :: x
    real, intent(in), dimension(n)    :: y

    real, intent(in), dimension(:, :), optional :: extra

    integer, dimension(2) :: extra_shape

    integer :: i = 0
    integer :: ierror = 0

    extra_shape = shape(extra)
    write(*,*) 'extra',extra_shape(1),extra_shape(2)

    if (present(fname)) then
       open(unit=lu, file=fname, status='REPLACE', action='WRITE', iostat=ierror)
    end if

    if (present(fname)) then
       if (present(extra)) then
          do i=1,n,1
             write(lu, 1000) i,x(i), y(i), extra(i,:)
          end do
       else
          do i=1,n,1
             write(lu, 1000) i,x(i), y(i)
          end do
       endif
    else
       if (present(extra)) then
          do i=1,n,1
             write(*, 1000) i,x(i), y(i), extra(i,:)
          end do
       else
          do i=1,n,1
             write(*, 1000) i,x(i), y(i)
          end do
       endif
    endif

    if (present(fname)) then
       close(unit=lu)
    end if

1000 format (1x,I5,:,10(',',X,F5.2))

  end subroutine output_series

  subroutine print_series(x, y, n)

    implicit none

    integer, intent(in)            :: n
    real, intent(in), dimension(n) :: x
    real, intent(in), dimension(n) :: y

    integer :: i

    do i=1,n,1
       write(*,*) i, x(i), y(i)
    end do

  end subroutine print_series

end module output
