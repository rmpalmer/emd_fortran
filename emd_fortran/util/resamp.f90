module resamp
    implicit none

contains

  subroutine resample(x, y, samcnt, n, new)

    integer, intent(in)                      :: samcnt
    real, intent(in), dimension(samcnt)      :: x
    real, intent(in), dimension(samcnt)      :: y
    integer, intent(in)                      :: n
    real, intent(out), dimension(n)          :: new

    integer      :: i

    if (samcnt .ge. 4) then

       call resamp_spline(x=x, y=y, samcnt=samcnt, n=n, new=new)

    else if (samcnt .eq. 3) then

       call resamp_quadratic(x=x, y=y, n=n, new=new)

    else if (samcnt .eq. 2) then

       call resamp_linear(x=x, y=y, n=n, new=new)

    else if (samcnt .eq. 1) then

       write (*,*) 'resamp constant'
       forall (i=1:n)
          new(i) = y(1)
       end forall

    end if

  end subroutine resample

  subroutine resamp_quadratic(x, y, n, new)

    use interp

    real, intent(in), dimension(3)    :: x
    real, intent(in), dimension(3)    :: y
    integer, intent(in)               :: n
    real, intent(out), dimension(n)   :: new

    integer i

    write(*,*) 'resamp_quadratic'

    i = 1
    do
       if (i .gt. n) exit
       new(i) = quadratic(x=x, y=y, abscissa=real(i))
       i = i+1
    end do

  end subroutine resamp_quadratic

  subroutine resamp_linear(x, y, n, new)

    use interp

    real, intent(in), dimension(2)   :: x
    real, intent(in), dimension(2)   :: y
    integer, intent(in)              :: n
    real, intent(out), dimension(n)  :: new

    integer i

    write(*,*) 'resamp_linear'

    i = 1
    do
       if (i .gt. n) exit
       new(i) = linear(x=x, y=y, abscissa=real(i))
       i = i+1
    end do

  end subroutine resamp_linear

  subroutine resamp_spline(x, y, samcnt, n, new)

    use interp
    use spline

    integer, intent(in)                  :: samcnt
    real, intent(in), dimension(samcnt)  :: x
    real, intent(in), dimension(samcnt)  :: y
    integer, intent(in)                  :: n
    real, intent(out), dimension(n)      :: new

    real, allocatable :: coef_a(:)
    real, allocatable :: coef_b(:)
    real, allocatable :: coef_c(:)
    real, allocatable :: coef_d(:)

    real abscissa

    integer i
    integer hind
    integer ierror
    integer cnt

    ! write(*,*) 'resamp_spline'

    ! find spline coefficients
    allocate(coef_a(samcnt - 1), stat=ierror)
    allocate(coef_b(samcnt - 1), stat=ierror)
    allocate(coef_c(samcnt - 1), stat=ierror)
    allocate(coef_d(samcnt - 1), stat=ierror)
    call points(x=x, y=y, n=samcnt, &
         coef_a=coef_a, coef_b=coef_b, coef_c=coef_c, coef_d=coef_d)


    ! initial interval (guess)
    hind = 0
    i = 1
    do
       ! do not go beyond the number of samples requested
       if (i .gt. n) exit

       ! evaluate spline coefficients
       ! note that old data x array is needed here
       new(i) = evaluate(abscissa=real(i), x=x, n=samcnt, hind=hind, &
            coef_a=coef_a, coef_b=coef_b, coef_c=coef_c, coef_d=coef_d)
       i = i + 1
    end do

    deallocate(coef_a, stat=ierror)
    deallocate(coef_b, stat=ierror)
    deallocate(coef_c, stat=ierror)
    deallocate(coef_d, stat=ierror)

  end subroutine resamp_spline


end module resamp
