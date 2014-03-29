module spline

  implicit none

contains

  subroutine points(x, y, n, coef_a, coef_b, coef_c, coef_d)

    use solver

    implicit none

    integer, intent(in) :: n
    real, intent(in), dimension(n) :: x
    real, intent(in), dimension(n) :: y

    real, intent(out), dimension(n-1) :: coef_a
    real, intent(out), dimension(n-1) :: coef_b
    real, intent(out), dimension(n-1) :: coef_c
    real, intent(out), dimension(n-1) :: coef_d

    real, dimension(n-2) :: b   ! diagonal elements of matrix
    real, dimension(n-2) :: d   ! rhs of equation
    real, dimension(n-1) :: h   ! interval lengths (eg x1-x0)
    real, dimension(n)   :: S   ! spline second derivatives at data points
    real, dimension(n-3) :: cp  ! work array of size n-3
    real, dimension(n-2) :: dp  ! work array of size n-2

    integer ierror
    integer i

    h = x(2:n) - x(1:n-1)
    b = 2.0 * (h(1:n-2) + h(2:n-1))
    d = 6.0 * ( (y(3:n) - y(2:n-1))/h(2:n-1) - (y(2:n-1)-y(1:n-2))/h(1:n-2) )

    S(1) = 0.0
    S(n) = 0.0
    call tdma(od_lower=h(2:n-2), diag=b, od_upper=h(2:n-2), rhs=d, unk=S(2:n-1), worka=cp, workb=dp, n=n-2)

    coef_a = (S(2:n) - S(1:n-1)) / (6.0 * h)
    coef_b = S(1:n-1) / 2.0
    coef_c = ( (y(2:n) - y(1:n-1))/h ) - ( (2*h*S(1:n-1) + h*S(2:n))/6.0 )
    coef_d = y(1:n-1)

    if (.FALSE.) then
       write (*,*) 'x'
       do i=1,n
          write (*,*) i,x(i)
       end do

       write (*,*) 'h'
       do i=1,n-1
          write(*,*) i,h(i)
       end do

       write (*,*) 'b and d'
       do i=1,n-1
          write (*,*) i, b(i), d(i)
       end do

       write (*,*) 'off diagonal'
       do i=2,n-2
          write(*,*) i, h(i)
       end do

       write (*,*) 'spline coefficients'
       do i=1,n-1
          write(*,*) i,coef_a(i), coef_b(i), coef_c(i), coef_d(i)
       end do
    end if

  end subroutine points

  integer function interval(abscissa, x, n)

    implicit none

    integer, intent(in) :: n

    real, intent(in) :: abscissa
    real, intent(in), dimension(n) :: x

    integer :: upper
    integer :: lower
    integer :: tmp

    interval = 0
    lower = 1
    upper = n
    tmp = 1

    do
       if ((upper - lower) .eq. 1) exit
       tmp = lower + (upper-lower)/2
       if (x(tmp) .gt. abscissa) then
          upper = tmp
       else
          lower = tmp
       end if
    end do

    interval = lower

    return

  end function interval

  real function evaluate(abscissa, x, n, hind, coef_a, coef_b, coef_c, coef_d)

    implicit none

    real, intent(in)               :: abscissa
    real, intent(in), dimension(n) :: x

    integer, intent(in) :: n

    integer, intent(inout) :: hind

    real, intent(in), dimension(n-1) :: coef_a
    real, intent(in), dimension(n-1) :: coef_b
    real, intent(in), dimension(n-1) :: coef_c
    real, intent(in), dimension(n-1) :: coef_d

    integer :: ind

    evaluate = 0.0

    if ( (hind .lt. 1) .or. (hind .ge. n)) then
       ind = interval(abscissa=abscissa, x=x, n=n)
       hind = ind
    else if ( (x(hind) .le. abscissa) .and. (x(hind+1) .gt. abscissa) ) then
       ind = hind
    else
       ind = interval(abscissa=abscissa, x=x, n=n)
       hind = ind
    endif

    evaluate = &
         coef_a(ind) * (abscissa-x(ind))**3 + &
         coef_b(ind) * (abscissa-x(ind))**2 + &
         coef_c(ind) * (abscissa-x(ind)) + &
         coef_d(ind)

    return

  end function evaluate

end module spline
