module interp

  implicit none

contains

  real function tricub(x, y, abscissa)

    implicit none

    real, dimension(6), intent(in) :: x
    real, dimension(6), intent(in) :: y
    real, intent(in)  :: abscissa

    tricub = ( &
         cubic(x(1:4), y(1:4), abscissa) + &
         cubic(x(2:5), y(2:5), abscissa) + &
         cubic(x(3:6), y(3:6), abscissa) ) / 3.0
    return

  end function tricub

  real function cubic(x, y, abscissa)

    implicit none

    real, dimension(4), intent(in) :: x
    real, dimension(4), intent(in) :: y
    real, intent(in)  :: abscissa

    cubic = &
         y(1) * ( (abscissa-x(2)) * (abscissa-x(3)) * (abscissa-x(4)) ) / ( (x(1)-x(2)) * (x(1)-x(3)) * (x(1)-x(4)) ) + &
         y(2) * ( (abscissa-x(1)) * (abscissa-x(3)) * (abscissa-x(4)) ) / ( (x(2)-x(1)) * (x(2)-x(3)) * (x(2)-x(4)) ) + &
         y(3) * ( (abscissa-x(1)) * (abscissa-x(2)) * (abscissa-x(4)) ) / ( (x(3)-x(1)) * (x(3)-x(2)) * (x(3)-x(4)) ) + &
         y(4) * ( (abscissa-x(1)) * (abscissa-x(2)) * (abscissa-x(3)) ) / ( (x(4)-x(1)) * (x(4)-x(2)) * (x(4)-x(3)) )

    return

  end function cubic

  real function quadratic(x, y, abscissa)

    implicit none

    real, dimension(3), intent(in) :: x
    real, dimension(3), intent(in) :: y
    real, intent(in)  :: abscissa

    quadratic = &
         y(1) * ( (abscissa-x(2)) * (abscissa-x(3)) ) / ( (x(1)-x(2)) * (x(1)-x(3)) ) + &
         y(2) * ( (abscissa-x(1)) * (abscissa-x(3)) ) / ( (x(2)-x(1)) * (x(2)-x(3)) ) + &
         y(3) * ( (abscissa-x(1)) * (abscissa-x(2)) ) / ( (x(3)-x(1)) * (x(3)-x(2)) )

    return

  end function quadratic

  real function linear(x, y, abscissa)

    implicit none

    real, dimension(2), intent(in) :: x
    real, dimension(2), intent(in) :: y
    real, intent(in)  :: abscissa

    linear = y(1) + (abscissa-x(1)) * ((y(2)-y(1))/(x(2)-x(1)))
    return

  end function linear

end module interp
