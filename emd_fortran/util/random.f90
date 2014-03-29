module random
    implicit none

  contains

    real function exponential(lambda)

      implicit none

      real, optional     :: lambda

      real               :: x
      real               :: l = 1.0

      if (present(lambda)) l = lambda

      call random_number(x)

      exponential = -1.0 * log(x)/ l

      return

    end function exponential

    real function normal(mean, std_dev)

      implicit none

      real, optional     :: mean
      real, optional     :: std_dev

      real, external     :: spmini

      real, dimension(2) :: x
      real               :: r
      real               :: thresh
      real               :: y

      real               :: s = 1.0
      real               :: m = 0.0

      if (present(mean))    m = mean
      if (present(std_dev)) s = std_dev

      thresh = 0.0001

      do

         call random_number(x)
         x = 2.0 * x - 1.0
         r = dot_product(x, x)
         if (r .le. 1.0) exit

      end do

      if (r .le. thresh) then
         y = 0.0
      else
         y = x(1) * sqrt(-2.0 * log(r) / r)
      end if

      normal = s * y + m

      return

    end function normal


end module random
