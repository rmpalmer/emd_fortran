module sum
    implicit none

contains
  subroutine sum_vector (data, n, answer)
  implicit none
  integer, dimension(1), intent(in) :: data
  integer, intent(in) :: n
  integer, intent(out) :: answer
  integer i

  answer = 0
  do i=1, n, 1
     answer = answer + data(i)
  end do

  end subroutine sum_vector

end module sum
