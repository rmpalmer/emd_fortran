module solver

  implicit none

contains

  subroutine tdma(od_lower, diag, od_upper, rhs, unk, worka, workb, n)

    ! http://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm

    implicit none

    integer, intent(in) :: n
    real, intent(in),  dimension(n-1) :: od_lower
    real, intent(in),  dimension(n)   :: diag
    real, intent(in),  dimension(n-1) :: od_upper
    real, intent(in),  dimension(n)   :: rhs

    real, intent(out), dimension(n-1) :: worka
    real, intent(out), dimension(n)   :: workb
    real, intent(out), dimension(n)   :: unk

    real factor
    integer i

    worka = od_upper
    workb = rhs
    worka(1) = worka(1)/diag(1)
    workb(1) = workb(1)/diag(1)

    do i=2,n
       factor = 1.0 / (diag(i) - worka(i-1)*od_lower(i-1))
       worka(i) = worka(i)* factor
       workb(i) = (workb(i) - workb(i-1)*od_lower(i-1)) * factor
    end do

    unk(n) = workb(n)
    do i=n-1,1,-1
       unk(i) = workb(i) - worka(i)*unk(i+1)
    end do

  end subroutine tdma

end module solver
