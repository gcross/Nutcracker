!@+leo-ver=4-thin
!@+node:gcross.20091110205054.1927:@thin utils.f95
!@@language fortran90
!@@tabwidth -2

module utils

  implicit none

contains

!@+others
!@+node:gcross.20091110205054.1929:svd
function svd( &
  n, m, rank, &
  a, &
  u, s, vt &
) result (info)
  integer, intent(in) :: N, M, rank
  double complex, intent(in) :: a(n,m)
  double complex, intent(out) :: u(n,rank), vt(rank,m)
  double precision :: s(rank)

  double complex, allocatable :: work(:)
  integer :: iwork(8*rank)
  double precision :: rwork(5*rank*rank + 5*rank)
  double complex :: optimal_lwork
  integer :: lwork, info

  external :: zgesdd

  lwork = -1

  call zgesdd( &
    'S', n, m, &
    a, n, &
    s, &
    u, n, &
    vt, rank, &
    optimal_lwork, lwork, &
    rwork, &
    iwork, &
    info &
  )

  lwork = floor(real(optimal_lwork))

  allocate(work(lwork))

  call zgesdd( &
    'S', n, m, &
    a, n, &
    s, &
    u, n, &
    vt, rank, &
    work, lwork, &
    rwork, &
    iwork, &
    info &
  )

  deallocate(work)

end function
!@nonl
!@-node:gcross.20091110205054.1929:svd
!@-others

end module
!@-node:gcross.20091110205054.1927:@thin utils.f95
!@-leo
