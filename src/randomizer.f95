!@+leo-ver=4-thin
!@+node:gcross.20091110205054.1919:@thin randomizer.f95
!@@language fortran90
!@@tabwidth -2

module randomizer

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
!@+node:gcross.20091110205054.1921:seed_randomizer
subroutine seed_randomizer(seed)
  integer, intent(in) :: seed
  call srand(seed)
end subroutine
!@-node:gcross.20091110205054.1921:seed_randomizer
!@+node:gcross.20091110205054.1920:randomize_state_site_tensor
subroutine randomize_state_site_tensor(br, bl, d, state_site_tensor)
  integer, intent(in) :: br, bl, d
  double complex, intent(out) :: state_site_tensor(br,bl,d)

  integer :: i, j, k

  do i = 1, br
  do j = 1, bl
  do k = 1, d
    state_site_tensor(i,j,k) = rand()*(1d0,0d0) + rand()*(0d0,1d0)
  end do
  end do
  end do
end subroutine
!@-node:gcross.20091110205054.1920:randomize_state_site_tensor
!@+node:gcross.20091110205054.1922:rand_norm_state_site_tensor
function rand_norm_state_site_tensor(bl, br, d, state_site_tensor) result (info)
  integer, intent(in) :: bl, br, d
  double complex, intent(out) :: state_site_tensor(br,bl,d)

  double complex :: u(bl,bl), vt(bl,br*d)
  double precision :: s(bl)
  integer :: info

  double complex :: normalized_state_site_tensor(bl,br*d)

  external :: zgemm

  if (br*d < bl) then
    print *, "Not enough degrees of freedom to normalize."
    print *, br*d, "<", bl
    stop
  end if

  call randomize_state_site_tensor(bl, br, d, normalized_state_site_tensor)

  info = svd(bl,br*d,bl,normalized_state_site_tensor,u,s,vt)

  call zgemm( &
    'N','N', &
    bl,br*d,bl, &
    (1d0,0d0), &
    u, bl, &
    vt, bl, &
    (0d0,0d0), &
    normalized_state_site_tensor, bl &
  )

  state_site_tensor = reshape(normalized_state_site_tensor,shape(state_site_tensor),order=(/2,1,3/))

end function
!@-node:gcross.20091110205054.1922:rand_norm_state_site_tensor
!@-others

end module
!@-node:gcross.20091110205054.1919:@thin randomizer.f95
!@-leo
