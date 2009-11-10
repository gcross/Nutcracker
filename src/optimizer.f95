!@+leo-ver=4-thin
!@+node:gcross.20091109182552.1535:@thin optimizer.f95
!@@language fortran90
!@@tabwidth -2

module optimizer

  use contractors

  implicit none

interface
  !@  << Interface >>
  !@+node:gcross.20091109182634.1538:<< Interface >>
  !@+others
  !@+node:gcross.20091109182634.1540:znaupd
  subroutine znaupd &
      ( ido, bmat, n, which, nev, tol, resid, ncv, v, ldv, iparam, &
        ipntr, workd, workl, lworkl, rwork, info )
      character :: bmat*1, which*2
      integer :: ido, info, ldv, lworkl, n, ncv, nev
      double precision :: tol
      integer :: iparam(11), ipntr(14)
      double complex :: resid(n), v(ldv,ncv), workd(3*n), workl(lworkl)
      double precision :: rwork(ncv)
  end subroutine
  !@nonl
  !@-node:gcross.20091109182634.1540:znaupd
  !@+node:gcross.20091109182634.1539:zneupd
  subroutine zneupd (rvec, howmny, select, d, z, ldz, sigma, &
                     workev, bmat, n, which, nev, tol, &
                     resid, ncv, v, ldv, iparam, ipntr, workd,  &
                     workl, lworkl, rwork, info)
        character  :: bmat, howmny, which*2
        logical    :: rvec
        integer    :: info, ldz, ldv, lworkl, n, ncv, nev
        double complex :: sigma
        Double precision :: tol
        integer    :: iparam(11), ipntr(14)
        logical    :: select(ncv)
        double precision :: rwork(ncv)
        double complex :: &
                  d(nev), resid(n), v(ldv,ncv), z(ldz, nev), &
                  workd(3*n), workl(lworkl), workev(2*ncv)

  end subroutine
  !@-node:gcross.20091109182634.1539:zneupd
  !@-others
  !@-node:gcross.20091109182634.1538:<< Interface >>
  !@nl
end interface

contains

!@+others
!@+node:gcross.20091109182634.1537:optimize
function optimize( &
  bl, br, & ! state bandwidth dimension
  c, & ! operator bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
  right_environment, &
  which, &
  tol, &
  number_of_iterations, &
  result &
) result (status)
  integer, intent(in) :: bl, br, c, d, number_of_matrices, sparse_operator_indices(2,number_of_matrices)
  integer, intent(inout) :: number_of_iterations
  double complex, intent(in) :: &
    left_environment(bl,bl,c), &
    right_environment(br,br,c), &
    sparse_operator_matrices(d,d,number_of_matrices)
  double complex, intent(inout) :: &
    result(br,bl,d)
  character, intent(in) :: which*2
  double precision, intent(in) :: tol

  integer, parameter :: nev = 1, ncv = 3

  integer :: status

  double complex :: &
    iteration_stage_1_tensor(bl,d,c,bl,d), &
    iteration_stage_2_tensor(br,c,bl,d), &
    v(br*bl*d,ncv), &
    workd(3*br*bl*d), &
    workl(3*ncv**2+5*ncv), &
    eigenvalues(nev+1), &
    workev(2*ncv)

  integer :: &
    iparam(11), &
    ipntr(14), &
    ido, &
    info

  logical :: &
    select(ncv)

  double precision :: rwork(ncv)

!@+at
! First do the stage 1 contraction, since it is independent of the state site 
! tensor.
!@-at
!@@c

  call iteration_stage_1( &
    bl, c, d, &
    left_environment, &
    number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
    iteration_stage_1_tensor &
  )

!@+at
! Set up the solver.
!@-at
!@@c

  iparam = 0
  ido = 0
  info = 1

  iparam(1) = 1
  iparam(3) = number_of_iterations
  iparam(7) = 1

!@+at
! Run the iteration.
!@-at
!@@c

  do while (ido /= 99)
    call znaupd ( &
      ido, 'I', d*bl*br, which, nev, tol, result, ncv, v, br*bl*d, &
      iparam, ipntr, workd, workl, 3*ncv**2+5*ncv, rwork, info &
    ) 
    if (ido == -1 .or. ido == 1) then
      call iteration_stage_2( &
        bl, br, c, d, &  ! physical dimension
        iteration_stage_1_tensor, &
        workd(ipntr(1)), &
        iteration_stage_2_tensor &
      )
      call iteration_stage_3( &
        bl, br, c, d, &  ! physical dimension
        iteration_stage_2_tensor, &
        right_environment, &
        workd(ipntr(2)) &
      )
    end if
  end do

  number_of_iterations = iparam(3)

  if( info < 0 ) then
    return
  end if

!@+at
! Post-processing
!@-at
!@@c

  call zneupd (.true.,'A', select, eigenvalues, v, br*bl*d, (0d0,0d0), workev, &
              'I', d*bl*br, which, nev, tol, result, ncv, &
              v, br*bl*d, iparam, ipntr, workd, workl, 3*ncv**2+5*ncv, &
              rwork, info)

  result = reshape(v(:,1),shape(result))

  status = info

end function
!@-node:gcross.20091109182634.1537:optimize
!@-others

end module
!@-node:gcross.20091109182552.1535:@thin optimizer.f95
!@-leo
