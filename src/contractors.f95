!@+leo-ver=4-thin
!@+node:gcross.20091105135203.1510:@thin contractors.f95
!@@language fortran90
!@@tabwidth -2

module contractors

  implicit none

contains

!@+others
!@+node:gcross.20091106154604.1512:pre_iteration
pure subroutine pre_iteration( &
  b, & ! state bandwidth dimension
  c, & ! operator bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
  output_tensor &
)
  integer, intent(in) :: b, c, d, number_of_matrices, sparse_operator_indices(2,number_of_matrices)
  double complex, intent(in) :: left_environment(b,b,c), sparse_operator_matrices(d,d,number_of_matrices)
  double complex, intent(out) :: output_tensor(b,d,c,b,d)

  integer :: index, i, j, k1, k2
  double complex :: matrix(d,d)

  output_tensor = 0

  do index = 1, number_of_matrices
    k1 = sparse_operator_indices(1,index)
    k2 = sparse_operator_indices(2,index)
    matrix  = sparse_operator_matrices(:,:,index)
    do j=1,b
      do i=1,b
        output_tensor(i,:,k2,j,:) = output_tensor(i,:,k2,j,:) + matrix(:,:)*left_environment(i,j,k1)
      end do
    end do
  end do

end subroutine
!@-node:gcross.20091106154604.1512:pre_iteration
!@+node:gcross.20091107163338.1529:iteration
subroutine iteration( &
  bl, & ! state left bandwidth dimension
  br, & ! state right bandwidth dimension
  c, &  ! operator bandwidth dimension
  d, &  ! physical dimension
  iteration_tensor, &
  state_site_tensor, &
  right_environment, &
  workspace, &
  output_state_site_tensor &
)
  integer, intent(in) :: bl, br, c, d
  double complex, intent(in) :: right_environment(br,br,c), state_site_tensor(br,bl,d), iteration_tensor(bl,d,c,bl,d)
  double complex, intent(out) :: workspace(br,c,bl,d), output_state_site_tensor(br,bl,d)

  external :: zgemm

  call zgemm( &
      'N','N', &
      br,c*bl*d,bl*d, &
      (1d0,0d0), &
      state_site_tensor(1,1,1), br, &
      iteration_tensor(1,1,1,1,1), bl*d, &
      (0d0,0d0), &
      workspace(1,1,1,1), br &
  )
  call zgemm( &
      'N','N', &
      br, bl*d, br*c, &
      (1d0,0d0), &
      right_environment(1,1,1), br, &
      workspace(1,1,1,1), br*c, &
      (0d0,0d0), &
      output_state_site_tensor(1,1,1), br &
  )

end subroutine
!@-node:gcross.20091107163338.1529:iteration
!@-others

end module
!@-node:gcross.20091105135203.1510:@thin contractors.f95
!@-leo
