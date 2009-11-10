!@+leo-ver=4-thin
!@+node:gcross.20091105135203.1510:@thin contractors.f95
!@@language fortran90
!@@tabwidth -2

module contractors

  implicit none

contains

!@+others
!@+node:gcross.20091106154604.1512:iteration_stage_1
subroutine iteration_stage_1( &
  b, & ! state bandwidth dimension
  c, & ! operator bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
  iteration_stage_1_tensor &
)
  integer, intent(in) :: b, c, d, number_of_matrices, sparse_operator_indices(2,number_of_matrices)
  double complex, intent(in) :: left_environment(b,b,c), sparse_operator_matrices(d,d,number_of_matrices)
  double complex, intent(out) :: iteration_stage_1_tensor(b,d,c,b,d)

  integer :: index, i, j, k1, k2
  double complex :: matrix(d,d)

  iteration_stage_1_tensor = 0

  do index = 1, number_of_matrices
    k1 = sparse_operator_indices(1,index)
    k2 = sparse_operator_indices(2,index)
    matrix  = sparse_operator_matrices(:,:,index)
    do j=1,b
      do i=1,b
        iteration_stage_1_tensor(i,:,k2,j,:) = iteration_stage_1_tensor(i,:,k2,j,:) + matrix(:,:)*left_environment(i,j,k1)
      end do
    end do
  end do

end subroutine
!@-node:gcross.20091106154604.1512:iteration_stage_1
!@+node:gcross.20091107163338.1529:iteration_stage_2
subroutine iteration_stage_2( &
  bl, & ! state left bandwidth dimension
  br, & ! state right bandwidth dimension
  c, &  ! operator bandwidth dimension
  d, &  ! physical dimension
  iteration_stage_1_tensor, &
  state_site_tensor, &
  iteration_stage_2_tensor &
)
  integer, intent(in) :: bl, br, c, d
  double complex, intent(in) :: state_site_tensor(br,bl,d), iteration_stage_1_tensor(bl,d,c,bl,d)
  double complex, intent(out) :: iteration_stage_2_tensor(br,c,bl,d)

  external :: zgemm

  call zgemm( &
      'N','N', &
      br,c*bl*d,bl*d, &
      (1d0,0d0), &
      state_site_tensor, br, &
      iteration_stage_1_tensor, bl*d, &
      (0d0,0d0), &
      iteration_stage_2_tensor, br &
  )

end subroutine
!@-node:gcross.20091107163338.1529:iteration_stage_2
!@+node:gcross.20091110011014.1551:iteration_stage_3
subroutine iteration_stage_3( &
  bl, & ! state left bandwidth dimension
  br, & ! state right bandwidth dimension
  c, &  ! operator bandwidth dimension
  d, &  ! physical dimension
  iteration_stage_2_tensor, &
  right_environment, &
  output_state_site_tensor &
)
  integer, intent(in) :: bl, br, c, d
  double complex, intent(in) :: right_environment(br,br,c), iteration_stage_2_tensor(br,c,bl,d)
  double complex, intent(out) :: output_state_site_tensor(br,bl,d)

  external :: zgemm

  call zgemm( &
      'N','N', &
      br, bl*d, br*c, &
      (1d0,0d0), &
      right_environment, br, &
      iteration_stage_2_tensor, br*c, &
      (0d0,0d0), &
      output_state_site_tensor, br &
  )

end subroutine
!@-node:gcross.20091110011014.1551:iteration_stage_3
!@+node:gcross.20091110011014.1549:contract_sos_left
subroutine contract_sos_left( &
  bl, & ! state left bandwidth dimension
  br, & ! state right bandwidth dimension
  c, &  ! operator bandwidth dimension
  d, &  ! physical dimension
  left_environment, &
  number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
  state_site_tensor, &
  new_left_environment &
)
  integer, intent(in) :: bl, br, c, d, number_of_matrices, sparse_operator_indices(2,number_of_matrices)
  double complex, intent(in) :: &
    left_environment(bl,bl,c), &
    state_site_tensor(br,bl,d), &
    sparse_operator_matrices(d,d,number_of_matrices)
  double complex, intent(out) :: new_left_environment(br,br,c)

  double complex :: &
    iteration_stage_1_tensor(bl,d,c,bl,d), &
    iteration_stage_2_tensor(br,c,bl,d), &
    iteration_stage_3_tensor(br,c,br)

  external :: zgemm

  ! Stage 1
  call iteration_stage_1( &
    bl, c, d, &
    left_environment, &
    number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
    iteration_stage_1_tensor &
  )
  ! Stage 2
  call iteration_stage_2( &
    bl, br, c, d, &
    iteration_stage_1_tensor, &
    state_site_tensor, &
    iteration_stage_2_tensor &
  )
  ! Stage 3
  call zgemm( &
      'N','C', &
      br*c,br,bl*d, &
      (1d0,0d0), &
      iteration_stage_2_tensor, br*c, &
      state_site_tensor, br, &
      (0d0,0d0), &
      iteration_stage_3_tensor, br*c &
  )
  ! Stage 4
  new_left_environment = reshape(iteration_stage_3_tensor,shape(new_left_environment),order=(/1,3,2/))

end subroutine
!@-node:gcross.20091110011014.1549:contract_sos_left
!@-others

end module
!@-node:gcross.20091105135203.1510:@thin contractors.f95
!@-leo
