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
!@-others

end module
!@-node:gcross.20091105135203.1510:@thin contractors.f95
!@-leo
