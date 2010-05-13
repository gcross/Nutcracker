!@+leo-ver=4-thin
!@+node:gcross.20091110205054.1939:@thin core.f95
!@@language fortran90
!@@tabwidth -2

!@+others
!@+node:gcross.20100512172859.1739:Utility Functions
!@+node:gcross.20091110205054.1929:mysvd
function mysvd ( &
  n, m, rank, &
  matrix, &
  u, s, vt &
) result (info)
  integer, intent(in) :: n, m, rank
  double complex, intent(in) :: matrix(n,m)
  double complex, intent(out) :: u(n,rank), vt(rank,m)
  double precision :: s(rank)

  double complex, allocatable :: work(:)
  integer :: iwork(8*rank)
  double precision :: rwork(5*rank*rank + 5*rank)
  double complex :: optimal_lwork, a(n,m)
  integer :: lwork, info

  external :: zgesdd

  lwork = -1

  a = matrix

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
!@-node:gcross.20091110205054.1929:mysvd
!@+node:gcross.20100512172859.1741:orthogonalize_matrix_in_place
subroutine orthogonalize_matrix_in_place( &
  m, n, &
  matrix &
)
  integer, intent(in) :: n, m
  double complex, intent(inout) :: matrix(m,n)

  integer :: jpvt(n), info, lwork
  double complex :: tau(n), lwork_as_complex
  double precision :: rwork(2*n)

  double complex, allocatable :: work(:)

  external :: zgeqp3, zungqr

  if (n >= m) then
    print *, "There are too many projectors and too few degrees of freedom!"
    stop
  end if

  lwork = -1
  call zgeqp3( &
    m, n, &
    matrix, m, &
    jpvt, &
    tau, &
    lwork_as_complex, lwork, &
    rwork, &
    info &
  )

  if (info /= 0) then
    print *, "Unable to factorize matrix (zgeqp3, workspace query); info =", info
  end if

  lwork = int(real(lwork_as_complex))
  allocate(work(lwork))
  call zgeqp3( &
    m, n, &
    matrix, m, &
    jpvt, &
    tau, &
    work, lwork, &
    rwork, &
    info &
  )

  if (info /= 0) then
    print *, "Unable to factorize matrix (zgeqp3); info =", info
  end if

  call zungqr( &
    m, n, n, &
    matrix, m, &
    tau, &
    work, lwork, &
    info &
  )

  if (info /= 0) then
    print *, "Unable to factorize matrix (zungqr); info =", info
  end if

  deallocate(work)

end subroutine
!@-node:gcross.20100512172859.1741:orthogonalize_matrix_in_place
!@-node:gcross.20100512172859.1739:Utility Functions
!@+node:gcross.20091110205054.1940:Contractors
!@+node:gcross.20091110205054.1910:Main iteration
!@+node:gcross.20091106154604.1512:iteration_stage_1
subroutine iteration_stage_1( &
  bl, & ! state bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
  iteration_stage_1_tensor &
)
  integer, intent(in) :: bl, cl, cr, d, number_of_matrices, sparse_operator_indices(2,number_of_matrices)
  double complex, intent(in) :: left_environment(bl,bl,cl), sparse_operator_matrices(d,d,number_of_matrices)
  double complex, intent(out) :: iteration_stage_1_tensor(bl,d,cr,bl,d)

  integer :: index, i, j, k1, k2
  double complex :: matrix(d,d)

  iteration_stage_1_tensor = 0

  do index = 1, number_of_matrices
    k1 = sparse_operator_indices(1,index)
    k2 = sparse_operator_indices(2,index)
    matrix  = sparse_operator_matrices(:,:,index)
    do j=1,bl
      do i=1,bl
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
  cr, &  ! operator right bandwidth dimension
  d, &  ! physical dimension
  iteration_stage_1_tensor, &
  state_site_tensor, &
  iteration_stage_2_tensor &
)
  integer, intent(in) :: bl, br, cr, d
  double complex, intent(in) :: state_site_tensor(br,bl,d), iteration_stage_1_tensor(bl,d,cr,bl,d)
  double complex, intent(out) :: iteration_stage_2_tensor(br,cr,bl,d)

  external :: zgemm

  call zgemm( &
      'N','N', &
      br,cr*bl*d,bl*d, &
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
  cr, &  ! operator right bandwidth dimension
  d, &  ! physical dimension
  iteration_stage_2_tensor, &
  right_environment, &
  output_state_site_tensor &
)
  integer, intent(in) :: bl, br, cr, d
  double complex, intent(in) :: right_environment(br,br,cr), iteration_stage_2_tensor(br,cr,bl,d)
  double complex, intent(out) :: output_state_site_tensor(br,bl,d)

  external :: zgemm

  call zgemm( &
      'N','N', &
      br, bl*d, br*cr, &
      (1d0,0d0), &
      right_environment, br, &
      iteration_stage_2_tensor, br*cr, &
      (0d0,0d0), &
      output_state_site_tensor, br &
  )

end subroutine
!@-node:gcross.20091110011014.1551:iteration_stage_3
!@-node:gcross.20091110205054.1910:Main iteration
!@+node:gcross.20091110205054.1911:Environment SOS contraction
!@+node:gcross.20091110011014.1549:contract_sos_left
subroutine contract_sos_left( &
  bl, & ! state left bandwidth dimension
  br, & ! state right bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, &  ! physical dimension
  left_environment, &
  number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
  state_site_tensor, &
  new_left_environment &
)
  integer, intent(in) :: bl, br, cl, cr, d, number_of_matrices, sparse_operator_indices(2,number_of_matrices)
  double complex, intent(in) :: &
    left_environment(bl,bl,cl), &
    state_site_tensor(br,bl,d), &
    sparse_operator_matrices(d,d,number_of_matrices)
  double complex, intent(out) :: new_left_environment(br,br,cr)

  double complex :: &
    iteration_stage_1_tensor(bl,d,cr,bl,d), &
    iteration_stage_2_tensor(br,cr,bl,d), &
    iteration_stage_3_tensor(br,cr,br)

  external :: zgemm

  ! Stage 1
  call iteration_stage_1( &
    bl, cl, cr, d, &
    left_environment, &
    number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
    iteration_stage_1_tensor &
  )
  ! Stage 2
  call iteration_stage_2( &
    bl, br, cr, d, &
    iteration_stage_1_tensor, &
    state_site_tensor, &
    iteration_stage_2_tensor &
  )
  ! Stage 3
  call zgemm( &
      'N','C', &
      br*cr,br,bl*d, &
      (1d0,0d0), &
      iteration_stage_2_tensor, br*cr, &
      state_site_tensor, br, &
      (0d0,0d0), &
      iteration_stage_3_tensor, br*cr &
  )
  ! Stage 4
  new_left_environment = reshape(iteration_stage_3_tensor,shape(new_left_environment),order=(/1,3,2/))

end subroutine
!@-node:gcross.20091110011014.1549:contract_sos_left
!@+node:gcross.20091110135225.1556:contract_sos_right_stage_1
subroutine contract_sos_right_stage_1( &
  bl, & ! state left bandwidth dimension
  br, & ! state right bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, &  ! physical dimension
  right_environment, &
  state_site_tensor, &
  sos_right_stage_1_tensor &
)
  integer, intent(in) :: bl, br, cr, d
  double complex, intent(in) :: &
    right_environment(br,br,cr), &
    state_site_tensor(br,bl,d)
  double complex, intent(out) :: sos_right_stage_1_tensor(bl,d,br,cr)

  external :: zgemm

  call zgemm( &
      'C','N', &
      bl*d, br*cr, br, &
      (1d0,0d0), &
      state_site_tensor, br, &
      right_environment, br, &
      (0d0,0d0), &
      sos_right_stage_1_tensor, bl*d &
  )
end subroutine
!@-node:gcross.20091110135225.1556:contract_sos_right_stage_1
!@+node:gcross.20091110135225.1564:contract_sos_right_stage_2a
subroutine contract_sos_right_stage_2a( &
  bl, & ! state left bandwidth dimension
  br, & ! state right bandwidth dimension
  d, &  ! physical dimension
  matrix, &
  state_site_tensor, &
  sos_right_stage_2a_tensor &
)
  integer, intent(in) :: bl, br, d
  double complex, intent(in) :: &
    matrix(d,d), &
    state_site_tensor(br,bl,d)
  double complex, intent(out) :: sos_right_stage_2a_tensor(bl,d,br)

  integer :: i, j, k

  do j = 1,bl
  do i = 1,br
  do k = 1,d
    sos_right_stage_2a_tensor(j,k,i) = sum(state_site_tensor(i,j,:)*matrix(:,k))
  end do
  end do
  end do

end subroutine
!@-node:gcross.20091110135225.1564:contract_sos_right_stage_2a
!@+node:gcross.20091110135225.1570:contract_sos_right_stage_2b
subroutine contract_sos_right_stage_2b( &
  bl, & ! state left bandwidth dimension
  br, & ! state right bandwidth dimension
  d, &  ! physical dimension
  sos_right_stage_1_tensor_slice, &
  sos_right_stage_2a_tensor, &
  new_right_environment_slice &
)
  integer, intent(in) :: bl, br, d
  double complex, intent(in) :: &
    sos_right_stage_1_tensor_slice(bl,d,br), &
    sos_right_stage_2a_tensor(bl,d,br)
  double complex, intent(inout) :: new_right_environment_slice(bl,bl)

  external :: zgemm

  call zgemm( &
      'N','T', &
      bl, bl, d*br, &
      (1d0,0d0), &
      sos_right_stage_1_tensor_slice(1,1,1), bl, &
      sos_right_stage_2a_tensor, bl, &
      (1d0,0d0), &
      new_right_environment_slice(1,1), bl &
  )

end subroutine
!@-node:gcross.20091110135225.1570:contract_sos_right_stage_2b
!@+node:gcross.20091110135225.1572:contract_sos_right_stage_2
subroutine contract_sos_right_stage_2( &
  bl, & ! state left bandwidth dimension
  br, & ! state right bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, &  ! physical dimension
  sos_right_stage_1_tensor, &
  number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
  state_site_tensor, &
  new_right_environment &
)
  integer, intent(in) :: bl, br, cl, cr, d, number_of_matrices, sparse_operator_indices(2,number_of_matrices)
  double complex, intent(in) :: &
    sos_right_stage_1_tensor(bl,d,br,cr), &
    sparse_operator_matrices(d,d,number_of_matrices), &
    state_site_tensor(br,bl,d)
  double complex, intent(out) :: new_right_environment(bl,bl,cl)

  integer :: index
  double complex :: &
    sos_right_stage_2a_tensor(bl,d,br)

  new_right_environment = 0

  do index = 1, number_of_matrices
    call contract_sos_right_stage_2a( &
      bl, br, d, &
      sparse_operator_matrices(:,:,index), &
      state_site_tensor, &
      sos_right_stage_2a_tensor &
    )
    call contract_sos_right_stage_2b( &
      bl, br, d, &
      sos_right_stage_1_tensor(1,1,1,sparse_operator_indices(2,index)), &
      sos_right_stage_2a_tensor, &
      new_right_environment(1,1,sparse_operator_indices(1,index)) &
    )
  end do

end subroutine
!@-node:gcross.20091110135225.1572:contract_sos_right_stage_2
!@+node:gcross.20091110205054.1907:contract_sos_right
subroutine contract_sos_right( &
  bl, & ! state left bandwidth dimension
  br, & ! state right bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, &  ! physical dimension
  right_environment, &
  number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
  state_site_tensor, &
  new_right_environment &
)
  integer, intent(in) :: bl, br, cl, cr, d, number_of_matrices, sparse_operator_indices(2,number_of_matrices)
  double complex, intent(in) :: &
    right_environment(br,br,cr), &
    state_site_tensor(br,bl,d), &
    sparse_operator_matrices(d,d,number_of_matrices)
  double complex, intent(out) :: new_right_environment(bl,bl,cl)

  double complex :: &
    sos_right_stage_1_tensor(bl,d,br,cr)

  call contract_sos_right_stage_1( &
    bl, br, cr, d, &
    right_environment, &
    state_site_tensor, &
    sos_right_stage_1_tensor &
  )

  call contract_sos_right_stage_2( &
    bl, br, cl, cr, d, &
    sos_right_stage_1_tensor, &
    number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
    state_site_tensor, &
    new_right_environment &
  )

end subroutine
!@-node:gcross.20091110205054.1907:contract_sos_right
!@-node:gcross.20091110205054.1911:Environment SOS contraction
!@+node:gcross.20091115224921.1737:Environment SS contraction
!@+node:gcross.20091115224921.1738:contract_ss_left
subroutine contract_ss_left( &
  b_left_old, b_right_old, &
  b_left_new, b_right_new, &
  d, &
  left_environment, &
  normalized_projector_site_tensor, &
  normalized_state_site_tensor, &
  new_left_environment &
)

  integer, intent(in) :: &
    b_left_old, b_right_old, &
    b_left_new, b_right_new, &
    d
  double complex, intent(in) :: &
    left_environment(b_left_new,b_left_old), &
    normalized_projector_site_tensor(b_left_old,d,b_right_old), &
    normalized_state_site_tensor(b_right_new,b_left_new,d)
  double complex, intent(out) :: new_left_environment(b_right_new,b_right_old)

  double complex :: &
    intermediate_tensor(b_left_new,d,b_right_old)

  new_left_environment = 0

  call zgemm( &
      'N','N', &
      b_left_new,d*b_right_old,b_left_old, &
      (1d0,0d0), &
      left_environment, b_left_new, &
      normalized_projector_site_tensor, b_left_old, &
      (0d0,0d0), &
      intermediate_tensor, b_left_new &
  )

  call zgemm( &
      'N','N', &
      b_right_new,b_right_old,b_left_new*d, &
      (1d0,0d0), &
      normalized_state_site_tensor, b_right_new, &
      intermediate_tensor, b_left_new*d, &
      (0d0,0d0), &
      new_left_environment, b_right_new &
  )

end subroutine
!@nonl
!@-node:gcross.20091115224921.1738:contract_ss_left
!@+node:gcross.20091116094945.1743:contract_ss_right
subroutine contract_ss_right( &
  b_left_old, b_right_old, &
  b_left_new, b_right_new, &
  d, &
  right_environment, &
  normalized_projector_site_tensor, &
  normalized_state_site_tensor, &
  new_right_environment &
)

  integer, intent(in) :: &
    b_left_old, b_right_old, &
    b_left_new, b_right_new, &
    d
  double complex, intent(in) :: &
    right_environment(b_right_old,b_right_new), &
    normalized_projector_site_tensor(b_left_old,d,b_right_old), &
    normalized_state_site_tensor(b_right_new,b_left_new,d)
  double complex, intent(out) :: new_right_environment(b_left_old,b_left_new)

  double complex :: &
    intermediate_tensor(b_left_old,d,b_right_new), &
    transposed_state_site_tensor(d,b_right_new,b_left_new)

  intermediate_tensor = 0
  new_right_environment = 0

  call zgemm( &
      'N','N', &
      b_left_old*d,b_right_new,b_right_old, &
      (1d0,0d0), &
      normalized_projector_site_tensor, b_left_old*d, &
      right_environment, b_right_old, &
      (0d0,0d0), &
      intermediate_tensor, b_left_old*d &
  )

  transposed_state_site_tensor = reshape( &
      normalized_state_site_tensor, &
      shape(transposed_state_site_tensor), &
      order=(/2,3,1/) &
  )

  call zgemm( &
      'N','N', &
      b_left_old,b_left_new,d*b_right_new, &
      (1d0,0d0), &
      intermediate_tensor, b_left_old, &
      transposed_state_site_tensor, d*b_right_new, &
      (0d0,0d0), &
      new_right_environment, b_left_old &
  )

end subroutine
!@-node:gcross.20091116094945.1743:contract_ss_right
!@+node:gcross.20091116094945.1748:form_overlap_vector
subroutine form_overlap_vector( &
  b_left_old, b_right_old, &
  b_left_new, b_right_new, &
  d, &
  left_environment, &
  right_environment, &
  unnormalized_projector_site_tensor, &
  overlap_vector &
)

  integer, intent(in) :: &
    b_left_old, b_right_old, &
    b_left_new, b_right_new, &
    d
  double complex, intent(in) :: &
    left_environment(b_left_new,b_left_old), &
    right_environment(b_right_old,b_right_new), &
    unnormalized_projector_site_tensor(b_left_old,d,b_right_old)
  double complex, intent(out) :: overlap_vector(b_right_new,b_left_new,d)

  double complex :: &
    intermediate_tensor(b_left_old,d,b_right_new), &
    transposed_overlap_vector(b_left_new,d,b_right_new)

  call zgemm( &
      'N','N', &
      b_left_old*d,b_right_new,b_right_old, &
      (1d0,0d0), &
      unnormalized_projector_site_tensor, b_left_old*d, &
      right_environment, b_right_old, &
      (0d0,0d0), &
      intermediate_tensor, b_left_old*d &
  )

  call zgemm( &
      'N','N', &
      b_left_new,d*b_right_new,b_left_old, &
      (1d0,0d0), &
      left_environment, b_left_new, &
      intermediate_tensor, b_left_old, &
      (0d0,0d0), &
      transposed_overlap_vector, b_left_new &
  )

  overlap_vector = reshape( &
    transposed_overlap_vector, &
    shape(overlap_vector), &
    order=(/2,3,1/) &
  )

end subroutine
!@-node:gcross.20091116094945.1748:form_overlap_vector
!@-node:gcross.20091115224921.1737:Environment SS contraction
!@+node:gcross.20091110205054.1916:compute_expectation
subroutine compute_expectation( &
  bl, & ! state left bandwidth dimension
  br, & ! state right bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, &  ! physical dimension
  left_environment, &
  state_site_tensor, &
  number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
  right_environment, &
  expectation &
)

  integer, intent(in) :: bl, br, cl, cr, d, number_of_matrices, sparse_operator_indices(2,number_of_matrices)
  double complex, intent(in) :: &
    left_environment(bl,bl,cl), &
    state_site_tensor(br,bl,d), &
    right_environment(br,br,cr), &
    sparse_operator_matrices(d,d,number_of_matrices)
  double complex, intent(out) :: expectation

  double complex :: new_right_environment(bl,bl,cl)
  integer :: i, j, k

  call contract_sos_right( &
    bl, br, cl, cr, d, &
    right_environment, &
    number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
    state_site_tensor, &
    new_right_environment &
  )

  expectation = 0

  do k = 1, cl
  do i = 1, bl
  do j = 1, bl
    expectation = expectation + new_right_environment(i,j,k)*left_environment(j,i,k)
  end do
  end do
  end do

end subroutine
!@-node:gcross.20091110205054.1916:compute_expectation
!@-node:gcross.20091110205054.1940:Contractors
!@+node:gcross.20091211120042.1683:apply_single_site_operator
subroutine apply_single_site_operator( &
  bl, & ! state left bandwidth dimension
  br, & ! state right bandwidth dimension
  d, &  ! physical dimension
  state_site_tensor, &
  operator, &
  new_state_site_tensor &
)
  integer, intent(in) :: bl, br, d
  double complex, intent(in) :: &
    state_site_tensor(br,bl,d), &
    operator
  double complex, intent(out) :: new_state_site_tensor(br,bl,d)

  external :: zgemm

  call zgemm( &
      'N','T', &
      br*bl,d,d, &
      (1d0,0d0), &
      state_site_tensor, br*bl, &
      operator, d, &
      (0d0,0d0), &
      new_state_site_tensor, br*bl &
  )

end subroutine
!@-node:gcross.20091211120042.1683:apply_single_site_operator
!@+node:gcross.20091115201814.1736:project
subroutine project(vector_size,number_of_projectors,projectors,input_vector,output_vector)
  integer, intent(in) :: number_of_projectors, vector_size
  double complex, intent(in) :: projectors(vector_size,number_of_projectors), input_vector(vector_size)
  double complex, intent(out) :: output_vector(vector_size)
  double complex :: projector_weights(number_of_projectors)
  integer :: i
  call zgemv( &
    'T', &
    vector_size,number_of_projectors, &
    (1d0,0d0),projectors,vector_size, &
    input_vector,1, &
    (0d0,0d0),projector_weights,1 &
  )
  forall (i = 1:vector_size) output_vector(i) = input_vector(i) - dot_product(projectors(i,:),projector_weights)
end subroutine
!@-node:gcross.20091115201814.1736:project
!@+node:gcross.20091109182634.1537:optimize
function optimize( &
  bl, br, & ! state bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
  right_environment, &
  number_of_projectors, projectors, &
  which, &
  tol, &
  number_of_iterations, &
  guess, &
  result, &
  eigenvalue &
) result (info)

  !@  << Declarations >>
  !@+node:gcross.20091115201814.1731:<< Declarations >>
  !@+others
  !@+node:gcross.20091115201814.1729:Arguments
  integer, intent(in) :: &
    bl, br, cl, cr, d, &
    number_of_matrices, sparse_operator_indices(2,number_of_matrices), &
    number_of_projectors
  integer, intent(inout) :: number_of_iterations
  double complex, intent(in) :: &
    left_environment(bl,bl,cl), &
    right_environment(br,br,cr), &
    sparse_operator_matrices(d,d,number_of_matrices), &
    projectors(br*bl*d,number_of_projectors), &
    guess(br,bl,d)
  double complex, intent(out) :: &
    result(br,bl,d), &
    eigenvalue
  character, intent(in) :: which*2
  double precision, intent(in) :: tol
  !@-node:gcross.20091115201814.1729:Arguments
  !@+node:gcross.20091109182634.1538:Interface
  interface
    !@  @+others
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
  end interface
  !@-node:gcross.20091109182634.1538:Interface
  !@+node:gcross.20091115201814.1730:Work arrays
  integer, parameter :: nev = 1, ncv = 3

  double complex :: &
    iteration_stage_1_tensor(bl,d,cr,bl,d), &
    iteration_stage_2_tensor(br,cr,bl,d), &
    v(br*bl*d,ncv), &
    workd(3*br*bl*d), &
    workl(3*ncv**2+5*ncv), &
    eigenvalues(nev+1), &
    workev(2*ncv), &
    resid(br,bl,d)

  integer :: &
    iparam(11), &
    ipntr(14), &
    ido, &
    info

  logical :: &
    select(ncv)

  double precision :: rwork(ncv)
  !@nonl
  !@-node:gcross.20091115201814.1730:Work arrays
  !@-others
  !@-node:gcross.20091115201814.1731:<< Declarations >>
  !@nl

  !@  << Setup >>
  !@+node:gcross.20091115201814.1732:<< Setup >>
  !@+at
  ! First do the stage 1 contraction, since it is independent of the state 
  ! site tensor.
  !@-at
  !@@c

    call iteration_stage_1( &
      bl, cl, cr, d, &
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

    resid = guess
  !@-node:gcross.20091115201814.1732:<< Setup >>
  !@nl

  !@  << Main iteration >>
  !@+node:gcross.20091115201814.1733:<< Main iteration >>
  !@+at
  ! Run the iteration.
  !@-at
  !@@c

    do while (ido /= 99)
      call znaupd ( &
        ido, 'I', d*bl*br, which, nev, tol, resid, ncv, v, br*bl*d, &
        iparam, ipntr, workd, workl, 3*ncv**2+5*ncv, rwork, info &
      ) 
      if (ido == -1 .or. ido == 1) then
        call project(bl*br*d,number_of_projectors,projectors,workd(ipntr(1)),workd(ipntr(1)))
        call iteration_stage_2( &
          bl, br, cr, d, &
          iteration_stage_1_tensor, &
          workd(ipntr(1)), &
          iteration_stage_2_tensor &
        )
        call iteration_stage_3( &
          bl, br, cr, d, &
          iteration_stage_2_tensor, &
          right_environment, &
          workd(ipntr(2)) &
        )
        call project(bl*br*d,number_of_projectors,projectors,workd(ipntr(2)),workd(ipntr(2)))
      end if
    end do
  !@-node:gcross.20091115201814.1733:<< Main iteration >>
  !@nl

  !@  << Post-processing >>
  !@+node:gcross.20091115201814.1728:<< Post-processing >>
  number_of_iterations = iparam(3)

  if( info < 0 ) then
    return
  end if

  call zneupd (.true.,'A', select, eigenvalues, v, br*bl*d, (0d0,0d0), workev, &
              'I', d*bl*br, which, nev, tol, resid, ncv, &
              v, br*bl*d, iparam, ipntr, workd, workl, 3*ncv**2+5*ncv, &
              rwork, info)

  result = reshape(v(:,1),shape(result))
  eigenvalue = eigenvalues(1)
  !@nonl
  !@-node:gcross.20091115201814.1728:<< Post-processing >>
  !@nl

end function
!@-node:gcross.20091109182634.1537:optimize
!@+node:gcross.20091110205054.1942:Randomization
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
subroutine rand_norm_state_site_tensor(br, bl, d, state_site_tensor)
  integer, intent(in) :: br, bl, d
  double complex, intent(out) :: state_site_tensor(br,bl,d)

  double complex :: u(bl,bl), vt(bl,br*d)
  double precision :: s(bl)
  integer :: info

  double complex :: normalized_state_site_tensor(bl,br*d)

  integer :: mysvd
  external :: zgemm

  if (br*d < bl) then
    print *, "Not enough degrees of freedom to normalize."
    print *, br*d, "<", bl
    stop
  end if

  call randomize_state_site_tensor(bl, br, d, normalized_state_site_tensor)

  info = mysvd(bl,br*d,bl,normalized_state_site_tensor,u,s,vt)
  if (info /= 0) then
    print *, "Unable to create normalized random state site tensor!"
    stop
  end if

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

end subroutine
!@-node:gcross.20091110205054.1922:rand_norm_state_site_tensor
!@+node:gcross.20091120134444.1600:rand_unnorm_state_site_tensor
subroutine rand_unnorm_state_site_tensor(br, bl, d, state_site_tensor)
  integer, intent(in) :: br, bl, d
  double complex, intent(out) :: state_site_tensor(br,bl,d)

  call randomize_state_site_tensor(bl, br, d, state_site_tensor)

  state_site_tensor = state_site_tensor / sqrt(dble(real(sum(conjg(state_site_tensor(:,:,:))*state_site_tensor(:,:,:)))))

end subroutine
!@-node:gcross.20091120134444.1600:rand_unnorm_state_site_tensor
!@-node:gcross.20091110205054.1942:Randomization
!@+node:gcross.20091110205054.1943:Normalization
!@+node:gcross.20091110205054.1926:norm_denorm_going_left
function norm_denorm_going_left( &
  bl,bm,br, &
  dl,dr, &
  site_tensor_to_denormalize, &
  site_tensor_to_normalize, &
  denormalized_site_tensor, &
  normalized_site_tensor &
) result (info)
  integer, intent(in) :: bl, bm, br, dl, dr
  double complex, intent(in) :: &
    site_tensor_to_denormalize(bm,bl,dl), &
    site_tensor_to_normalize(br,bm,dr)
  double complex, intent(out) :: &
    denormalized_site_tensor(bm,bl,dl), &
    normalized_site_tensor(br,bm,dr)
  double complex :: &
    denormalized_tensor_workspace(bm,bl,dl), &
    normalized_tensor_workspace(bm,br,dr)

  double complex :: u(bm,bm), vt(bm,br*dr)
  double precision :: s(bm)
  integer :: info, i, j

  integer :: mysvd
  external :: zgemm

  denormalized_site_tensor = 0
  normalized_site_tensor = 0
  denormalized_tensor_workspace = 0
  normalized_tensor_workspace = 0

  if (br*dr < bm) then
    print *, "Not enough degrees of freedom to normalize."
    print *, br*dr, "<", bm
    stop
  end if

  normalized_tensor_workspace = reshape(site_tensor_to_normalize,shape(normalized_tensor_workspace),order=(/2,1,3/))

  info = mysvd(bm,br*dr,bm,normalized_tensor_workspace,u,s,vt)

  call zgemm( &
    'N','N', &
    bm,br*dr,bm, &
    (1d0,0d0), &
    u, bm, &
    vt, bm, &
    (0d0,0d0), &
    normalized_tensor_workspace, bm &
  )

  normalized_site_tensor = reshape(normalized_tensor_workspace,shape(normalized_site_tensor),order=(/2,1,3/))

  u = conjg(u)

  call zgemm( &
    'C','N', &
    bm,bl*dl,bm, &
    (1d0,0d0), &
    u, bm, &
    site_tensor_to_denormalize, bm, &
    (0d0,0d0), &
    denormalized_tensor_workspace, bm &
  )

  forall (i=1:bl,j=1:dl) &
    denormalized_tensor_workspace(:,i,j) = denormalized_tensor_workspace(:,i,j) * s(:)

  call zgemm( &
    'N','N', &
    bm,bl*dl,bm, &
    (1d0,0d0), &
    u, bm, &
    denormalized_tensor_workspace, bm, &
    (0d0,0d0), &
    denormalized_site_tensor, bm &
  )

end function
!@nonl
!@-node:gcross.20091110205054.1926:norm_denorm_going_left
!@+node:gcross.20091110205054.1935:norm_denorm_going_right
function norm_denorm_going_right( &
  bl,bm,br, &
  dl,dr, &
  site_tensor_to_normalize, &
  site_tensor_to_denormalize, &
  normalized_site_tensor, &
  denormalized_site_tensor &
) result (info)
  integer, intent(in) :: bl, bm, br, dl, dr
  double complex, intent(in) :: &
    site_tensor_to_normalize(bm,bl,dl), &
    site_tensor_to_denormalize(br,bm,dr)
  double complex, intent(out) :: &
    normalized_site_tensor(bm,bl,dl), &
    denormalized_site_tensor(br,bm,dr)
  double complex :: &
    denormalized_tensor_workspace_1(bm,br,dr), &
    denormalized_tensor_workspace_2(bm,br,dr)

  double complex :: u(bm,bm), vt(bm,bl*dl)
  double precision :: s(bm)
  integer :: info, i, j

  integer :: mysvd
  external :: zgemm

  denormalized_site_tensor = 0
  normalized_site_tensor = 0
  denormalized_tensor_workspace_1 = 0
  denormalized_tensor_workspace_2 = 0

  if (bl*dl < bm) then
    print *, "Not enough degrees of freedom to normalize."
    print *, bl*dl, "<", bm
    stop
  end if

  info = mysvd(bm,bl*dl,bm,site_tensor_to_normalize,u,s,vt)

  call zgemm( &
    'N','N', &
    bm,bl*dl,bm, &
    (1d0,0d0), &
    u, bm, &
    vt, bm, &
    (0d0,0d0), &
    normalized_site_tensor, bm &
  )

  denormalized_tensor_workspace_1 = reshape( &
    site_tensor_to_denormalize, &
    shape(denormalized_tensor_workspace_1), &
    order=(/2,1,3/) &
  )

  u = conjg(u)

  call zgemm( &
    'C','N', &
    bm,br*dr,bm, &
    (1d0,0d0), &
    u, bm, &
    denormalized_tensor_workspace_1, bm, &
    (0d0,0d0), &
    denormalized_tensor_workspace_2, bm &
  )

  forall (i=1:br,j=1:dr) &
    denormalized_tensor_workspace_2(:,i,j) = denormalized_tensor_workspace_2(:,i,j) * s(:)

  call zgemm( &
    'N','N', &
    bm,br*dr,bm, &
    (1d0,0d0), &
    u, bm, &
    denormalized_tensor_workspace_2, bm, &
    (0d0,0d0), &
    denormalized_tensor_workspace_1, bm &
  )

  denormalized_site_tensor = reshape( &
    denormalized_tensor_workspace_1, &
    shape(denormalized_site_tensor), &
    order=(/2,1,3/) &
  )

end function
!@-node:gcross.20091110205054.1935:norm_denorm_going_right
!@-node:gcross.20091110205054.1943:Normalization
!@+node:gcross.20091115094257.1711:Bandwidth increasing
!@+node:gcross.20091115094257.1712:create_bandwidth_increase_matrix
subroutine create_bandwidth_increase_matrix(old_bandwidth,new_bandwidth,matrix)
  integer, intent(in) :: old_bandwidth, new_bandwidth
  double complex, intent(out) :: matrix(new_bandwidth,old_bandwidth)

  integer :: info, mysvd, i, j
  double complex :: &
    u(new_bandwidth,old_bandwidth), &
    s(old_bandwidth), &
    vt(old_bandwidth,old_bandwidth), &
    dummy(new_bandwidth,old_bandwidth)

  matrix = 0

  do j = 1, old_bandwidth
  do i = 1, new_bandwidth
    dummy(i,j) = rand()*(1d0,0d0) + rand()*(0d0,1d0)
  end do
  end do

  info = mysvd(new_bandwidth,old_bandwidth,old_bandwidth,dummy,u,s,vt)
  if (info /= 0) then
    print *, "Error creating bandwidth increase matrix!  info = ",info
  end if

  call zgemm( &
    'N','N', &
    new_bandwidth,old_bandwidth,old_bandwidth, &
    (1d0,0d0), &
    u, new_bandwidth, &
    vt, old_bandwidth, &
    (0d0,0d0), &
    matrix, new_bandwidth &
  )

end subroutine
!@-node:gcross.20091115094257.1712:create_bandwidth_increase_matrix
!@+node:gcross.20091115094257.1721:absorb_bi_matrix_from_left
subroutine absorb_bi_matrix_from_left( &
  br,bl,d, &
  new_bl, &
  old_state_site_tensor, &
  matrix, &
  new_state_site_tensor &
)
  integer, intent(in) :: br, bl, d, new_bl
  double complex, intent(in) :: &
    old_state_site_tensor(br,bl,d), &
    matrix(new_bl,bl)
  double complex, intent(out) :: &
    new_state_site_tensor(br,new_bl,d)

  double complex :: &
    intermediate_tensor_1(br,d,bl), &
    intermediate_tensor_2(br,d,new_bl)

  intermediate_tensor_1 = reshape(old_state_site_tensor,shape(intermediate_tensor_1),order=(/1,3,2/))
  intermediate_tensor_2 = 0

  call zgemm( &
    'N','C', &
    br*d,new_bl,bl, &
    (1d0,0d0), &
    intermediate_tensor_1, br*d, &
    matrix, new_bl, &
    (0d0,0d0), &
    intermediate_tensor_2, br*d &
  )

  new_state_site_tensor = reshape(intermediate_tensor_2,shape(new_state_site_tensor),order=(/1,3,2/))

end subroutine
!@-node:gcross.20091115094257.1721:absorb_bi_matrix_from_left
!@+node:gcross.20091115094257.1717:absorb_bi_matrix_from_right
subroutine absorb_bi_matrix_from_right( &
  br,bl,d, &
  new_br, &
  old_state_site_tensor, &
  matrix, &
  new_state_site_tensor &
)
  integer, intent(in) :: br, bl, d, new_br
  double complex, intent(in) :: &
    old_state_site_tensor(br,bl,d), &
    matrix(new_br,br)
  double complex, intent(out) :: &
    new_state_site_tensor(new_br,bl,d)

  new_state_site_tensor = 0
  call zgemm( &
    'N','N', &
    new_br,bl*d,br, &
    (1d0,0d0), &
    matrix, new_br, &
    old_state_site_tensor, br, &
    (0d0,0d0), &
    new_state_site_tensor, new_br &
  )

end subroutine
!@-node:gcross.20091115094257.1717:absorb_bi_matrix_from_right
!@+node:gcross.20091115105949.1728:increase_bandwidth_between
function increase_bandwidth_between( &
  bl,bm,br, &
  dl,dr, &
  new_bm, &
  normalized_tensor, &
  denormalized_tensor, &
  output_denormalized_tensor, &
  output_normalized_tensor &
) result (info)
  integer, intent(in) :: bl, bm, br, dl, dr, new_bm
  double complex, intent(in) :: &
    normalized_tensor(bm,bl,dl), &
    denormalized_tensor(br,bm,dr)
  double complex, intent(out) :: &
    output_denormalized_tensor(new_bm,bl,dl), &
    output_normalized_tensor(br,new_bm,dr)

  double complex :: &
    bandwidth_increase_matrix(new_bm,bm), &
    enlarged_tensor_1(new_bm,bl,dl), &
    enlarged_tensor_2(br,new_bm,dr)

  integer :: info, norm_denorm_going_left

  call create_bandwidth_increase_matrix(bm,new_bm,bandwidth_increase_matrix)

  call absorb_bi_matrix_from_right( &
      bm,bl,dl, &
      new_bm, &
      normalized_tensor, &
      bandwidth_increase_matrix, &
      enlarged_tensor_1 &
    )

  call absorb_bi_matrix_from_left( &
      br,bm,dr, &
      new_bm, &
      denormalized_tensor, &
      bandwidth_increase_matrix, &
      enlarged_tensor_2 &
    )

  info = norm_denorm_going_left( &
      bl,new_bm,br, &
      dl,dr, &
      enlarged_tensor_1, &
      enlarged_tensor_2, &
      output_denormalized_tensor, &
      output_normalized_tensor &
    )

end function
!@-node:gcross.20091115105949.1728:increase_bandwidth_between
!@-node:gcross.20091115094257.1711:Bandwidth increasing
!@+node:gcross.20091117140132.1799:Overlap tensor formation
!@+node:gcross.20091117140132.1800:form_overlap_site_tensor
subroutine form_overlap_site_tensor(br, bl, d, state_site_tensor, overlap_site_tensor)
  integer, intent(in) :: br, bl, d
  double complex, intent(in) :: state_site_tensor(br,bl,d)
  double complex, intent(out) :: overlap_site_tensor(bl,d,br)

  overlap_site_tensor = &
    conjg(reshape( &
      state_site_tensor, &
      shape(overlap_site_tensor), &
      order=(/3,1,2/) &
    ))

end subroutine
!@-node:gcross.20091117140132.1800:form_overlap_site_tensor
!@+node:gcross.20091117140132.1802:form_norm_overlap_tensors
subroutine form_norm_overlap_tensors( &
  bl, bm, br, &
  dl, dr, &
  unnormalized_state_tensor_1, &
  right_norm_state_tensor_2, &
  left_norm_overlap_tensor_1, &
  unnormalized_overlap_tensor_1, &
  unnormalized_state_tensor_2, &
  right_norm_overlap_tensor_2 &
)
  integer, intent(in) :: br, bm, bl, dl, dr
  double complex, intent(in) :: &
    unnormalized_state_tensor_1(bm,bl,dl), &
    right_norm_state_tensor_2(br,bm,dr)

  double complex, intent(out) :: &
    left_norm_overlap_tensor_1(bl,dl,bm), &
    unnormalized_overlap_tensor_1(bl,dl,bm), &
    unnormalized_state_tensor_2(br,bm,dr), &
    right_norm_overlap_tensor_2(bm,dr,br)

  double complex :: &
    left_norm_state_tensor_1(bm,bl,dl)
  integer :: info, norm_denorm_going_right

  info = norm_denorm_going_right( &
    bl,bm,br, &
    dl,dr, &
    unnormalized_state_tensor_1, &
    right_norm_state_tensor_2, &
    left_norm_state_tensor_1, &
    unnormalized_state_tensor_2 &
  )
  if (info /= 0) then
    print *, "Unable to normalize tensor."
    stop
  end if

  call form_overlap_site_tensor( &
    bm,bl,dl, &
    unnormalized_state_tensor_1, &
    unnormalized_overlap_tensor_1 &
  )

  call form_overlap_site_tensor( &
    bm,bl,dl, &
    left_norm_state_tensor_1, &
    left_norm_overlap_tensor_1 &
  )

  call form_overlap_site_tensor( &
    br,bm,dr, &
    right_norm_state_tensor_2, &
    right_norm_overlap_tensor_2 &
  )

end subroutine
!@-node:gcross.20091117140132.1802:form_norm_overlap_tensors
!@-node:gcross.20091117140132.1799:Overlap tensor formation
!@-others
!@-node:gcross.20091110205054.1939:@thin core.f95
!@-leo
