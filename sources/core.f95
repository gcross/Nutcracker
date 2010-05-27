!@+leo-ver=4-thin
!@+node:gcross.20091110205054.1939:@thin core.f95
!@@language fortran90
!@@tabwidth -2

!@+others
!@+node:gcross.20100512172859.1739:Utility Functions
!@+node:gcross.20091110205054.1929:mysvd
function mysvd ( &
  m, n, rank, &
  matrix, &
  u, s, vt &
) result (info)
  integer, intent(in) :: m, n, rank
  double complex, intent(in) :: matrix(m,n)
  double complex, intent(out) :: u(m,rank), vt(rank,n)
  double precision :: s(rank)

  double complex, allocatable :: work(:)
  integer :: iwork(8*rank)
  double precision :: rwork(5*rank*rank + 5*rank)
  double complex :: optimal_lwork, a(m,n)
  integer :: lwork, info

  external :: zgesdd

  lwork = -1

  a = matrix

  call zgesdd( &
    'S', m, n, &
    a, m, &
    s, &
    u, m, &
    vt, rank, &
    optimal_lwork, lwork, &
    rwork, &
    iwork, &
    info &
  )

  lwork = floor(real(optimal_lwork))

  allocate(work(lwork))

  call zgesdd( &
    'S', m, n, &
    a, m, &
    s, &
    u, m, &
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
!@+node:gcross.20100513214001.1746:compute_orthogonal_basis
subroutine compute_orthogonal_basis( &
  m, n, k, &
  vectors, &
  rank, &
  basis &
)
  integer, intent(in) :: n, m
  integer, intent(in) :: k
  integer, intent(out) :: rank
  double complex, intent(in) :: vectors(m,n)
  double complex, intent(out) :: basis(m,k)

  integer :: jpvt(n), info, lwork, lwork1, lwork2
  double complex :: tau(n), lwork_as_complex
  double precision :: rwork(2*n)

  double complex, allocatable :: work(:)

  external :: zgeqp3, zungqr, dznrm2
  double precision :: dznrm2

  if (m < n) then
    print *, "The input matrix has too many columns and not enough rows (column-major ordering) to orthogonalize!"
    stop
  end if

  if (k < n) then
    print *, "The output matrix has fewer columns than the input matrix!"
    stop
  end if

  basis(:,:n) = vectors(:,:)

  lwork1 = -1
  call zgeqp3( &
    m, n, &
    basis, m, &
    jpvt, &
    tau, &
    lwork_as_complex, lwork1, &
    rwork, &
    info &
  )
  lwork1 = int(real(lwork_as_complex))

  if (info /= 0) then
    print *, "Unable to factorize matrix (zgeqp3, workspace query); info =", info
    stop
  end if

  lwork2 = -1
  call zungqr( &
    m, k, n, &
    basis, m, &
    tau, &
    lwork_as_complex, lwork2, &
    info &
  )
  lwork2 = int(real(lwork_as_complex))

  if (info /= 0) then
    print *, "Unable to factorize matrix (zungqr, workspace query); info =", info
    stop
  end if

  lwork = max(lwork1,lwork2)
  allocate(work(lwork))

  call zgeqp3( &
    m, n, &
    basis, m, &
    jpvt, &
    tau, &
    work, lwork, &
    rwork, &
    info &
  )

  if (info /= 0) then
    print *, "Unable to factorize matrix (zgeqp3); info =", info
    stop
  end if

  rank = n
  do while (dznrm2(n-rank+1,basis(rank,rank),m) < 1e-7 .and. rank > 0)
    rank = rank - 1
  end do

  call zungqr( &
    m, k, n, &
    basis, m, &
    tau, &
    work, lwork, &
    info &
  )

  if (info /= 0) then
    print *, "Unable to factorize matrix (zungqr); info =", info
    stop
  end if

  deallocate(work)

end subroutine
!@-node:gcross.20100513214001.1746:compute_orthogonal_basis
!@+node:gcross.20100517000234.1790:compute_orthogonal_subspace
subroutine compute_orthogonal_subspace(n,number_of_projectors,projectors,orthogonal_basis)
  integer, intent(in) :: n, number_of_projectors
  double complex, intent(in) :: projectors(n,number_of_projectors)
  double complex, intent(out) :: orthogonal_basis(n,n-number_of_projectors)

  double complex :: &
    full_basis(n,n)

  integer :: m, rank

  m = n-number_of_projectors

  call compute_orthogonal_basis( &
    n, number_of_projectors, n, &
    projectors, &
    rank, &
    full_basis &
  )

  if (rank /= number_of_projectors) then
    print *, "compute_orthogonal_subspace: error computing orthogonal basis for projectors (",rank,"/=",number_of_projectors,")"
    stop
  end if

  orthogonal_basis(:,:) = full_basis(:,number_of_projectors+1:)

end subroutine
!@-node:gcross.20100517000234.1790:compute_orthogonal_subspace
!@+node:gcross.20100513214001.1742:orthogonalize_matrix_in_place
subroutine orthogonalize_matrix_in_place( &
  m, n, &
  matrix, &
  rank &
)
  integer, intent(in) :: n, m
  double complex, intent(inout) :: matrix(m,n)
  integer, intent(out) :: rank

  call compute_orthogonal_basis(m,n,n,matrix,rank,matrix)

end subroutine
!@-node:gcross.20100513214001.1742:orthogonalize_matrix_in_place
!@+node:gcross.20100514235202.1743:lapack_eigenvalue_minimizer
subroutine lapack_eigenvalue_minimizer(n,matrix,eigenvalue,eigenvector)
  integer, intent(in) :: n
  double complex, intent(in) :: matrix(n,n)

  double complex, intent(out) :: eigenvalue, eigenvector(n)

  integer :: number_of_eigenvalues_found, lwork, lrwork, liwork(1)
  double precision :: lrwork_as_double(1)
  double complex :: lwork_as_complex(1), temp(n,n)
  double complex, allocatable :: work(:)
  double precision, allocatable :: rwork(:)
  integer, allocatable :: iwork(:)

  integer :: info, eigenvector_support(2)
  double precision :: w(n)

  interface
    subroutine zheevr( &
      jobz, range, uplo, &
      n, &
      a, lda, &
      vl, vu, &
      il, iu, &
      abstol, &
      m, w, &
      z, ldz, isuppz, &
      work, lwork, &
      rwork, lrwork, &
      iwork, liwork, &
      info &
    )
      character, intent(in) :: jobz, range, uplo
      integer, intent(in) :: il, info, iu, lda, ldz, liwork, lrwork, lwork, n
      integer, intent(out) :: m
      double precision, intent(in) :: abstol, vl, vu
      integer, intent(inout) :: isuppz(2), iwork(liwork)
      double precision, intent(inout) :: rwork(lrwork), w(n)
      double complex, intent(inout) :: a(lda,n), work(lwork), z(ldz,1)
    end subroutine
  end interface

  temp = matrix

  call zheevr(&
    'V', &
    'I', &
    'U', &
    n, &
    temp, n, &
    0d0, 0d0, &
    1, 1, &
    0d0, &
    number_of_eigenvalues_found, &
    w, &
    eigenvector, n, eigenvector_support, &
    lwork_as_complex, -1, &
    lrwork_as_double, -1, &
    liwork, -1, &
    info &
  )

  if (info /= 0) then
    print *, "Error computing eigenvalue (zheevr, workspace query); info =", info
    print *, "n=",n
    stop
  end if

  lwork = int(lwork_as_complex(1))
  lrwork = int(lrwork_as_double(1))

  allocate(work(lwork),rwork(lrwork),iwork(liwork(1)))

  call zheevr(&
    'V', &
    'I', &
    'U', &
    n, &
    temp, n, &
    0d0, 0d0, &
    1, 1, &
    0d0, &
    number_of_eigenvalues_found, &
    w, &
    eigenvector, n, eigenvector_support, &
    work, lwork, &
    rwork, lrwork, &
    iwork, liwork(1), &
    info &
  )

  deallocate(work,rwork,iwork)

  if (info /= 0) then
    print *, "Error computing eigenvalue (zheevr); info =", info
    print *, "n=",n
    stop
  end if

  eigenvalue = w(1)*(1d0,0d0)

end subroutine
!@-node:gcross.20100514235202.1743:lapack_eigenvalue_minimizer
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
!@+node:gcross.20100513131210.1744:compute_optimization_matrix
subroutine compute_optimization_matrix( &
  bl, br, & ! state bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
  right_environment, &
  optimization_matrix &
)
  integer, intent(in) :: &
    bl, br, cl, cr, d, &
    number_of_matrices, sparse_operator_indices(2,number_of_matrices)
  double complex, intent(in) :: &
    left_environment(bl,bl,cl), &
    right_environment(br,br,cr), &
    sparse_operator_matrices(d,d,number_of_matrices)
  double complex, intent(out) :: &
    optimization_matrix(br,bl,d,br,bl,d)

  integer :: &
    n, &
    o1, o2, o3, o4, &
    l1, l2, l3, &
    r1, r2, r3

  optimization_matrix = 0
  do n = 1, number_of_matrices
    l3 = sparse_operator_indices(1,n)
    r3 = sparse_operator_indices(2,n)
    o3 = r3
    o4 = l3
    do o1 = 1, d
    do l1 = 1, bl
    do r2 = 1, br
    do o2 = 1, d
    do l2 = 1, bl
    do r1 = 1, br
      optimization_matrix(r1,l2,o2,r2,l1,o1) = optimization_matrix(r1,l2,o2,r2,l1,o1) + &
        left_environment(l1,l2,l3)*sparse_operator_matrices(o1,o2,n)*right_environment(r1,r2,r3)
    end do
    end do
    end do
    end do
    end do
    end do
  end do

end subroutine
!@-node:gcross.20100513131210.1744:compute_optimization_matrix
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
!@+node:gcross.20100520145029.1766:Projectors
!@+node:gcross.20100520145029.1765:compute_overlap_with_projectors
subroutine compute_overlap_with_projectors( &
  number_of_reflectors, reflectors, coefficients, &
  vector_size, vector, &
  overlap &
)
  implicit none
  integer, intent(in) :: vector_size, number_of_reflectors
  double complex, intent(in) :: &
    reflectors(vector_size,number_of_reflectors), &
    coefficients(number_of_reflectors), &
    vector(vector_size)
  double precision, intent(out) :: overlap

  interface
    function dznrm2 (n,x,incx)
      integer, intent(in) :: n, incx
      double complex, intent(in) :: x(n)
      double precision :: dznrm2
    end function
    subroutine ztrmv (uplo,trans,diag,n,a,lda,x,incx)
      character, intent(in) :: uplo, trans, diag
      integer, intent(in) :: n, lda, incx
      double complex, intent(in) :: a(n,n), x(n)
    end subroutine
  end interface

  double complex :: projected_vector(vector_size)

  if (number_of_reflectors == 0) then
    overlap = 0
    return
  end if

  call project_into_orthogonal_space( &
    vector_size, &
    number_of_reflectors, vector_size, reflectors, coefficients, &
    vector, &
    projected_vector &
  )

  call ztrmv( &
    'U', 'T', 'N', &
    number_of_reflectors, &
    reflectors(1,1), vector_size, &
    projected_vector(1), 1 &
  )

  overlap = dznrm2(number_of_reflectors,projected_vector,1)

end subroutine
!@-node:gcross.20100520145029.1765:compute_overlap_with_projectors
!@+node:gcross.20100525104555.1804:convert_vectors_to_reflectors
subroutine convert_vectors_to_reflectors( &
  m, n, &
  vectors, &
  rank, &
  coefficients &
)
  implicit none
  integer, intent(in) :: n, m
  integer, intent(out) :: rank
  double complex, intent(inout) :: vectors(m,n)
  double complex, intent(out) :: coefficients(n)

  integer :: jpvt(n), info, lwork
  double complex :: lwork_as_complex
  double precision :: rwork(2*n)

  double complex, allocatable :: work(:)

  external :: zgeqp3, zungqr, dznrm2
  double precision :: dznrm2

  if (n == 0) then
    return
  end if

  if (m < n) then
    print *, "The input matrix has too many columns and not enough rows (column-major ordering) to orthogonalize!"
    stop
  end if

  call zgeqp3( &
    m, n, &
    vectors, m, &
    jpvt, &
    coefficients, &
    lwork_as_complex, -1, &
    rwork, &
    info &
  )
  lwork = int(real(lwork_as_complex))

  if (info /= 0) then
    print *, "Unable to factorize matrix (zgeqp3, workspace query); info =", info
    stop
  end if

  allocate(work(lwork))

  jpvt = 0
  call zgeqp3( &
    m, n, &
    vectors, m, &
    jpvt, &
    coefficients, &
    work, lwork, &
    rwork, &
    info &
  )

  deallocate(work)

  if (info /= 0) then
    print *, "Unable to factorize matrix (zgeqp3); info =", info
    stop
  end if

  rank = n
  do while (dznrm2(n-rank+1,vectors(rank,rank),m) < 1e-12)
    rank = rank - 1
    if (rank == 0) then
      exit
    end if
  end do

end subroutine
!@-node:gcross.20100525104555.1804:convert_vectors_to_reflectors
!@+node:gcross.20100525120117.1807:compute_q_from_reflectors
subroutine compute_q_from_reflectors( &
  m, n, &
  reflectors, &
  coefficients, &
  q &
)
  implicit none
  integer, intent(in) :: n, m
  double complex, intent(in) :: reflectors(m,n), coefficients(n)
  double complex, intent(out) :: q(m,m)

  integer :: info, lwork
  double complex :: lwork_as_complex

  double complex, allocatable :: work(:)

  external :: zungqr, dznrm2
  double precision :: dznrm2

  q(:,:n) = reflectors
  q(:,n+1:) = 0

  call zungqr( &
    m, m, n, &
    q, m, &
    coefficients, &
    lwork_as_complex, -1, &
    info &
  )
  lwork = int(real(lwork_as_complex))

  if (info /= 0) then
    print *, "Error calling zungqr (workspace query); info =", info
    stop
  end if

  allocate(work(lwork))

  call zungqr( &
    m, m, n, &
    q, m, &
    coefficients, &
    work, lwork, &
    info &
  )

  deallocate(work)

  if (info /= 0) then
    print *, "Error calling zungqr (workspace query); info =", info
    stop
  end if

end subroutine
!@-node:gcross.20100525120117.1807:compute_q_from_reflectors
!@+node:gcross.20100525104555.1805:project_into_orthogonal_space
subroutine project_into_orthogonal_space( &
  full_space_dimension, &
  number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
  vector_in_full_space, &
  vector_in_orthogonal_space &
)
  implicit none
  integer, intent(in) :: full_space_dimension, number_of_reflectors, orthogonal_subspace_dimension
  double complex, intent(in) :: &
    reflectors(full_space_dimension,number_of_reflectors), &
    coefficients(number_of_reflectors), &
    vector_in_full_space(full_space_dimension)
  double complex, intent(out) :: &
    vector_in_orthogonal_space(orthogonal_subspace_dimension)

  double complex :: &
    lwork_as_complex, &
    intermediate_vector(full_space_dimension)
  double complex, allocatable :: work(:)
  integer :: lwork, info, start_of_orthogonal_subspace

  if (number_of_reflectors == 0) then
    vector_in_orthogonal_space = vector_in_full_space
    return
  end if

  start_of_orthogonal_subspace = full_space_dimension-(orthogonal_subspace_dimension-1)

  intermediate_vector = vector_in_full_space

  call zunmqr( &
    'R','N', &
    1, full_space_dimension, number_of_reflectors, &
    reflectors, full_space_dimension, &
    coefficients, &
    intermediate_vector, 1, &
    lwork_as_complex, -1, &
    info &
  )

  if (info /= 0) then
    print *, "project_into_orthogonal_space:  Error calling zunmqr (workspace query), info =", info
    stop
  end if

  lwork = int(lwork_as_complex)
  allocate(work(lwork))

  call zunmqr( &
    'R','N', &
    1, full_space_dimension, number_of_reflectors, &
    reflectors, full_space_dimension, &
    coefficients, &
    intermediate_vector, 1, &
    work, lwork, &
    info &
  )

  deallocate(work)

  if (info /= 0) then
    print *, "project_into_orthogonal_space:  Error calling zunmqr, info =", info
    stop
  end if

  vector_in_orthogonal_space = &
    intermediate_vector(start_of_orthogonal_subspace:full_space_dimension)

end subroutine
!@nonl
!@-node:gcross.20100525104555.1805:project_into_orthogonal_space
!@+node:gcross.20100525104555.1809:unproject_from_orthogonal_space
subroutine unproject_from_orthogonal_space( &
  full_space_dimension, &
  number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
  vector_in_orthogonal_space, &
  vector_in_full_space &
)
  implicit none
  integer, intent(in) :: full_space_dimension, number_of_reflectors, orthogonal_subspace_dimension
  double complex, intent(in) :: &
    reflectors(full_space_dimension,number_of_reflectors), &
    coefficients(number_of_reflectors), &
    vector_in_orthogonal_space(orthogonal_subspace_dimension)
  double complex, intent(out) :: &
    vector_in_full_space(full_space_dimension)

  double complex :: &
    lwork_as_complex
  double complex, allocatable :: work(:)
  integer :: lwork, info, start_of_orthogonal_subspace

  if (number_of_reflectors == 0) then
    vector_in_full_space = vector_in_orthogonal_space
    return
  end if

  start_of_orthogonal_subspace = full_space_dimension-(orthogonal_subspace_dimension-1)

  vector_in_full_space(1:start_of_orthogonal_subspace-1) = 0 
  vector_in_full_space(start_of_orthogonal_subspace:) = vector_in_orthogonal_space

  call zunmqr( &
    'R','C', &
    1, full_space_dimension, number_of_reflectors, &
    reflectors, full_space_dimension, &
    coefficients, &
    vector_in_full_space, 1, &
    lwork_as_complex, -1, &
    info &
  )

  if (info /= 0) then
    print *, "unproject_from_orthogonal_space:  Error calling zunmqr (workspace query), info =", info
    stop
  end if

  lwork = int(lwork_as_complex)
  allocate(work(lwork))

  call zunmqr( &
    'R','C', &
    1, full_space_dimension, number_of_reflectors, &
    reflectors, full_space_dimension, &
    coefficients, &
    vector_in_full_space, 1, &
    work, lwork, &
    info &
  )

  deallocate(work)

  if (info /= 0) then
    print *, "unproject_from_orthogonal_space:  Error calling zunmqr, info =", info
    stop
  end if

end subroutine
!@-node:gcross.20100525104555.1809:unproject_from_orthogonal_space
!@+node:gcross.20100525120117.1842:project_matrix_into_orthog_space
subroutine project_matrix_into_orthog_space( &
  full_space_dimension, &
  number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
  matrix_in_full_space, &
  matrix_in_orthogonal_space &
)
  implicit none
  integer, intent(in) :: full_space_dimension, number_of_reflectors, orthogonal_subspace_dimension
  double complex, intent(in) :: &
    reflectors(full_space_dimension,number_of_reflectors), &
    coefficients(number_of_reflectors), &
    matrix_in_full_space(full_space_dimension,full_space_dimension)
  double complex, intent(out) :: &
    matrix_in_orthogonal_space(orthogonal_subspace_dimension,orthogonal_subspace_dimension)

  double complex :: &
    lwork_as_complex_1, lwork_as_complex_2, &
    intermediate_matrix(full_space_dimension,full_space_dimension)
  double complex, allocatable :: work(:)
  integer :: lwork, info, start_of_orthogonal_subspace

  if (number_of_reflectors == 0) then
    matrix_in_orthogonal_space = matrix_in_full_space
    return
  end if

  start_of_orthogonal_subspace = full_space_dimension-(orthogonal_subspace_dimension-1)

  intermediate_matrix = conjg(matrix_in_full_space)

  call zunmqr( &
    'R','N', &
    full_space_dimension, full_space_dimension, number_of_reflectors, &
    reflectors, full_space_dimension, &
    coefficients, &
    intermediate_matrix, full_space_dimension, &
    lwork_as_complex_1, -1, &
    info &
  )

  call zunmqr( &
    'L','C', &
    full_space_dimension, full_space_dimension, number_of_reflectors, &
    reflectors, full_space_dimension, &
    coefficients, &
    intermediate_matrix, full_space_dimension, &
    lwork_as_complex_2, -1, &
    info &
  )

  if (info /= 0) then
    print *, "Error calling zunmqr (workspace query):  info =", info
    stop
  end if

  lwork = min(int(lwork_as_complex_1),int(lwork_as_complex_2))
  allocate(work(lwork))

  call zunmqr( &
    'R','N', &
    full_space_dimension, full_space_dimension, number_of_reflectors, &
    reflectors, full_space_dimension, &
    coefficients, &
    intermediate_matrix, full_space_dimension, &
    work, lwork, &
    info &
  )

  call zunmqr( &
    'L','C', &
    full_space_dimension, full_space_dimension, number_of_reflectors, &
    reflectors, full_space_dimension, &
    coefficients, &
    intermediate_matrix, full_space_dimension, &
    work, lwork, &
    info &
  )

  deallocate(work)

  if (info /= 0) then
    print *, "Error calling zunmqr:  info =", info
    stop
  end if

  matrix_in_orthogonal_space = &
    conjg(intermediate_matrix(start_of_orthogonal_subspace:full_space_dimension,start_of_orthogonal_subspace:full_space_dimension))

end subroutine
!@-node:gcross.20100525120117.1842:project_matrix_into_orthog_space
!@+node:gcross.20100525120117.1843:compute_opt_mat_in_orthog_space
subroutine compute_opt_mat_in_orthog_space( &
  bl, br, & ! state bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
  right_environment, &
  number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
  optimization_matrix &
)
  implicit none
  integer, intent(in) :: &
    bl, br, cl, cr, d, &
    number_of_matrices, sparse_operator_indices(2,number_of_matrices), &
    number_of_reflectors, orthogonal_subspace_dimension
  double complex, intent(in) :: &
    left_environment(bl,bl,cl), &
    right_environment(br,br,cr), &
    sparse_operator_matrices(d,d,number_of_matrices), &
    reflectors(br*bl*d,number_of_reflectors), &
    coefficients(number_of_reflectors)
  double complex, intent(out) :: &
    optimization_matrix(orthogonal_subspace_dimension,orthogonal_subspace_dimension)

  double complex :: &
    intermediate_matrix(br,bl,d,br,bl,d)

  call compute_optimization_matrix( &
    bl, br, cl, cr, d, &
    left_environment, &
    number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
    right_environment, &
    intermediate_matrix &
  )

  call project_matrix_into_orthog_space( &
    br*bl*d, &
    number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
    intermediate_matrix, &
    optimization_matrix &
  )

end subroutine
!@nonl
!@-node:gcross.20100525120117.1843:compute_opt_mat_in_orthog_space
!@+node:gcross.20100525120117.1845:compute_opt_matrix_all_cases
subroutine compute_opt_matrix_all_cases( &
  bl, br, & ! state bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
  right_environment, &
  number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
  optimization_matrix &
)
  implicit none
  integer, intent(in) :: &
    bl, br, cl, cr, d, &
    number_of_matrices, sparse_operator_indices(2,number_of_matrices), &
    number_of_reflectors, orthogonal_subspace_dimension
  double complex, intent(in) :: &
    left_environment(bl,bl,cl), &
    right_environment(br,br,cr), &
    sparse_operator_matrices(d,d,number_of_matrices), &
    reflectors(br*bl*d,number_of_reflectors), &
    coefficients(number_of_reflectors)
  double complex, intent(out) :: &
    optimization_matrix(orthogonal_subspace_dimension,orthogonal_subspace_dimension)

  integer :: full_space_dimension

  full_space_dimension = d*br*bl

  if (orthogonal_subspace_dimension > full_space_dimension) then
    print *, "orthogonal_subspace_dimension = ", orthogonal_subspace_dimension, &
             " but d*bl*br = ",d,"*",bl,"*",br,"=",d*bl*br
    stop
  end if

  if (number_of_reflectors == 0) then
    call compute_optimization_matrix( &
      bl, br, cl, cr, d, &
      left_environment, &
      number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
      right_environment, &
      optimization_matrix &
    )
  else
    call compute_opt_mat_in_orthog_space( &
      bl, br, cl, cr, d, &
      left_environment, &
      number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
      right_environment, &
      number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
      optimization_matrix &
    )
  end if
end subroutine
!@-node:gcross.20100525120117.1845:compute_opt_matrix_all_cases
!@+node:gcross.20100525190742.1822:filter_components_outside_orthog
subroutine filter_components_outside_orthog( &
  full_space_dimension, &
  number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
  input, &
  output &
)
  implicit none
  integer, intent(in) :: full_space_dimension, number_of_reflectors, orthogonal_subspace_dimension
  double complex, intent(in) :: &
    reflectors(full_space_dimension,number_of_reflectors), &
    coefficients(number_of_reflectors), &
    input(full_space_dimension)
  double complex, intent(out) :: &
    output(full_space_dimension)

  double complex :: &
    projected(orthogonal_subspace_dimension)

  if (number_of_reflectors == 0) then
    output = input
    return
  end if

  call project_into_orthogonal_space( &
    full_space_dimension, &
    number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
    input, &
    projected &
  )

  call unproject_from_orthogonal_space( &
    full_space_dimension, &
    number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
    projected, &
    output &
  )

end subroutine
!@-node:gcross.20100525190742.1822:filter_components_outside_orthog
!@-node:gcross.20100520145029.1766:Projectors
!@+node:gcross.20100517000234.1775:optimize
function optimize( &
  bl, br, & ! state bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
  right_environment, &
  number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
  which, &
  tol, &
  number_of_iterations, &
  guess, &
  result, &
  eigenvalue &
) result (info)
  implicit none
  integer, intent(in) :: &
    bl, br, cl, cr, d, &
    number_of_matrices, sparse_operator_indices(2,number_of_matrices), &
    number_of_reflectors, orthogonal_subspace_dimension
  integer, intent(inout) :: number_of_iterations
  double complex, intent(in) :: &
    left_environment(bl,bl,cl), &
    right_environment(br,br,cr), &
    sparse_operator_matrices(d,d,number_of_matrices), &
    reflectors(br*bl*d,number_of_reflectors), &
    coefficients(number_of_reflectors), &
    guess(br,bl,d)
  double complex, intent(out) :: &
    result(br,bl,d), &
    eigenvalue
  character, intent(in) :: which*2
  double precision, intent(in) :: tol

  integer :: info, full_space_dimension, strategy
  double precision :: overlap, norm_of_result

  interface
    function dznrm2 (n,x,incx)
      integer, intent(in) :: n, incx
      double complex, intent(in) :: x(n)
      double precision :: dznrm2
    end function
  end interface

  full_space_dimension = d*bl*br

  if (orthogonal_subspace_dimension > full_space_dimension) then
    info = 5
    return
  end if

  call compute_overlap_with_projectors( &
    number_of_reflectors, reflectors, coefficients, &
    full_space_dimension, guess(1,1,1), &
    overlap &
  )

  if ((orthogonal_subspace_dimension < full_space_dimension) .and. &
      (overlap > 1e-7) &
  ) then
    info = 11
    return
  end if

  if (orthogonal_subspace_dimension <= 4) then
    strategy = 1
    ! print *, "strategy 1", number_of_reflectors, full_space_dimension, orthogonal_subspace_dimension
    call optimize_strategy_1( &
      bl, br, &
      cl, &
      cr, &
      d, &
      left_environment, &
      number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
      right_environment, &
      number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
      which, &
      tol, &
      number_of_iterations, &
      guess, &
      info, &
      result, &
      eigenvalue &
    )
  else if ( bl*br < cl*cr ) then
    strategy = 2
    ! print *, "strategy 2"
    call optimize_strategy_2( &
      bl, br, &
      cl, &
      cr, &
      d, &
      left_environment, &
      number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
      right_environment, &
      number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
      which, &
      tol, &
      number_of_iterations, &
      guess, &
      info, &
      result, &
      eigenvalue &
    )
  else
    strategy = 3
    ! print *, "strategy 3", full_space_dimension, orthogonal_subspace_dimension
    call optimize_strategy_3( &
      bl, br, &
      cl, &
      cr, &
      d, &
      left_environment, &
      number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
      right_environment, &
      number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
      which, &
      tol, &
      number_of_iterations, &
      guess, &
      info, &
      result, &
      eigenvalue &
    )
  end if

  if ( info == -14 .and. full_space_dimension < number_of_iterations) then
    strategy = 1
    ! print *, "re-trying with strategy 1", d, bl, br, number_of_reflectors, full_space_dimension, orthogonal_subspace_dimension
    call optimize_strategy_1( &
      bl, br, &
      cl, &
      cr, &
      d, &
      left_environment, &
      number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
      right_environment, &
      number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
      which, &
      tol, &
      number_of_iterations, &
      guess, &
      info, &
      result, &
      eigenvalue &
    )
  end if

  if ( info < 0 ) then
    return
  end if

end function
!@-node:gcross.20100517000234.1775:optimize
!@+node:gcross.20100517000234.1786:optimize_strategy_1
subroutine optimize_strategy_1( &
  bl, br, & ! state bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
  right_environment, &
  number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
  which, &
  tol, &
  number_of_iterations, &
  guess, &
  info, &
  result, &
  eigenvalue &
)
  implicit none
  integer, intent(in) :: &
    bl, br, cl, cr, d, &
    number_of_matrices, sparse_operator_indices(2,number_of_matrices), &
    number_of_reflectors, orthogonal_subspace_dimension
  integer, intent(inout) :: number_of_iterations
  integer, intent(out) :: info
  double complex, intent(in) :: &
    left_environment(bl,bl,cl), &
    right_environment(br,br,cr), &
    sparse_operator_matrices(d,d,number_of_matrices), &
    reflectors(br*bl*d,number_of_reflectors), &
    coefficients(number_of_reflectors), &
    guess(br,bl,d)
  double complex, intent(out) :: &
    result(br,bl,d), &
    eigenvalue
  character, intent(in) :: which*2
  double precision, intent(in) :: tol

  double complex :: &
    optimization_matrix(orthogonal_subspace_dimension,orthogonal_subspace_dimension), &
    projected_eigenvector(orthogonal_subspace_dimension)

  call compute_opt_matrix_all_cases( &
    bl, br, cl, cr, d, &
    left_environment, &
    number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
    right_environment, &
    number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
    optimization_matrix &
  )

  call lapack_eigenvalue_minimizer(orthogonal_subspace_dimension,optimization_matrix,eigenvalue,projected_eigenvector)

  call unproject_from_orthogonal_space( &
    br*bl*d, &
    number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
    projected_eigenvector, &
    result(1,1,1) &
  )

  info = 0

end subroutine
!@-node:gcross.20100517000234.1786:optimize_strategy_1
!@+node:gcross.20091109182634.1537:optimize_strategy_2
subroutine optimize_strategy_2( &
  bl, br, & ! state bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
  right_environment, &
  number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
  which, &
  tol, &
  number_of_iterations, &
  guess, &
  info, &
  result, &
  eigenvalue &
)

  implicit none

  !@  << Declarations >>
  !@+node:gcross.20091115201814.1731:<< Declarations >>
  !@+others
  !@+node:gcross.20091115201814.1729:Arguments
  integer, intent(in) :: &
    bl, br, cl, cr, d, &
    number_of_matrices, sparse_operator_indices(2,number_of_matrices), &
    number_of_reflectors, orthogonal_subspace_dimension
  integer, intent(inout) :: number_of_iterations
  integer, intent(out) :: info
  double complex, intent(in) :: &
    left_environment(bl,bl,cl), &
    right_environment(br,br,cr), &
    sparse_operator_matrices(d,d,number_of_matrices), &
    reflectors(br*bl*d,number_of_reflectors), &
    coefficients(number_of_reflectors), &
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
    !@+node:gcross.20100513000837.1740:dznrm2
    function dznrm2 (n,x,incx)
      integer, intent(in) :: n, incx
      double complex, intent(in) :: x

      double precision :: dznrm2
    end function
    !@-node:gcross.20100513000837.1740:dznrm2
    !@-others
  end interface
  !@-node:gcross.20091109182634.1538:Interface
  !@+node:gcross.20091115201814.1730:Work arrays
  integer, parameter :: nev = 1, ncv = 3

  double complex :: &
    optimization_matrix(orthogonal_subspace_dimension,orthogonal_subspace_dimension), &
    projected_guess(orthogonal_subspace_dimension), &
    projected_result(orthogonal_subspace_dimension)
  !@nonl
  !@-node:gcross.20091115201814.1730:Work arrays
  !@-others
  !@-node:gcross.20091115201814.1731:<< Declarations >>
  !@nl

  call compute_opt_matrix_all_cases( &
    bl, br, cl, cr, d, &
    left_environment, &
    number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
    right_environment, &
    number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
    optimization_matrix &
  )

  call  project_into_orthogonal_space( &
    br*bl*d, &
    number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
    guess(1,1,1), &
    projected_guess &
  )

  call run_arpack(orthogonal_subspace_dimension,nev,ncv,projected_guess,eigenvalue,projected_result)

  call unproject_from_orthogonal_space( &
    br*bl*d, &
    number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
    projected_result, &
    result(1,1,1) &
  )

contains

  !@  @+others
  !@+node:gcross.20100516220142.1754:run_arpack
  subroutine run_arpack(n,nev,ncv,guess,eigenvalue,eigenvector)

    integer, intent(in) :: n, nev, ncv

    double complex, intent(in) :: guess(n)

    double complex, intent(out) :: eigenvalue, eigenvector(n)

    double complex :: &
      v(n,ncv), &
      workd(3*n), &
      workl(3*ncv**2+5*ncv), &
      eigenvalues(nev+1), &
      workev(2*ncv), &
      resid(n)

    integer :: &
      iparam(11), &
      ipntr(14), &
      ido

    logical :: &
      select(ncv)

    double precision :: rwork(ncv)

    resid = guess

    iparam = 0
    ido = 0
    info = 1

    iparam(1) = 1
    iparam(3) = number_of_iterations
    iparam(7) = 1

    do while (ido /= 99)
      call znaupd ( &
        ido, 'I', n, which, nev, tol, resid, ncv, v, n, &
        iparam, ipntr, workd, workl, 3*ncv**2+5*ncv, rwork, info &
      ) 
      if (ido == -1 .or. ido == 1) then
        call operate_on(workd(ipntr(1)),workd(ipntr(2)))
      end if
    end do

    number_of_iterations = iparam(3)

    call zneupd (.true.,'A', select, eigenvalues, v, n, (0d0,0d0), workev, &
                  'I', n, which, nev, tol, resid, ncv, &
                  v, n, iparam, ipntr, workd, workl, 3*ncv**2+5*ncv, &
                  rwork, info)

    eigenvalue = eigenvalues(1)
    eigenvector = v(:,1)

  end subroutine
  !@-node:gcross.20100516220142.1754:run_arpack
  !@+node:gcross.20100513000837.1743:operate_on
  subroutine operate_on(input,output)
    double complex :: input(orthogonal_subspace_dimension), output(orthogonal_subspace_dimension)

    call zhemv( &
      'U', &
      orthogonal_subspace_dimension, &
      (1d0,0d0), optimization_matrix, orthogonal_subspace_dimension, &
      input, 1, &
      (0d0,0d0), output, 1 &
    )

  end subroutine
  !@-node:gcross.20100513000837.1743:operate_on
  !@-others

end subroutine
!@nonl
!@-node:gcross.20091109182634.1537:optimize_strategy_2
!@+node:gcross.20100517000234.1753:optimize_strategy_3
subroutine optimize_strategy_3( &
  bl, br, & ! state bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
  right_environment, &
  number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
  which, &
  tol, &
  number_of_iterations, &
  guess, &
  info, &
  result, &
  eigenvalue &
)

  implicit none

  !@  << Declarations >>
  !@+node:gcross.20100517000234.1754:<< Declarations >>
  !@+others
  !@+node:gcross.20100517000234.1755:Arguments
  integer, intent(in) :: &
    bl, br, cl, cr, d, &
    number_of_matrices, sparse_operator_indices(2,number_of_matrices), &
    number_of_reflectors, orthogonal_subspace_dimension
  integer, intent(inout) :: number_of_iterations
  integer, intent(out) :: info
  double complex, intent(in) :: &
    left_environment(bl,bl,cl), &
    right_environment(br,br,cr), &
    sparse_operator_matrices(d,d,number_of_matrices), &
    reflectors(br*bl*d,number_of_reflectors), &
    coefficients(number_of_reflectors), &
    guess(br,bl,d)
  double complex, intent(out) :: &
    result(br,bl,d), &
    eigenvalue
  character, intent(in) :: which*2
  double precision, intent(in) :: tol
  !@-node:gcross.20100517000234.1755:Arguments
  !@+node:gcross.20100517000234.1756:Interface
  interface
    !@  @+others
    !@+node:gcross.20100517000234.1757:znaupd
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
    !@-node:gcross.20100517000234.1757:znaupd
    !@+node:gcross.20100517000234.1758:zneupd
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
    !@-node:gcross.20100517000234.1758:zneupd
    !@+node:gcross.20100517000234.1759:dznrm2
    function dznrm2 (n,x,incx)
      integer, intent(in) :: n, incx
      double complex, intent(in) :: x

      double precision :: dznrm2
    end function
    !@-node:gcross.20100517000234.1759:dznrm2
    !@-others
  end interface
  !@-node:gcross.20100517000234.1756:Interface
  !@+node:gcross.20100517000234.1760:Work arrays
  integer, parameter :: nev = 1, ncv = 3

  double complex :: &
    iteration_stage_1_tensor(bl,d,cr,bl,d), &
    iteration_stage_2_tensor(br,cr,bl,d)

  double complex :: &
    projected_guess(orthogonal_subspace_dimension), &
    projected_result(orthogonal_subspace_dimension)
  !@-node:gcross.20100517000234.1760:Work arrays
  !@-others
  !@-node:gcross.20100517000234.1754:<< Declarations >>
  !@nl

  call iteration_stage_1( &
    bl, cl, cr, d, &
    left_environment, &
    number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
    iteration_stage_1_tensor &
  )

  call  project_into_orthogonal_space( &
    br*bl*d, &
    number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
    guess(1,1,1), &
    projected_guess &
  )

  call run_arpack(orthogonal_subspace_dimension,nev,ncv,projected_guess,eigenvalue,projected_result)

  call unproject_from_orthogonal_space( &
    br*bl*d, &
    number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
    projected_result, &
    result(1,1,1) &
  )

contains

  !@  @+others
  !@+node:gcross.20100517000234.1761:run_arpack
  subroutine run_arpack(n,nev,ncv,guess,eigenvalue,eigenvector)

    integer, intent(in) :: n, nev, ncv

    double complex, intent(in) :: guess(n)

    double complex, intent(out) :: eigenvalue, eigenvector(n)

    double complex :: &
      v(n,ncv), &
      workd(3*n), &
      workl(3*ncv**2+5*ncv), &
      eigenvalues(nev+1), &
      workev(2*ncv), &
      resid(n)

    integer :: &
      iparam(11), &
      ipntr(14), &
      ido

    logical :: &
      select(ncv)

    double precision :: rwork(ncv)

    resid = guess

    iparam = 0
    ido = 0
    info = 1

    iparam(1) = 1
    iparam(3) = number_of_iterations
    iparam(7) = 1

    do while (ido /= 99)
      call znaupd ( &
        ido, 'I', n, which, nev, tol, resid, ncv, v, n, &
        iparam, ipntr, workd, workl, 3*ncv**2+5*ncv, rwork, info &
      ) 
      if (ido == -1 .or. ido == 1) then
        call operate_on(workd(ipntr(1)),workd(ipntr(2)))
      end if
    end do

    number_of_iterations = iparam(3)

    call zneupd (.true.,'A', select, eigenvalues, v, n, (0d0,0d0), workev, &
                  'I', n, which, nev, tol, resid, ncv, &
                  v, n, iparam, ipntr, workd, workl, 3*ncv**2+5*ncv, &
                  rwork, info)

    eigenvalue = eigenvalues(1)
    eigenvector = v(:,1)

  end subroutine
  !@-node:gcross.20100517000234.1761:run_arpack
  !@+node:gcross.20100517000234.1762:operate_on
  subroutine operate_on(input,output)
    double complex :: input(orthogonal_subspace_dimension), projected(br,bl,d), output(orthogonal_subspace_dimension)

    call unproject_from_orthogonal_space( &
      br*bl*d, &
      number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
      input, &
      projected &
    )
    call iteration_stage_2( &
      bl, br, cr, d, &
      iteration_stage_1_tensor, &
      projected, &
      iteration_stage_2_tensor &
    )
    call iteration_stage_3( &
      bl, br, cr, d, &
      iteration_stage_2_tensor, &
      right_environment, &
      projected &
    )
    call project_into_orthogonal_space( &
      br*bl*d, &
      number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, &
      projected, &
      output &
    )

  end subroutine
  !@-node:gcross.20100517000234.1762:operate_on
  !@-others

end subroutine
!@-node:gcross.20100517000234.1753:optimize_strategy_3
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
    state_site_tensor(i,j,k) = (0.5d0,0d0)-rand()*(1d0,0d0) + (0,0.5d0)-rand()*(0d0,1d0)
  end do
  end do
  end do
end subroutine
!@-node:gcross.20091110205054.1920:randomize_state_site_tensor
!@+node:gcross.20091110205054.1922:rand_norm_state_site_tensor
subroutine rand_norm_state_site_tensor(br, bl, d, state_site_tensor)
  integer, intent(in) :: br, bl, d
  double complex, intent(out) :: state_site_tensor(br,bl,d)

  double complex :: workspace(br,d,bl)
  integer :: rank

  if (br*d < bl) then
    print *, "Not enough degrees of freedom to normalize."
    print *, br*d, "<", bl
    stop
  end if

  call randomize_state_site_tensor(br, d, bl, workspace)

  call orthogonalize_matrix_in_place(br*d, bl, workspace, rank)
  if ( rank < bl ) then
    print *, "rand_norm_state_site_tensor:  Bad rank", rank, "<", bl
    stop
  end if

  state_site_tensor = reshape(workspace,shape(state_site_tensor),order=(/1,3,2/))

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
!@+node:gcross.20100521141104.1771:random_projector_matrix
subroutine random_projector_matrix( &
  projector_length, number_of_projectors, &
  rank, &
  reflectors, coefficients &
)
  integer, intent(in) :: projector_length, number_of_projectors
  double complex, intent(out) :: &
    reflectors(projector_length,number_of_projectors), &
    coefficients(number_of_projectors)
  integer, intent(out) :: rank

  integer :: i, j

  if (number_of_projectors == 0) then
    return
  end if

  do j = 1, number_of_projectors
  do i = 1, projector_length
    reflectors(i,j) = (0.5d0-rand())*(1d0,0d0) + (0.5d0-rand())*(0d0,1d0)
  end do
  end do

  call convert_vectors_to_reflectors( &
    projector_length, number_of_projectors, &
    reflectors, &
    rank, &
    coefficients &
  )
end subroutine
!@-node:gcross.20100521141104.1771:random_projector_matrix
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

  integer :: i, j, rank

  do j = 1, old_bandwidth
  do i = 1, new_bandwidth
    matrix(i,j) = (0.5d0,0d0)-rand()*(1d0,0d0) + (0,0.5d0)-rand()*(0d0,1d0)
  end do
  end do

  call orthogonalize_matrix_in_place(new_bandwidth,old_bandwidth,matrix,rank)

  if ( rank < old_bandwidth ) then
    print *, "create_bandwidth_increase_matrix:  Bad rank", rank, "<", old_bandwidth
    stop
  end if

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
