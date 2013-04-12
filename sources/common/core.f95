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
subroutine lapack_eigenvalue_real_optimizer(n,matrix,which,eigenvalue,eigenvector)
  integer, intent(in) :: n
  double complex, intent(in) :: matrix(n,n)
  character, intent(in) :: which*2

  double complex, intent(out) :: eigenvalue, eigenvector(n)

  integer :: number_of_eigenvalues_found, lwork, lrwork, liwork(1), eigenvalue_index
  double precision :: lrwork_as_double(1)
  double complex :: lwork_as_complex(1), temp(n,n)
  double complex, allocatable :: work(:)
  double precision, allocatable :: rwork(:)
  integer, allocatable :: iwork(:)

  integer :: info, eigenvector_support(2)
  double precision :: w(n)

  character, parameter :: SR*2 = 'SR', LR*2 = 'LR'

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

  if(which == SR) then
    eigenvalue_index = 1
  elseif(which == LR) then
    eigenvalue_index = n
  else
    print *, "Invalid eigenvalue requested: ", which
    stop
  end if

  call zheevr(&
    'V', &
    'I', &
    'U', &
    n, &
    temp, n, &
    0d0, 0d0, &
    eigenvalue_index, eigenvalue_index, &
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
    eigenvalue_index, eigenvalue_index, &
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
subroutine lapack_eigenvalue_mag_maximizer(n,matrix,eigenvalue,eigenvector)
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
  double precision :: w(n), w1

  double complex :: eigenvector2(n)

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
    n, n, &
    0d0, &
    number_of_eigenvalues_found, &
    w, &
    eigenvector, n, eigenvector_support, &
    work, lwork, &
    rwork, lrwork, &
    iwork, liwork(1), &
    info &
  )
  if (info /= 0) then
    print *, "Error computing eigenvalue (zheevr); info =", info
    print *, "n=",n
    stop
  end if

  w1 = w(1)

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
    eigenvector2, n, eigenvector_support, &
    work, lwork, &
    rwork, lrwork, &
    iwork, liwork(1), &
    info &
  )
  if (info /= 0) then
    print *, "Error computing eigenvalue (zheevr); info =", info
    print *, "n=",n
    stop
  end if

  deallocate(work,rwork,iwork)

  if(abs(w1) > abs(w(1))) then
    eigenvalue = w1*(1d0,0d0)
  else
    eigenvalue = w(1)*(1d0,0d0)
    eigenvector = eigenvector2
  end if

end subroutine
subroutine swap_inplace(n, swaps, vector)
  implicit none
  integer, intent(in) :: n, swaps(n)
  double complex, intent(inout) :: vector(n)

  integer :: i
  double complex :: shelf

  if (n == 0) then
    return
  end if

  do i = 1, n
    if (swaps(i) /= i) then
      shelf = vector(i)
      vector(i) = vector(swaps(i))
      vector(swaps(i)) = shelf
    end if
  end do
end subroutine
subroutine unswap_inplace(n, swaps, vector)
  implicit none
  integer, intent(in) :: n, swaps(n)
  double complex, intent(inout) :: vector(n)

  integer :: i
  double complex :: shelf

  if (n == 0) then
    return
  end if

  do i = n, 1, -1
    if (swaps(i) /= i) then
      shelf = vector(i)
      vector(i) = vector(swaps(i))
      vector(swaps(i)) = shelf
    end if
  end do
end subroutine
subroutine swap_matrix_inplace(n, swaps, matrix, leading_dimension)
  implicit none
  integer, intent(in) :: n, swaps(n), leading_dimension
  double complex, intent(inout) :: matrix(leading_dimension,n)

  interface
    subroutine zswap(n,x,incx,y,incy)
      integer, intent(in) :: n, incx, incy
      double complex, intent(inout) :: x, y
    end subroutine
  end interface

  integer :: i

  if (n == 0) then
    return
  end if

  do i = 1, n
    if (swaps(i) /= i) then
      call zswap(n,matrix(1,i),1,matrix(1,swaps(i)),1)
      call zswap(n,matrix(i,1),n,matrix(swaps(i),1),leading_dimension)
    end if
  end do
end subroutine
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
subroutine contract_vs_left( &
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
subroutine contract_vs_right( &
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
subroutine contract_operator_random_left( &
  cl, cr, d, &
  left_boundary_1, left_boundary_2, &
  number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
  new_left_boundary_1, new_left_boundary_2 &
)
  integer, intent(in) :: cl, cr, d, number_of_matrices, sparse_operator_indices(2,number_of_matrices)
  double complex, intent(in) :: &
    left_boundary_1(cl), left_boundary_2(cl), &
    sparse_operator_matrices(d*d,number_of_matrices)
  double complex, intent(out) :: new_left_boundary_1(cr), new_left_boundary_2(cr)

  integer :: matrix_number, left_index, right_index, i, j
  double complex :: matrix(d,d), vector_1(d*d), vector_2(d*d)

  do i = 1, d
  do j = 1, d
    matrix(i,j) = (0.5d0,0d0)-rand()*(1d0,0d0) + (0,0.5d0)-rand()*(0d0,1d0)
  end do
  end do

  vector_1 = reshape(matrix,shape(vector_1))
  vector_2 = reshape(conjg(transpose(matrix)),shape(vector_2))

  new_left_boundary_1 = 0
  new_left_boundary_2 = 0
  do matrix_number = 1, number_of_matrices
    left_index = sparse_operator_indices(1,matrix_number)
    right_index = sparse_operator_indices(2,matrix_number)
    new_left_boundary_1(right_index) = new_left_boundary_1(right_index) &
      + dot_product(vector_1,sparse_operator_matrices(:,matrix_number)*left_boundary_1(left_index))
    new_left_boundary_2(right_index) = new_left_boundary_2(right_index) &
      + dot_product(vector_2,sparse_operator_matrices(:,matrix_number)*left_boundary_2(left_index))
  end do
end subroutine
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
subroutine contract_expectation_boundaries( &
  b, & ! state bandwidth dimension
  c, & ! operator bandwidth dimension
  left_environment, &
  right_environment, &
  expectation &
)
  integer, intent(in) :: b,c
  double complex, intent(in) :: &
    left_environment(b,b,c), &
    right_environment(b,b,c)
  double complex :: expectation, transposed_left_environment(b,b,c)

  transposed_left_environment = reshape(left_environment,shape(left_environment),order=(/2,1,3/))

  expectation = sum(transposed_left_environment(:,:,:)*right_environment(:,:,:))

end subroutine
subroutine extend_state_vector_fragment( &
  bm,br, &
  dl,dr, &
  old_state_vector_fragment, &
  site_tensor, &
  new_state_vector_fragment &
)
  integer, intent(in) :: bm, br, dl, dr
  double complex, intent(in) :: &
    old_state_vector_fragment(bm,dl), &
    site_tensor(br,bm,dr)
  double complex, intent(out) :: &
    new_state_vector_fragment(br,dr,dl)
  double complex :: &
    transposed_site_tensor(br,dr,bm)

  transposed_site_tensor = reshape(site_tensor,shape(transposed_site_tensor),order=(/1,3,2/))

  call zgemm( &
    'N','N', &
    br*dr,dl,bm, &
    (1d0,0d0), &
    transposed_site_tensor, br*dr, &
    old_state_vector_fragment, bm, &
    (0d0,0d0), &
    new_state_vector_fragment, br*dr &
  )

end subroutine
subroutine contract_matrix_left( &
  bl,br, &
  left_environment, &
  matrix, &
  new_left_environment &
)
  integer, intent(in) :: bl, br
  double complex, intent(in) :: &
    left_environment(bl,bl), &
    matrix(br,bl)
  double complex, intent(out) :: new_left_environment(br,br)

  double complex :: &
    intermediate_tensor(br,bl)

  new_left_environment = 0

  call zgemm( &
      'N','N', &
      br,bl,bl, &
      (1d0,0d0), &
      matrix, br, &
      left_environment, bl, &
      (0d0,0d0), &
      intermediate_tensor, br &
  )

  call zgemm( &
      'N','C', &
      br,br,bl, &
      (1d0,0d0), &
      intermediate_tensor, br, &
      matrix, br, &
      (0d0,0d0), &
      new_left_environment, br &
  )

end subroutine
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
subroutine compute_overlap_with_projectors( &
  number_of_projectors, number_of_reflectors, reflectors, coefficients, swaps, &
  vector_size, vector, &
  overlap &
)
  implicit none
  integer, intent(in) :: &
    vector_size, &
    number_of_projectors, &
    number_of_reflectors, &
    swaps(number_of_reflectors)
  double complex, intent(in) :: &
    reflectors(vector_size,number_of_projectors), &
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
    subroutine zgemv (trans,m,n,alpha,a,lda,x,incx,beta,y,incy)
      character, intent(in) :: trans
      integer, intent(in) :: m, n, lda, incx, incy
      double complex, intent(in) :: alpha, beta, a(m,n), x(n), y(m)
    end subroutine
  end interface

  double complex :: projected_vector(vector_size), weight_vector(number_of_projectors)

  if (number_of_projectors == 0) then
    overlap = 0
    return
  end if

  call project_into_orthogonal_space( &
    vector_size, &
    number_of_projectors, number_of_reflectors, vector_size, reflectors, coefficients, swaps, &
    vector, &
    projected_vector &
  )

  call unswap_inplace(size(swaps),swaps,projected_vector(1:size(swaps)))

  weight_vector(1:number_of_reflectors) = projected_vector(1:number_of_reflectors)

  call ztrmv( &
    'U', 'T', 'N', &
    number_of_reflectors, &
    reflectors(:,:number_of_reflectors), vector_size, &
    weight_vector, 1 &
  )

  if (number_of_projectors > number_of_reflectors) then
    call zgemv( &
      'T', &
      vector_size, number_of_projectors-number_of_reflectors, &
      (1d0,0d0), reflectors(:,number_of_reflectors+1:), vector_size, &
      projected_vector, 1, &
      (0d0,0d0), weight_vector(number_of_reflectors+1:), 1 &
    )
  end if

  overlap = dznrm2(number_of_projectors,weight_vector,1)

end subroutine
subroutine convert_vectors_to_reflectors( &
  m, n, &
  vectors, &
  rank, &
  coefficients, swaps &
)
  implicit none
  integer, intent(in) :: m, n
  integer, intent(out) :: rank
  double complex, intent(inout) :: vectors(m,n)
  double complex, intent(out) :: coefficients(min(m,n))
  integer, intent(out) :: swaps(min(m,n))

  interface
    pure function dznrm2 (n,x,incx)
      integer, intent(in) :: n, incx
      double complex, intent(in) :: x(n)
      double precision :: dznrm2
    end function
  end interface

  integer :: jpvt(n), info, lwork, i, j
  double complex :: lwork_as_complex
  double precision :: rwork(2*n), shelf

  double complex, allocatable :: work(:)

  external :: zgeqp3, zungqr
  double precision :: norms(m)

  if (n == 0) then
    return
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

  forall (i = 1:size(swaps))
    norms(i) = dznrm2(n-i+1,vectors(i,i),m)
    swaps(i) = i
  end forall

  do i = 1, size(swaps)
    if (norms(i) < 1e-12) then
      j = i
      do
        j = j + 1
        if (j == size(swaps)+1) exit
        if (norms(j) > 1e-12) exit
      end do
      if (j == size(swaps)+1) exit
      shelf = norms(i)
      norms(i) = norms(j)
      norms(j) = shelf
      swaps(i) = j
    end if
  end do

  rank = i-1

end subroutine
subroutine compute_q_from_reflectors( &
  full_space_dimension, &
  number_of_projectors, number_of_reflectors, reflectors, coefficients, swaps, &
  q &
)
  implicit none
  integer, intent(in) :: &
    full_space_dimension, &
    number_of_reflectors, &
    number_of_projectors, &
    swaps(number_of_reflectors)
  double complex, intent(in) :: &
    reflectors(full_space_dimension,number_of_projectors), &
    coefficients(number_of_reflectors)
  double complex, intent(out) :: q(full_space_dimension,full_space_dimension)

  interface
    subroutine zswap(n,x,incx,y,incy)
      integer, intent(in) :: n, incx, incy
      double complex, intent(inout) :: x, y
    end subroutine
  end interface

  integer :: info, lwork, i
  double complex :: lwork_as_complex

  double complex, allocatable :: work(:)

  external :: zungqr

  if (full_space_dimension < number_of_projectors) then
    q = reflectors(:,:full_space_dimension)
  else
    q(:,:number_of_projectors) = reflectors
    q(:,number_of_projectors+1:) = 0
  end if

  call zungqr( &
    full_space_dimension, full_space_dimension, number_of_reflectors, &
    q, full_space_dimension, &
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
    full_space_dimension, full_space_dimension, number_of_reflectors, &
    q, full_space_dimension, &
    coefficients, &
    work, lwork, &
    info &
  )

  deallocate(work)

  if (info /= 0) then
    print *, "Error calling zungqr (workspace query); info =", info
    stop
  end if

  do i = 1, size(swaps)
    if (swaps(i) /= i) then
      call zswap(full_space_dimension,q(1,i),1,q(1,swaps(i)),1)
    end if
  end do
end subroutine
subroutine project_into_orthogonal_space( &
  full_space_dimension, &
  number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
  vector_in_full_space, &
  vector_in_orthogonal_space &
)
  implicit none
  integer, intent(in) :: &
    full_space_dimension, &
    number_of_reflectors, &
    number_of_projectors, &
    orthogonal_subspace_dimension, &
    swaps(number_of_reflectors)
  double complex, intent(in) :: &
    reflectors(full_space_dimension,number_of_projectors), &
    coefficients(number_of_reflectors), &
    vector_in_full_space(full_space_dimension)
  double complex, intent(out) :: &
    vector_in_orthogonal_space(orthogonal_subspace_dimension)

  double complex :: &
    lwork_as_complex, &
    intermediate_vector(full_space_dimension)
  double complex, allocatable :: work(:)
  integer :: lwork, info, start_of_orthogonal_subspace

  if (number_of_projectors == 0) then
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

  call swap_inplace(size(swaps),swaps,intermediate_vector(1:size(swaps)))

  vector_in_orthogonal_space = &
    intermediate_vector(start_of_orthogonal_subspace:full_space_dimension)

end subroutine
subroutine unproject_from_orthogonal_space( &
  full_space_dimension, &
  number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
  vector_in_orthogonal_space, &
  vector_in_full_space &
)
  implicit none
  integer, intent(in) :: &
    full_space_dimension, &
    number_of_reflectors, &
    number_of_projectors, &
    orthogonal_subspace_dimension, &
    swaps(number_of_reflectors)
  double complex, intent(in) :: &
    reflectors(full_space_dimension,number_of_projectors), &
    coefficients(number_of_reflectors), &
    vector_in_orthogonal_space(orthogonal_subspace_dimension)
  double complex, intent(out) :: &
    vector_in_full_space(full_space_dimension)

  double complex :: &
    lwork_as_complex
  double complex, allocatable :: work(:)
  integer :: lwork, info, start_of_orthogonal_subspace

  if (number_of_projectors == 0) then
    vector_in_full_space = vector_in_orthogonal_space
    return
  end if

  start_of_orthogonal_subspace = full_space_dimension-(orthogonal_subspace_dimension-1)

  vector_in_full_space(1:start_of_orthogonal_subspace-1) = 0 
  vector_in_full_space(start_of_orthogonal_subspace:) = vector_in_orthogonal_space

  call unswap_inplace(size(swaps),swaps,vector_in_full_space(1:size(swaps)))

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
subroutine project_matrix_into_orthog_space( &
  full_space_dimension, &
  number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
  matrix_in_full_space, &
  matrix_in_orthogonal_space &
)
  implicit none
  integer, intent(in) :: &
    full_space_dimension, &
    number_of_reflectors, &
    number_of_projectors, &
    orthogonal_subspace_dimension, &
    swaps(number_of_reflectors)
  double complex, intent(in) :: &
    reflectors(full_space_dimension,number_of_projectors), &
    coefficients(number_of_reflectors), &
    matrix_in_full_space(full_space_dimension,full_space_dimension)
  double complex, intent(out) :: &
    matrix_in_orthogonal_space(orthogonal_subspace_dimension,orthogonal_subspace_dimension)

  double complex :: &
    lwork_as_complex_1, lwork_as_complex_2, &
    intermediate_matrix(full_space_dimension,full_space_dimension)
  double complex, allocatable :: work(:)
  integer :: lwork, info, start_of_orthogonal_subspace

  if (number_of_projectors == 0) then
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

  call swap_matrix_inplace(size(swaps),swaps,intermediate_matrix,full_space_dimension)

  matrix_in_orthogonal_space = &
    conjg(intermediate_matrix(start_of_orthogonal_subspace:full_space_dimension,start_of_orthogonal_subspace:full_space_dimension))

end subroutine
subroutine compute_opt_mat_in_orthog_space( &
  bl, br, & ! state bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
  right_environment, &
  number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
  optimization_matrix &
)
  implicit none
  integer, intent(in) :: &
    bl, br, cl, cr, d, &
    number_of_matrices, sparse_operator_indices(2,number_of_matrices), &
    number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, swaps(min(br*bl*d,number_of_reflectors))
  double complex, intent(in) :: &
    left_environment(bl,bl,cl), &
    right_environment(br,br,cr), &
    sparse_operator_matrices(d,d,number_of_matrices), &
    reflectors(br*bl*d,number_of_projectors), &
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
    number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
    intermediate_matrix, &
    optimization_matrix &
  )

end subroutine
subroutine compute_opt_matrix_all_cases( &
  bl, br, & ! state bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
  right_environment, &
  number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
  optimization_matrix &
)
  implicit none
  integer, intent(in) :: &
    bl, br, cl, cr, d, &
    number_of_matrices, sparse_operator_indices(2,number_of_matrices), &
    number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, swaps(number_of_reflectors)
  double complex, intent(in) :: &
    left_environment(bl,bl,cl), &
    right_environment(br,br,cr), &
    sparse_operator_matrices(d,d,number_of_matrices), &
    reflectors(br*bl*d,number_of_projectors), &
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

  if (number_of_projectors == 0) then
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
      number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
      optimization_matrix &
    )
  end if
end subroutine
subroutine filter_components_outside_orthog( &
  full_space_dimension, &
  number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
  input, &
  output &
)
  implicit none
  integer, intent(in) :: &
    full_space_dimension, &
    number_of_projectors, &
    number_of_reflectors, &
    orthogonal_subspace_dimension, &
    swaps(min(full_space_dimension,number_of_reflectors))
  double complex, intent(in) :: &
    reflectors(full_space_dimension,number_of_projectors), &
    coefficients(number_of_reflectors), &
    input(full_space_dimension)
  double complex, intent(out) :: &
    output(full_space_dimension)

  double complex :: &
    projected(orthogonal_subspace_dimension)

  if (number_of_projectors == 0) then
    output = input
    return
  end if

  call project_into_orthogonal_space( &
    full_space_dimension, &
    number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
    input, &
    projected &
  )

  call unproject_from_orthogonal_space( &
    full_space_dimension, &
    number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
    projected, &
    output &
  )

end subroutine
function optimize( &
  bl, br, & ! state bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
  right_environment, &
  number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
  which, &
  tol, &
  number_of_iterations, &
  guess, &
  result, &
  eigenvalue, &
  normal &
) result (info)
  implicit none
  integer, intent(in) :: &
    bl, br, cl, cr, d, &
    number_of_matrices, sparse_operator_indices(2,number_of_matrices), &
    number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, swaps(number_of_reflectors)
  integer, intent(inout) :: number_of_iterations
  double complex, intent(in) :: &
    left_environment(bl,bl,cl), &
    right_environment(br,br,cr), &
    sparse_operator_matrices(d,d,number_of_matrices), &
    reflectors(br*bl*d,number_of_projectors), &
    coefficients(number_of_reflectors), &
    guess(br,bl,d)
  double complex, intent(out) :: &
    result(br,bl,d), &
    eigenvalue
  double precision, intent(out) :: normal
  character, intent(in) :: which*2
  double precision, intent(in) :: tol

  integer :: info, full_space_dimension
  double precision :: overlap

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
    number_of_projectors, number_of_reflectors, reflectors, coefficients, swaps, &
    full_space_dimension, guess(1,1,1), &
    overlap &
  )

  if (orthogonal_subspace_dimension <= 0) then
    info = 10
    return
  end if

  if ((orthogonal_subspace_dimension < full_space_dimension) .and. &
      (overlap > 1e-7) &
  ) then
    info = 11
    return
  end if

  if (orthogonal_subspace_dimension <= 4) then
    call optimize_strategy_1( &
      bl, br, &
      cl, &
      cr, &
      d, &
      left_environment, &
      number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
      right_environment, &
      number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
      which, &
      tol, &
      number_of_iterations, &
      guess, &
      info, &
      result, &
      eigenvalue &
    )
  else if ( bl*br < cl*cr ) then
    call optimize_strategy_2( &
      bl, br, &
      cl, &
      cr, &
      d, &
      left_environment, &
      number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
      right_environment, &
      number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
      which, &
      tol, &
      number_of_iterations, &
      guess, &
      info, &
      result, &
      eigenvalue &
    )
  else
    call optimize_strategy_3( &
      bl, br, &
      cl, &
      cr, &
      d, &
      left_environment, &
      number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
      right_environment, &
      number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
      which, &
      tol, &
      number_of_iterations, &
      guess, &
      info, &
      result, &
      eigenvalue &
    )
  end if

  normal = dznrm2(br*bl*d,result,1)

  if ( info < 0 ) then
    return
  end if

end function
subroutine optimize_strategy_1( &
  bl, br, & ! state bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
  right_environment, &
  number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps,  &
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
    number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, swaps(number_of_reflectors)
  integer, intent(inout) :: number_of_iterations
  integer, intent(out) :: info
  double complex, intent(in) :: &
    left_environment(bl,bl,cl), &
    right_environment(br,br,cr), &
    sparse_operator_matrices(d,d,number_of_matrices), &
    reflectors(br*bl*d,number_of_projectors), &
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

  character, parameter :: SR*2 = 'SR', LR*2 = 'LR', LM*2 = 'LM'

  call compute_opt_matrix_all_cases( &
    bl, br, cl, cr, d, &
    left_environment, &
    number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
    right_environment, &
    number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
    optimization_matrix &
  )

  if(which == SR .or. which == LR) then
    call lapack_eigenvalue_real_optimizer( &
      orthogonal_subspace_dimension, &
      optimization_matrix, &
      which, &
      eigenvalue, &
      projected_eigenvector &
    )
  elseif(which == LM) then
    call lapack_eigenvalue_mag_maximizer( &
      orthogonal_subspace_dimension, &
      optimization_matrix, &
      eigenvalue, &
      projected_eigenvector &
    )
  end if

  call unproject_from_orthogonal_space( &
    br*bl*d, &
    number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
    projected_eigenvector, &
    result(1,1,1) &
  )

  info = 0
  number_of_iterations = 0

end subroutine
subroutine optimize_strategy_2( &
  bl, br, & ! state bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
  right_environment, &
  number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps,  &
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
    number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, swaps(number_of_reflectors)
  integer, intent(inout) :: number_of_iterations
  integer, intent(out) :: info
  double complex, intent(in) :: &
    left_environment(bl,bl,cl), &
    right_environment(br,br,cr), &
    sparse_operator_matrices(d,d,number_of_matrices), &
    reflectors(br*bl*d,number_of_projectors), &
    coefficients(number_of_reflectors), &
    guess(br,bl,d)
  double complex, intent(out) :: &
    result(br,bl,d), &
    eigenvalue
  character, intent(in) :: which*2
  double precision, intent(in) :: tol
  interface
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
    function dznrm2 (n,x,incx)
      integer, intent(in) :: n, incx
      double complex, intent(in) :: x

      double precision :: dznrm2
    end function
  end interface
  integer, parameter :: nev = 1
  integer :: full_space_dimension

  double complex :: &
    optimization_matrix(orthogonal_subspace_dimension,orthogonal_subspace_dimension), &
    projected_guess(orthogonal_subspace_dimension), &
    projected_result(orthogonal_subspace_dimension)

  full_space_dimension = d*bl*br

  call compute_opt_matrix_all_cases( &
    bl, br, cl, cr, d, &
    left_environment, &
    number_of_matrices,sparse_operator_indices,sparse_operator_matrices, &
    right_environment, &
    number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
    optimization_matrix &
  )

  call  project_into_orthogonal_space( &
    full_space_dimension, &
    number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
    guess(1,1,1), &
    projected_guess &
  )

  call doit

contains

  subroutine run_arpack(n,nev,ncv,tolerance,guess,eigenvalue,eigenvector)

    integer, intent(in) :: n, nev, ncv

    double precision, intent(in) :: tolerance

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
      ido, &
      index_of_solution(1), &
      number_of_converged_eigenvalues

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
        ido, 'I', n, which, nev, tolerance, resid, ncv, v, n, &
        iparam, ipntr, workd, workl, 3*ncv**2+5*ncv, rwork, info &
      ) 
      if (ido == -1 .or. ido == 1) then
        call operate_on(workd(ipntr(1)),workd(ipntr(2)))
      end if
    end do

    number_of_iterations = iparam(3)
    number_of_converged_eigenvalues = iparam(5)

    call zneupd (.true.,'A', select, eigenvalues, v, n, (0d0,0d0), workev, &
                  'I', n, which, nev, tolerance, resid, ncv, &
                  v, n, iparam, ipntr, workd, workl, 3*ncv**2+5*ncv, &
                  rwork, info)

    index_of_solution = minloc(real(eigenvalues(:number_of_converged_eigenvalues)))
    eigenvalue = eigenvalues(index_of_solution(1))
    eigenvector = v(:,index_of_solution(1))

  end subroutine
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
  subroutine postprocess
    call unproject_from_orthogonal_space( &
      full_space_dimension, &
      number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
      projected_result, &
      result(1,1,1) &
    )
  end subroutine
  subroutine doit
    integer :: ncv
    ncv = nev*2+1

    call run_arpack(orthogonal_subspace_dimension,nev,ncv,tol,projected_guess,eigenvalue,projected_result)

    if (info == -14) then
      if (full_space_dimension < number_of_iterations) then
        ! print *, "Retrying with strategy 1"
        call optimize_strategy_1( &
          bl, br, &
          cl, &
          cr, &
          d, &
          left_environment, &
          number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
          right_environment, &
          number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
          which, &
          tol, &
          number_of_iterations, &
          guess, &
          info, &
          result, &
          eigenvalue &
        )
      else
        do while (ncv <= number_of_iterations)
          ncv = ncv * 3 / 2
          call run_arpack(orthogonal_subspace_dimension,nev,ncv,tol,projected_guess,eigenvalue,projected_result)
          ! print *, "Retried with ncv = ", ncv, "; new info = ", info
          if (info /= -14) then
            ! print *, "Success with ncv = ", ncv
            call postprocess
            return
          end if
        end do
      end if
    else
      call postprocess
    end if

  end subroutine

end subroutine
subroutine optimize_strategy_3( &
  bl, br, & ! state bandwidth dimension
  cl, & ! operator left  bandwidth dimension
  cr, & ! operator right bandwidth dimension
  d, & ! physical dimension
  left_environment, &
  number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
  right_environment, &
  number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps,  &
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
    number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, swaps(number_of_reflectors)
  integer, intent(inout) :: number_of_iterations
  integer, intent(out) :: info
  double complex, intent(in) :: &
    left_environment(bl,bl,cl), &
    right_environment(br,br,cr), &
    sparse_operator_matrices(d,d,number_of_matrices), &
    reflectors(br*bl*d,number_of_projectors), &
    coefficients(number_of_reflectors), &
    guess(br,bl,d)
  double complex, intent(out) :: &
    result(br,bl,d), &
    eigenvalue
  character, intent(in) :: which*2
  double precision, intent(in) :: tol
  interface
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
    function dznrm2 (n,x,incx)
      integer, intent(in) :: n, incx
      double complex, intent(in) :: x

      double precision :: dznrm2
    end function
  end interface
  integer, parameter :: nev = 1
  integer :: full_space_dimension

  double complex :: &
    iteration_stage_1_tensor(bl,d,cr,bl,d), &
    iteration_stage_2_tensor(br,cr,bl,d)

  double complex :: &
    projected_guess(orthogonal_subspace_dimension), &
    projected_result(orthogonal_subspace_dimension)

  full_space_dimension = br*bl*d

  call iteration_stage_1( &
    bl, cl, cr, d, &
    left_environment, &
    number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
    iteration_stage_1_tensor &
  )

  call  project_into_orthogonal_space( &
    full_space_dimension, &
    number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
    guess(1,1,1), &
    projected_guess &
  )

  call doit

contains

  subroutine run_arpack(n,nev,ncv,tolerance,guess,eigenvalue,eigenvector)

    integer, intent(in) :: n, nev, ncv

    double precision, intent(in) :: tolerance

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
      ido, &
      index_of_solution(1), &
      number_of_converged_eigenvalues

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
        ido, 'I', n, which, nev, tolerance, resid, ncv, v, n, &
        iparam, ipntr, workd, workl, 3*ncv**2+5*ncv, rwork, info &
      ) 
      if (ido == -1 .or. ido == 1) then
        call operate_on(workd(ipntr(1)),workd(ipntr(2)))
      end if
    end do

    number_of_iterations = iparam(3)
    number_of_converged_eigenvalues = iparam(5)

    call zneupd (.true.,'A', select, eigenvalues, v, n, (0d0,0d0), workev, &
                  'I', n, which, nev, tolerance, resid, ncv, &
                  v, n, iparam, ipntr, workd, workl, 3*ncv**2+5*ncv, &
                  rwork, info)

    index_of_solution = minloc(real(eigenvalues(:number_of_converged_eigenvalues)))
    eigenvalue = eigenvalues(index_of_solution(1))
    eigenvector = v(:,index_of_solution(1))

  end subroutine

  subroutine operate_on(input,output)
    double complex :: input(orthogonal_subspace_dimension), projected(br,bl,d), output(orthogonal_subspace_dimension)
    call unproject_from_orthogonal_space( &
      full_space_dimension, &
      number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
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
      full_space_dimension, &
      number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
      projected, &
      output &
    )

  end subroutine
  subroutine postprocess
    call unproject_from_orthogonal_space( &
      full_space_dimension, &
      number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
      projected_result, &
      result(1,1,1) &
    )
  end subroutine
  subroutine doit
    integer :: ncv
    ncv = nev*2+1

    call run_arpack(orthogonal_subspace_dimension,nev,ncv,tol,projected_guess,eigenvalue,projected_result)

    if (info == -14) then
      if (full_space_dimension < number_of_iterations) then
        ! print *, "Retrying with strategy 1"
        call optimize_strategy_1( &
          bl, br, &
          cl, &
          cr, &
          d, &
          left_environment, &
          number_of_matrices, sparse_operator_indices, sparse_operator_matrices, &
          right_environment, &
          number_of_projectors, number_of_reflectors, orthogonal_subspace_dimension, reflectors, coefficients, swaps, &
          which, &
          tol, &
          number_of_iterations, &
          guess, &
          info, &
          result, &
          eigenvalue &
        )
      else
        do while (ncv <= number_of_iterations)
          ncv = ncv * 3 / 2
          call run_arpack(orthogonal_subspace_dimension,nev,ncv,tol,projected_guess,eigenvalue,projected_result)
          ! print *, "Retried with ncv = ", ncv, "; new info = ", info
          if (info /= -14) then
            ! print *, "Success with ncv = ", ncv
            call postprocess
            return
          end if
        end do
      end if
    else
      call postprocess
    end if

  end subroutine

end subroutine
subroutine seed_randomizer(seed)
  integer, intent(in) :: seed
  call srand(seed)
end subroutine
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
subroutine rand_unnorm_state_site_tensor(br, bl, d, state_site_tensor)
  integer, intent(in) :: br, bl, d
  double complex, intent(out) :: state_site_tensor(br,bl,d)

  call randomize_state_site_tensor(bl, br, d, state_site_tensor)

  state_site_tensor = state_site_tensor / sqrt(dble(real(sum(conjg(state_site_tensor(:,:,:))*state_site_tensor(:,:,:)))))

end subroutine
subroutine random_projector_matrix( &
  projector_length, number_of_projectors, &
  rank, &
  reflectors, coefficients, swaps &
)
  integer, intent(in) :: projector_length, number_of_projectors
  double complex, intent(out) :: &
    reflectors(projector_length,number_of_projectors), &
    coefficients(number_of_projectors), &
    swaps(min(number_of_projectors,projector_length))
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
    coefficients, swaps &
  )
end subroutine
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
