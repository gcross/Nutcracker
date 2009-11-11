!@+leo-ver=4-thin
!@+node:gcross.20091110205054.1925:@thin normalizer.f95
!@@language fortran90
!@@tabwidth -2

module normalizer

  use utils

  implicit none

contains

!@+others
!@+node:gcross.20091110205054.1926:norm_denorm_going_left
function norm_denorm_going_left( &
  bll,bl,br, &
  dl,d, &
  site_tensor_to_denormalize, &
  site_tensor_to_normalize, &
  denormalized_site_tensor, &
  normalized_site_tensor &
) result (info)
  integer, intent(in) :: bll, bl, br, dl, d
  double complex, intent(in) :: &
    site_tensor_to_denormalize(bl,bll,dl), &
    site_tensor_to_normalize(br,bl,d)
  double complex, intent(out) :: &
    denormalized_site_tensor(bl,bll,dl), &
    normalized_site_tensor(br,bl,d)
  double complex :: &
    denormalized_tensor_workspace(bl,bll,dl), &
    normalized_tensor_workspace(bl,br,d)

  double complex :: u(bl,bl), vt(bl,br*d)
  double precision :: s(bl)
  integer :: info, i, j

  external :: zgemm

  if (br*d < bl) then
    print *, "Not enough degrees of freedom to normalize."
    print *, br*d, "<", bl
    stop
  end if

  normalized_tensor_workspace = reshape(site_tensor_to_normalize,shape(normalized_tensor_workspace),order=(/2,1,3/))

  info = svd(bl,br*d,bl,normalized_tensor_workspace,u,s,vt)

  call zgemm( &
    'N','N', &
    bl,br*d,bl, &
    (1d0,0d0), &
    u, bl, &
    vt, bl, &
    (0d0,0d0), &
    normalized_tensor_workspace, bl &
  )

  normalized_site_tensor = reshape(normalized_tensor_workspace,shape(normalized_site_tensor),order=(/2,1,3/))

  u = conjg(u)

  call zgemm( &
    'C','N', &
    bl,bll*dl,bl, &
    (1d0,0d0), &
    u, bl, &
    site_tensor_to_denormalize, bl, &
    (0d0,0d0), &
    denormalized_tensor_workspace, bl &
  )

  forall (i=1:bll,j=1:dl) &
    denormalized_tensor_workspace(:,i,j) = denormalized_tensor_workspace(:,i,j) * s(:)

  call zgemm( &
    'N','N', &
    bl,bll*dl,bl, &
    (1d0,0d0), &
    u, bl, &
    denormalized_tensor_workspace, bl, &
    (0d0,0d0), &
    denormalized_site_tensor, bl &
  )

end function
!@-node:gcross.20091110205054.1926:norm_denorm_going_left
!@+node:gcross.20091110205054.1935:norm_denorm_going_right
function norm_denorm_going_right( &
  bl,br,brr, &
  d,dr, &
  site_tensor_to_normalize, &
  site_tensor_to_denormalize, &
  normalized_site_tensor, &
  denormalized_site_tensor &
) result (info)
  integer, intent(in) :: bl, br, brr, dr, d
  double complex, intent(in) :: &
    site_tensor_to_normalize(br,bl,d), &
    site_tensor_to_denormalize(brr,br,dr)
  double complex, intent(out) :: &
    normalized_site_tensor(br,bl,d), &
    denormalized_site_tensor(brr,br,dr)
  double complex :: &
    denormalized_tensor_workspace_1(br,brr,dr), &
    denormalized_tensor_workspace_2(br,brr,dr)

  double complex :: u(br,br), vt(br,bl*d)
  double precision :: s(br)
  integer :: info, i, j

  external :: zgemm

  if (bl*d < br) then
    print *, "Not enough degrees of freedom to normalize."
    print *, bl*d, "<", br
    stop
  end if

  info = svd(br,bl*d,br,site_tensor_to_normalize,u,s,vt)

  call zgemm( &
    'N','N', &
    br,bl*d,br, &
    (1d0,0d0), &
    u, br, &
    vt, br, &
    (0d0,0d0), &
    normalized_site_tensor, br &
  )

  denormalized_tensor_workspace_1 = reshape( &
    site_tensor_to_denormalize, &
    shape(denormalized_tensor_workspace_1), &
    order=(/2,1,3/) &
  )

  u = conjg(u)

  call zgemm( &
    'C','N', &
    br,brr*dr,br, &
    (1d0,0d0), &
    u, br, &
    denormalized_tensor_workspace_1, br, &
    (0d0,0d0), &
    denormalized_tensor_workspace_2, br &
  )

  forall (i=1:brr,j=1:dr) &
    denormalized_tensor_workspace_2(:,i,j) = denormalized_tensor_workspace_2(:,i,j) * s(:)

  call zgemm( &
    'N','N', &
    br,brr*dr,br, &
    (1d0,0d0), &
    u, br, &
    denormalized_tensor_workspace_2, br, &
    (0d0,0d0), &
    denormalized_tensor_workspace_1, br &
  )

  denormalized_site_tensor = reshape( &
    denormalized_tensor_workspace_1, &
    shape(denormalized_site_tensor), &
    order=(/2,1,3/) &
  )

end function
!@-node:gcross.20091110205054.1935:norm_denorm_going_right
!@-others

end module
!@-node:gcross.20091110205054.1925:@thin normalizer.f95
!@-leo
