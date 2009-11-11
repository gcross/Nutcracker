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
!@-others

end module
!@-node:gcross.20091110205054.1925:@thin normalizer.f95
!@-leo
