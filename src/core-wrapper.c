//@+leo-ver=4-thin
//@+node:gcross.20091112145455.1619:@thin core-wrapper.c
//@@language C

// #include <stdio.h>

//@+others
//@+node:gcross.20091112145455.1621:compute_expectation
void compute_expectation_(
    int*,
    int*,
    int*,
    int*,
    int*,
    double*,
    double*,
    int*, int*, double*,
    double*,
    double*
);

void compute_expectation(
    int bl,
    int br,
    int cl,
    int cr,
    int d,
    double* left_environment,
    double* state_site_tensor,
    int number_of_matrices, int* sparse_operator_indices, double* sparse_operator_matrices,
    double* right_environment,
    double* expectation
) {
    compute_expectation_(
        &bl,
        &br,
        &cl,
        &cr,
        &d,
        left_environment,
        state_site_tensor,
        &number_of_matrices, sparse_operator_indices, sparse_operator_matrices,
        right_environment,
        expectation
    );
}
//@-node:gcross.20091112145455.1621:compute_expectation
//@+node:gcross.20091112145455.1622:optimize
int optimize_(
    int* bl,
    int* br,
    int* cl,
    int* cr,
    int* d,
    double* left_environment,
    int* number_of_matrices, int* sparse_operator_indices, double* sparse_operator_matrices,
    double* right_environment,
    char* which,
    double* tol,
    int* number_of_iterations,
    double* guess,
    double* result,
    double* eigenvalue
);

int optimize(
    int bl,
    int br,
    int cl,
    int cr,
    int d,
    double* left_environment,
    int number_of_matrices, int* sparse_operator_indices, double* sparse_operator_matrices,
    double* right_environment,
    char* which,
    double tol,
    int* number_of_iterations,
    double* guess,
    double* result,
    double* eigenvalue
) {
    return optimize_(
        &bl,
        &br,
        &cl,
        &cr,
        &d,
        left_environment,
        &number_of_matrices, sparse_operator_indices, sparse_operator_matrices,
        right_environment,
        which,
        &tol,
        number_of_iterations,
        guess,
        result,
        eigenvalue
    );
}
//@-node:gcross.20091112145455.1622:optimize
//@+node:gcross.20091112145455.1623:contract_sos_left
void contract_sos_left_(
    int* bl,
    int* br,
    int* cl,
    int* cr,
    int* d,
    double* left_environment,
    int* number_of_matrices, int* sparse_operator_indices, double* sparse_operator_matrices,
    double* state_site_tensor,
    double* new_left_environment
);

void contract_sos_left(
    int bl,
    int br,
    int cl,
    int cr,
    int d,
    double* left_environment,
    int number_of_matrices, int* sparse_operator_indices, double* sparse_operator_matrices,
    double* state_site_tensor,
    double* new_left_environment
) {
    contract_sos_left_(
        &bl,
        &br,
        &cl,
        &cr,
        &d,
        left_environment,
        &number_of_matrices, sparse_operator_indices, sparse_operator_matrices,
        state_site_tensor,
        new_left_environment
    );
}
//@-node:gcross.20091112145455.1623:contract_sos_left
//@+node:gcross.20091112145455.1637:contract_sos_right
void contract_sos_right_(
    int* bl,
    int* br,
    int* cl,
    int* cr,
    int* d,
    double* right_environment,
    int* number_of_matrices, int* sparse_operator_indices, double* sparse_operator_matrices,
    double* state_site_tensor,
    double* new_right_environment
);

void contract_sos_right(
    int bl,
    int br,
    int cl,
    int cr,
    int d,
    double* right_environment,
    int number_of_matrices, int* sparse_operator_indices, double* sparse_operator_matrices,
    double* state_site_tensor,
    double* new_right_environment
) {
    contract_sos_right_(
        &bl,
        &br,
        &cl,
        &cr,
        &d,
        right_environment,
        &number_of_matrices, sparse_operator_indices, sparse_operator_matrices,
        state_site_tensor,
        new_right_environment
    );
}
//@-node:gcross.20091112145455.1637:contract_sos_right
//@+node:gcross.20091112145455.1655:randomize_state_site_tensor
void randomize_state_site_tensor_(int* br, int* bl, int* d, double* state_site_tensor);

void randomize_state_site_tensor(int br, int bl, int d, double* state_site_tensor) {
    randomize_state_site_tensor_(&br,&bl,&d,state_site_tensor);
}
//@-node:gcross.20091112145455.1655:randomize_state_site_tensor
//@+node:gcross.20091112145455.1673:rand_norm_state_site_tensor
void rand_norm_state_site_tensor_(int* br, int* bl, int* d, double* state_site_tensor);

void rand_norm_state_site_tensor(int br, int bl, int d, double* state_site_tensor) {
    rand_norm_state_site_tensor_(&br,&bl,&d,state_site_tensor);
}
//@-node:gcross.20091112145455.1673:rand_norm_state_site_tensor
//@+node:gcross.20091113125544.1647:norm_denorm_going_left
int norm_denorm_going_left_(
  int* bll, int* bl, int* br,
  int* dl, int* d,
  double* site_tensor_to_denormalize,
  double* site_tensor_to_normalize,
  double* denormalized_site_tensor,
  double* normalized_site_tensor
);

int norm_denorm_going_left(
  int bll, int bl, int br,
  int dl, int d,
  double* site_tensor_to_denormalize,
  double* site_tensor_to_normalize,
  double* denormalized_site_tensor,
  double* normalized_site_tensor
) {
    return norm_denorm_going_left_(
      &bll, &bl, &br,
      &dl, &d,
      site_tensor_to_denormalize,
      site_tensor_to_normalize,
      denormalized_site_tensor,
      normalized_site_tensor
    );
}
//@-node:gcross.20091113125544.1647:norm_denorm_going_left
//@+node:gcross.20091113125544.1648:norm_denorm_going_right
int norm_denorm_going_right_(
  int* bl, int* br, int* brr,
  int* d, int* dr,
  double* site_tensor_to_normalize,
  double* site_tensor_to_denormalize,
  double* normalized_site_tensor,
  double* denormalized_site_tensor
);

int norm_denorm_going_right(
  int bl, int br, int brr,
  int d, int dr,
  double* site_tensor_to_normalize,
  double* site_tensor_to_denormalize,
  double* normalized_site_tensor,
  double* denormalized_site_tensor
) {
    return norm_denorm_going_right_(
      &bl, &br, &brr,
      &d, &dr,
      site_tensor_to_normalize,
      site_tensor_to_denormalize,
      normalized_site_tensor,
      denormalized_site_tensor
    );
}
//@-node:gcross.20091113125544.1648:norm_denorm_going_right
//@-others
//@-node:gcross.20091112145455.1619:@thin core-wrapper.c
//@-leo
