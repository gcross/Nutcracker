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
    int* number_of_projectors, double* projectors,
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
    int number_of_projectors, double* projectors,
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
        &number_of_projectors, projectors,
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
//@+node:gcross.20091116175016.1772:contract_ss_left
void contract_ss_left_(
    int* b_left_old, int* b_right_old,
    int* b_left_new, int* b_right_new,
    int* d,
    double* left_environment,
    double* normalized_projector_site_tensor,
    double* normalized_state_site_tensor,
    double* new_left_environment
);

void contract_ss_left(
    int b_left_old, int b_right_old,
    int b_left_new, int b_right_new,
    int d,
    double* left_environment,
    double* normalized_projector_site_tensor,
    double* normalized_state_site_tensor,
    double* new_left_environment
) {
    contract_ss_left_(
        &b_left_old, &b_right_old,
        &b_left_new, &b_right_new,
        &d,
        left_environment,
        normalized_projector_site_tensor,
        normalized_state_site_tensor,
        new_left_environment
    );
}
//@-node:gcross.20091116175016.1772:contract_ss_left
//@+node:gcross.20091116175016.1784:contract_ss_right
void contract_ss_right_(
    int* b_left_old, int* b_right_old,
    int* b_left_new, int* b_right_new,
    int* d,
    double* right_environment,
    double* normalized_projector_site_tensor,
    double* normalized_state_site_tensor,
    double* new_right_environment
);

void contract_ss_right(
    int b_left_old, int b_right_old,
    int b_left_new, int b_right_new,
    int d,
    double* right_environment,
    double* normalized_projector_site_tensor,
    double* normalized_state_site_tensor,
    double* new_right_environment
) {
    contract_ss_right_(
        &b_left_old, &b_right_old,
        &b_left_new, &b_right_new,
        &d,
        right_environment,
        normalized_projector_site_tensor,
        normalized_state_site_tensor,
        new_right_environment
    );
}
//@-node:gcross.20091116175016.1784:contract_ss_right
//@+node:gcross.20091116175016.1799:form_overlap_vector
void form_overlap_vector_(
  int* b_left_old, int* b_right_old,
  int* b_left_new, int* b_right_new,
  int* d,
  double* left_environment,
  double* right_environment,
  double* unnormalized_projector_site_tensor,
  double* overlap_vector
);

void form_overlap_vector(
  int b_left_old, int b_right_old,
  int b_left_new, int b_right_new,
  int d,
  double* left_environment,
  double* right_environment,
  double* unnormalized_projector_site_tensor,
  double* overlap_vector
) {
    form_overlap_vector_(
      &b_left_old, &b_right_old,
      &b_left_new, &b_right_new,
      &d,
      left_environment,
      right_environment,
      unnormalized_projector_site_tensor,
      overlap_vector
    );
}
//@-node:gcross.20091116175016.1799:form_overlap_vector
//@+node:gcross.20091112145455.1655:rand_unnorm_state_site_tensor
void rand_unnorm_state_site_tensor_(int* br, int* bl, int* d, double* state_site_tensor);

void rand_unnorm_state_site_tensor(int br, int bl, int d, double* state_site_tensor) {
    rand_unnorm_state_site_tensor_(&br,&bl,&d,state_site_tensor);
}
//@-node:gcross.20091112145455.1655:rand_unnorm_state_site_tensor
//@+node:gcross.20091112145455.1673:rand_norm_state_site_tensor
void rand_norm_state_site_tensor_(int* br, int* bl, int* d, double* state_site_tensor);

void rand_norm_state_site_tensor(int br, int bl, int d, double* state_site_tensor) {
    rand_norm_state_site_tensor_(&br,&bl,&d,state_site_tensor);
}
//@-node:gcross.20091112145455.1673:rand_norm_state_site_tensor
//@+node:gcross.20100521141104.1777:random_projector_matrix
void random_projector_matrix_(int* projector_length, int* number_of_projectors, double* projector_matrix);

void random_projector_matrix(int projector_length, int number_of_projectors, double* projector_matrix) {
    random_projector_matrix_(&projector_length,&number_of_projectors,projector_matrix);
}
//@-node:gcross.20100521141104.1777:random_projector_matrix
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
//@+node:gcross.20091115105949.1731:increase_bandwidth_between
int increase_bandwidth_between_(
  int* bl, int* bm, int* br,
  int* dl, int* dr,
  int* new_bm,
  double* site_tensor_to_normalize,
  double* site_tensor_to_denormalize,
  double* normalized_site_tensor,
  double* denormalized_site_tensor
);

int increase_bandwidth_between(
  int bl, int bm, int br,
  int dl, int dr,
  int new_bm,
  double* site_tensor_to_normalize,
  double* site_tensor_to_denormalize,
  double* normalized_site_tensor,
  double* denormalized_site_tensor
) {
    return increase_bandwidth_between_(
      &bl, &bm, &br,
      &dl, &dr,
      &new_bm,
      site_tensor_to_normalize,
      site_tensor_to_denormalize,
      normalized_site_tensor,
      denormalized_site_tensor
    );
}
//@-node:gcross.20091115105949.1731:increase_bandwidth_between
//@+node:gcross.20091116175016.1801:orthogonalize_projector_matrix
int orthogonalize_matrix_in_place_(
  int* n,
  int* m,
  double* matrix,
  int* rank
);

int orthogonalize_matrix_in_place(
  int n,
  int m,
  double* matrix
) {
    int rank;
    orthogonalize_matrix_in_place_(
        &n,
        &m,
        matrix,
        &rank
    );
    return rank;
}
//@-node:gcross.20091116175016.1801:orthogonalize_projector_matrix
//@+node:gcross.20091118141720.1801:form_overlap_site_tensor
void form_overlap_site_tensor_(int* br, int* bl, int* d, double* state_site_tensor, double* overlap_site_tensor);

void form_overlap_site_tensor(int br, int bl, int d, double* state_site_tensor, double* overlap_site_tensor) {
    return form_overlap_site_tensor_(&br,&bl,&d,state_site_tensor,overlap_site_tensor);
}
//@-node:gcross.20091118141720.1801:form_overlap_site_tensor
//@+node:gcross.20091118141720.1804:form_norm_overlap_tensors
void form_norm_overlap_tensors_(
  int* bl, int* bm, int* br,
  int* dl, int* dr,
  double* unnormalized_state_tensor_1,
  double* right_norm_state_tensor_2,
  double* left_norm_overlap_tensor_1,
  double* unnormalized_overlap_tensor_1,
  double* unnormalized_state_tensor_2,
  double* right_norm_overlap_tensor_2
);

void form_norm_overlap_tensors(
  int bl, int bm, int br,
  int dl, int dr,
  double* unnormalized_state_tensor_1,
  double* right_norm_state_tensor_2,
  double* left_norm_overlap_tensor_1,
  double* unnormalized_overlap_tensor_1,
  double* unnormalized_state_tensor_2,
  double* right_norm_overlap_tensor_2
) {
    return form_norm_overlap_tensors_(
      &bl, &bm, &br,
      &dl, &dr,
      unnormalized_state_tensor_1,
      right_norm_state_tensor_2,
      left_norm_overlap_tensor_1,
      unnormalized_overlap_tensor_1,
      unnormalized_state_tensor_2,
      right_norm_overlap_tensor_2
    );
}
//@-node:gcross.20091118141720.1804:form_norm_overlap_tensors
//@+node:gcross.20091120134444.1596:project
void project_(int* vector_size,int* number_of_projectors,double* projectors,double* input_vector,double* output_vector);

void project(int vector_size,int number_of_projectors,double* projectors,double* input_vector,double* output_vector) {
    project_(&vector_size,&number_of_projectors,projectors,input_vector,output_vector);
}
//@-node:gcross.20091120134444.1596:project
//@+node:gcross.20100520145029.1768:compute_overlap_with_projectors
double compute_overlap_with_projectors_(int* vector_size,int* number_of_projectors,double* projectors,double* vector);
double compute_overlap_with_projectors(int vector_size,int number_of_projectors,double* projectors,double* vector) {
    return compute_overlap_with_projectors_(&vector_size,&number_of_projectors,projectors,vector);
}
//@-node:gcross.20100520145029.1768:compute_overlap_with_projectors
//@+node:gcross.20091211120042.1691:apply_single_site_operator
void apply_single_site_operator_(int* br, int* bl, int* d, double* state_site_tensor, double* operator, double* new_state_site_tensor);

void apply_single_site_operator(int br, int bl, int d, double* state_site_tensor, double* operator, double* new_state_site_tensor) {
    return apply_single_site_operator_(&br,&bl,&d,state_site_tensor,operator,new_state_site_tensor);
}
//@-node:gcross.20091211120042.1691:apply_single_site_operator
//@-others
//@-node:gcross.20091112145455.1619:@thin core-wrapper.c
//@-leo
