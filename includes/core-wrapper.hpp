//@+leo-ver=5-thin
//@+node:gcross.20110124161335.1941: * @thin core-wrapper.hpp
//@@language C

#ifndef CORE_WRAPPER_HPP
#define CORE_WRAPPER_HPP

#ifdef _cplusplus
extern "C" {
#endif

//@+others
//@+node:gcross.20110124161335.1942: ** compute_expectation
extern void compute_expectation_(
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

inline void compute_expectation(
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
//@+node:gcross.20110124161335.1943: ** optimize
extern int optimize_(
    int* bl,
    int* br,
    int* cl,
    int* cr,
    int* d,
    double* left_environment,
    int* number_of_matrices, int* sparse_operator_indices, double* sparse_operator_matrices,
    double* right_environment,
    int* number_of_projectors, int* number_of_reflectors, int* orthogonal_subspace_dimension, double* reflectors, double* coefficients, int* swaps,
    char* which,
    double* tol,
    int* number_of_iterations,
    double* guess,
    double* result,
    double* eigenvalue
);

inline int optimize(
    int bl,
    int br,
    int cl,
    int cr,
    int d,
    double* left_environment,
    int number_of_matrices, int* sparse_operator_indices, double* sparse_operator_matrices,
    double* right_environment,
    int number_of_projectors, int number_of_reflectors, int orthogonal_subspace_dimension, double* reflectors, double* coefficients, int* swaps,
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
        &number_of_projectors, &number_of_reflectors, &orthogonal_subspace_dimension, reflectors, coefficients, swaps,
        which,
        &tol,
        number_of_iterations,
        guess,
        result,
        eigenvalue
    );
}
//@+node:gcross.20110124161335.1944: ** contract_sos_left
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

inline void contract_sos_left(
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
//@+node:gcross.20110124161335.1945: ** contract_sos_right
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
//@+node:gcross.20110124161335.1946: ** contract_ss_left
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
//@+node:gcross.20110124161335.1947: ** contract_ss_right
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
//@+node:gcross.20110124161335.1948: ** form_overlap_vector
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
//@+node:gcross.20110124161335.1949: ** rand_unnorm_state_site_tensor
void rand_unnorm_state_site_tensor_(int* br, int* bl, int* d, double* state_site_tensor);

void rand_unnorm_state_site_tensor(int br, int bl, int d, double* state_site_tensor) {
    rand_unnorm_state_site_tensor_(&br,&bl,&d,state_site_tensor);
}
//@+node:gcross.20110124161335.1950: ** rand_norm_state_site_tensor
void rand_norm_state_site_tensor_(int* br, int* bl, int* d, double* state_site_tensor);

void rand_norm_state_site_tensor(int br, int bl, int d, double* state_site_tensor) {
    rand_norm_state_site_tensor_(&br,&bl,&d,state_site_tensor);
}
//@+node:gcross.20110124161335.1951: ** norm_denorm_going_left
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
//@+node:gcross.20110124161335.1952: ** norm_denorm_going_right
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
//@+node:gcross.20110124161335.1953: ** increase_bandwidth_between
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
//@+node:gcross.20110124161335.1954: ** form_overlap_site_tensor
void form_overlap_site_tensor_(int* br, int* bl, int* d, double* state_site_tensor, double* overlap_site_tensor);

void form_overlap_site_tensor(int br, int bl, int d, double* state_site_tensor, double* overlap_site_tensor) {
    return form_overlap_site_tensor_(&br,&bl,&d,state_site_tensor,overlap_site_tensor);
}
//@+node:gcross.20110124161335.1955: ** form_norm_overlap_tensors
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
//@+node:gcross.20110124161335.1956: ** apply_single_site_operator
void apply_single_site_operator_(int* br, int* bl, int* d, double* state_site_tensor, double* operator, double* new_state_site_tensor);

void apply_single_site_operator(int br, int bl, int d, double* state_site_tensor, double* operator, double* new_state_site_tensor) {
    return apply_single_site_operator_(&br,&bl,&d,state_site_tensor,operator,new_state_site_tensor);
}
//@+node:gcross.20110124161335.1957: ** Projectors
//@+node:gcross.20110124161335.1958: *3* convert_vectors_to_reflectors
int convert_vectors_to_reflectors_(
  int* n,
  int* m,
  double* vectors,
  int* rank,
  double* coefficients,
  int* swaps
);

int convert_vectors_to_reflectors(
  int n,
  int m,
  double* vectors,
  double* coefficients,
  int* swaps
) {
    int rank;
    convert_vectors_to_reflectors_(
        &n,
        &m,
        vectors,
        &rank,
        coefficients,
        swaps
    );
    return rank;
}
//@+node:gcross.20110124161335.1959: *3* filter_components_outside_orthog
void filter_components_outside_orthog_(
  int* full_space_dimension,
  int* number_of_projectors, int* number_of_reflectors, int* orthogonal_subspace_dimension, double* reflectors, double* coefficients, int* swaps,
  double* input,
  double* output
);

void filter_components_outside_orthog(
  int full_space_dimension,
  int number_of_projectors, int number_of_reflectors, int orthogonal_subspace_dimension, double* reflectors, double* coefficients, int* swaps,
  double* input,
  double* output
) {
    filter_components_outside_orthog_(
        &full_space_dimension,
        &number_of_projectors, &number_of_reflectors, &orthogonal_subspace_dimension, reflectors, coefficients, swaps,
        input,
        output
    );
}
//@+node:gcross.20110124161335.1960: *3* random_projector_matrix
void random_projector_matrix_(
    int* projector_length, int* number_of_projectors,
    int* rank,
    double* reflectors, double* coefficients, int* swaps
);

int random_projector_matrix(
    int projector_length, int number_of_projectors,
    double* reflectors, double* coefficients, int* swaps
) {
    int rank;
    random_projector_matrix_(&projector_length,&number_of_projectors,&rank,reflectors,coefficients,swaps);
    return rank;
}
//@+node:gcross.20110124161335.1961: *3* compute_overlap_with_projectors
void compute_overlap_with_projectors_(
  int* number_of_projectors, int* number_of_reflectors, double* reflectors, double* coefficients, int* swaps,
  int* vector_size, double* vector,
  double* overlap
);

double compute_overlap_with_projectors(
  int number_of_projectors, int number_of_reflectors, double* reflectors, double* coefficients, int* swaps,
  int vector_size, double* vector
) {
    double overlap;
    compute_overlap_with_projectors_(
      &number_of_projectors, &number_of_reflectors, reflectors, coefficients, swaps,
      &vector_size, vector,
      &overlap
    );
    return overlap;
}
//@+node:gcross.20110124161335.1962: ** contract_operator_random_left
void contract_operator_random_left_(
    int* cl, int* cr, int* d,
    double* left_boundary_1, double* left_boundary_2,
    int* number_of_matrices, int* sparse_operator_indices, double* sparse_operator_matrices,
    double* new_left_boundary_1, double* new_left_boundary_2
);

void contract_operator_random_left(
    int cl, int cr, int d,
    double* left_boundary_1, double* left_boundary_2,
    int number_of_matrices, int* sparse_operator_indices, double* sparse_operator_matrices,
    double* new_left_boundary_1, double* new_left_boundary_2
) {
    contract_operator_random_left_(
        &cl, &cr, &d,
        left_boundary_1, left_boundary_2,
        &number_of_matrices, sparse_operator_indices, sparse_operator_matrices,
        new_left_boundary_1, new_left_boundary_2
    );
}
//@-others

#ifdef _cplusplus
}
#endif

#endif CORE_WRAPPER_HPP
//@-leo
