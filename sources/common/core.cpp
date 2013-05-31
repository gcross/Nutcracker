/*!
\file core.cpp
\brief Core numeric kernels
*/

#include "nutcracker/core.hpp"

namespace Nutcracker { namespace Core {

// apply_single_site_operator {{{
// extern "C" void apply_single_site_operator_(
//     uint32_t const* br,
//     uint32_t const* bl,
//     uint32_t const* d,
//     complex<double> const* state_site_tensor,
//     complex<double> const* local_operator,
//     complex<double>* new_state_site_tensor
// );
// 
// static void apply_single_site_operator(
//     uint32_t const br,
//     uint32_t const bl,
//     uint32_t const d,
//     complex<double> const* state_site_tensor,
//     complex<double> const* local_operator,
//     complex<double>* new_state_site_tensor
// ) {
//     return apply_single_site_operator_(&br,&bl,&d,state_site_tensor,local_operator,new_state_site_tensor);
// }
// }}}

// compute_expectation {{{
extern "C" void compute_expectation_(
    uint32_t const*,
    uint32_t const*,
    uint32_t const*,
    uint32_t const*,
    uint32_t const*,
    complex<double> const*,
    complex<double> const*,
    uint32_t const*, uint32_t const*, complex<double> const*,
    complex<double> const*,
    complex<double>*
);

complex<double> compute_expectation(
    uint32_t const bl,
    uint32_t const br,
    uint32_t const cl,
    uint32_t const cr,
    uint32_t const d,
    complex<double> const* left_environment,
    complex<double> const* state_site_tensor,
    uint32_t const number_of_matrices, uint32_t const* sparse_operator_indices, complex<double> const* sparse_operator_matrices,
    complex<double> const* right_environment
) {
    complex<double> expectation;
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
        &expectation
    );
    return expectation;
}
// }}}

// compute_optimization_matrix {{{
extern "C" void compute_optimization_matrix_(
    uint32_t const* bl, uint32_t const* br,
    uint32_t const* cl,
    uint32_t const* cr,
    uint32_t const* d,
    complex<double> const* left_environment,
    uint32_t const* number_of_matrices, uint32_t const* sparse_operator_indices, complex<double> const* spare_operator_matrices,
    complex<double> const* right_environment,
    complex<double>* optimization_matrix
);
void compute_optimization_matrix(
    uint32_t const bl, uint32_t const br,
    uint32_t const cl,
    uint32_t const cr,
    uint32_t const d,
    complex<double> const* left_environment,
    uint32_t const number_of_matrices, uint32_t const* sparse_operator_indices, complex<double> const* sparse_operator_matrices,
    complex<double> const* right_environment,
    complex<double>* optimization_matrix
) {
    compute_optimization_matrix_(
        &bl, &br,
        &cl,
        &cr,
        &d,
        left_environment,
        &number_of_matrices, sparse_operator_indices, sparse_operator_matrices,
        right_environment,
        optimization_matrix
    );
}
// }}}

// compute_overlap_with_projectors {{{
extern "C" void compute_overlap_with_projectors_(
      uint32_t const* number_of_projectors
    , uint32_t const* number_of_reflectors
    , complex<double> const* reflectors
    , complex<double> const* coefficients
    , uint32_t const* swaps
    , uint32_t const* vector_size
    , complex<double> const* vector
    , complex<double> const* overlap
);
complex<double> compute_overlap_with_projectors(
    uint32_t const number_of_projectors
  , uint32_t const number_of_reflectors
  , complex<double> const* reflectors
  , complex<double> const* coefficients
  , uint32_t const* swaps
  , uint32_t const vector_size
  , complex<double> const* vector
) {
    complex<double> overlap;
    compute_overlap_with_projectors_(
        &number_of_projectors, &number_of_reflectors, reflectors, coefficients, swaps,
        &vector_size, vector,
        &overlap
    );
    return overlap;
}
// }}}

// construct_left_exp_boundary {{{
extern "C" void construct_left_exp_boundary_(
    uint32_t const* b, uint32_t const* c,
    complex<double> const* state_boundary,
    complex<double> const* operator_boundary,
    complex<double>* left_expectation_boundary
);
void construct_left_exp_boundary(
    uint32_t const b, uint32_t const c,
    complex<double> const* state_boundary,
    complex<double> const* operator_boundary,
    complex<double>* left_expectation_boundary
) {
    construct_left_exp_boundary_(
        &b,&c,
        state_boundary,
        operator_boundary,
        left_expectation_boundary
    );
}
// }}}

// construct_right_exp_boundary {{{
extern "C" void construct_right_exp_boundary_(
    uint32_t const* b, uint32_t const* c,
    complex<double> const* state_boundary,
    complex<double> const* operator_boundary,
    complex<double>* right_expectation_boundary
);
void construct_right_exp_boundary(
    uint32_t const b, uint32_t const c,
    complex<double> const* state_boundary,
    complex<double> const* operator_boundary,
    complex<double>* right_expectation_boundary
) {
    construct_right_exp_boundary_(
        &b,&c,
        state_boundary,
        operator_boundary,
        right_expectation_boundary
    );
}
// }}}

// {{{ contract_expectation_boundaries
extern "C" void contract_expectation_boundaries_(
      uint32_t const* b
    , uint32_t const* c
    , complex<double> const* left_boundary
    , complex<double> const* right_boundary
    , complex<double>* expectation
);

complex<double> contract_expectation_boundaries(
      uint32_t const b
    , uint32_t const c
    , complex<double> const* left_boundary
    , complex<double> const* right_boundary
) {
    complex<double> expectation;
    contract_expectation_boundaries_(
         &b
        ,&c
        ,left_boundary
        ,right_boundary
        ,&expectation
    );
    return expectation;
}
// }}}

// contract_matrix_left {{{
extern "C" void contract_matrix_left_(
    uint32_t const* bl
  , uint32_t const* br
  , complex<double> const* left_environment
  , complex<double> const* matrix
  , complex<double>* new_left_environment
);
void contract_matrix_left(
    uint32_t bl
  , uint32_t br
  , complex<double> const* left_environment
  , complex<double> const* matrix
  , complex<double>* new_left_environment
) {
    contract_matrix_left_(
         &bl
        ,&br
        ,left_environment
        ,matrix
        ,new_left_environment
    );
}
// }}}

// contract_sos_left {{{
extern "C" void contract_sos_left_(
    uint32_t const* bl,
    uint32_t const* br,
    uint32_t const* cl,
    uint32_t const* cr,
    uint32_t const* d,
    complex<double> const* left_environment,
    uint32_t const* number_of_matrices, uint32_t const* sparse_operator_indices, complex<double> const* sparse_operator_matrices,
    complex<double> const* state_site_tensor,
    complex<double>* new_left_environment
);
void contract_sos_left(
    uint32_t const bl,
    uint32_t const br,
    uint32_t const cl,
    uint32_t const cr,
    uint32_t const d,
    complex<double> const* left_environment,
    uint32_t const number_of_matrices, uint32_t const* sparse_operator_indices, complex<double> const* sparse_operator_matrices,
    complex<double> const* state_site_tensor,
    complex<double>* new_left_environment
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
// }}}

// contract_sos_right {{{
extern "C" void contract_sos_right_(
    uint32_t const* bl,
    uint32_t const* br,
    uint32_t const* cl,
    uint32_t const* cr,
    uint32_t const* d,
    complex<double> const* right_environment,
    uint32_t const* number_of_matrices, uint32_t const* sparse_operator_indices, complex<double> const* sparse_operator_matrices,
    complex<double> const* state_site_tensor,
    complex<double>* new_right_environment
);
void contract_sos_right(
    uint32_t const bl,
    uint32_t const br,
    uint32_t const cl,
    uint32_t const cr,
    uint32_t const d,
    complex<double> const* right_environment,
    uint32_t const number_of_matrices, uint32_t const* sparse_operator_indices, complex<double> const* sparse_operator_matrices,
    complex<double> const* state_site_tensor,
    complex<double>* new_right_environment
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
// }}}

// contract_vs_left {{{
extern "C" void contract_vs_left_(
    uint32_t const* b_left_old, uint32_t const* b_right_old,
    uint32_t const* b_left_new, uint32_t const* b_right_new,
    uint32_t const* d,
    complex<double> const* left_environment,
    complex<double> const* normalized_projector_site_tensor,
    complex<double> const* normalized_state_site_tensor,
    complex<double>* new_left_environment
);
void contract_vs_left(
    uint32_t const b_left_old, uint32_t const b_right_old,
    uint32_t const b_left_new, uint32_t const b_right_new,
    uint32_t const d,
    complex<double> const* left_environment,
    complex<double> const* normalized_projector_site_tensor,
    complex<double> const* normalized_state_site_tensor,
    complex<double>* new_left_environment
) {
    contract_vs_left_(
        &b_left_old, &b_right_old,
        &b_left_new, &b_right_new,
        &d,
        left_environment,
        normalized_projector_site_tensor,
        normalized_state_site_tensor,
        new_left_environment
    );
}
// }}}

// contract_vs_right {{{
extern "C" void contract_vs_right_(
    uint32_t const* b_left_old, uint32_t const* b_right_old,
    uint32_t const* b_left_new, uint32_t const* b_right_new,
    uint32_t const* d,
    complex<double> const* right_environment,
    complex<double> const* normalized_projector_site_tensor,
    complex<double> const* normalized_state_site_tensor,
    complex<double>* new_right_environment
);
void contract_vs_right(
    uint32_t const b_left_old, uint32_t const b_right_old,
    uint32_t const b_left_new, uint32_t const b_right_new,
    uint32_t const d,
    complex<double> const* right_environment,
    complex<double> const* normalized_projector_site_tensor,
    complex<double> const* normalized_state_site_tensor,
    complex<double>* new_right_environment
) {
    contract_vs_right_(
        &b_left_old, &b_right_old,
        &b_left_new, &b_right_new,
        &d,
        right_environment,
        normalized_projector_site_tensor,
        normalized_state_site_tensor,
        new_right_environment
    );
}
// }}}

// convert_vectors_to_reflectors {{{
extern "C" uint32_t convert_vectors_to_reflectors_(
    uint32_t const* n,
    uint32_t const* m,
    complex<double> const* vectors,
    uint32_t* rank,
    complex<double>* coefficients,
    uint32_t* swaps
);
uint32_t convert_vectors_to_reflectors(
    uint32_t const n,
    uint32_t const m,
    complex<double>* vectors,
    complex<double>* coefficients,
    uint32_t* swaps
) {
    uint32_t rank;
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
// }}}

// extend_state_vector_fragment {{{
extern "C" void extend_state_vector_fragment_(
    uint32_t const* bm
  , uint32_t const* br
  , uint32_t const* dl
  , uint32_t const* dr
  , complex<double> const* old_state_vector_fragment
  , complex<double> const* site_tensor
  , complex<double>* new_state_vector_fragment
);
void extend_state_vector_fragment(
    uint32_t const bm
  , uint32_t const br
  , uint32_t const dl
  , uint32_t const dr
  , complex<double> const* old_state_vector_fragment
  , complex<double> const* site_tensor
  , complex<double>* new_state_vector_fragment
) {
    extend_state_vector_fragment_(
         &bm
        ,&br
        ,&dl
        ,&dr
        ,old_state_vector_fragment
        ,site_tensor
        ,new_state_vector_fragment
    );
}
// }}}

// filter_components_outside_orthog {{{
extern "C" void filter_components_outside_orthog_(
    uint32_t const* full_space_dimension
  , uint32_t const* number_of_projectors
  , uint32_t const* number_of_reflectors
  , uint32_t const* orthogonal_subspace_dimension
  , complex<double> const* reflectors
  , complex<double> const* coefficients
  , uint32_t const* swaps
  , complex<double> const* input
  , complex<double> const* output
);
void filter_components_outside_orthog(
    uint32_t const full_space_dimension
  , uint32_t const number_of_projectors
  , uint32_t const number_of_reflectors
  , uint32_t const orthogonal_subspace_dimension
  , complex<double> const* reflectors
  , complex<double> const* coefficients
  , uint32_t const* swaps
  , complex<double> const* input
  , complex<double> const* output
) {
    filter_components_outside_orthog_(
        &full_space_dimension,
        &number_of_projectors, &number_of_reflectors, &orthogonal_subspace_dimension, reflectors, coefficients, swaps,
        input,
        output
    );
}
// }}}

// form_norm_overlap_tensors {{{
extern "C" void form_norm_overlap_tensors_(
    uint32_t const* bl, uint32_t const* bm, uint32_t const* br,
    uint32_t const* dl, uint32_t const* dr,
    complex<double> const* unnormalized_state_tensor_1,
    complex<double> const* right_norm_state_tensor_2,
    complex<double> const* left_norm_overlap_tensor_1,
    complex<double> const* unnormalized_overlap_tensor_1,
    complex<double> const* unnormalized_state_tensor_2,
    complex<double> const* right_norm_overlap_tensor_2
);
void form_norm_overlap_tensors(
    uint32_t const bl, uint32_t const bm, uint32_t const br,
    uint32_t const dl, uint32_t const dr,
    complex<double> const* unnormalized_state_tensor_1,
    complex<double> const* right_norm_state_tensor_2,
    complex<double> const* left_norm_overlap_tensor_1,
    complex<double> const* unnormalized_overlap_tensor_1,
    complex<double> const* unnormalized_state_tensor_2,
    complex<double> const* right_norm_overlap_tensor_2
) {
    return
    form_norm_overlap_tensors_(
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
// }}}

//form_overlap_site_tensor {{{
extern "C" void form_overlap_site_tensor_(
    uint32_t const* br,
    uint32_t const* bl,
    uint32_t const* d, 
    complex<double> const* state_site_tensor,
    complex<double>* overlap_site_tensor
);
void form_overlap_site_tensor(
    uint32_t const br,
    uint32_t const bl,
    uint32_t const d,
    complex<double> const* state_site_tensor,
    complex<double>* overlap_site_tensor
) {
    return form_overlap_site_tensor_(&br,&bl,&d,state_site_tensor,overlap_site_tensor);
}
// }}}

// form_overlap_vector {{{
extern "C" void form_overlap_vector_(
    uint32_t const* b_left_old, uint32_t const* b_right_old,
    uint32_t const* b_left_new, uint32_t const* b_right_new,
    uint32_t const* d,
    complex<double> const* left_environment,
    complex<double> const* right_environment,
    complex<double> const* unnormalized_projector_site_tensor,
    complex<double>* overlap_vector
);
void form_overlap_vector(
    uint32_t const b_left_old, uint32_t const b_right_old,
    uint32_t const b_left_new, uint32_t const b_right_new,
    uint32_t const d,
    complex<double> const* left_environment,
    complex<double> const* right_environment,
    complex<double> const* unnormalized_projector_site_tensor,
    complex<double>* overlap_vector
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
// }}}

// increase_bandwidth_between {{{
extern "C" int increase_bandwidth_between_(
    uint32_t const* bl, uint32_t const* bm, uint32_t const* br,
    uint32_t const* dl, uint32_t const* dr,
    uint32_t const* new_bm,
    complex<double> const* site_tensor_to_normalize,
    complex<double> const* site_tensor_to_denormalize,
    complex<double>* normalized_site_tensor,
    complex<double>* denormalized_site_tensor
);
int increase_bandwidth_between(
    uint32_t const bl, uint32_t const bm, uint32_t const br,
    uint32_t const dl, uint32_t const dr,
    uint32_t const new_bm,
    complex<double> const* site_tensor_to_normalize,
    complex<double> const* site_tensor_to_denormalize,
    complex<double>* normalized_site_tensor,
    complex<double>* denormalized_site_tensor
) {
    return
    increase_bandwidth_between_(
        &bl, &bm, &br,
        &dl, &dr,
        &new_bm,
        site_tensor_to_normalize,
        site_tensor_to_denormalize,
        normalized_site_tensor,
        denormalized_site_tensor
    );
}
// }}}

// norm_denorm_going_left {{{
extern "C" int norm_denorm_going_left_(
    uint32_t const* bll, uint32_t const* bl, uint32_t const* br,
    uint32_t const* dl, uint32_t const* d,
    complex<double> const* site_tensor_to_denormalize,
    complex<double> const* site_tensor_to_normalize,
    complex<double>* denormalized_site_tensor,
    complex<double>* normalized_site_tensor
);
int norm_denorm_going_left(
    uint32_t const bll, uint32_t const bl, uint32_t const br,
    uint32_t const dl, uint32_t const d,
    complex<double> const* site_tensor_to_denormalize,
    complex<double> const* site_tensor_to_normalize,
    complex<double>* denormalized_site_tensor,
    complex<double>* normalized_site_tensor
) {
    return
    norm_denorm_going_left_(
        &bll, &bl, &br,
        &dl, &d,
        site_tensor_to_denormalize,
        site_tensor_to_normalize,
        denormalized_site_tensor,
        normalized_site_tensor
    );
}
// }}}

// norm_denorm_going_right {{{
extern "C" int norm_denorm_going_right_(
    uint32_t const* bl, uint32_t const* br, uint32_t const* brr,
    uint32_t const* d, uint32_t const* dr,
    complex<double> const* site_tensor_to_normalize,
    complex<double> const* site_tensor_to_denormalize,
    complex<double>* normalized_site_tensor,
    complex<double>* denormalized_site_tensor
);
int norm_denorm_going_right(
    uint32_t const bl, uint32_t const br, uint32_t const brr,
    uint32_t const d, uint32_t const dr,
    complex<double> const* site_tensor_to_normalize,
    complex<double> const* site_tensor_to_denormalize,
    complex<double>* normalized_site_tensor,
    complex<double>* denormalized_site_tensor
) {
    return
    norm_denorm_going_right_(
        &bl, &br, &brr,
        &d, &dr,
        site_tensor_to_normalize,
        site_tensor_to_denormalize,
        normalized_site_tensor,
        denormalized_site_tensor
    );
}
// }}}

// norm_for_left {{{
extern "C" int32_t norm_for_left_(
    uint32_t const* br, uint32_t const* bl, uint32_t const* d,
    complex<double> const* site_tensor,
    complex<double>* normalized_site_tensor
);
int norm_for_left(
    uint32_t const br, uint32_t const bl, uint32_t const d,
    complex<double> const* site_tensor,
    complex<double>* normalized_site_tensor
) {
    return norm_for_left_(
        &br,&bl,&d,
        site_tensor,
        normalized_site_tensor
    );
}
// }}}

// norm_for_right {{{
extern "C" int32_t norm_for_right_(
    uint32_t const* br, uint32_t const* bl, uint32_t const* d,
    complex<double> const* site_tensor,
    complex<double>* normalized_site_tensor
);
int norm_for_right(
    uint32_t const br, uint32_t const bl, uint32_t const d,
    complex<double> const* site_tensor,
    complex<double>* normalized_site_tensor
) {
    return norm_for_right_(
        &br,&bl,&d,
        site_tensor,
        normalized_site_tensor
    );
}
// }}}

// optimize {{{
extern "C" uint32_t optimize_(
    uint32_t const* bl,
    uint32_t const* br,
    uint32_t const* cl,
    uint32_t const* cr,
    uint32_t const* d,
    complex<double> const* left_environment,
    uint32_t const* number_of_matrices, uint32_t const* sparse_operator_indices, complex<double> const* sparse_operator_matrices,
    complex<double> const* right_environment,
    uint32_t const* number_of_projectors, uint32_t const* number_of_reflectors, uint32_t const* orthogonal_subspace_dimension, complex<double> const* reflectors, complex<double> const* coefficients, uint32_t const* swaps,
    const char* which,
    double const* tol,
    uint32_t* number_of_iterations,
    complex<double> const* guess,
    complex<double>* result,
    complex<double>* eigenvalue,
    double* normal
);
uint32_t optimize(
    uint32_t const bl,
    uint32_t const br,
    uint32_t const cl,
    uint32_t const cr,
    uint32_t const d,
    complex<double> const* left_environment,
    uint32_t const number_of_matrices, uint32_t const* sparse_operator_indices, complex<double> const* sparse_operator_matrices,
    complex<double> const* right_environment,
    uint32_t const number_of_projectors, uint32_t const number_of_reflectors, uint32_t const orthogonal_subspace_dimension, complex<double> const* reflectors, complex<double> const* coefficients, uint32_t const* swaps,
    const char* which,
    double const tol,
    uint32_t& number_of_iterations,
    complex<double> const* guess,
    complex<double>* result,
    complex<double>& eigenvalue,
    double& normal
) {
    return
    optimize_(
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
        &number_of_iterations,
        guess,
        result,
        &eigenvalue,
        &normal
    );
}
// }}}

// rand_norm_state_site_tensor {{{
extern "C" void rand_norm_state_site_tensor_(
    uint32_t const* br,
    uint32_t const* bl,
    uint32_t const* d,
    complex<double>* state_site_tensor
);
void rand_norm_state_site_tensor(
    uint32_t const br,
    uint32_t const bl,
    uint32_t const d,
    complex<double>* state_site_tensor
) {
    rand_norm_state_site_tensor_(&br,&bl,&d,state_site_tensor);
}
// }}}

// rand_unnorm_state_site_tensor {{{
extern "C" void rand_unnorm_state_site_tensor_(
    uint32_t const* br,
    uint32_t const* bl,
    uint32_t const* d,
    complex<double>* state_site_tensor
);
void rand_unnorm_state_site_tensor(
    uint32_t const br,
    uint32_t const bl,
    uint32_t const d,
    complex<double>* state_site_tensor
) {
    rand_unnorm_state_site_tensor_(&br,&bl,&d,state_site_tensor);
}
// }}}

// random_projector_matrix {{{
extern "C" void random_projector_matrix_(
    uint32_t const* projector_length, uint32_t const* number_of_projectors,
    uint32_t const* rank,
    complex<double>* reflectors, complex<double>* coefficients, uint32_t* swaps
);
uint32_t random_projector_matrix(
    uint32_t const projector_length, uint32_t const number_of_projectors,
    complex<double>* reflectors, complex<double>* coefficients, uint32_t* swaps
) {
    uint32_t rank;
    random_projector_matrix_(&projector_length,&number_of_projectors,&rank,reflectors,coefficients,swaps);
    return rank;
}
// }}}

// increase_bandwidth_with_environment {{{
extern "C" void increase_bandwidth_with_environment_(
    uint32_t const* b,
    uint32_t const* c,
    uint32_t const* d,
    uint32_t const* new_b,
    complex<double> const* old_state_site_tensor,
    complex<double> const* old_left_exp_environment,
    complex<double> const* old_right_exp_environment,
    complex<double>* new_state_site_tensor,
    complex<double>* new_left_exp_environment,
    complex<double>* new_right_exp_environment
);
void increase_bandwidth_with_environment(
    uint32_t const b,
    uint32_t const c,
    uint32_t const d,
    uint32_t const new_b,
    complex<double> const* old_state_site_tensor,
    complex<double> const* old_left_exp_environment,
    complex<double> const* old_right_exp_environment,
    complex<double>* new_state_site_tensor,
    complex<double>* new_left_exp_environment,
    complex<double>* new_right_exp_environment
) {
    increase_bandwidth_with_environment_(
        &b,&c,&d,&new_b,
        old_state_site_tensor,
        old_left_exp_environment,
        old_right_exp_environment,
        new_state_site_tensor,
        new_left_exp_environment,
        new_right_exp_environment
    );
}
// }}}

}}
