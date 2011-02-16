//@+leo-ver=5-thin
//@+node:gcross.20110125120748.2458: * @thin core.hpp
//@@language cplusplus

#ifndef NUTCRACKER_CORE_HPP
#define NUTCRACKER_CORE_HPP

//@+<< Includes >>
//@+node:gcross.20110125120748.2459: ** << Includes >>
#include <boost/container/vector.hpp>
#include <boost/optional.hpp>
#include <boost/tuple/tuple.hpp>
#include <complex>
#include <utility>

#include "tensors.hpp"
#include "utilities.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110125120748.2460: ** << Usings >>
using boost::container::vector;
using boost::copy;
using boost::optional;

using std::min;
//@-<< Usings >>

//@+others
//@+node:gcross.20110213233103.2753: ** Functions
namespace Core {

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
);

complex<double> compute_overlap_with_projectors(
    uint32_t const number_of_projectors
  , uint32_t const number_of_reflectors
  , complex<double> const* reflectors
  , complex<double> const* coefficients
  , uint32_t const* swaps
  , uint32_t const vector_size
  , complex<double> const* vector
);

complex<double> contract_expectation_boundaries(
      uint32_t const b
    , uint32_t const c
    , complex<double> const* left_boundary
    , complex<double> const* right_boundary
);

void contract_matrix_left(
    uint32_t bl
  , uint32_t br
  , complex<double> const* left_environment
  , complex<double> const* matrix
  , complex<double>* new_left_environment
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
);

void contract_ss_left(
    uint32_t const b_left_old, uint32_t const b_right_old,
    uint32_t const b_left_new, uint32_t const b_right_new,
    uint32_t const d,
    complex<double> const* left_environment,
    complex<double> const* normalized_projector_site_tensor,
    complex<double> const* normalized_state_site_tensor,
    complex<double>* new_left_environment
);

void contract_ss_right(
    uint32_t const b_left_old, uint32_t const b_right_old,
    uint32_t const b_left_new, uint32_t const b_right_new,
    uint32_t const d,
    complex<double> const* right_environment,
    complex<double> const* normalized_projector_site_tensor,
    complex<double> const* normalized_state_site_tensor,
    complex<double>* new_right_environment
);

uint32_t convert_vectors_to_reflectors(
    uint32_t const n,
    uint32_t const m,
    complex<double>* vectors,
    complex<double>* coefficients,
    uint32_t* swaps
);

void extend_state_vector_fragment(
    uint32_t const bm
  , uint32_t const br
  , uint32_t const dl
  , uint32_t const dr
  , complex<double> const* old_state_vector_fragment
  , complex<double> const* site_tensor
  , complex<double>* new_state_vector_fragment
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
);

void form_overlap_site_tensor(
    uint32_t const br,
    uint32_t const bl,
    uint32_t const d,
    complex<double> const* state_site_tensor,
    complex<double>* overlap_site_tensor
);

void form_overlap_vector(
    uint32_t const b_left_old, uint32_t const b_right_old,
    uint32_t const b_left_new, uint32_t const b_right_new,
    uint32_t const d,
    complex<double> const* left_environment,
    complex<double> const* right_environment,
    complex<double> const* unnormalized_projector_site_tensor,
    complex<double>* overlap_vector
);

int increase_bandwidth_between(
    uint32_t const bl, uint32_t const bm, uint32_t const br,
    uint32_t const dl, uint32_t const dr,
    uint32_t const new_bm,
    complex<double> const* site_tensor_to_normalize,
    complex<double> const* site_tensor_to_denormalize,
    complex<double>* normalized_site_tensor,
    complex<double>* denormalized_site_tensor
);

int norm_denorm_going_left(
    uint32_t const bll, uint32_t const bl, uint32_t const br,
    uint32_t const dl, uint32_t const d,
    complex<double> const* site_tensor_to_denormalize,
    complex<double> const* site_tensor_to_normalize,
    complex<double>* denormalized_site_tensor,
    complex<double>* normalized_site_tensor
);

int norm_denorm_going_right(
    uint32_t const bl, uint32_t const br, uint32_t const brr,
    uint32_t const d, uint32_t const dr,
    complex<double> const* site_tensor_to_normalize,
    complex<double> const* site_tensor_to_denormalize,
    complex<double>* normalized_site_tensor,
    complex<double>* denormalized_site_tensor
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
);

void rand_norm_state_site_tensor(
    uint32_t const br,
    uint32_t const bl,
    uint32_t const d,
    complex<double>* state_site_tensor
);

void rand_unnorm_state_site_tensor(
    uint32_t const br,
    uint32_t const bl,
    uint32_t const d,
    complex<double>* state_site_tensor
);

uint32_t random_projector_matrix(
    uint32_t const projector_length, uint32_t const number_of_projectors,
    complex<double>* reflectors, complex<double>* coefficients, uint32_t* swaps
);

}
//@-others

}

#endif
//@-leo
