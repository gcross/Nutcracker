/*!
\file core.hpp
\brief Core numeric kernels
*/

#ifndef NUTCRACKER_CORE_HPP
#define NUTCRACKER_CORE_HPP

#include <boost/container/vector.hpp>
#include <boost/optional.hpp>
#include <boost/tuple/tuple.hpp>
#include <complex>
#include <utility>

#include "nutcracker/tensors.hpp"
#include "nutcracker/utilities.hpp"

namespace Nutcracker {

using boost::container::vector;
using boost::copy;
using boost::optional;

using std::min;

//! Core numeric kernels
namespace Core {

//! \defgroup CoreKernels Core numeric kernels

//! @{


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

void compute_optimization_matrix(
    uint32_t const bl, uint32_t const br,
    uint32_t const cl,
    uint32_t const cr,
    uint32_t const d,
    complex<double> const* left_environment,
    uint32_t const number_of_matrices, uint32_t const* sparse_operator_indices, complex<double> const* sparse_operator_matrices,
    complex<double> const* right_environment,
    complex<double>* optimization_matrix
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

void construct_left_exp_boundary(
    uint32_t const b, uint32_t const c,
    complex<double> const* state_boundary,
    complex<double> const* operator_boundary,
    complex<double>* left_expectation_boundary
);

void construct_right_exp_boundary(
    uint32_t const b, uint32_t const c,
    complex<double> const* state_boundary,
    complex<double> const* operator_boundary,
    complex<double>* right_expectation_boundary
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

//! Contracts the state and operator site tensors into the left boundary.
/*!
\image html contract_sos_left.png
\image latex contract_sos_left.eps
*/
void contract_sos_left(
    uint32_t const bl, //!< the state site tensor left dimension
    uint32_t const br, //!< the state site tensor right dimension
    uint32_t const cl, //!< the operator site tensor left dimension
    uint32_t const cr, //!< the operator site tensor right dimension
    uint32_t const d, //!< the physical dimension (shared by both the state and operator site)
    complex<double> const* left_environment, //!< read-only pointer to the (old) left expectation boundary data
    uint32_t const number_of_matrices, //!< the number of operator transition matrices
    uint32_t const* sparse_operator_indices, //!< read-only pointer to the operator transition index data
    complex<double> const* sparse_operator_matrices, //!< read-only pointer the operator transition matrix data
    complex<double> const* state_site_tensor, //!< read-only pointer to the state site tensor data
    complex<double>* new_left_environment //!< writable pointer to the new left expectation boundary tensor
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

//! Contracts the overlap and state site tensors into the left (overlap) boundary.
/*!
\image html contract_vs_left.png
\image latex contract_vs_left.eps
*/
void contract_vs_left(
    uint32_t const b_left_old, //!< the overlap site tensor left dimension
    uint32_t const b_right_old, //!< the overlap site tensor right dimension
    uint32_t const b_left_new, //!< the state site tensor left dimension
    uint32_t const b_right_new, //!< the state site tensor right dimension
    uint32_t const d, //!< the physical dimension (shared by both the overlap and state site tensors)
    complex<double> const* left_environment, //!< read-only pointer to the (old) left overlap boundary data
    complex<double> const* normalized_projector_site_tensor, //!< read-only pointer to the overlap site tensor data
    complex<double> const* normalized_state_site_tensor, //!< read-only pointer to the state site tensor data
    complex<double>* new_left_environment //!< writable pointer to the new left overlap boundary tensor data
);

void contract_vs_right(
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
);

}

//! @}

}

#endif
