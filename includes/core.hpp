//@+leo-ver=5-thin
//@+node:gcross.20110125120748.2458: * @file core.hpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2010: ** << License >>
//@+at
// Copyright (c) 2011, Gregory Crosswhite
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
// 
//     * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//@@c
//@-<< License >>

//@+<< Documentation >>
//@+node:gcross.20110509212455.3993: ** << Documentation >>
/*!
\file core.hpp
\brief Core numeric kernels
*/
//@-<< Documentation >>

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

}

//! @}
//@-others

}

#endif
//@-leo
