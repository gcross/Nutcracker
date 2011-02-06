//@+leo-ver=5-thin
//@+node:gcross.20091112145455.1619: * @thin core.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110125120748.2118: ** << Includes >>
#include <boost/foreach.hpp>
#include <stdint.h>

#include "core.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110125120748.2430: ** << Usings >>
using namespace boost;
using namespace std;
//@-<< Usings >>

//@+others
//@+node:gcross.20110124175241.1623: ** Fortran wrappers
//@+others
//@+node:gcross.20110124175241.1638: *3* apply_single_site_operator
//@+at
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
//     unsigned int const br,
//     unsigned int const bl,
//     unsigned int const d,
//     complex<double> const* state_site_tensor,
//     complex<double> const* local_operator,
//     complex<double>* new_state_site_tensor
// ) {
//     return apply_single_site_operator_(&br,&bl,&d,state_site_tensor,local_operator,new_state_site_tensor);
// }
//@+node:gcross.20110124175241.1624: *3* compute_expectation
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

static complex<double> compute_expectation(
    unsigned int const bl,
    unsigned int const br,
    unsigned int const cl,
    unsigned int const cr,
    unsigned int const d,
    complex<double> const* left_environment,
    complex<double> const* state_site_tensor,
    unsigned int const number_of_matrices, uint32_t const* sparse_operator_indices, complex<double> const* sparse_operator_matrices,
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
//@+node:gcross.20110124175241.1643: *3* compute_overlap_with_projectors
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

static complex<double> compute_overlap_with_projectors(
    unsigned int const number_of_projectors
  , unsigned int const number_of_reflectors
  , complex<double> const* reflectors
  , complex<double> const* coefficients
  , uint32_t const* swaps
  , unsigned int const vector_size
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
//@+node:gcross.20110128003411.1644: *3* contract_expectation_boundaries
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
//@+node:gcross.20110124175241.1626: *3* contract_sos_left
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

static void contract_sos_left(
    unsigned int const bl,
    unsigned int const br,
    unsigned int const cl,
    unsigned int const cr,
    unsigned int const d,
    complex<double> const* left_environment,
    unsigned int const number_of_matrices, uint32_t const* sparse_operator_indices, complex<double> const* sparse_operator_matrices,
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
//@+node:gcross.20110124175241.1627: *3* contract_sos_right
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

static void contract_sos_right(
    unsigned int const bl,
    unsigned int const br,
    unsigned int const cl,
    unsigned int const cr,
    unsigned int const d,
    complex<double> const* right_environment,
    unsigned int const number_of_matrices, uint32_t const* sparse_operator_indices, complex<double> const* sparse_operator_matrices,
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
//@+node:gcross.20110124175241.1628: *3* contract_ss_left
extern "C" void contract_ss_left_(
    uint32_t const* b_left_old, uint32_t const* b_right_old,
    uint32_t const* b_left_new, uint32_t const* b_right_new,
    uint32_t const* d,
    complex<double> const* left_environment,
    complex<double> const* normalized_projector_site_tensor,
    complex<double> const* normalized_state_site_tensor,
    complex<double>* new_left_environment
);

static void contract_ss_left(
    unsigned int const b_left_old, unsigned int const b_right_old,
    unsigned int const b_left_new, unsigned int const b_right_new,
    unsigned int const d,
    complex<double> const* left_environment,
    complex<double> const* normalized_projector_site_tensor,
    complex<double> const* normalized_state_site_tensor,
    complex<double>* new_left_environment
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
//@+node:gcross.20110124175241.1629: *3* contract_ss_right
extern "C" void contract_ss_right_(
    uint32_t const* b_left_old, uint32_t const* b_right_old,
    uint32_t const* b_left_new, uint32_t const* b_right_new,
    uint32_t const* d,
    complex<double> const* right_environment,
    complex<double> const* normalized_projector_site_tensor,
    complex<double> const* normalized_state_site_tensor,
    complex<double>* new_right_environment
);

static void contract_ss_right(
    unsigned int const b_left_old, unsigned int const b_right_old,
    unsigned int const b_left_new, unsigned int const b_right_new,
    unsigned int const d,
    complex<double> const* right_environment,
    complex<double> const* normalized_projector_site_tensor,
    complex<double> const* normalized_state_site_tensor,
    complex<double>* new_right_environment
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
//@+node:gcross.20110124175241.1640: *3* convert_vectors_to_reflectors
extern "C" unsigned int convert_vectors_to_reflectors_(
    uint32_t const* n,
    uint32_t const* m,
    complex<double> const* vectors,
    uint32_t* rank,
    complex<double>* coefficients,
    uint32_t* swaps
);

static unsigned int convert_vectors_to_reflectors(
    unsigned int const n,
    unsigned int const m,
    complex<double>* vectors,
    complex<double>* coefficients,
    uint32_t* swaps
) {
    unsigned int rank;
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
//@+node:gcross.20110124175241.1641: *3* filter_components_outside_orthog
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

static void filter_components_outside_orthog(
    unsigned int const full_space_dimension
  , unsigned int const number_of_projectors
  , unsigned int const number_of_reflectors
  , unsigned int const orthogonal_subspace_dimension
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
//@+node:gcross.20110124175241.1637: *3* form_norm_overlap_tensors
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

static void form_norm_overlap_tensors(
    unsigned int const bl, unsigned int const bm, unsigned int const br,
    unsigned int const dl, unsigned int const dr,
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
//@+node:gcross.20110124175241.1636: *3* form_overlap_site_tensor
extern "C" void form_overlap_site_tensor_(
    uint32_t const* br,
    uint32_t const* bl,
    uint32_t const* d, 
    complex<double> const* state_site_tensor,
    complex<double>* overlap_site_tensor
);

static void form_overlap_site_tensor(
    unsigned int const br,
    unsigned int const bl,
    unsigned int const d,
    complex<double> const* state_site_tensor,
    complex<double>* overlap_site_tensor
) {
    return form_overlap_site_tensor_(&br,&bl,&d,state_site_tensor,overlap_site_tensor);
}
//@+node:gcross.20110124175241.1630: *3* form_overlap_vector
extern "C" void form_overlap_vector_(
    uint32_t const* b_left_old, uint32_t const* b_right_old,
    uint32_t const* b_left_new, uint32_t const* b_right_new,
    uint32_t const* d,
    complex<double> const* left_environment,
    complex<double> const* right_environment,
    complex<double> const* unnormalized_projector_site_tensor,
    complex<double>* overlap_vector
);

static void form_overlap_vector(
    unsigned int const b_left_old, unsigned int const b_right_old,
    unsigned int const b_left_new, unsigned int const b_right_new,
    unsigned int const d,
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
//@+node:gcross.20110124175241.1635: *3* increase_bandwidth_between
extern "C" int increase_bandwidth_between_(
    uint32_t const* bl, uint32_t const* bm, uint32_t const* br,
    uint32_t const* dl, uint32_t const* dr,
    uint32_t const* new_bm,
    complex<double> const* site_tensor_to_normalize,
    complex<double> const* site_tensor_to_denormalize,
    complex<double>* normalized_site_tensor,
    complex<double>* denormalized_site_tensor
);

static int increase_bandwidth_between(
    unsigned int const bl, unsigned int const bm, unsigned int const br,
    unsigned int const dl, unsigned int const dr,
    unsigned int const new_bm,
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
//@+node:gcross.20110124175241.1633: *3* norm_denorm_going_left
extern "C" int norm_denorm_going_left_(
    uint32_t const* bll, uint32_t const* bl, uint32_t const* br,
    uint32_t const* dl, uint32_t const* d,
    complex<double> const* site_tensor_to_denormalize,
    complex<double> const* site_tensor_to_normalize,
    complex<double>* denormalized_site_tensor,
    complex<double>* normalized_site_tensor
);

static int norm_denorm_going_left(
    unsigned int const bll, unsigned int const bl, unsigned int const br,
    unsigned int const dl, unsigned int const d,
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
//@+node:gcross.20110124175241.1634: *3* norm_denorm_going_right
extern "C" int norm_denorm_going_right_(
    uint32_t const* bl, uint32_t const* br, uint32_t const* brr,
    uint32_t const* d, uint32_t const* dr,
    complex<double> const* site_tensor_to_normalize,
    complex<double> const* site_tensor_to_denormalize,
    complex<double>* normalized_site_tensor,
    complex<double>* denormalized_site_tensor
);

static int norm_denorm_going_right(
    unsigned int const bl, unsigned int const br, unsigned int const brr,
    unsigned int const d, unsigned int const dr,
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
//@+node:gcross.20110124175241.1625: *3* optimize
extern "C" unsigned int optimize_(
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

static unsigned int optimize(
    unsigned int const bl,
    unsigned int const br,
    unsigned int const cl,
    unsigned int const cr,
    unsigned int const d,
    complex<double> const* left_environment,
    unsigned int const number_of_matrices, uint32_t const* sparse_operator_indices, complex<double> const* sparse_operator_matrices,
    complex<double> const* right_environment,
    unsigned int const number_of_projectors, unsigned int const number_of_reflectors, unsigned int const orthogonal_subspace_dimension, complex<double> const* reflectors, complex<double> const* coefficients, uint32_t const* swaps,
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
//@+node:gcross.20110124175241.1632: *3* rand_norm_state_site_tensor
extern "C" void rand_norm_state_site_tensor_(
    uint32_t const* br,
    uint32_t const* bl,
    uint32_t const* d,
    complex<double>* state_site_tensor
);

static void rand_norm_state_site_tensor(
    unsigned int const br,
    unsigned int const bl,
    unsigned int const d,
    complex<double>* state_site_tensor
) {
    rand_norm_state_site_tensor_(&br,&bl,&d,state_site_tensor);
}
//@+node:gcross.20110124175241.1631: *3* rand_unnorm_state_site_tensor
extern "C" void rand_unnorm_state_site_tensor_(
    uint32_t const* br,
    uint32_t const* bl,
    uint32_t const* d,
    complex<double>* state_site_tensor
);

static void rand_unnorm_state_site_tensor(
    unsigned int const br,
    unsigned int const bl,
    unsigned int const d,
    complex<double>* state_site_tensor
) {
    rand_unnorm_state_site_tensor_(&br,&bl,&d,state_site_tensor);
}
//@+node:gcross.20110124175241.1642: *3* random_projector_matrix
extern "C" void random_projector_matrix_(
    uint32_t const* projector_length, uint32_t const* number_of_projectors,
    uint32_t const* rank,
    complex<double>* reflectors, complex<double>* coefficients, uint32_t* swaps
);

static unsigned int random_projector_matrix(
    unsigned int const projector_length, unsigned int const number_of_projectors,
    complex<double>* reflectors, complex<double>* coefficients, uint32_t* swaps
) {
    unsigned int rank;
    random_projector_matrix_(&projector_length,&number_of_projectors,&rank,reflectors,coefficients,swaps);
    return rank;
}
//@-others
//@+node:gcross.20110125120748.2471: ** Exceptions
//@+node:gcross.20110125120748.2472: *3* NormalizationError
NormalizationError::NormalizationError(int info)
    : Exception(
        (format("Numerical error encountered when normalizing a state site (info = %1%)")
            % info
        ).str()
      )
    , info(info)
{ }
//@+node:gcross.20110125120748.2474: *3* OptimizerFailure
OptimizerFailure::OptimizerFailure(string const& message)
    : Exception(message)
{ }
//@+node:gcross.20110125202132.2184: *4* OptimizerGivenGuessInProjectorSpace
OptimizerGivenGuessInProjectorSpace::OptimizerGivenGuessInProjectorSpace()
    : OptimizerFailure(
        "Optimizer was given a guess within the forbidden orthogonal space"
      )
{ }
//@+node:gcross.20110125202132.2180: *4* OptimizerGivenTooManyProjectors
OptimizerGivenTooManyProjectors::OptimizerGivenTooManyProjectors(
      unsigned int const number_of_projectors
    , PhysicalDimension const physical_dimension
    , LeftDimension const left_dimension
    , RightDimension const right_dimension
) : OptimizerFailure(
        (format("Optimizer was given too many projectors (%1% >= %2%*%3%*%4% = %5%)") 
            % number_of_projectors
            % *physical_dimension
            % *left_dimension
            % *right_dimension
            % ((*physical_dimension)*(*left_dimension)*(*right_dimension))
        ).str()
    )
  , number_of_projectors(number_of_projectors)
  , physical_dimension(physical_dimension)
  , left_dimension(left_dimension)
  , right_dimension(right_dimension)
{ }
//@+node:gcross.20110125202132.2166: *4* OptimizerObtainedComplexEigenvalue
OptimizerObtainedComplexEigenvalue::OptimizerObtainedComplexEigenvalue(
       complex<double> const eigenvalue
) : OptimizerFailure(
        (format("Optimizer obtained complex eigenvalue (%1%)") 
            % eigenvalue
        ).str()
    )
  , eigenvalue(eigenvalue)
{ }
//@+node:gcross.20110125202132.2164: *4* OptimizerObtainedEigenvalueDifferentFromExpectationValue
OptimizerObtainedEigenvalueDifferentFromExpectationValue::OptimizerObtainedEigenvalueDifferentFromExpectationValue(
      complex<double> const eigenvalue
    , complex<double> const expected_value
) : OptimizerFailure(
        (format("Optimizer obtained eigenvalue that was different from the final expected value (%1% != %2%)")
            % eigenvalue
            % expected_value
        ).str()
    )
  , eigenvalue(eigenvalue)
  , expected_value(expected_value)
{ }
//@+node:gcross.20110125202132.2176: *4* OptimizerObtainedEigenvectorInProjectorSpace
OptimizerObtainedEigenvectorInProjectorSpace::OptimizerObtainedEigenvectorInProjectorSpace(
       double const overlap
) : OptimizerFailure(
        (format("Optimizer obtained eigenvector overlapping with the forbidden orthogonal space (overlap = %1%)") 
            % overlap
        ).str()
    )
  , overlap(overlap)
{ }
//@+node:gcross.20110206130502.1753: *4* OptimizerObtainedGreaterEigenvalue
OptimizerObtainedGreaterEigenvalue::OptimizerObtainedGreaterEigenvalue(
      double const old_eigenvalue
    , double const new_eigenvalue
) : OptimizerFailure(
        (format("Optimizer obtained an eigenvalue that greater than the old eigenvalue (%2% > %1%)")
            % new_eigenvalue
            % old_eigenvalue
        ).str()
    )
  , old_eigenvalue(old_eigenvalue)
  , new_eigenvalue(new_eigenvalue)
{ }
//@+node:gcross.20110125202132.2168: *4* OptimizerObtainedVanishingEigenvector
OptimizerObtainedVanishingEigenvector::OptimizerObtainedVanishingEigenvector(
       double const norm
) : OptimizerFailure(
        (format("Optimizer obtained vanishing eigenvector (norm = %1%)") 
            % norm
        ).str()
    )
  , norm(norm)
{ }
//@+node:gcross.20110125120748.2476: *4* OptimizerUnableToConverge
OptimizerUnableToConverge::OptimizerUnableToConverge(
       unsigned int const number_of_iterations
) : OptimizerFailure(
        (format("Optimizer failed to converge after %1% iterations") 
            % number_of_iterations
        ).str()
    )
  , number_of_iterations(number_of_iterations)
{ }
//@+node:gcross.20110125202132.2186: *4* OptimizerUnknownFailure
OptimizerUnknownFailure::OptimizerUnknownFailure(
       int const error_code
) : OptimizerFailure(
        (format("Optimizer failed with an unknown error code: %1%") 
            % error_code
        ).str()
    )
  , error_code(error_code)
{ }
//@+node:gcross.20110124175241.1583: ** Functions
//@+node:gcross.20110124175241.1600: *3* Contractors
//@+node:gcross.20110124175241.1585: *4* computeExpectationValue
complex<double> computeExpectationValue(
      ExpectationBoundary<Left> const& left_boundary
    , StateSite<Middle> const& state_site
    , OperatorSite const& operator_site
    , ExpectationBoundary<Right> const& right_boundary
) {
    return compute_expectation(
         left_boundary || state_site
        ,state_site || right_boundary
        ,left_boundary || operator_site
        ,operator_site || right_boundary
        ,operator_site || state_site
        ,left_boundary
        ,state_site
        ,operator_site.numberOfMatrices(),operator_site,operator_site
        ,right_boundary
    );
}
//@+node:gcross.20110128003411.1643: *4* contractExpectationBoundaries
complex<double> contractExpectationBoundaries(
      ExpectationBoundary<Left> const& left_boundary
    , ExpectationBoundary<Right> const& right_boundary
) {
    return contract_expectation_boundaries(
         connectDimensions(
             "left boundary state"
            ,left_boundary.stateDimension(as_unsigned_integer)
            ,"right boundary state"
            ,right_boundary.stateDimension(as_unsigned_integer)
         )
        ,connectDimensions(
             "left boundary operator"
            ,left_boundary.operatorDimension(as_unsigned_integer)
            ,"right boundary operator"
            ,right_boundary.operatorDimension(as_unsigned_integer)
         )
        ,left_boundary
        ,right_boundary
    );
}
//@+node:gcross.20110124175241.1589: *4* contractSOSLeft
ExpectationBoundary<Left> contractSOSLeft(
      ExpectationBoundary<Left> const& old_boundary
    , StateSite<Left> const& state_site
    , OperatorSite const& operator_site
) {
    ExpectationBoundary<Left> new_boundary
        (OperatorDimension(operator_site.rightDimension(as_unsigned_integer))
        ,StateDimension(state_site.rightDimension(as_unsigned_integer))
        );
    contract_sos_left(
         old_boundary || state_site
        ,state_site.rightDimension(as_unsigned_integer)
        ,old_boundary || operator_site
        ,operator_site.rightDimension(as_unsigned_integer)
        ,operator_site || state_site
        ,old_boundary
        ,operator_site.numberOfMatrices(),operator_site,operator_site
        ,state_site
        ,new_boundary
    );
    return boost::move(new_boundary);
}
//@+node:gcross.20110124175241.1593: *4* contractSOSRight
ExpectationBoundary<Right> contractSOSRight(
      ExpectationBoundary<Right> const& old_boundary
    , StateSite<Right> const& state_site
    , OperatorSite const& operator_site
) {
    ExpectationBoundary<Right> new_boundary
        (OperatorDimension(operator_site.leftDimension(as_unsigned_integer))
        ,StateDimension(state_site.leftDimension(as_unsigned_integer))
        );
    contract_sos_right(
         state_site.leftDimension(as_unsigned_integer)
        ,state_site || old_boundary
        ,operator_site.leftDimension(as_unsigned_integer)
        ,operator_site || old_boundary
        ,operator_site || state_site
        ,old_boundary
        ,operator_site.numberOfMatrices(),operator_site,operator_site
        ,state_site
        ,new_boundary
    );
    return boost::move(new_boundary);
}
//@+node:gcross.20110124175241.1595: *4* contractSSLeft
OverlapBoundary<Left> contractSSLeft(
      OverlapBoundary<Left> const& old_boundary
    , OverlapSite<Left> const& overlap_site
    , StateSite<Left> const& state_site
) {
    OverlapBoundary<Left> new_boundary
        (OverlapDimension(overlap_site.rightDimension(as_unsigned_integer))
        ,StateDimension(state_site.rightDimension(as_unsigned_integer))
        );
    contract_ss_left(
         old_boundary || overlap_site
        ,overlap_site.rightDimension(as_unsigned_integer)
        ,old_boundary || state_site
        ,state_site.rightDimension(as_unsigned_integer)
        ,overlap_site || state_site
        ,old_boundary
        ,overlap_site
        ,state_site
        ,new_boundary
    );
    return boost::move(new_boundary);
}
//@+node:gcross.20110124175241.1597: *4* contractSSRight
OverlapBoundary<Right> contractSSRight(
      OverlapBoundary<Right> const& old_boundary
    , OverlapSite<Right> const& overlap_site
    , StateSite<Right> const& state_site
) {
    OverlapBoundary<Right> new_boundary
        (OverlapDimension(overlap_site.leftDimension(as_unsigned_integer))
        ,StateDimension(state_site.leftDimension(as_unsigned_integer))
        );
    contract_ss_right(
         overlap_site.leftDimension(as_unsigned_integer)
        ,overlap_site || old_boundary
        ,state_site.leftDimension(as_unsigned_integer)
        ,state_site || old_boundary
        ,overlap_site || state_site
        ,old_boundary
        ,overlap_site
        ,state_site
        ,new_boundary
    );
    return boost::move(new_boundary);
}
//@+node:gcross.20110124175241.1652: *3* Cursor movement
//@+node:gcross.20110124175241.1654: *4* moveSiteCursorLeft
MoveSiteCursorResult<Left> moveSiteCursorLeft(
      StateSite<Middle> const& old_state_site_2
    , StateSite<Left> const& old_state_site_1
) {
    StateSite<Middle> new_state_site_1(dimensionsOf(old_state_site_1));
    StateSite<Right> new_state_site_2(dimensionsOf(old_state_site_2));
    unsigned int const info =
    norm_denorm_going_left(
         old_state_site_1.leftDimension(as_unsigned_integer)
        ,old_state_site_1 || old_state_site_2
        ,old_state_site_2.rightDimension(as_unsigned_integer)
        ,old_state_site_1.physicalDimension(as_unsigned_integer)
        ,old_state_site_2.physicalDimension(as_unsigned_integer)
        ,old_state_site_1
        ,old_state_site_2
        ,new_state_site_1
        ,new_state_site_2
    );
    if(info != 0) throw NormalizationError(info);
    return MoveSiteCursorResult<Left>
            (boost::move(new_state_site_1)
            ,boost::move(new_state_site_2)
            );
}
//@+node:gcross.20110125120748.1518: *4* moveSiteCursorRight
MoveSiteCursorResult<Right> moveSiteCursorRight(
      StateSite<Middle> const& old_state_site_1
    , StateSite<Right> const& old_state_site_2
) {
    StateSite<Left> new_state_site_1(dimensionsOf(old_state_site_1));
    StateSite<Middle> new_state_site_2(dimensionsOf(old_state_site_2));
    unsigned int const info =
    norm_denorm_going_right(
         old_state_site_1.leftDimension(as_unsigned_integer)
        ,old_state_site_1 || old_state_site_2
        ,old_state_site_2.rightDimension(as_unsigned_integer)
        ,old_state_site_1.physicalDimension(as_unsigned_integer)
        ,old_state_site_2.physicalDimension(as_unsigned_integer)
        ,old_state_site_1
        ,old_state_site_2
        ,new_state_site_1
        ,new_state_site_2
    );
    if(info != 0) throw NormalizationError(info);
    return MoveSiteCursorResult<Right>
            (boost::move(new_state_site_2)
            ,boost::move(new_state_site_1)
            );
}
//@+node:gcross.20110125120748.2463: *3* Miscellaneous
//@+node:gcross.20110124175241.1649: *4* increaseDimensionBetween
template<Side side1,Side side2>
static
void implIncreaseDimensionBetween(
      unsigned int const new_dimension
    , StateSite<side1> const& old_site_1
    , StateSite<side2> const& old_site_2
    , auto_ptr<StateSite<side1> const>& new_site_1
    , auto_ptr<StateSite<side2> const>& new_site_2
) {
    unsigned int const old_dimension = old_site_1 || old_site_2;
    assert(new_dimension >= old_dimension);

    auto_ptr<StateSite<side1> > new_site_1_writable(
        new StateSite<side1>(
             old_site_1.physicalDimension()
            ,old_site_1.leftDimension()
            ,RightDimension(new_dimension)
        )
    );
    auto_ptr<StateSite<side2> > new_site_2_writable(
        new StateSite<side2>(
             old_site_2.physicalDimension()
            ,LeftDimension(new_dimension)
            ,old_site_2.rightDimension()
        )
    );

    int const info =
        new_dimension > old_dimension
            ? increase_bandwidth_between(
                 old_site_1.leftDimension(as_unsigned_integer)
                ,old_dimension
                ,old_site_2.rightDimension(as_unsigned_integer)
                ,old_site_1.physicalDimension(as_unsigned_integer)
                ,old_site_2.physicalDimension(as_unsigned_integer)
                ,new_dimension
                ,old_site_1
                ,old_site_2
                ,*new_site_1_writable
                ,*new_site_2_writable
              )
            : norm_denorm_going_left(
                 old_site_1.leftDimension(as_unsigned_integer)
                ,old_dimension
                ,old_site_2.rightDimension(as_unsigned_integer)
                ,old_site_1.physicalDimension(as_unsigned_integer)
                ,old_site_2.physicalDimension(as_unsigned_integer)
                ,old_site_1
                ,old_site_2
                ,*new_site_1_writable
                ,*new_site_2_writable
              )
    ;
    if(info != 0) throw NormalizationError(info);
    new_site_1 = new_site_1_writable;
    new_site_2 = new_site_2_writable;    
}

void increaseDimensionBetweenRightRight(
      unsigned int new_dimension
    , StateSite<Right> const& old_site_1
    , StateSite<Right> const& old_site_2
    , auto_ptr<StateSite<Right> const>& new_site_1
    , auto_ptr<StateSite<Right> const>& new_site_2
) { implIncreaseDimensionBetween(new_dimension,old_site_1,old_site_2,new_site_1,new_site_2); }

void increaseDimensionBetweenMiddleRight(
      unsigned int new_dimension
    , StateSite<Middle> const& old_site_1
    , StateSite<Right> const& old_site_2
    , auto_ptr<StateSite<Middle> const>& new_site_1
    , auto_ptr<StateSite<Right> const>& new_site_2
) { implIncreaseDimensionBetween(new_dimension,old_site_1,old_site_2,new_site_1,new_site_2); }
//@+node:gcross.20110124175241.1587: *4* optimizeStateSite
OptimizerResult optimizeStateSite(
      ExpectationBoundary<Left> const& left_boundary
    , StateSite<Middle> const& current_state_site
    , OperatorSite const& operator_site
    , ExpectationBoundary<Right> const& right_boundary
    , optional<ProjectorMatrix const&> maybe_projector_matrix
    , double const tolerance
    , unsigned int const maximum_number_of_iterations
) {
    uint32_t number_of_iterations = maximum_number_of_iterations;
    complex<double> eigenvalue;
    StateSite<Middle> new_state_site(dimensionsOf(current_state_site));

    double normal;
    int const status =
        maybe_projector_matrix
            ? optimize(
                 left_boundary || current_state_site
                ,current_state_site || right_boundary
                ,left_boundary || operator_site
                ,operator_site || right_boundary
                ,operator_site || current_state_site
                ,left_boundary
                ,operator_site.numberOfMatrices(),operator_site,operator_site
                ,right_boundary
                ,maybe_projector_matrix->numberOfProjectors()
                ,maybe_projector_matrix->numberOfReflectors()
                ,maybe_projector_matrix->orthogonalSubspaceDimension()
                ,maybe_projector_matrix->reflectorData()
                ,maybe_projector_matrix->coefficientData()
                ,maybe_projector_matrix->swapData()
                ,"SR"
                ,tolerance
                ,number_of_iterations
                ,current_state_site
                ,new_state_site
                ,eigenvalue
                ,normal
              )
            : optimize(
                 left_boundary || current_state_site
                ,current_state_site || right_boundary
                ,left_boundary || operator_site
                ,operator_site || right_boundary
                ,operator_site || current_state_site
                ,left_boundary
                ,operator_site.numberOfMatrices(),operator_site,operator_site
                ,right_boundary
                ,0
                ,0
                ,current_state_site.size()
                ,NULL
                ,NULL
                ,NULL
                ,"SR"
                ,tolerance
                ,number_of_iterations
                ,current_state_site
                ,new_state_site
                ,eigenvalue
                ,normal
              )
            ;
    complex<double> const expectation_value =
        computeExpectationValue(
             left_boundary
            ,new_state_site
            ,operator_site
            ,right_boundary
        );
    double const overlap =
        maybe_projector_matrix
            ? computeOverlapWithProjectors(
                 *maybe_projector_matrix
                ,new_state_site
              )
            : 0
            ;
    switch(status) {
        case -14:
            throw OptimizerUnableToConverge(number_of_iterations);
        case  10:
            throw OptimizerGivenTooManyProjectors(
                 maybe_projector_matrix->numberOfProjectors()
                ,current_state_site.physicalDimension()
                ,current_state_site.leftDimension()
                ,current_state_site.rightDimension()
            );
        case 11:
            throw OptimizerGivenGuessInProjectorSpace();
        case 0:
            if(abs(eigenvalue-expectation_value)/(abs(eigenvalue)+abs(expectation_value)) > tolerance)
                throw OptimizerObtainedEigenvalueDifferentFromExpectationValue(
                     eigenvalue
                    ,expectation_value
                );
            if(eigenvalue.imag()/abs(eigenvalue) > tolerance)
                throw OptimizerObtainedComplexEigenvalue(eigenvalue);
            if(normal < 1-tolerance)
                throw OptimizerObtainedVanishingEigenvector(normal);
            if(overlap > tolerance)
                throw OptimizerObtainedEigenvectorInProjectorSpace(overlap);
            break;
        default:
            throw OptimizerUnknownFailure(status);
    }
    return OptimizerResult(
         number_of_iterations
        ,eigenvalue.real()
        ,boost::move(new_state_site)
    );
}
//@+node:gcross.20110126102637.2187: *3* Overlap tensor formation
//@+node:gcross.20110126102637.2188: *4* computeMiddleOverlapSiteFromStateSite
OverlapSite<Middle> computeOverlapSiteFromStateSite(StateSite<Middle> const& state_site) {
    OverlapSite<Middle> overlap_site(dimensionsOf(state_site));
    form_overlap_site_tensor(
         state_site.rightDimension(as_unsigned_integer)
        ,state_site.leftDimension(as_unsigned_integer)
        ,state_site.physicalDimension(as_unsigned_integer)
        ,state_site
        ,overlap_site
    );
    return boost::move(overlap_site);
}
//@+node:gcross.20110126102637.2189: *4* computeOverlapSitesFromStateSitesAndNormalize
OverlapSitesFromStateSitesAndNormalizeResult computeOverlapSitesFromStateSitesAndNormalize(
      StateSite<Middle> const& middle_state_site
     ,StateSite<Right> const& right_state_site
) {
    OverlapSite<Left> left_overlap_site_from_middle_state_site(dimensionsOf(middle_state_site));
    OverlapSite<Middle> middle_overlap_site_from_middle_state_site(dimensionsOf(middle_state_site));
    StateSite<Middle> middle_state_site_from_right_state_site(dimensionsOf(right_state_site));
    OverlapSite<Right> right_overlap_site_from_right_state_site(dimensionsOf(right_state_site));
    form_norm_overlap_tensors(
         middle_state_site.leftDimension(as_unsigned_integer)
        ,middle_state_site || right_state_site
        ,right_state_site.rightDimension(as_unsigned_integer)
        ,middle_state_site.physicalDimension(as_unsigned_integer)
        ,right_state_site.physicalDimension(as_unsigned_integer)
        ,middle_state_site
        ,right_state_site
        ,left_overlap_site_from_middle_state_site
        ,middle_overlap_site_from_middle_state_site
        ,middle_state_site_from_right_state_site
        ,right_overlap_site_from_right_state_site
    );
    return
        OverlapSitesFromStateSitesAndNormalizeResult(
             boost::move(left_overlap_site_from_middle_state_site)
            ,boost::move(middle_overlap_site_from_middle_state_site)
            ,boost::move(middle_state_site_from_right_state_site)
            ,boost::move(right_overlap_site_from_right_state_site)
        );
}
//@+node:gcross.20110126102637.2190: *3* Projectors
//@+node:gcross.20110126102637.2196: *4* applyProjectorMatrix
StateSite<Middle> applyProjectorMatrix(
      ProjectorMatrix const& projector_matrix
    , StateSite<Middle> const& old_state_site
) {
    StateSite<Middle> new_state_site(dimensionsOf(old_state_site));
    filter_components_outside_orthog(
         projector_matrix || old_state_site
        ,projector_matrix.numberOfProjectors()
        ,projector_matrix.numberOfReflectors()
        ,projector_matrix.orthogonalSubspaceDimension()
        ,projector_matrix.reflectorData()
        ,projector_matrix.coefficientData()
        ,projector_matrix.swapData()
        ,old_state_site
        ,new_state_site
    );
    return boost::move(new_state_site);
}
//@+node:gcross.20110126102637.2194: *4* computeOverlapWithProjectors
double computeOverlapWithProjectors(
     ProjectorMatrix const& projector_matrix
    ,StateSite<Middle> const& state_site
) {
    return abs(compute_overlap_with_projectors(
         projector_matrix.numberOfProjectors()
        ,projector_matrix.numberOfReflectors()
        ,projector_matrix.reflectorData()
        ,projector_matrix.coefficientData()
        ,projector_matrix.swapData()
        ,projector_matrix || state_site
        ,state_site
    ));
}
//@+node:gcross.20110126102637.2191: *4* formProjectorMatrix
ProjectorMatrix formProjectorMatrix(
    vector<OverlapVectorTrio> const& overlaps
) {
    unsigned int const
         number_of_projectors = overlaps.size()
        ,state_physical_dimension = overlaps[0].middle_site.physicalDimension(as_unsigned_integer)
        ,state_left_dimension = overlaps[0].left_boundary.stateDimension(as_unsigned_integer)
        ,state_right_dimension = overlaps[0].right_boundary.stateDimension(as_unsigned_integer)
        ,overlap_vector_length = state_physical_dimension*state_left_dimension*state_right_dimension
        ,number_of_reflectors = min(overlap_vector_length,number_of_projectors)
        ;
    complex<double>* overlap_vectors
        = new complex<double>[number_of_projectors*overlap_vector_length];
    complex<double>* overlap_vector = overlap_vectors;
    BOOST_FOREACH(
         OverlapVectorTrio const& overlap
        ,overlaps
    ) {
        form_overlap_vector(
             overlap.left_boundary || overlap.middle_site
            ,overlap.middle_site || overlap.right_boundary
            ,state_left_dimension
            ,state_right_dimension
            ,state_physical_dimension
            ,overlap.left_boundary
            ,overlap.right_boundary
            ,overlap.middle_site
            ,overlap_vector
        );
        overlap_vector += overlap_vector_length;
    }
    complex<double>* coefficients = new complex<double>[number_of_reflectors];
    uint32_t* swaps = new uint32_t[number_of_reflectors];
    unsigned int const subspace_dimension =
    convert_vectors_to_reflectors(
         overlap_vector_length
        ,number_of_projectors
        ,overlap_vectors
        ,coefficients
        ,swaps
    );
    return ProjectorMatrix(
             number_of_projectors
            ,overlap_vector_length
            ,number_of_reflectors
            ,overlap_vector_length-subspace_dimension
            ,overlap_vectors
            ,coefficients
            ,swaps
    );
}
//@+node:gcross.20110126102637.2201: *4* randomProjectorMatrix
ProjectorMatrix randomProjectorMatrix(
     unsigned int const vector_length
    ,unsigned int const number_of_projectors
) {
    unsigned int const number_of_reflectors = min(number_of_projectors,vector_length);
    complex<double>* reflectors
        = new complex<double>[number_of_projectors*vector_length];
    complex<double>* coefficients
        = new complex<double>[number_of_reflectors];
    uint32_t* swaps
        = new uint32_t[number_of_reflectors];
    unsigned const int subspace_dimension =
    random_projector_matrix(
         vector_length
        ,number_of_projectors
        ,reflectors
        ,coefficients
        ,swaps
    );
    return ProjectorMatrix(
             number_of_projectors
            ,vector_length
            ,number_of_reflectors
            ,vector_length-subspace_dimension
            ,reflectors
            ,coefficients
            ,swaps
    );
}
//@+node:gcross.20110124175241.1601: *3* Randomizers
//@+node:gcross.20110124175241.1645: *4* randomStateSiteMiddle
StateSite<Middle> randomStateSiteMiddle(
      const PhysicalDimension physical_dimension
    , const LeftDimension left_dimension
    , const RightDimension right_dimension
) {
    StateSite<Middle> state_site
        (physical_dimension
        ,left_dimension
        ,right_dimension
        );
    rand_unnorm_state_site_tensor(
         *right_dimension
        ,*left_dimension
        ,*physical_dimension
        ,state_site
    );  
    return boost::move(state_site);
}
//@+node:gcross.20110124175241.1647: *4* randomStateSiteRight
StateSite<Right> randomStateSiteRight(
      const PhysicalDimension physical_dimension
    , const LeftDimension left_dimension
    , const RightDimension right_dimension
) {
    if((*right_dimension) >= (*physical_dimension)*(*left_dimension)) {
        throw NotEnoughDegreesOfFreedomToNormalizeError(
                 "right"
                ,*right_dimension
                ,"physical"
                ,*physical_dimension
                ,"left"
                ,*left_dimension
        );
    }
    if((*left_dimension) >= (*physical_dimension)*(*right_dimension)) {
        throw NotEnoughDegreesOfFreedomToNormalizeError(
                 "left"
                ,*left_dimension
                ,"physical"
                ,*physical_dimension
                ,"right"
                ,*right_dimension
        );
    }
    StateSite<Right> state_site
        (physical_dimension
        ,left_dimension
        ,right_dimension
        );
    rand_norm_state_site_tensor(
         *right_dimension
        ,*left_dimension
        ,*physical_dimension
        ,state_site
    );
    return boost::move(state_site);
}
//@-others

}
//@-leo
