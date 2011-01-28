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
            % physical_dimension()
            % left_dimension()
            % right_dimension()
            % (physical_dimension()*left_dimension()*right_dimension())
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
//@+node:gcross.20110127123226.2816: ** Values
OptimizerSelectionStrategy const
     optimize_for_lowest_real_part("SR")
    ,optimize_for_largest_magnitude("LM")
    ;

DimensionsOf const dimensionsOf = {};
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
        ,operator_site.number_of_matrices,operator_site,operator_site
        ,right_boundary
    );
}
//@+node:gcross.20110124175241.1589: *4* contractSOSLeft
auto_ptr<ExpectationBoundary<Left> const> contractSOSLeft(
      ExpectationBoundary<Left> const& old_boundary
    , StateSite<Left> const& state_site
    , OperatorSite const& operator_site
) {
    auto_ptr<ExpectationBoundary<Left> > new_boundary(
        new ExpectationBoundary<Left>
            (OperatorDimension(operator_site.right_dimension())
            ,StateDimension(state_site.right_dimension())
            )
    );
    contract_sos_left(
         old_boundary || state_site
        ,state_site.right_dimension()
        ,old_boundary || operator_site
        ,operator_site.right_dimension()
        ,operator_site || state_site
        ,old_boundary
        ,operator_site.number_of_matrices,operator_site,operator_site
        ,state_site
        ,*new_boundary
    );
    return (auto_ptr<ExpectationBoundary<Left> const>) new_boundary;
}
//@+node:gcross.20110124175241.1593: *4* contractSOSRight
auto_ptr<ExpectationBoundary<Right> const> contractSOSRight(
      ExpectationBoundary<Right> const& old_boundary
    , StateSite<Right> const& state_site
    , OperatorSite const& operator_site
) {
    auto_ptr<ExpectationBoundary<Right> > new_boundary(
        new ExpectationBoundary<Right>
            (OperatorDimension(operator_site.left_dimension())
            ,StateDimension(state_site.left_dimension())
            )
    );
    contract_sos_right(
         state_site.left_dimension()
        ,state_site || old_boundary
        ,operator_site.left_dimension()
        ,operator_site || old_boundary
        ,operator_site || state_site
        ,old_boundary
        ,operator_site.number_of_matrices,operator_site,operator_site
        ,state_site
        ,*new_boundary
    );
    return (auto_ptr<ExpectationBoundary<Right> const>) new_boundary;
}
//@+node:gcross.20110124175241.1595: *4* contractSSLeft
auto_ptr<OverlapBoundary<Left> const> contractSSLeft(
      OverlapBoundary<Left> const& old_boundary
    , OverlapSite<Left> const& overlap_site
    , StateSite<Left> const& state_site
) {
    auto_ptr<OverlapBoundary<Left> > new_boundary(
        new OverlapBoundary<Left>
            (OverlapDimension(overlap_site.right_dimension())
            ,StateDimension(state_site.right_dimension())
            )
    );
    contract_ss_left(
         old_boundary || overlap_site
        ,overlap_site.right_dimension()
        ,old_boundary || state_site
        ,state_site.right_dimension()
        ,overlap_site || state_site
        ,old_boundary
        ,overlap_site
        ,state_site
        ,*new_boundary
    );
    return (auto_ptr<OverlapBoundary<Left> const>) new_boundary;
}
//@+node:gcross.20110124175241.1597: *4* contractSSRight
auto_ptr<OverlapBoundary<Right> const> contractSSRight(
      OverlapBoundary<Right> const& old_boundary
    , OverlapSite<Right> const& overlap_site
    , StateSite<Right> const& state_site
) {
    auto_ptr<OverlapBoundary<Right> > new_boundary(
        new OverlapBoundary<Right>
            (OverlapDimension(overlap_site.left_dimension())
            ,StateDimension(state_site.left_dimension())
            )
    );
    contract_ss_right(
         overlap_site.left_dimension()
        ,overlap_site || old_boundary
        ,state_site.left_dimension()
        ,state_site || old_boundary
        ,overlap_site || state_site
        ,old_boundary
        ,overlap_site
        ,state_site
        ,*new_boundary
    );
    return (auto_ptr<OverlapBoundary<Right> const>) (new_boundary);
}
//@+node:gcross.20110124175241.1652: *3* Cursor movement
//@+node:gcross.20110124175241.1654: *4* moveSiteCursorLeft
pair <shared_ptr<StateSite<Middle> const>
     ,shared_ptr<StateSite<Right> const>
> moveSiteCursorLeft(
      StateSite<Left> const& old_state_site_1
    , StateSite<Middle> const& old_state_site_2
) {
    shared_ptr<StateSite<Middle> > new_state_site_1(
        new StateSite<Middle>(dimensionsOf,old_state_site_1)
    );
    shared_ptr<StateSite<Right> >  new_state_site_2(
        new StateSite<Right>(dimensionsOf,old_state_site_2)
    );
    unsigned int const info =
    norm_denorm_going_left(
         old_state_site_1.left_dimension()
        ,old_state_site_1 || old_state_site_2
        ,old_state_site_2.right_dimension()
        ,old_state_site_1.physical_dimension()
        ,old_state_site_2.physical_dimension()
        ,old_state_site_1
        ,old_state_site_2
        ,*new_state_site_1
        ,*new_state_site_2
    );
    if(info != 0) throw NormalizationError(info);
    return make_pair(new_state_site_1,new_state_site_2);
}
//@+node:gcross.20110125120748.1518: *4* moveSiteCursorRight
pair <shared_ptr<StateSite<Left> const>
     ,shared_ptr<StateSite<Middle> const>
> moveSiteCursorRight(
      StateSite<Middle> const& old_state_site_1
    , StateSite<Right> const& old_state_site_2
) {
    shared_ptr<StateSite<Left> > new_state_site_1(
        new StateSite<Left>(dimensionsOf,old_state_site_1)
    );
    shared_ptr<StateSite<Middle> >  new_state_site_2(
        new StateSite<Middle>(dimensionsOf,old_state_site_2)
    );
    unsigned int const info =
    norm_denorm_going_right(
         old_state_site_1.left_dimension()
        ,old_state_site_1 || old_state_site_2
        ,old_state_site_2.right_dimension()
        ,old_state_site_1.physical_dimension()
        ,old_state_site_2.physical_dimension()
        ,old_state_site_1
        ,old_state_site_2
        ,*new_state_site_1
        ,*new_state_site_2
    );
    if(info != 0) throw NormalizationError(info);
    return make_pair(new_state_site_1,new_state_site_2);
}
//@+node:gcross.20110125120748.2463: *3* Miscellaneous
//@+node:gcross.20110124175241.1649: *4* increaseDimensionBetween
template<Side side1,Side side2>
static
pair <shared_ptr<StateSite<side1> const>
     ,shared_ptr<StateSite<side2> const>
> implIncreaseDimensionBetween(
      unsigned int const new_dimension
    , StateSite<side1> const& old_site_1
    , StateSite<side2> const& old_site_2
) {
    unsigned int const old_dimension = old_site_1 || old_site_2;
    assert(new_dimension >= old_dimension);

    shared_ptr<StateSite<side1> > new_site_1(
        new StateSite<side1>(
             old_site_1.physical_dimension
            ,old_site_1.left_dimension
            ,RightDimension(new_dimension)
        )
    );
    shared_ptr<StateSite<side2> > new_site_2(
        new StateSite<side2>(
             old_site_2.physical_dimension
            ,LeftDimension(new_dimension)
            ,old_site_2.right_dimension
        )
    );

    int const info =
        new_dimension > old_dimension
            ? increase_bandwidth_between(
                 old_site_1.left_dimension()
                ,old_dimension
                ,old_site_2.right_dimension()
                ,old_site_1.physical_dimension()
                ,old_site_2.physical_dimension()
                ,new_dimension
                ,old_site_1
                ,old_site_2
                ,*new_site_1
                ,*new_site_2
              )
            : norm_denorm_going_left(
                 old_site_1.left_dimension()
                ,old_dimension
                ,old_site_2.right_dimension()
                ,old_site_1.physical_dimension()
                ,old_site_2.physical_dimension()
                ,old_site_1
                ,old_site_2
                ,*new_site_1
                ,*new_site_2
              )
    ;
    if(info != 0) throw NormalizationError(info);
    return make_pair(new_site_1,new_site_2);
}

pair <shared_ptr<StateSite<Right> const>
     ,shared_ptr<StateSite<Right> const>
> increaseDimensionBetween(
      unsigned int new_dimension
    , const StateSite<Right> old_site_1
    , const StateSite<Right> old_site_2
) { return implIncreaseDimensionBetween(new_dimension,old_site_1,old_site_2); }

pair <shared_ptr<StateSite<Middle> const>
     ,shared_ptr<StateSite<Right> const>
> increaseDimensionBetween(
      unsigned int new_dimension
    , const StateSite<Middle> old_site_1
    , const StateSite<Right> old_site_2
) { return implIncreaseDimensionBetween(new_dimension,old_site_1,old_site_2); }
//@+node:gcross.20110124175241.1587: *4* optimizeStateSite
tuple<unsigned int
     ,double
     ,shared_ptr<StateSite<Middle> const>
> optimizeStateSite(
      ExpectationBoundary<Left> const& left_boundary
    , StateSite<Middle> const& current_state_site
    , OperatorSite const& operator_site
    , ExpectationBoundary<Right> const& right_boundary
    , ProjectorMatrix const& projector_matrix
    , OptimizerSelectionStrategy const& strategy
    , double const tolerance
    , unsigned int const maximum_number_of_iterations
) {
    unsigned int number_of_iterations = maximum_number_of_iterations;
    complex<double> eigenvalue;
    shared_ptr<StateSite<Middle> > new_state_site(
        new StateSite<Middle>(dimensionsOf,current_state_site)
    );
    double normal;
    int const status =
    optimize(
         left_boundary || current_state_site
        ,current_state_site || right_boundary
        ,left_boundary || operator_site
        ,operator_site || right_boundary
        ,operator_site || current_state_site
        ,left_boundary
        ,operator_site.number_of_matrices,operator_site,operator_site
        ,right_boundary
        ,projector_matrix.number_of_projectors
        ,projector_matrix.number_of_reflectors
        ,projector_matrix.subspace_dimension
        ,projector_matrix.reflectorData()
        ,projector_matrix.coefficientData()
        ,projector_matrix.swapData()
        ,strategy
        ,tolerance
        ,number_of_iterations
        ,current_state_site
        ,*new_state_site
        ,eigenvalue
        ,normal
    );
    complex<double> const expectation_value =
        computeExpectationValue(
             left_boundary
            ,*new_state_site
            ,operator_site
            ,right_boundary
        );
    double const overlap =
        computeOverlapWithProjectors(
             projector_matrix
            ,*new_state_site
        );
    switch(status) {
        case -14:
            throw OptimizerUnableToConverge(number_of_iterations);
        case  10:
            throw OptimizerGivenTooManyProjectors(
                 projector_matrix.number_of_projectors
                ,current_state_site.physical_dimension
                ,current_state_site.left_dimension
                ,current_state_site.right_dimension
            );
        case 11:
            throw OptimizerGivenGuessInProjectorSpace();
        case 0:
            if(abs(eigenvalue-expectation_value) > 1e-7)
                throw OptimizerObtainedEigenvalueDifferentFromExpectationValue(
                     eigenvalue
                    ,expectation_value
                );
            if(eigenvalue.imag() > 1e-10)
                throw OptimizerObtainedComplexEigenvalue(eigenvalue);
            if(normal < 1-1e-7)
                throw OptimizerObtainedVanishingEigenvector(normal);
            if(overlap > 1e-10)
                throw OptimizerObtainedEigenvectorInProjectorSpace(overlap);
            break;
        default:
            throw OptimizerUnknownFailure(status);
    }
    return make_tuple(number_of_iterations,eigenvalue.real(),new_state_site);
}
//@+node:gcross.20110126102637.2187: *3* Overlap tensor formation
//@+node:gcross.20110126102637.2188: *4* computeMiddleOverlapSiteFromStateSite
auto_ptr<OverlapSite<Middle> const>
computeOverlapSiteFromStateSite(StateSite<Middle> const& state_site) {
    auto_ptr<OverlapSite<Middle> > overlap_site(
        new OverlapSite<Middle>(dimensionsOf,state_site)
    );
    form_overlap_site_tensor(
         state_site.right_dimension()
        ,state_site.left_dimension()
        ,state_site.physical_dimension()
        ,state_site
        ,*overlap_site
    );
    return (auto_ptr<OverlapSite<Middle> const>) overlap_site;
}
//@+node:gcross.20110126102637.2189: *4* computeOverlapSitesFromStateSitesAndNormalize
tuple<shared_ptr<OverlapSite<Left> const>
     ,shared_ptr<OverlapSite<Middle> const>
     ,shared_ptr<StateSite<Middle> const>
     ,shared_ptr<OverlapSite<Right> const>
> computeOverlapSitesFromStateSitesAndNormalize(
      StateSite<Middle> const& middle_state_site
     ,StateSite<Right> const& right_state_site
) {
    shared_ptr<OverlapSite<Left> > left_overlap_site_from_middle_state_site(
        new OverlapSite<Left>(dimensionsOf,middle_state_site)
    );
    shared_ptr<OverlapSite<Middle> > middle_overlap_site_from_middle_state_site(
        new OverlapSite<Middle>(dimensionsOf,middle_state_site)
    );
    shared_ptr<StateSite<Middle> > middle_state_site_from_right_state_site(
        new StateSite<Middle>(dimensionsOf,right_state_site)
    );
    shared_ptr<OverlapSite<Right> > right_overlap_site_from_right_state_site(
        new OverlapSite<Right>(dimensionsOf,right_state_site)
    );
    form_norm_overlap_tensors(
         middle_state_site.left_dimension()
        ,middle_state_site || right_state_site
        ,right_state_site.right_dimension()
        ,middle_state_site.physical_dimension()
        ,right_state_site.physical_dimension()
        ,middle_state_site
        ,right_state_site
        ,*left_overlap_site_from_middle_state_site
        ,*middle_overlap_site_from_middle_state_site
        ,*middle_state_site_from_right_state_site
        ,*right_overlap_site_from_right_state_site
    );
    return
        make_tuple(
             left_overlap_site_from_middle_state_site
            ,middle_overlap_site_from_middle_state_site
            ,middle_state_site_from_right_state_site
            ,right_overlap_site_from_right_state_site
        );
}
//@+node:gcross.20110126102637.2190: *3* Projectors
//@+node:gcross.20110126102637.2196: *4* applyProjectorMatrix
shared_ptr<StateSite<Middle> const> applyProjectorMatrix(
      ProjectorMatrix const& projector_matrix
    , shared_ptr<StateSite<Middle> const> old_state_site
) {
    if(!projector_matrix) return old_state_site;
    shared_ptr<StateSite<Middle> > new_state_site(
        new StateSite<Middle>(dimensionsOf,*old_state_site)
    );
    filter_components_outside_orthog(
         projector_matrix || old_state_site
        ,projector_matrix.number_of_projectors
        ,projector_matrix.number_of_reflectors
        ,projector_matrix.subspace_dimension
        ,projector_matrix.reflectorData()
        ,projector_matrix.coefficientData()
        ,projector_matrix.swapData()
        ,*old_state_site
        ,*new_state_site
    );
    return new_state_site;
}
//@+node:gcross.20110126102637.2194: *4* computeOverlapWithProjectors
double computeOverlapWithProjectors(
     ProjectorMatrix const& projector_matrix
    ,StateSite<Middle> const& state_site
) {
    if(!projector_matrix) return 0;
    return abs(compute_overlap_with_projectors(
         projector_matrix.number_of_projectors
        ,projector_matrix.number_of_reflectors
        ,projector_matrix.reflectorData()
        ,projector_matrix.coefficientData()
        ,projector_matrix.swapData()
        ,projector_matrix || state_site
        ,state_site
    ));
}
//@+node:gcross.20110126102637.2191: *4* formProjectorMatrix
auto_ptr<ProjectorMatrix const> formProjectorMatrix(
    vector<OverlapVectorTrio> const& overlaps
) {
    unsigned int const number_of_projectors = overlaps.size();
    if(number_of_projectors == 0)
        return auto_ptr<ProjectorMatrix const>(new ProjectorMatrix());
    unsigned int const
         state_physical_dimension = overlaps[0].middle_site->physical_dimension()
        ,state_left_dimension = overlaps[0].left_boundary->state_dimension()
        ,state_right_dimension = overlaps[0].right_boundary->state_dimension()
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
             *overlap.left_boundary || *overlap.middle_site
            ,*overlap.middle_site || *overlap.right_boundary
            ,state_left_dimension
            ,state_right_dimension
            ,state_physical_dimension
            ,*overlap.left_boundary
            ,*overlap.right_boundary
            ,*overlap.middle_site
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
    return auto_ptr<ProjectorMatrix const>(
        new ProjectorMatrix(
             number_of_projectors
            ,overlap_vector_length
            ,number_of_reflectors
            ,subspace_dimension
            ,overlap_vectors
            ,coefficients
            ,swaps
        )
    );
}
//@+node:gcross.20110126102637.2201: *4* randomProjectorMatrix
auto_ptr<ProjectorMatrix const> randomProjectorMatrix(
     unsigned int const vector_length
    ,unsigned int const number_of_projectors
) {
    if(number_of_projectors == 0)
        return auto_ptr<ProjectorMatrix const>(new ProjectorMatrix());
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
    return auto_ptr<ProjectorMatrix const>(
        new ProjectorMatrix(
             number_of_projectors
            ,vector_length
            ,number_of_reflectors
            ,subspace_dimension
            ,reflectors
            ,coefficients
            ,swaps
        )
    );
}
//@+node:gcross.20110124175241.1601: *3* Randomizers
//@+node:gcross.20110124175241.1645: *4* randomStateSiteMiddle
auto_ptr<StateSite<Middle> const> randomStateSiteMiddle(
      unsigned int const physical_dimension
    , unsigned int const left_dimension
    , unsigned int const right_dimension
) {
    auto_ptr<StateSite<Middle> > state_site(
        new StateSite<Middle>
            (PhysicalDimension(physical_dimension)
            ,LeftDimension(left_dimension)
            ,RightDimension(right_dimension)
            )
    );
    rand_unnorm_state_site_tensor(
         right_dimension
        ,left_dimension
        ,physical_dimension
        ,*state_site
    );  
    return (auto_ptr<StateSite<Middle> const>) state_site;
}
//@+node:gcross.20110124175241.1647: *4* randomStateSiteRight
auto_ptr<StateSite<Right> const> randomStateSiteRight(
      unsigned int const physical_dimension
    , unsigned int const left_dimension
    , unsigned int const right_dimension
) {
    auto_ptr<StateSite<Right> > state_site(
        new StateSite<Right>
            (PhysicalDimension(physical_dimension)
            ,LeftDimension(left_dimension)
            ,RightDimension(right_dimension)
            )
    );
    rand_norm_state_site_tensor(
         right_dimension
        ,left_dimension
        ,physical_dimension
        ,*state_site
    );
    return (auto_ptr<StateSite<Right> const>) state_site;
}
//@-others

}
//@-leo
