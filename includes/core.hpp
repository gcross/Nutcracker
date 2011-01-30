//@+leo-ver=5-thin
//@+node:gcross.20110125120748.2458: * @thin core.hpp
//@@language cplusplus

#ifndef NUTCRACKER_CORE_HPP
#define NUTCRACKER_CORE_HPP

//@+<< Includes >>
//@+node:gcross.20110125120748.2459: ** << Includes >>
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
using namespace boost;
using namespace std;
//@-<< Usings >>

//@+others
//@+node:gcross.20110129220506.1656: ** Classes
//@+node:gcross.20110129220506.1657: *3* OptimizerResult
struct OptimizerResult {
    unsigned int const number_of_iterations;
    double const eigenvalue;
    shared_ptr<StateSite<Middle> const> const state_site;

    OptimizerResult(
          unsigned int const number_of_iterations
        , double const eigenvalue
        , shared_ptr<StateSite<Middle> const> const state_site
    ) : number_of_iterations(number_of_iterations)
      , eigenvalue(eigenvalue)
      , state_site(state_site)
    {}
};
//@+node:gcross.20110125120748.2469: *3* OptimizerSelectionStrategy
extern struct OptimizerSelectionStrategy {
    const string argument;
    OptimizerSelectionStrategy(const string argument) : argument(argument) { }
    operator const char*() const { return argument.c_str(); }
} const optimize_for_lowest_real_part, optimize_for_largest_magnitude;
//@+node:gcross.20110125120748.2467: ** Exceptions
//@+node:gcross.20110125120748.2468: *3* NormalizationError
struct NormalizationError : public Exception {
    int const info;
    NormalizationError(int info);
};
//@+node:gcross.20110125120748.2473: *3* OptimizerFailure
struct OptimizerFailure : public Exception {
protected:
    OptimizerFailure(string const& message);
};
//@+node:gcross.20110125202132.2169: *4* OptimizerUnableToConverge
struct OptimizerUnableToConverge : public OptimizerFailure {
    unsigned int const number_of_iterations;
    OptimizerUnableToConverge(unsigned int number_of_iterations);
};
//@+node:gcross.20110125202132.2170: *4* OptimizerObtainedEigenvalueDifferentFromExpectationValue
struct OptimizerObtainedEigenvalueDifferentFromExpectationValue : public OptimizerFailure {
    complex<double> const eigenvalue, expected_value;
    OptimizerObtainedEigenvalueDifferentFromExpectationValue(
          complex<double> eigenvalue
        , complex<double> expected_value
    );
};
//@+node:gcross.20110125202132.2171: *4* OptimizerObtainedComplexEigenvalue
struct OptimizerObtainedComplexEigenvalue : public OptimizerFailure {
    complex<double> const eigenvalue;
    OptimizerObtainedComplexEigenvalue(complex<double> eigenvalue);
};
//@+node:gcross.20110125202132.2172: *4* OptimizerObtainedVanishingEigenvector
struct OptimizerObtainedVanishingEigenvector : public OptimizerFailure {
    double const norm;
    OptimizerObtainedVanishingEigenvector(double norm);
};
//@+node:gcross.20110125202132.2174: *4* OptimizerObtainedEigenvectorInProjectorSpace
struct OptimizerObtainedEigenvectorInProjectorSpace : public OptimizerFailure {
    double const overlap;
    OptimizerObtainedEigenvectorInProjectorSpace(double overlap);
};
//@+node:gcross.20110125202132.2178: *4* OptimizerGivenTooManyProjectors
struct OptimizerGivenTooManyProjectors : public OptimizerFailure {
    unsigned int const number_of_projectors;
    PhysicalDimension const physical_dimension;
    LeftDimension const left_dimension;
    RightDimension const right_dimension;

    OptimizerGivenTooManyProjectors(
          unsigned int number_of_projectors
        , PhysicalDimension physical_dimension
        , LeftDimension left_dimension
        , RightDimension right_dimension
    );
};
//@+node:gcross.20110125202132.2182: *4* OptimizerGivenGuessInProjectorSpace
struct OptimizerGivenGuessInProjectorSpace : public OptimizerFailure {
    OptimizerGivenGuessInProjectorSpace();
};
//@+node:gcross.20110125202132.2188: *4* OptimizerUnknownFailure
struct OptimizerUnknownFailure : public OptimizerFailure {
    int const error_code;
    OptimizerUnknownFailure(int error_code);
};
//@+node:gcross.20110125120748.2461: ** Functions
//@+node:gcross.20110125120748.2462: *3* Contractors
complex<double> computeExpectationValue(
      ExpectationBoundary<Left> const& left_boundary
    , StateSite<Middle> const& state_site
    , OperatorSite const& operator_site
    , ExpectationBoundary<Right> const& right_boundary
);

complex<double> contractExpectationBoundaries(
      ExpectationBoundary<Left> const& left_boundary
    , ExpectationBoundary<Right> const& right_boundary
);

auto_ptr<ExpectationBoundary<Left> const> contractSOSLeft(
      ExpectationBoundary<Left> const& old_boundary
    , StateSite<Left> const& state_site
    , OperatorSite const& operator_site
);

auto_ptr<ExpectationBoundary<Right> const> contractSOSRight(
      ExpectationBoundary<Right> const& old_boundary
    , StateSite<Right> const& state_site
    , OperatorSite const& operator_site
);

auto_ptr<OverlapBoundary<Left> const> contractSSLeft(
      OverlapBoundary<Left> const& old_boundary
    , OverlapSite<Left> const& overlap_site
    , StateSite<Left> const& state_site
);

auto_ptr<OverlapBoundary<Right> const> contractSSRight(
      OverlapBoundary<Right> const& old_boundary
    , OverlapSite<Right> const& overlap_site
    , StateSite<Right> const& state_site
);
//@+node:gcross.20110125120748.2465: *3* Cursor movement
pair <shared_ptr<StateSite<Middle> const>
     ,shared_ptr<StateSite<Right> const>
> moveSiteCursorLeft(
      StateSite<Left> const& old_state_site_1
    , StateSite<Middle> const& old_state_site_2
);

pair <shared_ptr<StateSite<Left> const>
     ,shared_ptr<StateSite<Middle> const>
> moveSiteCursorRight(
      StateSite<Middle> const& old_state_site_1
    , StateSite<Right> const& old_state_site_2
);
//@+node:gcross.20110125120748.2466: *3* Miscellaneous
pair <shared_ptr<StateSite<Right> const>
     ,shared_ptr<StateSite<Right> const>
> increaseDimensionBetween(
      unsigned int new_dimension
    , const StateSite<Right> old_site_1
    , const StateSite<Right> old_site_2
);

pair <shared_ptr<StateSite<Middle> const>
     ,shared_ptr<StateSite<Right> const>
> increaseDimensionBetween(
      unsigned int new_dimension
    , const StateSite<Middle> old_site_1
    , const StateSite<Right> old_site_2
);

OptimizerResult optimizeStateSite(
      ExpectationBoundary<Left> const& left_boundary
    , StateSite<Middle> const& current_state_site
    , OperatorSite const& operator_site
    , ExpectationBoundary<Right> const& right_boundary
    , optional<ProjectorMatrix const&> projector_matrix
    , OptimizerSelectionStrategy const& strategy
    , double const tolerance
    , unsigned int const maximum_number_of_iterations
);
//@+node:gcross.20110126102637.2193: *3* Overlap tensor formation
auto_ptr<OverlapSite<Middle> const> computeOverlapSiteFromStateSite(StateSite<Middle> const& state_site);

tuple<shared_ptr<OverlapSite<Left> const>
     ,shared_ptr<OverlapSite<Middle> const>
     ,shared_ptr<StateSite<Middle> const>
     ,shared_ptr<OverlapSite<Right> const>
> computeOverlapSitesFromStateSitesAndNormalize(
      StateSite<Middle> const& middle_state_site
     ,StateSite<Right> const& right_state_site
);
//@+node:gcross.20110126102637.2195: *3* Projectors
shared_ptr<StateSite<Middle> const> applyProjectorMatrix(
      ProjectorMatrix const& projector_matrix
    , shared_ptr<StateSite<Middle> const> old_state_site
);

double computeOverlapWithProjectors(
     ProjectorMatrix const& projector_matrix
    ,StateSite<Middle> const& state_site
);

auto_ptr<ProjectorMatrix const> formProjectorMatrix(
    vector<OverlapVectorTrio> const& overlaps
);

auto_ptr<ProjectorMatrix const> randomProjectorMatrix(
     unsigned int const vector_length
    ,unsigned int const number_of_projectors
);
//@+node:gcross.20110125120748.2464: *3* Randomizers
auto_ptr<StateSite<Middle> const> randomStateSiteMiddle(
      unsigned int const physical_dimension
    , unsigned int const left_dimension
    , unsigned int const right_dimension
);

auto_ptr<StateSite<Right> const> randomStateSiteRight(
      unsigned int const physical_dimension
    , unsigned int const left_dimension
    , unsigned int const right_dimension
);
//@-others

}

#endif
//@-leo
