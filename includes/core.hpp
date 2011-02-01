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
    unsigned int number_of_iterations;
    double eigenvalue;
    mutable auto_ptr<StateSite<Middle> const> state_site;

    OptimizerResult(
          unsigned int const number_of_iterations
        , double const eigenvalue
        , auto_ptr<StateSite<Middle> > state_site
    ) : number_of_iterations(number_of_iterations)
      , eigenvalue(eigenvalue)
      , state_site(state_site)
    {}

    OptimizerResult(
          OptimizerResult const& other
    ) : number_of_iterations(other.number_of_iterations)
      , eigenvalue(other.eigenvalue)
      , state_site(other.state_site)
    {}
};
//@+node:gcross.20110125120748.2469: *3* OptimizerSelectionStrategy
extern struct OptimizerSelectionStrategy {
    const string argument;
    OptimizerSelectionStrategy(const string argument) : argument(argument) { }
    operator const char*() const { return argument.c_str(); }
} const optimize_for_lowest_real_part, optimize_for_largest_magnitude;
//@+node:gcross.20110131180117.1687: *3* OverlapSitesFromStateSitesAndNormalizeResult
struct OverlapSitesFromStateSitesAndNormalizeResult {
    mutable auto_ptr<OverlapSite<Left> const> left_overlap_site_from_middle_state_site;
    mutable auto_ptr<OverlapSite<Middle> const> middle_overlap_site_from_middle_state_site;
    mutable auto_ptr<StateSite<Middle> const> middle_state_site_from_right_state_site;
    mutable auto_ptr<OverlapSite<Right> const> right_overlap_site_from_right_state_site;

    OverlapSitesFromStateSitesAndNormalizeResult(
          auto_ptr<OverlapSite<Left> > left_overlap_site_from_middle_state_site
        , auto_ptr<OverlapSite<Middle> > middle_overlap_site_from_middle_state_site
        , auto_ptr<StateSite<Middle> > middle_state_site_from_right_state_site
        , auto_ptr<OverlapSite<Right> > right_overlap_site_from_right_state_site
    ) : left_overlap_site_from_middle_state_site(left_overlap_site_from_middle_state_site)
      , middle_overlap_site_from_middle_state_site(middle_overlap_site_from_middle_state_site)
      , middle_state_site_from_right_state_site(middle_state_site_from_right_state_site)
      , right_overlap_site_from_right_state_site(right_overlap_site_from_right_state_site)
    {}

    OverlapSitesFromStateSitesAndNormalizeResult(
          OverlapSitesFromStateSitesAndNormalizeResult const& other
    ) : left_overlap_site_from_middle_state_site(other.left_overlap_site_from_middle_state_site)
      , middle_overlap_site_from_middle_state_site(other.middle_overlap_site_from_middle_state_site)
      , middle_state_site_from_right_state_site(other.middle_state_site_from_right_state_site)
      , right_overlap_site_from_right_state_site(other.right_overlap_site_from_right_state_site)
    {}
};
//@+node:gcross.20110131180117.1689: *3* OverlapVectorTrio
struct OverlapVectorTrio {
    OverlapBoundary<Left> const& left_boundary;
    OverlapBoundary<Right> const& right_boundary;
    OverlapSite<Middle> const& middle_site;

    OverlapVectorTrio(
          OverlapBoundary<Left> const& left_boundary
        , OverlapBoundary<Right> const& right_boundary
        , OverlapSite<Middle> const& middle_site
    ) : left_boundary(left_boundary)
      , right_boundary(right_boundary)
      , middle_site(middle_site)
    {}
};
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
void moveSiteCursorLeft(
      StateSite<Left> const& old_state_site_1
    , StateSite<Middle> const& old_state_site_2
    , auto_ptr<StateSite<Middle> const>& new_state_site_1
    , auto_ptr<StateSite<Right> const>& new_state_site_2
);

void moveSiteCursorRight(
      StateSite<Middle> const& old_state_site_1
    , StateSite<Right> const& old_state_site_2
    , auto_ptr<StateSite<Left> const>& new_state_site_1
    , auto_ptr<StateSite<Middle> const>& new_state_site_2
);
//@+node:gcross.20110125120748.2466: *3* Miscellaneous
void increaseDimensionBetweenRightRight(
      unsigned int new_dimension
    , StateSite<Right> const& old_site_1
    , StateSite<Right> const& old_site_2
    , auto_ptr<StateSite<Right> const>& new_site_1
    , auto_ptr<StateSite<Right> const>& new_site_2
);

void increaseDimensionBetweenMiddleRight(
      unsigned int new_dimension
    , StateSite<Middle> const& old_site_1
    , StateSite<Right> const& old_site_2
    , auto_ptr<StateSite<Middle> const>& new_site_1
    , auto_ptr<StateSite<Right> const>& new_site_2
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

OverlapSitesFromStateSitesAndNormalizeResult computeOverlapSitesFromStateSitesAndNormalize(
      StateSite<Middle> const& middle_state_site
     ,StateSite<Right> const& right_state_site
);
//@+node:gcross.20110126102637.2195: *3* Projectors
auto_ptr<StateSite<Middle> const> applyProjectorMatrix(
      ProjectorMatrix const& projector_matrix
    , StateSite<Middle> const& old_state_site
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
      const PhysicalDimension physical_dimension
    , const LeftDimension left_dimension
    , const RightDimension right_dimension
);

auto_ptr<StateSite<Right> const> randomStateSiteRight(
      const PhysicalDimension physical_dimension
    , const LeftDimension left_dimension
    , const RightDimension right_dimension
);
//@+node:gcross.20110131234725.1689: ** Side-agnostic interface
//@+node:gcross.20110131234725.1690: *3* contract
template<Side side> struct contract {};

template<> struct contract<Left> {
    static auto_ptr<ExpectationBoundary<Left> const> SOS(
          ExpectationBoundary<Left> const& old_boundary
        , StateSite<Left> const& state_site
        , OperatorSite const& operator_site
    ) { return contractSOSLeft(old_boundary,state_site,operator_site); }

    static auto_ptr<OverlapBoundary<Left> const> SS(
          OverlapBoundary<Left> const& old_boundary
        , OverlapSite<Left> const& overlap_site
        , StateSite<Left> const& state_site
    ) { return contractSSLeft(old_boundary,overlap_site,state_site); }
};

template<> struct contract<Right> {
    static auto_ptr<ExpectationBoundary<Right> const> SOS(
          ExpectationBoundary<Right> const& old_boundary
        , StateSite<Right> const& state_site
        , OperatorSite const& operator_site
    ) { return contractSOSRight(old_boundary,state_site,operator_site); }

    static auto_ptr<OverlapBoundary<Right> const> SS(
          OverlapBoundary<Right> const& old_boundary
        , OverlapSite<Right> const& overlap_site
        , StateSite<Right> const& state_site
    ) { return contractSSRight(old_boundary,overlap_site,state_site); }
};
//@+node:gcross.20110131234725.1691: *3* moveSiteCursor
template<Side side> struct moveSiteCursor { };

template<> struct moveSiteCursor<Left> {
    static void fromTo(
          StateSite<Middle> const& old_middle_state_site
	, StateSite<Left> const& old_left_state_site
        , auto_ptr<StateSite<Middle> const>& new_middle_state_site
        , auto_ptr<StateSite<Right> const>& new_right_state_site
    ) { return
            moveSiteCursorLeft(
                 old_left_state_site
                ,old_middle_state_site
                ,new_middle_state_site
                ,new_right_state_site
            );
    }
};

template<> struct moveSiteCursor<Right> {
    static void fromTo(
          StateSite<Middle> const& old_middle_state_site
	, StateSite<Right> const& old_right_state_site
        , auto_ptr<StateSite<Middle> const>& new_middle_state_site
        , auto_ptr<StateSite<Left> const>& new_left_state_site
    ) { return
            moveSiteCursorRight(
                 old_middle_state_site
                ,old_right_state_site
                ,new_left_state_site
                ,new_middle_state_site
            );
    }
};
//@-others

}

#endif
//@-leo
