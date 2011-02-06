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
//@+node:gcross.20110201193729.1694: *3* MoveSiteCursorResult
template<Side side> class MoveSiteCursorResult {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(MoveSiteCursorResult)
public:
    typedef Other<side> other;

    StateSite<Middle> middle_state_site;
    StateSite<other::side> other_side_state_site;

    MoveSiteCursorResult(BOOST_RV_REF(MoveSiteCursorResult) other)
      : middle_state_site(boost::move(other.middle_state_site))
      , other_side_state_site(boost::move(other.other_side_state_site))
    {}

    MoveSiteCursorResult(
          BOOST_RV_REF(StateSite<Middle>) middle_state_site
        , BOOST_RV_REF(StateSite<other::side>) other_side_state_site
    ) : middle_state_site(middle_state_site)
      , other_side_state_site(other_side_state_site)
    {}
};
//@+node:gcross.20110129220506.1657: *3* OptimizerResult
struct OptimizerResult {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(OptimizerResult)
public:
    unsigned int number_of_iterations;
    double eigenvalue;
    StateSite<Middle> state_site;

    OptimizerResult(BOOST_RV_REF(OptimizerResult) other)
      : number_of_iterations(other.number_of_iterations)
      , eigenvalue(other.eigenvalue)
      , state_site(boost::move(other.state_site))
    {}

    OptimizerResult(
          unsigned int const number_of_iterations
        , double const eigenvalue
        , BOOST_RV_REF(StateSite<Middle>) state_site
    ) : number_of_iterations(number_of_iterations)
      , eigenvalue(eigenvalue)
      , state_site(state_site)
    {}
};
//@+node:gcross.20110131180117.1687: *3* OverlapSitesFromStateSitesAndNormalizeResult
class OverlapSitesFromStateSitesAndNormalizeResult {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(OverlapSitesFromStateSitesAndNormalizeResult)
public:
    OverlapSite<Left> left_overlap_site_from_middle_state_site;
    OverlapSite<Middle> middle_overlap_site_from_middle_state_site;
    StateSite<Middle> middle_state_site_from_right_state_site;
    OverlapSite<Right> right_overlap_site_from_right_state_site;

    OverlapSitesFromStateSitesAndNormalizeResult(
          BOOST_RV_REF(OverlapSitesFromStateSitesAndNormalizeResult) other
    ) : left_overlap_site_from_middle_state_site(boost::move(other.left_overlap_site_from_middle_state_site))
      , middle_overlap_site_from_middle_state_site(boost::move(other.middle_overlap_site_from_middle_state_site))
      , middle_state_site_from_right_state_site(boost::move(other.middle_state_site_from_right_state_site))
      , right_overlap_site_from_right_state_site(boost::move(other.right_overlap_site_from_right_state_site))
    {}

    OverlapSitesFromStateSitesAndNormalizeResult(
          BOOST_RV_REF(OverlapSite<Left>) left_overlap_site_from_middle_state_site
        , BOOST_RV_REF(OverlapSite<Middle>) middle_overlap_site_from_middle_state_site
        , BOOST_RV_REF(StateSite<Middle>) middle_state_site_from_right_state_site
        , BOOST_RV_REF(OverlapSite<Right>) right_overlap_site_from_right_state_site
    ) : left_overlap_site_from_middle_state_site(left_overlap_site_from_middle_state_site)
      , middle_overlap_site_from_middle_state_site(middle_overlap_site_from_middle_state_site)
      , middle_state_site_from_right_state_site(middle_state_site_from_right_state_site)
      , right_overlap_site_from_right_state_site(right_overlap_site_from_right_state_site)
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
//@+node:gcross.20110202223558.1716: *3* NotEnoughDegreesOfFreedomToNormalizeError
struct NotEnoughDegreesOfFreedomToNormalizeError : public Exception {
    string n1, n2, n3;
    unsigned int d1, d2, d3;
    NotEnoughDegreesOfFreedomToNormalizeError(
         string const& n1
        ,unsigned int const d1
        ,string const& n2
        ,unsigned int const d2
        ,string const& n3
        ,unsigned int const d3
    ) : Exception((
            format("Not enough degrees of freedom to normalize (%1% (%2%) >= %3% (%4%) * %5% (%6%))")
                % n1
                % d1
                % n2
                % d2
                % n3
                % d3
        ).str())
      , n1(n1)
      , n2(n2)
      , n3(n3)
      , d1(d1)
      , d2(d2)
      , d3(d3)
    { }
    virtual ~NotEnoughDegreesOfFreedomToNormalizeError() throw() {}
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
//@+node:gcross.20110206130502.1756: *4* OptimizerObtainedGreaterEigenvalue
struct OptimizerObtainedGreaterEigenvalue : public OptimizerFailure {
    double const old_eigenvalue, new_eigenvalue;
    OptimizerObtainedGreaterEigenvalue(
          double const old_eigenvalue
        , double const new_eigenvalue
    );
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

ExpectationBoundary<Left> contractSOSLeft(
      ExpectationBoundary<Left> const& old_boundary
    , StateSite<Left> const& state_site
    , OperatorSite const& operator_site
);

ExpectationBoundary<Right> contractSOSRight(
      ExpectationBoundary<Right> const& old_boundary
    , StateSite<Right> const& state_site
    , OperatorSite const& operator_site
);

OverlapBoundary<Left> contractSSLeft(
      OverlapBoundary<Left> const& old_boundary
    , OverlapSite<Left> const& overlap_site
    , StateSite<Left> const& state_site
);

OverlapBoundary<Right> contractSSRight(
      OverlapBoundary<Right> const& old_boundary
    , OverlapSite<Right> const& overlap_site
    , StateSite<Right> const& state_site
);
//@+node:gcross.20110125120748.2465: *3* Cursor movement
MoveSiteCursorResult<Left> moveSiteCursorLeft(
      StateSite<Middle> const& old_state_site_2
    , StateSite<Left> const& old_state_site_1
);

MoveSiteCursorResult<Right> moveSiteCursorRight(
      StateSite<Middle> const& old_state_site_1
    , StateSite<Right> const& old_state_site_2
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
    , double const tolerance
    , unsigned int const maximum_number_of_iterations
);
//@+node:gcross.20110126102637.2193: *3* Overlap tensor formation
OverlapSite<Middle> computeOverlapSiteFromStateSite(StateSite<Middle> const& state_site);

OverlapSitesFromStateSitesAndNormalizeResult computeOverlapSitesFromStateSitesAndNormalize(
      StateSite<Middle> const& middle_state_site
     ,StateSite<Right> const& right_state_site
);
//@+node:gcross.20110126102637.2195: *3* Projectors
StateSite<Middle> applyProjectorMatrix(
      ProjectorMatrix const& projector_matrix
    , StateSite<Middle> const& old_state_site
);

double computeOverlapWithProjectors(
     ProjectorMatrix const& projector_matrix
    ,StateSite<Middle> const& state_site
);

ProjectorMatrix formProjectorMatrix(
    vector<OverlapVectorTrio> const& overlaps
);

ProjectorMatrix randomProjectorMatrix(
     unsigned int const vector_length
    ,unsigned int const number_of_projectors
);
//@+node:gcross.20110125120748.2464: *3* Randomizers
StateSite<Middle> randomStateSiteMiddle(
      const PhysicalDimension physical_dimension
    , const LeftDimension left_dimension
    , const RightDimension right_dimension
);

StateSite<Right> randomStateSiteRight(
      const PhysicalDimension physical_dimension
    , const LeftDimension left_dimension
    , const RightDimension right_dimension
);
//@+node:gcross.20110131234725.1689: ** Side-agnostic interface
//@+node:gcross.20110131234725.1690: *3* contract
template<Side side> struct contract {};

template<> struct contract<Left> {
    static ExpectationBoundary<Left> SOS(
          ExpectationBoundary<Left> const& old_boundary
        , StateSite<Left> const& state_site
        , OperatorSite const& operator_site
    ) { return contractSOSLeft(old_boundary,state_site,operator_site); }

    static OverlapBoundary<Left> SS(
          OverlapBoundary<Left> const& old_boundary
        , OverlapSite<Left> const& overlap_site
        , StateSite<Left> const& state_site
    ) { return contractSSLeft(old_boundary,overlap_site,state_site); }
};

template<> struct contract<Right> {
    static ExpectationBoundary<Right> SOS(
          ExpectationBoundary<Right> const& old_boundary
        , StateSite<Right> const& state_site
        , OperatorSite const& operator_site
    ) { return contractSOSRight(old_boundary,state_site,operator_site); }

    static OverlapBoundary<Right> SS(
          OverlapBoundary<Right> const& old_boundary
        , OverlapSite<Right> const& overlap_site
        , StateSite<Right> const& state_site
    ) { return contractSSRight(old_boundary,overlap_site,state_site); }
};
//@+node:gcross.20110131234725.1691: *3* moveSiteCursor
template<Side side> struct moveSiteCursor { };

template<> struct moveSiteCursor<Left> {
    static MoveSiteCursorResult<Left> from(
          StateSite<Middle> const& old_middle_state_site
	   , StateSite<Left> const& old_left_state_site
    ) { return moveSiteCursorLeft(old_middle_state_site,old_left_state_site); }
};

template<> struct moveSiteCursor<Right> {
    static MoveSiteCursorResult<Right> from(
          StateSite<Middle> const& old_middle_state_site
	   , StateSite<Right> const& old_right_state_site
    ) { return moveSiteCursorRight(old_middle_state_site,old_right_state_site); }
};
//@-others

}

#endif
//@-leo
