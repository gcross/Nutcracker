/*!
\file optimizer.cpp
\brief Classes and functions relating to the optimizer
*/

#include <boost/algorithm/string/case_conv.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/range/adaptor/map.hpp>
#include <boost/range/algorithm/copy.hpp>
#include <cstring>

#include "nutcracker/boundaries.hpp"
#include "nutcracker/core.hpp"
#include "nutcracker/optimizer.hpp"
#include "nutcracker/projectors.hpp"

namespace lam = boost::lambda;

using boost::adaptors::map_keys;
using boost::copy;
using boost::to_lower;

using std::istream;
using std::ostream;
using std::string;

namespace Nutcracker {

OptimizerFailure::OptimizerFailure(string const& message)
    : std::runtime_error(message)
{ }
OptimizerGivenGuessInProjectorSpace::OptimizerGivenGuessInProjectorSpace()
    : OptimizerFailure(
        "Optimizer was given a guess within the forbidden orthogonal space"
      )
{ }
OptimizerGivenTooManyProjectors::OptimizerGivenTooManyProjectors(
      unsigned int const number_of_projectors
    , unsigned int const physical_dimension
    , unsigned int const left_dimension
    , unsigned int const right_dimension
) : OptimizerFailure(
        (format("Optimizer was given too many projectors (%1% >= %2%*%3%*%4% = %5%)") 
            % number_of_projectors
            % physical_dimension
            % left_dimension
            % right_dimension
            % (physical_dimension*left_dimension*right_dimension)
        ).str()
    )
  , number_of_projectors(number_of_projectors)
  , physical_dimension(physical_dimension)
  , left_dimension(left_dimension)
  , right_dimension(right_dimension)
{ }
OptimizerObtainedComplexEigenvalue::OptimizerObtainedComplexEigenvalue(
       complex<double> const eigenvalue
) : OptimizerFailure(
        (format("Optimizer obtained complex eigenvalue (%|.15|)") 
            % eigenvalue
        ).str()
    )
  , eigenvalue(eigenvalue)
{ }
OptimizerObtainedEigenvalueDifferentFromExpectationValue::OptimizerObtainedEigenvalueDifferentFromExpectationValue(
      complex<double> const eigenvalue
    , complex<double> const expected_value
) : OptimizerFailure(
        (format("Optimizer obtained eigenvalue that was different from the final expected value (%|.15| != %|.15|)")
            % eigenvalue
            % expected_value
        ).str()
    )
  , eigenvalue(eigenvalue)
  , expected_value(expected_value)
{ }
OptimizerObtainedEigenvectorInProjectorSpace::OptimizerObtainedEigenvectorInProjectorSpace(
       double const overlap
) : OptimizerFailure(
        (format("Optimizer obtained eigenvector overlapping with the forbidden orthogonal space (overlap = %|.15|)") 
            % overlap
        ).str()
    )
  , overlap(overlap)
{ }
OptimizerObtainedRegressiveEigenvalue::OptimizerObtainedRegressiveEigenvalue(
      double const old_eigenvalue
    , double const new_eigenvalue
) : OptimizerFailure(
        (format("Optimizer obtained an eigenvalue that has regressed from the old eigenvalue (%|.15| vs %|.15|)")
            % new_eigenvalue
            % old_eigenvalue
        ).str()
    )
  , old_eigenvalue(old_eigenvalue)
  , new_eigenvalue(new_eigenvalue)
{ }
OptimizerObtainedVanishingEigenvector::OptimizerObtainedVanishingEigenvector(
       double const norm
) : OptimizerFailure(
        (format("Optimizer obtained vanishing eigenvector (norm = %|.15|)") 
            % norm
        ).str()
    )
  , norm(norm)
{ }
OptimizerUnableToConverge::OptimizerUnableToConverge(
       unsigned int const number_of_iterations
) : OptimizerFailure(
        (format("Optimizer failed to converge after %1% iterations") 
            % number_of_iterations
        ).str()
    )
  , number_of_iterations(number_of_iterations)
{ }
OptimizerUnknownFailure::OptimizerUnknownFailure(
       int const error_code
) : OptimizerFailure(
        (format("Optimizer failed with an unknown error code: %1%") 
            % error_code
        ).str()
    )
  , error_code(error_code)
{ }
NoSuchOptimizerModeError::NoSuchOptimizerModeError(string const& name)
  : std::logic_error((format("There is no such optimizer mode '%1%'.") % name).str())
  , name(name)
{}
OptimizerMode::OptimizerMode(
    char const* name
  , char const* which
  , char const* description
  , RegressionChecker regression_checker
)
  : name(name)
  , which(which)
  , description(description)
  , regression_checker(regression_checker)
{}
char const* OptimizerMode::getDescription() const { return description; }
char const* OptimizerMode::getName() const { return name; }
char const* OptimizerMode::getWhich() const { return which; }
bool OptimizerMode::checkForRegressionFromTo(double old_value, double new_value, double tolerance) const {
    return regression_checker(old_value,new_value,tolerance);
}

bool OptimizerMode::operator ==(OptimizerMode const& other) const {
    return strcmp(name,other.getName()) == 0;
}
OptimizerMode::OptimizerModeRegistry const& OptimizerMode::getRegistry() {
    static OptimizerModeRegistry modes;
    if(modes.empty()) {
        modes[least_value.name] = least_value;
        modes[greatest_value.name] = greatest_value;
        modes[largest_magnitude.name] = largest_magnitude;
    }
    return modes;
}

vector<string> OptimizerMode::listNames() {
    vector<string> names;
    copy(getRegistry() | map_keys,back_inserter(names));
    return boost::move(names);
}

OptimizerMode const& OptimizerMode::lookupName(string const& name) {
    OptimizerModeRegistry const& registry = getRegistry();
    OptimizerModeRegistry::const_iterator iter = registry.find(name);
    if(iter == registry.end()) throw NoSuchOptimizerModeError(name);
    else return iter->second;
}
OptimizerMode
    OptimizerMode::least_value(
        "<v","SR",
        "least value",
        checkForLeastValueRegressionFromTo
    )
  , OptimizerMode::greatest_value(
        ">v","LR",
        "greatest value",
        checkForGreatestValueRegressionFromTo
    )
  , OptimizerMode::largest_magnitude(
        ">m","LM",
        "largest magnitude",
        checkForLargestMagnitudeRegressionFromTo
    )
  ;
bool checkForLargestMagnitudeRegressionFromTo(double from, double to, double tolerance) {
    return abs(to) < abs(from) && outsideTolerance(abs(from),abs(to),tolerance);
}

bool checkForLeastValueRegressionFromTo(double from, double to, double tolerance) {
    return to > from && outsideTolerance(from,to,tolerance);
}

bool checkForGreatestValueRegressionFromTo(double from, double to, double tolerance) {
    return to < from && outsideTolerance(from,to,tolerance);
}
OptimizerResult optimizeStateSite(
      ExpectationBoundary<Left> const& left_boundary
    , StateSite<Middle> const& current_state_site
    , OperatorSite const& operator_site
    , ExpectationBoundary<Right> const& right_boundary
    , ProjectorMatrix const& projector_matrix
    , double const convergence_threshold
    , double const sanity_check_threshold
    , unsigned int const maximum_number_of_iterations
    , OptimizerMode const& optimizer_mode
) {
    uint32_t number_of_iterations = maximum_number_of_iterations;
    complex<double> eigenvalue;
    StateSite<Middle> new_state_site(dimensionsOf(current_state_site));

    double normal;
    int const status =
        projector_matrix.valid()
            ? Core::optimize(
                 left_boundary | current_state_site
                ,current_state_site | right_boundary
                ,left_boundary | operator_site
                ,operator_site | right_boundary
                ,operator_site | current_state_site
                ,left_boundary
                ,operator_site.numberOfMatrices(),operator_site,operator_site
                ,right_boundary
                ,projector_matrix.numberOfProjectors()
                ,projector_matrix.numberOfReflectors()
                ,projector_matrix.orthogonalSubspaceDimension()
                ,projector_matrix.reflectorData()
                ,projector_matrix.coefficientData()
                ,projector_matrix.swapData()
                ,optimizer_mode.getWhich()
                ,convergence_threshold
                ,number_of_iterations
                ,current_state_site
                ,new_state_site
                ,eigenvalue
                ,normal
              )
            : Core::optimize(
                 left_boundary | current_state_site
                ,current_state_site | right_boundary
                ,left_boundary | operator_site
                ,operator_site | right_boundary
                ,operator_site | current_state_site
                ,left_boundary
                ,operator_site.numberOfMatrices(),operator_site,operator_site
                ,right_boundary
                ,0
                ,0
                ,current_state_site.size()
                ,NULL
                ,NULL
                ,NULL
                ,optimizer_mode.getWhich()
                ,convergence_threshold
                ,number_of_iterations
                ,current_state_site
                ,new_state_site
                ,eigenvalue
                ,normal
              )
            ;
    complex<double> const expectation_value =
        computeExpectationValueAtSite(
             left_boundary
            ,new_state_site
            ,operator_site
            ,right_boundary
        );
    double const overlap =
        projector_matrix.valid()
            ? computeOverlapWithProjectors(
                 projector_matrix
                ,new_state_site
              )
            : 0
            ;
    switch(status) {
        case -14:
            throw OptimizerUnableToConverge(number_of_iterations);
        case  10:
            throw OptimizerGivenTooManyProjectors(
                 projector_matrix.numberOfProjectors()
                ,current_state_site.physicalDimension()
                ,current_state_site.leftDimension()
                ,current_state_site.rightDimension()
            );
        case 11:
            throw OptimizerGivenGuessInProjectorSpace();
        case 0:
            if(outsideTolerance(eigenvalue,expectation_value,sanity_check_threshold))
                throw OptimizerObtainedEigenvalueDifferentFromExpectationValue(
                     eigenvalue
                    ,expectation_value
                );
            if(abs(eigenvalue) > sanity_check_threshold
            && abs(eigenvalue.imag())/abs(eigenvalue) > sanity_check_threshold
              ) throw OptimizerObtainedComplexEigenvalue(eigenvalue);
            if(normal < 1-sanity_check_threshold)
                throw OptimizerObtainedVanishingEigenvector(normal);
            if(overlap > sanity_check_threshold)
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

}

namespace std {

istream& operator >>(istream& in, Nutcracker::OptimizerMode& mode) {
    string name;
    in >> name;
    boost::to_lower(name);
    mode = Nutcracker::OptimizerMode::lookupName(name);
    return in;
}

ostream& operator <<(ostream& out, Nutcracker::OptimizerMode const& mode) {
    return out << mode.getName();
}

}
