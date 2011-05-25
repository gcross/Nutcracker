//@+leo-ver=5-thin
//@+node:gcross.20110214155808.1922: * @file optimizer.hpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2014: ** << License >>
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
//@+node:gcross.20110429225820.2529: ** << Documentation >>
/*!
\file optimizer.hpp
\brief Classes and functions relating to the optimizer
*/
//@-<< Documentation >>

#ifndef NUTCRACKER_OPTIMIZER_HPP
#define NUTCRACKER_OPTIMIZER_HPP

//@+<< Includes >>
//@+node:gcross.20110214155808.1923: ** << Includes >>
#include <boost/function.hpp>
#include <map>

#include "operators.hpp"
#include "projectors.hpp"
#include "states.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110214155808.1924: ** << Usings >>
using boost::function;

using std::abs;
using std::map;
//@-<< Usings >>

//! \defgroup Optimizer
//! @{

//@+others
//@+node:gcross.20110214155808.1940: ** Exceptions
//@+node:gcross.20110518200233.5038: *3* NoSuchOptimizerModeError
struct NoSuchOptimizerModeError : public Exception {
    string const name;
    NoSuchOptimizerModeError(string const& name);
    virtual ~NoSuchOptimizerModeError() throw() {}
};
//@+node:gcross.20110214155808.1943: *3* OptimizerFailure
//! \defgroup OptimizerFailures Optimizer failures
//! @{

//! Base class for exceptions thrown when the optimizer fails to produce a valid solution.
struct OptimizerFailure : public Exception {
protected:
    //! Constructs the exception with the given message.
    OptimizerFailure(string const& message);
};

//@+others
//@+node:gcross.20110214155808.1951: *4* OptimizerGivenGuessInProjectorSpace
//! Exception thrown when the optimizer was given an initial guess that was in the projector space.
struct OptimizerGivenGuessInProjectorSpace : public OptimizerFailure {
    //! Constructor
    OptimizerGivenGuessInProjectorSpace();
};
//@+node:gcross.20110214155808.1950: *4* OptimizerGivenTooManyProjectors
//! Exception thrown when the optimizer has been given so many projectors that all non-zero solutions are excluded.
struct OptimizerGivenTooManyProjectors : public OptimizerFailure {

    //! The number of projectors
    unsigned int const number_of_projectors;
    //! The physical dimension of the site.
    PhysicalDimension const physical_dimension;
    //! The left dimension of the site.
    LeftDimension const left_dimension;
    //! The right dimension of the site.
    RightDimension const right_dimension;

    //! Constructs this exception given the number of projectors and site dimensions.
    OptimizerGivenTooManyProjectors(
          unsigned int number_of_projectors
        , PhysicalDimension physical_dimension
        , LeftDimension left_dimension
        , RightDimension right_dimension
    );

};
//@+node:gcross.20110214155808.1946: *4* OptimizerObtainedComplexEigenvalue
//! Exception thrown when the solution obtained by the optimizer had a complex eigenvalue.
struct OptimizerObtainedComplexEigenvalue : public OptimizerFailure {

    //! The eigenvalue obtained by the optimizer.
    complex<double> const eigenvalue;

    //! Constructs this exception with the given eigenvalue.
    OptimizerObtainedComplexEigenvalue(complex<double> eigenvalue);

};
//@+node:gcross.20110214155808.1945: *4* OptimizerObtainedEigenvalueDifferentFromExpectationValue
//! Exception thrown when the optimizer obtained a solution whose eigenvalue was different from its expectation value.
struct OptimizerObtainedEigenvalueDifferentFromExpectationValue : public OptimizerFailure {

    complex<double> const
        eigenvalue,     //!< the eigenvalue obtained by the optimizer
        expected_value; //!< the expected value of the solution

    //! Constructs this exception given the eigenvalue obtained by the optimizer and the expected value of the solution
    OptimizerObtainedEigenvalueDifferentFromExpectationValue(
          complex<double> eigenvalue
        , complex<double> expected_value
    );

};
//@+node:gcross.20110214155808.1949: *4* OptimizerObtainedEigenvectorInProjectorSpace
//! Exception thrown when the optimizer has obtained a solution in the projector space.
struct OptimizerObtainedEigenvectorInProjectorSpace : public OptimizerFailure {

    //! The overlap between the solution and the projector space.
    double const overlap;

    //! Constructs this exception with the given overlap between the solution and the projector space.
    OptimizerObtainedEigenvectorInProjectorSpace(double overlap);

};
//@+node:gcross.20110214155808.1947: *4* OptimizerObtainedRegressiveEigenvalue
//! Exception thrown when the optimizer has obtained a new solution with an eigenvalue that has regressed from the old solution.
struct OptimizerObtainedRegressiveEigenvalue : public OptimizerFailure {

    double const
        old_eigenvalue, //!< The eigenvalue of the old solution.
        new_eigenvalue; //!< The eigenvalue of the new solution.

    //! Construct this exception with the eigenvalues of the old and new solutions.
    OptimizerObtainedRegressiveEigenvalue(
          double const old_eigenvalue
        , double const new_eigenvalue
    );

};
//@+node:gcross.20110214155808.1948: *4* OptimizerObtainedVanishingEigenvector
//! Exception thrown when the optimizer has obtained a vanishing solution.
struct OptimizerObtainedVanishingEigenvector : public OptimizerFailure {

    //! The norm of the obtained solution.
    double const norm;

    //! Construct this exception given the norm of the obtained solution.
    OptimizerObtainedVanishingEigenvector(double norm);

};
//@+node:gcross.20110214155808.1944: *4* OptimizerUnableToConverge
//! Exception thrown when the optimizer was unable converge to a solution within the specified number of iterations.
struct OptimizerUnableToConverge : public OptimizerFailure {

    //! The number of iterations taken by the optimizer.
    unsigned int const number_of_iterations;

    //! Constructs this exception given the number of iterations taken by the optimizer
    OptimizerUnableToConverge(unsigned int number_of_iterations);

};
//@+node:gcross.20110214155808.1952: *4* OptimizerUnknownFailure
//! Optimizer failed for an unknown reason.
struct OptimizerUnknownFailure : public OptimizerFailure {

    //! The error code returned by ARPACK.
    int const error_code;

    //! Constructs this exception with the error code returned by ARPACK.
    OptimizerUnknownFailure(int error_code);
};
//@-others

//! @}
//@+node:gcross.20110214155808.1981: ** Classes
//@+node:gcross.20110517202745.2516: *3* OptimizerMode
class OptimizerMode {

public:
    typedef function<bool (double,double,double)> RegressionChecker;
    typedef map<string,OptimizerMode> OptimizerModeRegistry;

protected:
    char const* name;
    char const* which;
    char const* description;
    RegressionChecker regression_checker;

    OptimizerMode(
        char const* name
      , char const* which
      , char const* description
      , RegressionChecker regression_checker
    );

public:
    OptimizerMode() {}

    bool operator ==(OptimizerMode const& other) const;

    char const* getDescription() const;
    char const* getName() const;
    char const* getWhich() const;

    bool checkForRegressionFromTo(double old_value, double new_value, double tolerance) const;

    static map<string,OptimizerMode> const& getRegistry();
    static OptimizerMode const& lookupName(string const& name);
    static vector<string> listNames();

    static OptimizerMode least_value, greatest_value, largest_magnitude;

};
//@+node:gcross.20110214155808.1982: *3* OptimizerResult
//@+<< Description >>
//@+node:gcross.20110429225820.2521: *4* << Description >>
//! The result of optimizing a site.
/*! \note This class is moveable but not copyable, and uses Boost.Move to implement these semantics. */
//@-<< Description >>
struct OptimizerResult {
    //@+others
    //@+node:gcross.20110429225820.2523: *4* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(OptimizerResult)
    //@+node:gcross.20110429225820.2522: *4* Constructors
    //! \name Constructors
    //! @{

    public:

    //! Move constructor.
    OptimizerResult(BOOST_RV_REF(OptimizerResult) other)
      : number_of_iterations(other.number_of_iterations)
      , eigenvalue(other.eigenvalue)
      , state_site(boost::move(other.state_site))
    {}

    //! Create a new instance given the number of iterations, eigenvalue, and solution returned by the optimizer.
    /*!
    \note The solution is moved, not copied, into this class.
    */
    OptimizerResult(
          unsigned int const number_of_iterations
        , double const eigenvalue
        , BOOST_RV_REF(StateSite<Middle>) state_site
    ) : number_of_iterations(number_of_iterations)
      , eigenvalue(eigenvalue)
      , state_site(state_site)
    {}

    //! @}
    //@+node:gcross.20110429225820.2524: *4* Fields
    public:

    //! The number of iterations taken by the optimizer.
    unsigned int number_of_iterations;

    //! The eigenvalue returned by the optimizer.
    double eigenvalue;

    //! The solution obtained by the optimizer.
    StateSite<Middle> state_site;
    //@-others
};
//@+node:gcross.20110214155808.1926: ** Functions
//@+node:gcross.20110518200233.5050: *3* checkFor
bool checkForLargestMagnitudeRegressionFromTo(double from, double to, double tolerance);
bool checkForLeastValueRegressionFromTo(double from, double to, double tolerance);
bool checkForGreatestValueRegressionFromTo(double from, double to, double tolerance);
//@+node:gcross.20110518200233.5051: *3* optimizeStateSite
//! Optimizes a site and returns the result.
/*!
\param left_boundary the left boundary of the site environment
\param current_state_site the current state site tensor
\param operator_site the operator site tensor
\param right_boundary the right boundary of the site environment
\param projector_matrix the projector matrix
\param convergence_threshold the threshold to use to determine when the eigenvalue has converged
\param sanity_check_threshold the threshold to use when performing sanity checks
\param maximum_number_of_iterations the maximum number of iterations to allow the optimizer to take
*/
Nutcracker::OptimizerResult optimizeStateSite(
      Nutcracker::ExpectationBoundary<Left> const& left_boundary
    , Nutcracker::StateSite<Middle> const& current_state_site
    , Nutcracker::OperatorSite const& operator_site
    , Nutcracker::ExpectationBoundary<Right> const& right_boundary
    , Nutcracker::ProjectorMatrix const& projector_matrix
    , double const convergence_threshold
    , double const sanity_check_threshold
    , unsigned int const maximum_number_of_iterations
    , OptimizerMode const& optimizer_mode = OptimizerMode::least_value
);
//@-others

//! @}

}

//@+<< Outside namespace >>
//@+node:gcross.20110524225044.2433: ** << Outside namespace >>
//@+others
//@+node:gcross.20110524225044.2436: *3* std
namespace std {

istream& operator >>(istream& in, Nutcracker::OptimizerMode& mode);
ostream& operator <<(ostream& out, Nutcracker::OptimizerMode const& mode);

}
//@-others
//@-<< Outside namespace >>

#endif
//@-leo
