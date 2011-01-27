//@+leo-ver=5-thin
//@+node:gcross.20110127123226.2504: * @thin core.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110127123226.2505: ** << Includes >>
#include <illuminate.hpp>

#include "core.hpp"

using namespace Nutcracker;
//@-<< Includes >>

//@+others
//@+node:gcross.20110127123226.2506: ** Tests
TEST_SUITE(Core) {

//@+others
//@+node:gcross.20110127123226.2507: *3* computeExpectationValue
TEST_SUITE(computeExpectationValue) {

    //@+others
    //@+node:gcross.20110127123226.2518: *4* trivial, all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        complex<double> expected_expectation_value = 1;
        complex<double> actual_expectation_value =
            computeExpectationValue(
                 ExpectationBoundary<Left>::trivial
                ,StateSite<Middle>::trivial
                ,OperatorSite::trivial
                ,ExpectationBoundary<Right>::trivial
            );
        ASSERT_EQ(expected_expectation_value,actual_expectation_value);
    }
    //@-others

}
//@-others

}
//@-others
//@-leo
