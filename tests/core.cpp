//@+leo-ver=5-thin
//@+node:gcross.20110127123226.2504: * @thin core.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110127123226.2505: ** << Includes >>
#include <boost/range/algorithm/equal.hpp>
#include <illuminate.hpp>

#include "core.hpp"

using namespace boost;
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
    //@+node:gcross.20110127123226.2819: *4* trivial, all dimensions 1, imaginary
    TEST_CASE(trivial_with_all_dimensions_1_and_imaginary_component) {
        complex<double> expected_expectation_value(0,1);
        complex<double> actual_expectation_value =
            computeExpectationValue(
                 ExpectationBoundary<Left>::trivial
                ,StateSite<Middle>::trivial
                ,OperatorSite(1,1,list_of(1)(1),list_of(complex<double>(0,1)))
                ,ExpectationBoundary<Right>::trivial
            );
        ASSERT_EQ(expected_expectation_value,actual_expectation_value);
    }
    //@+node:gcross.20110127123226.2821: *4* trivial, d = 2
    TEST_SUITE(trivial_with_physical_dimension_2) {

        void runTest(
              complex<double> const a
            , complex<double> const b
            , complex<double> const c
        ) {
            complex<double> expectation_value =
                computeExpectationValue(
                     ExpectationBoundary<Left>::trivial
                    ,StateSite<Middle>(1,1,list_of(a)(b))
                    ,OperatorSite(1,1,list_of(1)(1),list_of(1)(0)(0)(-1))
                    ,ExpectationBoundary<Right>::trivial
                );
            ASSERT_EQ(c,expectation_value);
        }

        TEST_CASE(_1_0) { runTest(1,0,1); }
        TEST_CASE(_i_0) { runTest(complex<double>(0,1),0,1); }
        TEST_CASE(_0_1) { runTest(0,1,-1); }
        TEST_CASE(_0_i) { runTest(0,complex<double>(0,1),-1); }
    }
    //@-others

}
//@+node:gcross.20110127123226.2822: *3* computeSOSLeft
TEST_SUITE(computeSOSLeft) {

    //@+others
    //@+node:gcross.20110127123226.2823: *4* trivial with all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        auto_ptr<ExpectationBoundary<Left> const> new_boundary(
            contractSOSLeft(
                 ExpectationBoundary<Left>::trivial
                ,StateSite<Left>::trivial
                ,OperatorSite::trivial
            )
        );
        ASSERT_EQ(1,new_boundary->state_dimension);
        ASSERT_EQ(1,new_boundary->operator_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(1)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2827: *4* trivial, c = 2
    TEST_CASE(trivial_with_operator_dimension_2) {
        auto_ptr<ExpectationBoundary<Left> const> new_boundary(
            contractSOSLeft(
                 ExpectationBoundary<Left>::trivial
                ,StateSite<Left>::trivial
                ,OperatorSite(1,2,list_of(1)(2),list_of(complex<double>(0,1)))
            )
        );
        ASSERT_EQ(1,new_boundary->state_dimension);
        ASSERT_EQ(2,new_boundary->operator_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(0))(complex<double>(0,1)),*new_boundary));
    }
    //@-others

}
//@+node:gcross.20110127123226.2831: *3* computeSOSRight
TEST_SUITE(computeSOSRight) {

    //@+others
    //@+node:gcross.20110127123226.2832: *4* trivial with all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        auto_ptr<ExpectationBoundary<Right> const> new_boundary(
            contractSOSRight(
                 ExpectationBoundary<Right>::trivial
                ,StateSite<Right>::trivial
                ,OperatorSite::trivial
            )
        );
        ASSERT_EQ(1,new_boundary->state_dimension);
        ASSERT_EQ(1,new_boundary->operator_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(1)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2833: *4* trivial, c = 2
    TEST_CASE(trivial_with_operator_dimension_2) {
        auto_ptr<ExpectationBoundary<Right> const> new_boundary(
            contractSOSRight(
                 ExpectationBoundary<Right>::trivial
                ,StateSite<Right>::trivial
                ,OperatorSite(2,1,list_of(2)(1),list_of(complex<double>(0,1)))
            )
        );
        ASSERT_EQ(1,new_boundary->state_dimension);
        ASSERT_EQ(2,new_boundary->operator_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(0))(complex<double>(0,1)),*new_boundary));
    }
    //@-others

}
//@-others

}
//@-others
//@-leo
