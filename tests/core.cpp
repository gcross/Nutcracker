//@+leo-ver=5-thin
//@+node:gcross.20110127123226.2504: * @thin core.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110127123226.2505: ** << Includes >>
#include <boost/foreach.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <illuminate.hpp>
#include <iostream>

#include "core.hpp"

using namespace boost;
using namespace Nutcracker;
using namespace std;
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
        OperatorSite const operator_site
            (LeftDimension(1)
            ,RightDimension(1)
            ,fillWithRange(list_of(1)(1))
            ,fillWithRange(list_of(complex<double>(0,1)))
            );

        complex<double> expected_expectation_value(0,1);
        complex<double> actual_expectation_value =
            computeExpectationValue(
                 ExpectationBoundary<Left>::trivial
                ,StateSite<Middle>::trivial
                ,operator_site
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
            StateSite<Middle> const state_site
                (LeftDimension(1)
                ,RightDimension(1)
                ,fillWithRange(list_of(a)(b))
                );
            OperatorSite const operator_site
                (LeftDimension(1)
                ,RightDimension(1)
                ,fillWithRange(list_of(1)(1))
                ,fillWithRange(list_of(1)(0)(0)(-1))
                );
            complex<double> expectation_value =
                computeExpectationValue(
                     ExpectationBoundary<Left>::trivial
                    ,state_site
                    ,operator_site
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
//@+node:gcross.20110128131637.1649: *3* contractExpectationBoundaries
TEST_SUITE(contractExpectationBoundaries) {

    //@+others
    //@+node:gcross.20110128131637.1650: *4* trivial, all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        complex<double> expected_value = 1;
        complex<double> actual_value =
            contractExpectationBoundaries(
                 ExpectationBoundary<Left>::trivial
                ,ExpectationBoundary<Right>::trivial
            );
        ASSERT_EQ(expected_value,actual_value);
    }
    //@+node:gcross.20110128131637.1654: *4* non-trivial
    TEST_CASE(non_trivial) {

        ExpectationBoundary<Left> const left_boundary
            (OperatorDimension(2)
            ,fillWithRange(list_of
                (c(1,2))(c(2,3))(c(3,4))
                (c(4,5))(c(5,6))(c(6,7))
                (c(7,8))(c(8,9))(c(9,10))

                (c(1,9))(c(2,8))(c(3,7))
                (c(4,6))(c(5,5))(c(6,4))
                (c(7,3))(c(8,2))(c(9,1))
            ));

        ExpectationBoundary<Right> const right_boundary
            (OperatorDimension(2)
            ,fillWithRange(list_of
                (c(1,1))(c(2,2))(c(3,3))
                (c(6,6))(c(5,5))(c(4,4))
                (c(7,7))(c(8,8))(c(9,9))

                (c(1,-1))(c(2,-2))(c(3,-3))
                (c(6,-6))(c(5,-5))(c(4,-4))
                (c(7,-7))(c(8,-8))(c(9,-9))
            ));

        complex<double> expected_value = c(405,495);
        complex<double> actual_value =
            contractExpectationBoundaries(
                 left_boundary
                ,right_boundary
            );
        ASSERT_EQ(expected_value,actual_value);
    }
    //@-others

}
//@+node:gcross.20110127123226.2822: *3* contractSOSLeft
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
        ASSERT_EQ(OperatorDimension(1),new_boundary->operator_dimension);
        ASSERT_EQ(StateDimension(1),new_boundary->state_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(1)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2827: *4* trivial, c = 2
    TEST_CASE(trivial_with_operator_dimension_2) {
        OperatorSite const operator_site
            (LeftDimension(1)
            ,RightDimension(2)
            ,fillWithRange(list_of(1)(2))
            ,fillWithRange(list_of(complex<double>(0,1)))
            );
        auto_ptr<ExpectationBoundary<Left> const> new_boundary(
            contractSOSLeft(
                 ExpectationBoundary<Left>::trivial
                ,StateSite<Left>::trivial
                ,operator_site
            )
        );
        ASSERT_EQ(OperatorDimension(2),new_boundary->operator_dimension);
        ASSERT_EQ(StateDimension(1),new_boundary->state_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(0))(complex<double>(0,1)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2875: *4* non-trivial
    TEST_CASE(non_trivial) {
        ExpectationBoundary<Left> const boundary
            (OperatorDimension(3)
            ,fillWithRange(list_of
                (c(2,0))(c(0,4))
                (c(1,0))(c(0,5))

                (c(1,0))(c(0,5))
                (c(1,0))(c(1,0))

                (c(3,0))(c(0,6))
                (c(0,2))(c(-2,0))
            ));
        StateSite<Left> const state_site
            (LeftDimension(2)
            ,RightDimension(4)
            ,fillWithRange(list_of
                (c(1,0))(c(1,0))(c(-1,0))(c(0,2))
                (c(1,0))(c(-1,0))(c(2,0))(c(-2,0))
            ));
        OperatorSite const operator_site
            (LeftDimension(3)
            ,RightDimension(2)
            ,fillWithRange(list_of(1)(2)(3)(1))
            ,fillWithRange(list_of(complex<double>(1,0))(complex<double>(0,1)))
            );
        auto_ptr<ExpectationBoundary<Left> const> actual_boundary(
            contractSOSLeft(
                 boundary
                ,state_site
                ,operator_site
            )
        );
        ASSERT_EQ(OperatorDimension(2),actual_boundary->operator_dimension);
        ASSERT_EQ(StateDimension(4),actual_boundary->state_dimension);
        complex<double> const expected_boundary[] =
            {c(-8.0,1.0),c(4.0,5.0),c(-10.0,-7.0),c(6.0,0.0)
            ,c(-4.0,5.0),c(8.0,1.0),c(-14.0,1.0),c(6.0,0.0)
            ,c(2.0,-7.0),c(-10.0,1.0),c(16.0,-5.0),c(-6.0,0.0)
            ,c(10.0,16.0),c(10.0,-16.0),c(-10.0,32.0),c(0.0,-12.0)

            ,c(3.0,9.0),c(3.0,-9.0),c(-3.0,18.0),c(0.0,-12.0)
            ,c(1.0,-1.0),c(1.0,1.0),c(-1.0,-2.0),c(0.0,4.0)
            ,c(0.0,6.0),c(0.0,-6.0),c(0.0,12.0),c(0.0,-12.0)
            ,c(6.0,-14.0),c(-10.0,6.0),c(18.0,-16.0),c(-8.0,16.0)
            };
        complex<double> const *actual = *actual_boundary;
        BOOST_FOREACH(complex<double> expected, expected_boundary) {
            ASSERT_EQ(expected,*actual);
            ++actual;
        }
    }
    //@-others

}
//@+node:gcross.20110127123226.2831: *3* contractSOSRight
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
        ASSERT_EQ(OperatorDimension(1),new_boundary->operator_dimension);
        ASSERT_EQ(StateDimension(1),new_boundary->state_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(1)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2833: *4* trivial, c = 2
    TEST_CASE(trivial_with_operator_dimension_2) {
        OperatorSite const operator_site
            (LeftDimension(2)
            ,RightDimension(1)
            ,fillWithRange(list_of(2)(1))
            ,fillWithRange(list_of(complex<double>(0,1)))
            );
        auto_ptr<ExpectationBoundary<Right> const> new_boundary(
            contractSOSRight(
                 ExpectationBoundary<Right>::trivial
                ,StateSite<Right>::trivial
                ,operator_site
            )
        );
        ASSERT_EQ(OperatorDimension(2),new_boundary->operator_dimension);
        ASSERT_EQ(StateDimension(1),new_boundary->state_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(0))(complex<double>(0,1)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2873: *4* non-trivial
    TEST_CASE(non_trivial) {
        ExpectationBoundary<Right> const boundary
            (OperatorDimension(3)
            ,fillWithRange(list_of
                (c(2,0))(c(0,4))
                (c(1,0))(c(0,5))

                (c(1,0))(c(0,5))
                (c(1,0))(c(1,0))

                (c(3,0))(c(0,6))
                (c(0,2))(c(-2,0))
            ));
        StateSite<Right> const state_site
            (LeftDimension(4)
            ,RightDimension(2)
            ,fillWithRange(list_of
                (c(1,0))(c(1,0))
                (c(1,0))(c(-1,0))
                (c(-1,0))(c(2,0))
                (c(0,2))(c(-2,0))
            ));
        OperatorSite const operator_site
            (LeftDimension(2)
            ,RightDimension(3)
            ,fillWithRange(list_of(2)(1)(1)(3))
            ,fillWithRange(list_of(complex<double>(1,0))(complex<double>(0,1)))
            );
        auto_ptr<ExpectationBoundary<Right> const> actual_boundary(
            contractSOSRight(
                 boundary
                ,state_site
                ,operator_site
            )
        );
        ASSERT_EQ(OperatorDimension(2),actual_boundary->operator_dimension);
        ASSERT_EQ(StateDimension(4),actual_boundary->state_dimension);
        complex<double> const expected_boundary[] =
            {c(-8.0,1.0),c(4.0,5.0),c(-10.0,-7.0),c(18.0,8.0)
            ,c(-4.0,5.0),c(8.0,1.0),c(-14.0,1.0),c(18.0,-8.0)
            ,c(2.0,-7.0),c(-10.0,1.0),c(16.0,-5.0),c(-18.0,16.0)
            ,c(-2.0,-8.0),c(-2.0,8.0),c(2.0,-16.0),c(0.0,20.0)

            ,c(3.0,9.0),c(3.0,-9.0),c(-3.0,18.0),c(0.0,-24.0)
            ,c(1.0,-1.0),c(1.0,1.0),c(-1.0,-2.0),c(0.0,0.0)
            ,c(0.0,6.0),c(0.0,-6.0),c(0.0,12.0),c(0.0,-12.0)
            ,c(-10.0,-6.0),c(6.0,14.0),c(-14.0,-24.0),c(24.0,24.0)
            };
        complex<double> const *actual = *actual_boundary;
        BOOST_FOREACH(complex<double> expected, expected_boundary) {
            ASSERT_EQ(expected,*actual);
            ++actual;
        }
    }
    //@-others

}
//@+node:gcross.20110127123226.2837: *3* contractSSLeft
TEST_SUITE(computeSSLeft) {

    //@+others
    //@+node:gcross.20110127123226.2838: *4* trivial with all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        auto_ptr<OverlapBoundary<Left> const> new_boundary(
            contractSSLeft(
                 OverlapBoundary<Left>::trivial
                ,OverlapSite<Left>::trivial
                ,StateSite<Left>::trivial
            )
        );
        ASSERT_EQ(OverlapDimension(1),new_boundary->overlap_dimension);
        ASSERT_EQ(StateDimension(1),new_boundary->state_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(1)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2849: *4* trivial, physical dimension 2
    TEST_CASE(trivial_with_physical_dimension_2) {
        OverlapSite<Left> const overlap_site
            (RightDimension(1)
            ,LeftDimension(1)
            ,fillWithRange(list_of(5)(-1))
            );
        StateSite<Left> const state_site
            (LeftDimension(1)
            ,RightDimension(1)
            ,fillWithRange(list_of(1)(1))
            );
        auto_ptr<OverlapBoundary<Left> const> new_boundary(
            contractSSLeft(
                 OverlapBoundary<Left>::trivial
                ,overlap_site
                ,state_site
            )
        );
        ASSERT_EQ(OverlapDimension(1),new_boundary->overlap_dimension);
        ASSERT_EQ(StateDimension(1),new_boundary->state_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(4)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2851: *4* non-trivial
    TEST_CASE(non_trivial) {
        OverlapBoundary<Left> const boundary
            (OverlapDimension(3)
            ,fillWithRange(list_of
                (c(2,0))(c(0,4))
                (c(1,0))(c(0,5))
                (c(3,0))(c(0,6))
            ));
        OverlapSite<Left> const overlap_site
            (RightDimension(2)
            ,LeftDimension(3)
            ,fillWithRange(list_of
                (c( 1,0))(c( 0,-1))(c(0, 1))
                (c( 1,0))(c( 1, 0))(c(0,-1))
            ));
        StateSite<Left> const state_site
            (LeftDimension(2)
            ,RightDimension(5)
            ,fillWithRange(list_of
                (c(1,0))(c(1,0))(c( 1,0))(c(-1, 0))(c(-1,0))
                (c(2,0))(c(0,2))(c(-2,0))(c( 1,-2))(c( 2,0))
            ));
        auto_ptr<OverlapBoundary<Left> const> actual_boundary(
            contractSSLeft(
                 boundary
                ,overlap_site
                ,state_site
            )
        );
        ASSERT_EQ(OverlapDimension(2),actual_boundary->overlap_dimension);
        ASSERT_EQ(StateDimension(5),actual_boundary->state_dimension);
        complex<double> const expected_boundary[] =
            {c(0,10),c(-6,0),c(4,-6),c(5,4),c(-4,6)
            ,c(15,15),c(-15,9),c(-9,-21),c(21,0),c(9,21)
            }
        ;
        complex<double> const *actual = *actual_boundary;
        BOOST_FOREACH(complex<double> expected, expected_boundary) {
            ASSERT_EQ(expected,*actual);
            ++actual;
        }
    }
    //@-others

}
//@+node:gcross.20110127123226.2862: *3* contractSSRight
TEST_SUITE(computeSSRight) {

    //@+others
    //@+node:gcross.20110127123226.2863: *4* trivial with all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        auto_ptr<OverlapBoundary<Right> const> new_boundary(
            contractSSRight(
                 OverlapBoundary<Right>::trivial
                ,OverlapSite<Right>::trivial
                ,StateSite<Right>::trivial
            )
        );
        ASSERT_EQ(OverlapDimension(1),new_boundary->overlap_dimension);
        ASSERT_EQ(StateDimension(1),new_boundary->state_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(1)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2864: *4* trivial, physical dimension 2
    TEST_CASE(trivial_with_physical_dimension_2) {
        OverlapSite<Right> const overlap_site
            (RightDimension(1)
            ,LeftDimension(1)
            ,fillWithRange(list_of(5)(-1)
            ));
        StateSite<Right> const state_site
            (LeftDimension(1)
            ,RightDimension(1)
            ,fillWithRange(list_of(1)(1)
            ));
        auto_ptr<OverlapBoundary<Right> const> new_boundary(
            contractSSRight(
                 OverlapBoundary<Right>::trivial
                ,overlap_site
                ,state_site
            )
        );
        ASSERT_EQ(OverlapDimension(1),new_boundary->overlap_dimension);
        ASSERT_EQ(StateDimension(1),new_boundary->state_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(4)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2865: *4* non-trivial
    TEST_CASE(non_trivial) {
        OverlapBoundary<Right> const boundary
            (OverlapDimension(2)
            ,fillWithRange(list_of
                (c(2,0))(c(1,0))
                (c(3,0))(c(1,1))
                (c(1,2))(c(0,6))
                (c(0,4))(c(0,5))
                (c(1,1))(c(2,1))
            ));
        OverlapSite<Right> const overlap_site
            (RightDimension(2)
            ,LeftDimension(3)
            ,fillWithRange(list_of
                (c( 1,0))(c( 0,-1))(c(0, 1))
                (c( 1,0))(c( 1, 0))(c(0,-1))
            ));
        StateSite<Right> const state_site
            (LeftDimension(2)
            ,RightDimension(5)
            ,fillWithRange(list_of
                (c(1,0))(c(1,0))(c( 1,0))(c(-1, 0))(c(-1,0))
                (c(2,0))(c(0,2))(c(-2,0))(c( 1,-2))(c( 2,0))
            ));
        auto_ptr<OverlapBoundary<Right> const> actual_boundary(
            contractSSRight(
                 boundary
                ,overlap_site
                ,state_site
            )
        );
        ASSERT_EQ(OverlapDimension(3),actual_boundary->overlap_dimension);
        ASSERT_EQ(StateDimension(2),actual_boundary->state_dimension);
        complex<double> const expected_boundary[] =
            {c(5,-2),c(-3,-4),c(4,5)
            ,c(26,5),c(22,-15),c(-11,-2)
            }
        ;
        complex<double> const *actual = *actual_boundary;
        BOOST_FOREACH(complex<double> expected, expected_boundary) {
            ASSERT_EQ(expected,*actual);
            ++actual;
        }
    }
    //@-others

}
//@-others

}
//@-others
//@-leo