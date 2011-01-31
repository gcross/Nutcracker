//@+leo-ver=5-thin
//@+node:gcross.20110127123226.2504: * @thin core.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110127123226.2505: ** << Includes >>
#include <boost/assign.hpp>
#include <boost/foreach.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <boost/range/algorithm/generate.hpp>
#include <boost/random.hpp>
#include <boost/random/normal_distribution.hpp>
#include <boost/random/uniform_smallint.hpp>
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
//@+node:gcross.20110129220506.1649: *3* struct RNG
struct RNG {
    taus88 generator;

    normal_distribution<double> normal;
    variate_generator<taus88,normal_distribution<double> > randomDouble;

    uniform_smallint<unsigned int> smallint;
    variate_generator<taus88,uniform_smallint<unsigned int> > randomInteger;

    template<typename T> struct Generator {
        RNG& rng;
        Generator(RNG& rng) : rng(rng) {}
        T operator()() { return c(rng.randomDouble(),rng.randomDouble()); }
    };

    Generator<complex<double> > randomComplexDouble;

    struct IndexGenerator {
        uniform_smallint<unsigned int> smallint;
        variate_generator<taus88,uniform_smallint<unsigned int> > randomInteger;
        IndexGenerator(
              RNG& rng
            , unsigned int lo
            , unsigned int hi
        ) : smallint(lo,hi)
          , randomInteger(rng.generator,smallint)
        {}
        uint32_t operator()() { return (uint32_t)randomInteger(); }
    };

    RNG()
      : normal(0,1)
      , randomDouble(generator,normal)
      , smallint(1,10)
      , randomInteger(generator,smallint)
      , randomComplexDouble(*this)
    {}

    operator unsigned int() { return randomInteger(); }
    operator complex<double>() { return randomComplexDouble(); }

};
//@+node:gcross.20110129220506.1650: *3* Consistency
TEST_SUITE(Consistency) {

    //@+others
    //@+node:gcross.20110129220506.1651: *4* expectation value contraction
    TEST_CASE(expectation_value_contraction) {

        RNG random;

        REPEAT(10) {

            unsigned int const
                 left_state_dimension = random
                ,operator_dimension = random
                ,physical_dimension = random
                ,right_state_dimension = random
                ,number_of_matrices = random
                ;
            ExpectationBoundary<Left> const left_boundary
                (OperatorDimension(operator_dimension)
                ,StateDimension(left_state_dimension)
                ,fillWithGenerator(random.randomComplexDouble)
                );
            StateSite<Middle> const state_site
                (PhysicalDimension(physical_dimension)
                ,LeftDimension(left_state_dimension)
                ,RightDimension(right_state_dimension)
                ,fillWithGenerator(random.randomComplexDouble)
                );
            StateSite<Left> const left_state_site(copyFrom(state_site));
            StateSite<Right> const right_state_site(copyFrom(state_site));
            RNG::IndexGenerator randomIndex(random,1,operator_dimension);
            OperatorSite const operator_site
                (number_of_matrices
                ,PhysicalDimension(physical_dimension)
                ,LeftDimension(operator_dimension)
                ,RightDimension(operator_dimension)
                ,fillWithGenerator(randomIndex)
                ,fillWithGenerator(random.randomComplexDouble)
                );
            ExpectationBoundary<Right> const right_boundary
                (OperatorDimension(operator_dimension)
                ,StateDimension(right_state_dimension)
                ,fillWithGenerator(random.randomComplexDouble)
                );
            auto_ptr<ExpectationBoundary<Left> const> const new_left_boundary =
                contractSOSLeft(
                     left_boundary
                    ,left_state_site
                    ,operator_site
                );
            auto_ptr<ExpectationBoundary<Right> const> const new_right_boundary =
                contractSOSRight(
                     right_boundary
                    ,right_state_site
                    ,operator_site
                );

            complex<double> const
                 result_from_computeExpectationValue =
                    computeExpectationValue(
                         left_boundary
                        ,state_site
                        ,operator_site
                        ,right_boundary
                    )
                ,result_from_contractSOSLeft =
                    contractExpectationBoundaries(
                         *new_left_boundary
                        ,right_boundary
                    )
                ,result_from_contractSOSRight =
                    contractExpectationBoundaries(
                         left_boundary
                        ,*new_right_boundary
                    )
                ;
            ASSERT_NEAR_QUOTED(result_from_computeExpectationValue,result_from_contractSOSLeft,1e-10);
            ASSERT_NEAR_QUOTED(result_from_computeExpectationValue,result_from_contractSOSRight,1e-10);
        }
    }
    //@+node:gcross.20110129220506.1658: *4* apply projector overlap
    TEST_CASE(apply_projector_overlap) {

        RNG random;

        REPEAT(10) {

            unsigned int const
                  physical_dimension = random
                , left_dimension = random
                , right_dimension = random
                , projector_length =
                     physical_dimension
                    *left_dimension
                    *right_dimension
                , number_of_projectors =
                    RNG::IndexGenerator(random,1,projector_length-1)()
                ;

            StateSite<Middle> const state_site(
                 PhysicalDimension(physical_dimension)
                ,LeftDimension(left_dimension)
                ,RightDimension(right_dimension)
                ,fillWithGenerator(random.randomComplexDouble)
            );

            auto_ptr<ProjectorMatrix const> const projector_matrix(
                randomProjectorMatrix(
                     projector_length
                    ,number_of_projectors
                )
            );

            auto_ptr<StateSite<Middle> const> const projected_state_site(
                applyProjectorMatrix(
                     *projector_matrix
                    ,state_site
                )
            );

            double const overlap =
                computeOverlapWithProjectors(
                     *projector_matrix
                    ,*projected_state_site
                );

            ASSERT_EQ_QUOTED(
                 physical_dimension
                ,projected_state_site->physical_dimension()
            );
            ASSERT_EQ_QUOTED(
                 left_dimension
                ,projected_state_site->left_dimension()
            );
            ASSERT_EQ_QUOTED(
                 right_dimension
                ,projected_state_site->right_dimension()
            );

            ASSERT_NEAR(0,overlap,1e-12);

        }

    }
    //@-others

}
//@+node:gcross.20110129220506.1653: *3* Contractors
TEST_SUITE(Contractors) {

//@+others
//@+node:gcross.20110127123226.2507: *4* computeExpectationValue
TEST_SUITE(computeExpectationValue) {

    //@+others
    //@+node:gcross.20110127123226.2518: *5* trivial, all dimensions 1
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
    //@+node:gcross.20110127123226.2819: *5* trivial, all dimensions 1, imaginary
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
    //@+node:gcross.20110127123226.2821: *5* trivial, d = 2
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
//@+node:gcross.20110128131637.1649: *4* contractExpectationBoundaries
TEST_SUITE(contractExpectationBoundaries) {

    //@+others
    //@+node:gcross.20110128131637.1650: *5* trivial, all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        complex<double> expected_value = 1;
        complex<double> actual_value =
            contractExpectationBoundaries(
                 ExpectationBoundary<Left>::trivial
                ,ExpectationBoundary<Right>::trivial
            );
        ASSERT_EQ(expected_value,actual_value);
    }
    //@+node:gcross.20110128131637.1654: *5* non-trivial
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
//@+node:gcross.20110127123226.2822: *4* contractSOSLeft
TEST_SUITE(computeSOSLeft) {

    //@+others
    //@+node:gcross.20110127123226.2823: *5* trivial with all dimensions 1
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
    //@+node:gcross.20110127123226.2827: *5* trivial, c = 2
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
    //@+node:gcross.20110127123226.2875: *5* non-trivial
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
//@+node:gcross.20110127123226.2831: *4* contractSOSRight
TEST_SUITE(computeSOSRight) {

    //@+others
    //@+node:gcross.20110127123226.2832: *5* trivial with all dimensions 1
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
    //@+node:gcross.20110127123226.2833: *5* trivial, c = 2
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
    //@+node:gcross.20110127123226.2873: *5* non-trivial
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
//@+node:gcross.20110127123226.2837: *4* contractSSLeft
TEST_SUITE(computeSSLeft) {

    //@+others
    //@+node:gcross.20110127123226.2838: *5* trivial with all dimensions 1
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
    //@+node:gcross.20110127123226.2849: *5* trivial, physical dimension 2
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
    //@+node:gcross.20110127123226.2851: *5* non-trivial
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
//@+node:gcross.20110127123226.2862: *4* contractSSRight
TEST_SUITE(computeSSRight) {

    //@+others
    //@+node:gcross.20110127123226.2863: *5* trivial with all dimensions 1
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
    //@+node:gcross.20110127123226.2864: *5* trivial, physical dimension 2
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
    //@+node:gcross.20110127123226.2865: *5* non-trivial
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
//@+node:gcross.20110129220506.1654: *3* optimizeStateSite
TEST_SUITE(optimizeStateSite) {

//@+others
//@+node:gcross.20110129220506.1655: *4* trivial, d = 4
TEST_CASE(trivial_with_physical_dimension_4) {
    RNG rng;

    StateSite<Middle> state_site(
         PhysicalDimension(4)
        ,LeftDimension(1)
        ,RightDimension(1)
        ,fillWithGenerator(rng.randomComplexDouble)
    );

    OperatorSite operator_site(
         LeftDimension(1)
        ,RightDimension(1)
        ,fillWithRange(list_of(1)(1))
        ,fillWithRange(list_of
            ( 1)( 0)( 0)( 0)
            ( 0)( 1)( 0)( 0)
            ( 0)( 0)( 1)( 0)
            ( 0)( 0)( 0)(-1)
         )
    );

    OptimizerResult optimizer_result =
        optimizeStateSite(
             ExpectationBoundary<Left>::trivial
            ,state_site
            ,operator_site
            ,ExpectationBoundary<Right>::trivial
            ,none
            ,optimize_for_lowest_real_part
            ,0
            ,10000
        );

    StateSite<Middle> const& new_state_site = *optimizer_result.state_site;

    ASSERT_EQ(4,new_state_site.physical_dimension());
    ASSERT_EQ(1,new_state_site.left_dimension());
    ASSERT_EQ(1,new_state_site.right_dimension());
    ASSERT_NEAR(c(0,0),new_state_site[0],1e-7);
    ASSERT_NEAR(c(0,0),new_state_site[1],1e-7);
    ASSERT_NEAR(c(0,0),new_state_site[2],1e-7);
    ASSERT_NEAR(1,abs(new_state_site[3]),1e-7);
    ASSERT_NEAR(-1,optimizer_result.eigenvalue,1e-7);
    ASSERT_EQ(0,optimizer_result.number_of_iterations);
}
//@-others

}
//@+node:gcross.20110129220506.1659: *3* formProjectorMatrix
TEST_SUITE(formProjectorMatrix) {

//@+others
//@+node:gcross.20110129220506.1660: *4* trivial
TEST_CASE(trivial) {
    auto_ptr<ProjectorMatrix const> projector_matrix(
        formProjectorMatrix(
            list_of(
                OverlapVectorTrio(
                     make_shared_ptr(new OverlapBoundary<Left>(make_trivial))
                    ,make_shared_ptr(new OverlapBoundary<Right>(make_trivial))
                    ,make_shared_ptr(new OverlapSite<Middle>(make_trivial))
                )
            )
        )
    );
    ASSERT_EQ(1,projector_matrix->number_of_projectors);
    ASSERT_EQ(1,projector_matrix->projector_length);
    ASSERT_EQ(1,projector_matrix->number_of_reflectors);
    ASSERT_EQ(0,projector_matrix->orthogonal_subspace_dimension);
}
//@+node:gcross.20110129220506.1663: *4* physical dimension 4, one projector
TEST_CASE(physical_dimension_4_with_one_projector) {
    auto_ptr<ProjectorMatrix const> projector_matrix(
        formProjectorMatrix(list_of
            (OverlapVectorTrio(
                 make_shared_ptr(new OverlapBoundary<Left>(make_trivial))
                ,make_shared_ptr(new OverlapBoundary<Right>(make_trivial))
                ,make_shared_ptr(new OverlapSite<Middle>(
                     RightDimension(1)
                    ,LeftDimension(1)
                    ,fillWithRange(list_of(1)(1)(1)(1))
                 ))
            ))
        )
    );
    ASSERT_EQ(1,projector_matrix->number_of_projectors);
    ASSERT_EQ(4,projector_matrix->projector_length);
    ASSERT_EQ(1,projector_matrix->number_of_reflectors);
    ASSERT_EQ(3,projector_matrix->orthogonal_subspace_dimension);
}
//@+node:gcross.20110129220506.1665: *4* physical dimension 4, two projectors
TEST_CASE(physical_dimension_4_with_two_projectors) {
    auto_ptr<ProjectorMatrix const> projector_matrix(
        formProjectorMatrix(list_of
            (OverlapVectorTrio(
                 make_shared_ptr(new OverlapBoundary<Left>(make_trivial))
                ,make_shared_ptr(new OverlapBoundary<Right>(make_trivial))
                ,make_shared_ptr(new OverlapSite<Middle>(
                     RightDimension(1)
                    ,LeftDimension(1)
                    ,fillWithRange(list_of(1)(1)(1)(1))
                 ))
            ))
            (OverlapVectorTrio(
                 make_shared_ptr(new OverlapBoundary<Left>(make_trivial))
                ,make_shared_ptr(new OverlapBoundary<Right>(make_trivial))
                ,make_shared_ptr(new OverlapSite<Middle>(
                     RightDimension(1)
                    ,LeftDimension(1)
                    ,fillWithRange(list_of(1)(-1)(1)(1))
                 ))
            ))
        )
    );
    ASSERT_EQ(2,projector_matrix->number_of_projectors);
    ASSERT_EQ(4,projector_matrix->projector_length);
    ASSERT_EQ(2,projector_matrix->number_of_reflectors);
    ASSERT_EQ(2,projector_matrix->orthogonal_subspace_dimension);
}
//@+node:gcross.20110129220506.1667: *4* physical dimension 4, three projectors
TEST_CASE(physical_dimension_4_with_three_projectors) {
    auto_ptr<ProjectorMatrix const> projector_matrix(
        formProjectorMatrix(list_of
            (OverlapVectorTrio(
                 make_shared_ptr(new OverlapBoundary<Left>(make_trivial))
                ,make_shared_ptr(new OverlapBoundary<Right>(make_trivial))
                ,make_shared_ptr(new OverlapSite<Middle>(
                     RightDimension(1)
                    ,LeftDimension(1)
                    ,fillWithRange(list_of(1)(1)(1)(1))
                 ))
            ))
            (OverlapVectorTrio(
                 make_shared_ptr(new OverlapBoundary<Left>(make_trivial))
                ,make_shared_ptr(new OverlapBoundary<Right>(make_trivial))
                ,make_shared_ptr(new OverlapSite<Middle>(
                     RightDimension(1)
                    ,LeftDimension(1)
                    ,fillWithRange(list_of(1)(-1)(1)(1))
                 ))
            ))
            (OverlapVectorTrio(
                 make_shared_ptr(new OverlapBoundary<Left>(make_trivial))
                ,make_shared_ptr(new OverlapBoundary<Right>(make_trivial))
                ,make_shared_ptr(new OverlapSite<Middle>(
                     RightDimension(1)
                    ,LeftDimension(1)
                    ,fillWithRange(list_of(1)(-1)(-1)(1))
                 ))
            ))
        )
    );
    ASSERT_EQ(3,projector_matrix->number_of_projectors);
    ASSERT_EQ(4,projector_matrix->projector_length);
    ASSERT_EQ(3,projector_matrix->number_of_reflectors);
    ASSERT_EQ(1,projector_matrix->orthogonal_subspace_dimension);
}
//@-others

}
//@-others

}
//@-others
//@-leo
