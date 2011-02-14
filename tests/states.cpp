//@+leo-ver=5-thin
//@+node:gcross.20110213161858.1822: * @thin states.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110213161858.1823: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/container/vector.hpp>
#include <boost/numeric/ublas/vector_expression.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <boost/range/irange.hpp>
#include <boost/range/numeric.hpp>
#include <illuminate.hpp>

#include "states.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::assign::list_of;
using boost::container::vector;
using boost::equal;
using boost::inner_product;
using boost::irange;
using boost::numeric::ublas::norm_1;
//@-<< Includes >>

//@+others
//@+node:gcross.20110213161858.1824: ** Tests
TEST_SUITE(States) {

//@+others
//@+node:gcross.20110213161858.1825: *3* computeStateVector
TEST_SUITE(computeStateVector) {

//@+others
//@+node:gcross.20110213161858.1826: *4* trivial
TEST_CASE(trivial) {
    ASSERT_TRUE(equal(StateVector(),computeStateVector(vector<StateSiteAny const*>())));
}
//@+node:gcross.20110213161858.1828: *4* single site
TEST_CASE(single_site) {
    RNG random;

    REPEAT(10) {
        PhysicalDimension physical_dimension(random);
        StateSite<Middle> state_site(
             physical_dimension
            ,LeftDimension(1)
            ,RightDimension(1)
            ,fillWithGenerator(random.randomComplexDouble)
        );
        StateVector state_vector = computeStateVector(vector<StateSiteAny const*>(1,&state_site));
        ASSERT_EQ_QUOTED(*physical_dimension,state_vector.size());
        ASSERT_TRUE(equal(state_site,state_vector));
    }
}
//@+node:gcross.20110213161858.1830: *4* single site squared
TEST_CASE(single_site_squared) {
    RNG random;

    REPEAT(10) {
        unsigned int d = random;
        PhysicalDimension physical_dimension(d);
        StateSite<Middle> state_site(
             physical_dimension
            ,LeftDimension(1)
            ,RightDimension(1)
            ,fillWithGenerator(random.randomComplexDouble)
        );
        StateVector actual_state_vector = computeStateVector(vector<StateSiteAny const*>(2,&state_site));
        StateVector correct_state_vector(d*d);
        BOOST_FOREACH(unsigned int i, irange(0u,d)) {
            BOOST_FOREACH(unsigned int j, irange(0u,d)) {
                correct_state_vector[i*d+j] = (state_site.begin()[i]) * (state_site.begin()[j]);
            }
        }
        ASSERT_EQ_QUOTED(correct_state_vector.size(),actual_state_vector.size());
        ASSERT_TRUE(equal(correct_state_vector,actual_state_vector));
    }
}
//@+node:gcross.20110213161858.1832: *4* two trivial sites
TEST_CASE(two_trivial_sites) {
    RNG random;

    REPEAT(10) {
        unsigned int b = random;
        StateSite<Middle>
             state_site_1
                (PhysicalDimension(1)
                ,LeftDimension(1)
                ,RightDimension(b)
                ,fillWithGenerator(random.randomComplexDouble)
                )
            ,state_site_2
                (PhysicalDimension(1)
                ,LeftDimension(b)
                ,RightDimension(1)
                ,fillWithGenerator(random.randomComplexDouble)
                )
            ;
        StateVector state_vector = computeStateVector(list_of(&state_site_1)(&state_site_2));
        ASSERT_EQ(1,state_vector.size());
        ASSERT_NEAR(inner_product(state_site_1,state_site_2,c(0,0)),state_vector[0],1e-15);
    }
}
//@+node:gcross.20110213161858.1835: *4* W state
TEST_SUITE(W_state) {

//@+others
//@+node:gcross.20110213161858.1834: *5* two sites
TEST_CASE(two_sites) {
    StateSite<Middle>
         state_site_1
            (LeftDimension(1)
            ,RightDimension(2)
            ,fillWithRange(list_of
                (1)
                (0)

                (0)
                (1)
             )
            )
        ,state_site_2
            (LeftDimension(2)
            ,RightDimension(1)
            ,fillWithRange(list_of
                (0)
                (1)

                (1)
                (0)                
             )
            )
        ;
    StateVector actual_state_vector = computeStateVector(list_of(&state_site_1)(&state_site_2));
    ASSERT_EQ(4,actual_state_vector.size());
    complex<double> correct_state_vector[] = {0,1,1,0};
    ASSERT_TRUE(equal(correct_state_vector,actual_state_vector));
}
//@+node:gcross.20110213161858.1837: *5* three sites
TEST_CASE(three_sites) {
    StateSite<Middle>
         state_site_1
            (LeftDimension(1)
            ,RightDimension(2)
            ,fillWithRange(list_of
                (1)
                (0)

                (0)
                (1)
             )
            )
        ,state_site_2
            (LeftDimension(2)
            ,RightDimension(2)
            ,fillWithRange(list_of
                (1)(0)
                (0)(1)

                (0)(1)
                (0)(0)
             )
            )
        ,state_site_3
            (LeftDimension(2)
            ,RightDimension(1)
            ,fillWithRange(list_of
                (0)
                (1)

                (1)
                (0)                
             )
            )
        ;
    StateVector actual_state_vector = computeStateVector(list_of(&state_site_1)(&state_site_2)(&state_site_3));
    ASSERT_EQ(8,actual_state_vector.size());
    complex<double> correct_state_vector[] =
        {0 // 000
        ,1 // 001
        ,1 // 010
        ,0 // 011
        ,1 // 100
        ,0 // 101
        ,0 // 110
        ,0 // 111
        }
    ;
    ASSERT_TRUE(equal(correct_state_vector,actual_state_vector));
}
//@-others

}
//@-others

}
//@-others

}
//@-others
//@-leo
