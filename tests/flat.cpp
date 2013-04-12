#include <boost/assign/list_of.hpp>
#include <boost/container/vector.hpp>
#include <boost/numeric/ublas/vector_expression.hpp>
#include <boost/range/adaptor/indirected.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <boost/range/irange.hpp>
#include <boost/range/numeric.hpp>
#include <complex>
#include <illuminate.hpp>

#include "nutcracker/flat.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::adaptors::indirected;
using boost::adaptors::transformed;
using boost::assign::list_of;
using boost::container::vector;
using boost::equal;
using boost::inner_product;
using boost::irange;
using boost::numeric::ublas::norm_1;

using std::abs;

TEST_SUITE(Flat) {

TEST_SUITE(computeStateVector) {

TEST_CASE(single_site) {
    RNG random;

    REPEAT(10) {
        PhysicalDimension physical_dimension(random);
        StateSite<None> state_site(
             physical_dimension
            ,LeftDimension(1)
            ,RightDimension(1)
            ,fillWithGenerator(random.randomComplexDouble)
        );
        Vector state_vector = computeStateVector(vector<StateSiteAny const*>(1,&state_site) | indirected);
        ASSERT_EQ(*physical_dimension,state_vector.size());
        ASSERT_TRUE(equal(state_site,state_vector));
    }
}
TEST_CASE(single_site_squared) {
    RNG random;

    REPEAT(10) {
        unsigned int const d = random;
        StateSite<None> state_site(
             PhysicalDimension(d)
            ,LeftDimension(1)
            ,RightDimension(1)
            ,fillWithGenerator(random.randomComplexDouble)
        );
        Vector actual_state_vector = computeStateVector(vector<StateSiteAny const*>(2,&state_site) | indirected);
        Vector correct_state_vector(d*d);
        BOOST_FOREACH(unsigned int i, irange(0u,d)) {
            BOOST_FOREACH(unsigned int j, irange(0u,d)) {
                correct_state_vector[i*d+j] = (state_site.begin()[i]) * (state_site.begin()[j]);
            }
        }
        ASSERT_EQ(correct_state_vector.size(),actual_state_vector.size());
        ASSERT_TRUE(equal(correct_state_vector,actual_state_vector));
    }
}
TEST_CASE(two_trivial_sites) {
    RNG random;

    REPEAT(10) {
        unsigned int b = random;
        StateSite<None>
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
        Vector state_vector = computeStateVector(list_of(&state_site_1)(&state_site_2) | indirected);
        ASSERT_EQ(1u,state_vector.size());
        ASSERT_NEAR_REL(inner_product(state_site_1,state_site_2,c(0,0)),state_vector[0],1e-15);
    }
}
TEST_SUITE(W_state) {

TEST_CASE(two_sites) {
    StateSite<None>
         state_site_1
            (LeftDimension(1)
            ,RightDimension(2)
            ,fillWithRange(list_of
                (1)
                (0)

                (0)
                (-1)
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
    Vector actual_state_vector = computeStateVector(list_of(&state_site_1)(&state_site_2) | indirected);
    ASSERT_EQ(4u,actual_state_vector.size());
    complex<double> correct_state_vector[] = {0,1,-1,0};
    ASSERT_TRUE(equal(correct_state_vector,actual_state_vector));
}
TEST_CASE(three_sites) {
    StateSite<None>
         state_site_1
            (LeftDimension(1)
            ,RightDimension(2)
            ,fillWithRange(list_of
                (1)
                (0)

                (0)
                (-1)
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

                (2)
                (0)
             )
            )
        ;
    Vector actual_state_vector = computeStateVector(list_of(&state_site_1)(&state_site_2)(&state_site_3) | indirected);
    ASSERT_EQ(8u,actual_state_vector.size());
    complex<double> correct_state_vector[] =
        {0  // 000
        ,2  // 001
        ,1  // 010
        ,0  // 011
        ,-1 // 100
        ,0  // 101
        ,0  // 110
        ,0  // 111
        }
    ;
    ASSERT_TRUE(equal(correct_state_vector,actual_state_vector));
}

}

}
TEST_SUITE(computeStateVectorComponent) {

TEST_CASE(single_site) {
    RNG random;

    REPEAT(10) {
        StateSite<None> state_site(
             PhysicalDimension(random)
            ,LeftDimension(1)
            ,RightDimension(1)
            ,fillWithGenerator(random.randomComplexDouble)
        );
        BOOST_FOREACH(unsigned int const i, irange(0u,state_site.physicalDimension())) {
            ASSERT_EQ(state_site.begin()[i],computeStateVectorComponent(list_of(&state_site) | indirected,i));
        }
    }
}
TEST_CASE(single_site_squared) {
    RNG random;

    REPEAT(10) {
        unsigned int d = random;
        StateSite<None> state_site(
             PhysicalDimension(d)
            ,LeftDimension(1)
            ,RightDimension(1)
            ,fillWithGenerator(random.randomComplexDouble)
        );
        BOOST_FOREACH(unsigned int i, irange(0u,d)) {
            BOOST_FOREACH(unsigned int j, irange(0u,d)) {
                ASSERT_EQ(state_site.begin()[i]*state_site.begin()[j],computeStateVectorComponent(list_of(&state_site)(&state_site) | indirected,i*d+j));
            }
        }
    }
}
TEST_CASE(two_trivial_sites) {
    RNG random;

    REPEAT(10) {
        StateSite<None>
             state_site_1
                (PhysicalDimension(random)
                ,LeftDimension(1)
                ,RightDimension(1)
                ,fillWithGenerator(random.randomComplexDouble)
                )
            ,state_site_2
                (PhysicalDimension(random)
                ,LeftDimension(1)
                ,RightDimension(1)
                ,fillWithGenerator(random.randomComplexDouble)
                )
            ;
        BOOST_FOREACH(unsigned int const i, irange(0u,state_site_1.physicalDimension())) {
            BOOST_FOREACH(unsigned int const j, irange(0u,state_site_2.physicalDimension())) {
                ASSERT_EQ(
                     state_site_1.begin()[i]*state_site_2.begin()[j]
                    ,computeStateVectorComponent(
                         list_of(&state_site_1)(&state_site_2) | indirected
                        ,i*state_site_2.physicalDimension()+j
                     )
                );
            }
        }
    }
}
TEST_SUITE(W_state) {

TEST_CASE(two_sites) {
    StateSite<None>
         state_site_1
            (LeftDimension(1)
            ,RightDimension(2)
            ,fillWithRange(list_of
                (1)
                (0)

                (0)
                (-1)
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
    vector<StateSiteAny const*> state = list_of(&state_site_1)(&state_site_2);
    ASSERT_EQ(c( 0,0),computeStateVectorComponent(state | indirected,0));
    ASSERT_EQ(c( 1,0),computeStateVectorComponent(state | indirected,1));
    ASSERT_EQ(c(-1,0),computeStateVectorComponent(state | indirected,2));
    ASSERT_EQ(c( 0,0),computeStateVectorComponent(state | indirected,3));
}
TEST_CASE(three_sites) {
    StateSite<None>
         state_site_1
            (LeftDimension(1)
            ,RightDimension(2)
            ,fillWithRange(list_of
                (1)
                (0)

                (0)
                (-1)
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

                (2)
                (0)                
             )
            )
        ;
    vector<StateSiteAny const*> state = list_of(&state_site_1)(&state_site_2)(&state_site_3);
    ASSERT_EQ(c( 0,0),computeStateVectorComponent(state | indirected,0));
    ASSERT_EQ(c( 2,0),computeStateVectorComponent(state | indirected,1));
    ASSERT_EQ(c( 1,0),computeStateVectorComponent(state | indirected,2));
    ASSERT_EQ(c( 0,0),computeStateVectorComponent(state | indirected,3));
    ASSERT_EQ(c(-1,0),computeStateVectorComponent(state | indirected,4));
    ASSERT_EQ(c( 0,0),computeStateVectorComponent(state | indirected,5));
    ASSERT_EQ(c( 0,0),computeStateVectorComponent(state | indirected,6));
    ASSERT_EQ(c( 0,0),computeStateVectorComponent(state | indirected,7));
}

}

}
TEST_SUITE(computeStateVectorLength) {

TEST_CASE(single_site) {
    RNG random;

    REPEAT(10) {
        StateSite<None> state_site(
             PhysicalDimension(random)
            ,LeftDimension(1)
            ,RightDimension(1)
        );
        ASSERT_EQ(
             state_site.physicalDimension()
            ,computeStateVectorLength(vector<StateSiteAny const*>(1,&state_site) | indirected)
        );
    }
}
TEST_CASE(single_site_squared) {
    RNG random;

    REPEAT(10) {
        StateSite<None> state_site(
             PhysicalDimension(random)
            ,LeftDimension(1)
            ,RightDimension(1)
        );
        ASSERT_EQ(
             state_site.physicalDimension()*state_site.physicalDimension()
            ,computeStateVectorLength(vector<StateSiteAny const*>(2,&state_site) | indirected)
        );
    }
}
TEST_CASE(two_trivial_sites) {
    RNG random;

    REPEAT(10) {
        unsigned int b = random;
        StateSite<None>
             state_site_1
                (PhysicalDimension(random)
                ,LeftDimension(1)
                ,RightDimension(b)
                ,fillWithGenerator(random.randomComplexDouble)
                )
            ,state_site_2
                (PhysicalDimension(random)
                ,LeftDimension(b)
                ,RightDimension(1)
                ,fillWithGenerator(random.randomComplexDouble)
                )
            ;
        ASSERT_EQ(
             state_site_1.physicalDimension()*state_site_2.physicalDimension()
            ,computeStateVectorLength(list_of(&state_site_1)(&state_site_2) | indirected)
        );
    }
}

}
TEST_CASE(computeStateVector_consistent_with_computeStateVectorComponent) {
    using namespace boost;

    RNG random;

    REPEAT(10) {
        State state = random.randomState();
        Vector state_vector = computeStateVector(state);
        unsigned long long const state_length = computeStateVectorLength(state);
        REPEAT(10) {
            unsigned long long const index = random(0,state_length-1);
            complex<double> const component = computeStateVectorComponent(state,index);
            ASSERT_NEAR_REL(component,state_vector[index],1e-13);
        }
    }
}

}
