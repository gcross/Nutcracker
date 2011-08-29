//@+leo-ver=5-thin
//@+node:gcross.20110827234144.2562: * @file states.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110826235932.2540: ** << License >>
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

//@+<< Includes >>
//@+node:gcross.20110826235932.2541: ** << Includes >>
#include <boost/range/adaptor/indirected.hpp>
#include <complex>
#include <illuminate.hpp>

#include "states.hpp"

#include "test_utils.hpp"

using boost::adaptors::indirected;

using std::abs;
//@-<< Includes >>

//@+others
//@+node:gcross.20110827234144.2592: ** Functions
//@+node:gcross.20110827234144.2593: *3* vectorFromRange
template<typename Range> VectorPtr vectorFromRange(Range const& range) {
    VectorPtr vector = make_shared<Vector>(range.size());
    copy(range,vector->begin());
    return vector;
}
//@+node:gcross.20110826235932.2542: ** Tests
TEST_SUITE(States) {

//@+others
//@+node:gcross.20110217014932.1935: *3* computeStateOverlap
TEST_SUITE(computeStateOverlap) {

//@+others
//@+node:gcross.20110217014932.1937: *4* self-overlap is 1
TEST_CASE(self_overlap_is_1) {

    RNG random;

    REPEAT(10) {
        State state(random.randomState());
        ASSERT_NEAR_REL(
             c(1,0)
            ,computeStateOverlap(state,state)
            ,1e-15
        )
    }

}
//@+node:gcross.20110218150430.2598: *4* function is hermitian
TEST_CASE(function_is_hermitian) {

    RNG random;

    REPEAT(10) {
        vector<unsigned int> physical_dimensions(random.randomUnsignedIntegerVector(random(1,5)));
        State const state_1(random.randomState(physical_dimensions))
                  , state_2(random.randomState(physical_dimensions))
                  ;
        ASSERT_NEAR_REL(
             conj(computeStateOverlap(state_1,state_2))
            ,computeStateOverlap(state_2,state_1)
            ,1e-14
        )
    }

}
//@+node:gcross.20110218150430.2596: *4* single site orthogonal overlap is zero
TEST_SUITE(single_site_orthogonal_overlap_is_zero) {

    void runTest(unsigned int const physical_dimension) {
        vector<State> states;  states.reserve(physical_dimension);
        BOOST_FOREACH(unsigned int i, irange(0u,physical_dimension)) {
            vector<complex<double> > data(physical_dimension,c(0,0));
            data[i] = c(1,0);
            StateSite<Middle> first_state_site
                (LeftDimension(1)
                ,RightDimension(1)
                ,fillWithRange(data)
                );
            vector<StateSite<Right> > rest_state_sites;
            states.emplace_back(
                 boost::move(first_state_site)
                ,boost::move(rest_state_sites)
            );
        }
        BOOST_FOREACH(unsigned int i, irange(0u,physical_dimension)) {
            BOOST_FOREACH(unsigned int j, irange(i+1,physical_dimension)) {
                ASSERT_NEAR_REL(
                     c(0,0)
                    ,computeStateOverlap(states[i],states[j])
                    ,1e-15
                )
            }
        }
    }

    TEST_CASE(physical_dimension_2) { runTest(2); }
    TEST_CASE(physical_dimension_3) { runTest(3); }
    TEST_CASE(physical_dimension_4) { runTest(4); }

}
//@-others

}
//@+node:gcross.20110827234144.2577: *3* constructStateSite
TEST_SUITE(constructStateSite) {

//@+others
//@+node:gcross.20110828143807.2637: *4* single site
TEST_SUITE(single_site) {
//@+others
//@+node:gcross.20110827234144.2578: *5* bandwidth dimension 1
TEST_CASE(bandwidth_dimension_1) {
    RNG random;

    REPEAT(100) {
        PhysicalDimension const physical_dimension(random);
        VectorConstPtr const vector = random.randomVector(*physical_dimension);
        StateSite<None> state_site = constructStateSite(
             physical_dimension
            ,LeftDimension(1)
            ,RightDimension(1)
            ,list_of(StateSiteLink(1,1,vector))
        );
        ASSERT_EQ(state_site.physicalDimension(),*physical_dimension);
        ASSERT_EQ_VAL(state_site.leftDimension(),1);
        ASSERT_EQ_VAL(state_site.rightDimension(),1);
        ASSERT_TRUE(boost::equal(state_site,*vector));
    }
}
//@+node:gcross.20110828143807.2639: *5* random left dimension
TEST_CASE(random_left_dimension) {
    RNG random;

    REPEAT(100) {
        LeftDimension const left_dimension(random);
        unsigned int const left_index = random(1,*left_dimension);
        complex<double> const value = random;
        StateSite<None> state_site = constructStateSite(
             PhysicalDimension(1)
            ,left_dimension
            ,RightDimension(1)
            ,list_of(StateSiteLink(left_index,1,make_shared<Vector>(1u,value)))
        );
        ASSERT_EQ_VAL(state_site.physicalDimension(),1u);
        ASSERT_EQ(state_site.leftDimension(),*left_dimension);
        ASSERT_EQ_VAL(state_site.rightDimension(),1u);
        Vector correct_data(state_site.size(),c(0,0));
        correct_data[left_index-1] = value;
        ASSERT_TRUE(boost::equal(state_site,correct_data));
    }
}
//@+node:gcross.20110828143807.2641: *5* random right dimension
TEST_CASE(random_right_dimension) {
    RNG random;

    REPEAT(100) {
        RightDimension const right_dimension(random);
        unsigned int const right_index = random(1,*right_dimension);
        complex<double> const value = random;
        StateSite<None> state_site = constructStateSite(
             PhysicalDimension(1)
            ,LeftDimension(1)
            ,right_dimension
            ,list_of(StateSiteLink(1,right_index,make_shared<Vector>(1u,value)))
        );
        ASSERT_EQ_VAL(state_site.physicalDimension(),1u);
        ASSERT_EQ_VAL(state_site.leftDimension(),1u);
        ASSERT_EQ(state_site.rightDimension(),*right_dimension);
        Vector correct_data(state_site.size(),c(0,0));
        correct_data[right_index-1] = value;
        ASSERT_TRUE(boost::equal(state_site,correct_data));
    }
}
//@+node:gcross.20110828143807.2643: *5* random bandwidth dimensions
TEST_CASE(random_bandwidth_dimensions) {
    RNG random;

    REPEAT(100) {
        LeftDimension const left_dimension(random);
        RightDimension const right_dimension(random);
        unsigned int const
            left_index = random(1,*left_dimension),
            right_index = random(1,*right_dimension);
        complex<double> const value = random;
        StateSite<None> state_site = constructStateSite(
             PhysicalDimension(1)
            ,left_dimension
            ,right_dimension
            ,list_of(StateSiteLink(left_index,right_index,make_shared<Vector>(1u,value)))
        );
        ASSERT_EQ_VAL(state_site.physicalDimension(),1u);
        ASSERT_EQ(state_site.leftDimension(),*left_dimension);
        ASSERT_EQ(state_site.rightDimension(),*right_dimension);
        Vector correct_data(state_site.size(),c(0,0));
        correct_data[(left_index-1)*(*right_dimension)+(right_index-1)] = value;
        ASSERT_TRUE(boost::equal(state_site,correct_data));
    }
}
//@+node:gcross.20110828143807.2645: *5* random all dimensions
TEST_CASE(random_all_dimensions) {
    RNG random;

    REPEAT(100) {
        PhysicalDimension const physical_dimension(random);
        LeftDimension const left_dimension(random);
        RightDimension const right_dimension(random);
        unsigned int const
            left_index = random(1,*left_dimension),
            right_index = random(1,*right_dimension);
        VectorConstPtr const vector = random.randomVector(*physical_dimension);
        StateSite<None> state_site = constructStateSite(
             physical_dimension
            ,left_dimension
            ,right_dimension
            ,list_of(StateSiteLink(left_index,right_index,vector))
        );
        ASSERT_EQ(state_site.physicalDimension(),*physical_dimension);
        ASSERT_EQ(state_site.leftDimension(),*left_dimension);
        ASSERT_EQ(state_site.rightDimension(),*right_dimension);
        Vector correct_data(state_site.size(),c(0,0));
        BOOST_FOREACH(unsigned int const physical_index, irange(0u,*physical_dimension)) {
           correct_data[physical_index*(*left_dimension)*(*right_dimension)+(left_index-1)*(*right_dimension)+(right_index-1)] = (*vector)[physical_index];
        }
        ASSERT_TRUE(boost::equal(state_site,correct_data));
    }
}
//@-others
}
//@+node:gcross.20110827234144.2585: *4* two trivial sites
TEST_CASE(two_trivial_sites) {
    RNG random;

    REPEAT(10) {
        unsigned int const b = random;
        vector<StateSiteLink> links_1, links_2;
        complex<double> inner_product = c(0,0);
        vector<complex<double> > data_1, data_2;
        BOOST_FOREACH(unsigned int const i, irange(1u,1u+b)) {
            complex<double> const x = random, y = random;
            inner_product += x*y;
            data_1.emplace_back(x);
            data_2.emplace_back(y);
            links_1.emplace_back(1,i,make_shared<Vector>(1,x));
            links_2.emplace_back(i,1,make_shared<Vector>(1,y));
        }
        StateSite<None>
             state_site_1 = constructStateSite
                (PhysicalDimension(1)
                ,LeftDimension(1)
                ,RightDimension(b)
                ,links_1
                )
            ,state_site_2 = constructStateSite
                (PhysicalDimension(1)
                ,LeftDimension(b)
                ,RightDimension(1)
                ,links_2
                )
            ;
        ASSERT_TRUE(boost::equal(state_site_1,data_1));
        ASSERT_TRUE(boost::equal(state_site_2,data_2));
        Vector state_vector = computeStateVector(list_of(&state_site_1)(&state_site_2) | indirected);
        ASSERT_EQ(1u,state_vector.size());
        ASSERT_NEAR_REL(inner_product,state_vector[0],1e-15);
    }
}
//@+node:gcross.20110827234144.2589: *4* W state
TEST_SUITE(W_state) {

//@+others
//@+node:gcross.20110827234144.2590: *5* two sites
TEST_CASE(two_sites) {
    StateSite<None>
         state_site_1 = constructStateSite
            (PhysicalDimension(2)
            ,LeftDimension(1)
            ,RightDimension(2)
            ,list_of(StateSiteLink(1,1,vectorFromRange(list_of(1)(0))))
                    (StateSiteLink(1,2,vectorFromRange(list_of(0)(-1))))
            )
        ,state_site_2 = constructStateSite
            (PhysicalDimension(2)
            ,LeftDimension(2)
            ,RightDimension(1)
            ,list_of(StateSiteLink(2,1,vectorFromRange(list_of(1)(0))))
                    (StateSiteLink(1,1,vectorFromRange(list_of(0)(1))))
            )
        ;
    Vector actual_state_vector = computeStateVector(list_of(&state_site_1)(&state_site_2) | indirected);
    ASSERT_EQ(4u,actual_state_vector.size());
    complex<double> correct_state_vector[] = {0,1,-1,0};
    ASSERT_TRUE(boost::equal(correct_state_vector,actual_state_vector));
}
//@+node:gcross.20110827234144.2591: *5* three sites
TEST_CASE(three_sites) {
    StateSite<None>
         state_site_1 = constructStateSite
            (PhysicalDimension(2)
            ,LeftDimension(1)
            ,RightDimension(2)
            ,list_of(StateSiteLink(1,1,vectorFromRange(list_of(1)(0))))
                    (StateSiteLink(1,2,vectorFromRange(list_of(0)(-1))))
            )
        ,state_site_2 = constructStateSite
            (PhysicalDimension(2)
            ,LeftDimension(2)
            ,RightDimension(2)
            ,list_of(StateSiteLink(1,1,vectorFromRange(list_of(1)(0))))
                    (StateSiteLink(2,2,vectorFromRange(list_of(1)(0))))
                    (StateSiteLink(1,2,vectorFromRange(list_of(0)(1))))
            )
        ,state_site_3 = constructStateSite
            (PhysicalDimension(2)
            ,LeftDimension(2)
            ,RightDimension(1)
            ,list_of(StateSiteLink(2,1,vectorFromRange(list_of(1)(0))))
                    (StateSiteLink(1,1,vectorFromRange(list_of(0)(2))))
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
    ASSERT_TRUE(boost::equal(correct_state_vector,actual_state_vector));
}
//@-others

}
//@+node:gcross.20110828143807.2636: *4* random site
TEST_CASE(random_site) {
    RNG random;

    REPEAT(100) {
        unsigned int const
              physical_dimension = random
            , left_dimension = random
            , right_dimension = random
            , increment = left_dimension*right_dimension
            ;
        StateSite<None> state_site_1 = StateSite<None>
            (PhysicalDimension(physical_dimension)
            ,LeftDimension(left_dimension)
            ,RightDimension(right_dimension)
            );
        complex<double>* left_right_ptr = state_site_1.begin();
        vector<StateSiteLink> links;
        BOOST_FOREACH(unsigned int left, irange(0u,left_dimension)) {
            BOOST_FOREACH(unsigned int right, irange(0u,right_dimension)) {
                complex<double>* current_ptr = left_right_ptr++;
                VectorPtr vector = make_shared<Vector>(physical_dimension);
                BOOST_FOREACH(unsigned int physical, irange(0u,physical_dimension)) {
                    complex<double> x = random;
                    (*vector)[physical] = x;
                    *current_ptr = x;
                    current_ptr += increment;
                }
                links.emplace_back(left+1,right+1,vector);
            }
        }
        StateSite<None> state_site_2 = constructStateSite
            (PhysicalDimension(physical_dimension)
            ,LeftDimension(left_dimension)
            ,RightDimension(right_dimension)
            ,links
            );
        ASSERT_EQ(state_site_1.physicalDimension(),state_site_2.physicalDimension());
        ASSERT_EQ(state_site_1.leftDimension(),state_site_2.leftDimension());
        ASSERT_EQ(state_site_1.rightDimension(),state_site_2.rightDimension());
        ASSERT_TRUE(boost::equal(state_site_1,state_site_2));
    }
}
//@-others

}
//@+node:gcross.20110828142008.2597: *3* normalization
TEST_CASE(normalization) {
    RNG random;

    REPEAT(100) {
        State state = random.randomState();
        ASSERT_NEAR_ABS_VAL(computeStateOverlap(state,state),c(1,0),1e-13);
    }
}
//@+node:gcross.20110828142008.2596: *3* range constructor
TEST_CASE(range_constructor) {
    RNG random;

    REPEAT(100) {
        State const
            state_1 = random.randomState(),
            state_2(copyFrom(state_1));
        Vector const
            v1 = computeStateVector(state_1),
            v2 = computeStateVector(state_2);
        ASSERT_NEAR_ABS_VAL(computeStateOverlap(state_1,state_1),c(1,0),1e-13);
        ASSERT_NEAR_ABS_VAL(computeStateOverlap(state_2,state_2),c(1,0),1e-13);
        ASSERT_NEAR_ABS_VAL(computeStateOverlap(state_1,state_2),c(1,0),1e-13);
        ASSERT_NEAR_ABS_VAL(computeStateOverlap(state_2,state_1),c(1,0),1e-13);
        ASSERT_EQ(v1.size(),v2.size());
        BOOST_FOREACH(size_t const index, irange<size_t>(0u,v1.size())) {
            ASSERT_NEAR_REL(v1[index],v2[index],1e-13);
        }
    }
}
//@-others

}
//@-others
//@-leo
