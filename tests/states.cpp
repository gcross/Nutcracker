#include <boost/range/adaptor/indirected.hpp>
#include <complex>
#include <illuminate.hpp>

#include "nutcracker/states.hpp"

#include "test_utils.hpp"

using boost::adaptors::indirected;

using std::abs;

TEST_SUITE(States) {

TEST_SUITE(computeStateOverlap) {

TEST_CASE(self_overlap_is_1) {

    RNG random;

    REPEAT(10) {
        State state(random.randomState());
        ASSERT_NEAR_REL_VAL(
             computeStateOverlap(state,state)
            ,c(1,0)
            ,1e-14
        )
    }

}
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
            ,1e-12
        )
    }

}
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

}
TEST_SUITE(constructStateSite) {

TEST_SUITE(single_site) {
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
}
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
TEST_SUITE(W_state) {

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

}
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

}
TEST_CASE(normalization) {
    RNG random;

    REPEAT(100) {
        State state = random.randomState();
        ASSERT_NEAR_ABS_VAL(computeStateOverlap(state,state),c(1,0),1e-13);
    }
}
TEST_SUITE(range_constructor) {
TEST_CASE(correct_on_self) {
    RNG random;

    REPEAT(100) {
        State const
            state_1 = random.randomState(),
            state_2(copyFrom(state_1));
        Vector const
            v1 = computeStateVector(state_1),
            v2 = computeStateVector(state_2);
        ASSERT_NEAR_ABS_VAL(computeStateOverlap(state_1,state_1),c(1,0),1e-12);
        ASSERT_NEAR_ABS_VAL(computeStateOverlap(state_2,state_2),c(1,0),1e-12);
        ASSERT_NEAR_ABS_VAL(computeStateOverlap(state_1,state_2),c(1,0),1e-12);
        ASSERT_NEAR_ABS_VAL(computeStateOverlap(state_2,state_1),c(1,0),1e-12);
        ASSERT_EQ(v1.size(),v2.size());
        BOOST_FOREACH(size_t const index, irange<size_t>(0u,v1.size())) {
            ASSERT_NEAR_REL(v1[index],v2[index],1e-12);
        }
    }
}
TEST_CASE(correct_on_random) {
    RNG random;

    REPEAT(100) {
        unsigned int number_of_sites = random(1,6);
        vector<unsigned int> const
            physical_dimensions = random.randomUnsignedIntegerVector(number_of_sites),
            bandwidth_dimensions = computeBandwidthDimensionSequence(random(1,maximumBandwidthDimension(physical_dimensions)),physical_dimensions);
        vector<StateSite<Middle> > sites;
        BOOST_FOREACH(unsigned int const site_number, irange(0u,number_of_sites)) {
            sites.push_back(randomStateSiteMiddle(
                PhysicalDimension(physical_dimensions[site_number]),
                LeftDimension(bandwidth_dimensions[site_number]),
                RightDimension(bandwidth_dimensions[site_number+1])
            ));
        }
        State state(copyFrom(sites));
        ASSERT_NEAR_ABS_VAL(computeStateOverlap(state,state),c(1,0),1e-13);
        ASSERT_NEAR_ABS_VAL(computeStateOverlap(state,sites)/sqrt(computeStateOverlap(sites,sites)),c(1,0),1e-13);
    }
}
}

}
