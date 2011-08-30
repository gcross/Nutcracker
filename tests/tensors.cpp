//@+leo-ver=5-thin
//@+node:gcross.20110204201608.1725: * @file tensors.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2058: ** << License >>
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
//@+node:gcross.20110204201608.1726: ** << Includes >>
#include <algorithm>
#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <complex>
#include <illuminate.hpp>
#include <sstream>

#include "boundaries.hpp"
#include "operators.hpp"
#include "states.hpp"
#include "utilities.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::equal;

using std::abs;
using std::endl;
using std::equal;
using std::istringstream;
using std::ostringstream;
using std::stringstream;
//@-<< Includes >>

//@+others
//@+node:gcross.20110204201608.1727: ** Tests
TEST_SUITE(Tensors) {

//@+others
//@+node:gcross.20110204201608.1751: *3* ExpectationBoundary
TEST_SUITE(ExpectationBoundary) {

//@+others
//@+node:gcross.20110204201608.1752: *4* move assignable
TEST_CASE(move_assignable) {
    RNG random;

    REPEAT(10) {
        OperatorDimension const operator_dimension(random);
        StateDimension const state_dimension(random);

        ExpectationBoundary<Left> old_boundary
            (operator_dimension
            ,state_dimension
            ,fillWithGenerator(random.randomComplexDouble)
            );

        ASSERT_TRUE(old_boundary.valid());
        ASSERT_EQ(operator_dimension,old_boundary.operatorDimension(as_dimension));
        ASSERT_EQ(state_dimension,old_boundary.stateDimension(as_dimension));

        ExpectationBoundary<Left> new_boundary;

        ASSERT_FALSE(new_boundary.valid());
        ASSERT_EQ(0u,new_boundary.operatorDimension());
        ASSERT_EQ(0u,new_boundary.stateDimension());

        new_boundary = boost::move(old_boundary);

        ASSERT_FALSE(old_boundary.valid());
        ASSERT_EQ(0u,old_boundary.operatorDimension());
        ASSERT_EQ(0u,old_boundary.stateDimension());

        ASSERT_TRUE(new_boundary);
        ASSERT_EQ(operator_dimension,new_boundary.operatorDimension(as_dimension));
        ASSERT_EQ(state_dimension,new_boundary.stateDimension(as_dimension));
    }
}
//@+node:gcross.20110204201608.1755: *4* move constructable
TEST_CASE(move_constructable) {
    RNG random;

    REPEAT(10) {
        OperatorDimension const operator_dimension(random);
        StateDimension const state_dimension(random);

        ExpectationBoundary<Left> old_boundary
            (operator_dimension
            ,state_dimension
            ,fillWithGenerator(random.randomComplexDouble)
            );

        ASSERT_TRUE(old_boundary.valid());
        ASSERT_EQ(operator_dimension,old_boundary.operatorDimension(as_dimension));
        ASSERT_EQ(state_dimension,old_boundary.stateDimension(as_dimension));

        ExpectationBoundary<Left> new_boundary(boost::move(old_boundary));

        ASSERT_FALSE(old_boundary.valid());
        ASSERT_EQ(0u,old_boundary.operatorDimension());
        ASSERT_EQ(0u,old_boundary.stateDimension());

        ASSERT_TRUE(new_boundary.valid());
        ASSERT_EQ(operator_dimension,new_boundary.operatorDimension(as_dimension));
        ASSERT_EQ(state_dimension,new_boundary.stateDimension(as_dimension));
    }
}
//@-others

}
//@+node:gcross.20110204201608.1742: *3* StateSite
TEST_SUITE(StateSite) {

//@+others
//@+node:gcross.20110204201608.1743: *4* move assignable
TEST_CASE(move_assignable) {
    RNG random;

    REPEAT(10) {
        PhysicalDimension const physical_dimension(random);
        LeftDimension const left_dimension(random);
        RightDimension const right_dimension(random);

        StateSite<Middle> old_site
            (physical_dimension
            ,left_dimension
            ,right_dimension
            ,fillWithGenerator(random.randomComplexDouble)
            );

        ASSERT_TRUE(old_site.valid());
        ASSERT_EQ(physical_dimension,old_site.physicalDimension(as_dimension));
        ASSERT_EQ(left_dimension,old_site.leftDimension(as_dimension));
        ASSERT_EQ(right_dimension,old_site.rightDimension(as_dimension));

        StateSite<Middle> new_site;

        ASSERT_FALSE(new_site.valid());
        ASSERT_EQ(0u,new_site.physicalDimension());
        ASSERT_EQ(0u,new_site.leftDimension());
        ASSERT_EQ(0u,new_site.rightDimension());

        new_site = boost::move(old_site);

        ASSERT_FALSE(old_site.valid());
        ASSERT_EQ(0u,old_site.physicalDimension());
        ASSERT_EQ(0u,old_site.leftDimension());
        ASSERT_EQ(0u,old_site.rightDimension());

        ASSERT_TRUE(new_site.valid());
        ASSERT_EQ(physical_dimension,new_site.physicalDimension(as_dimension));
        ASSERT_EQ(left_dimension,new_site.leftDimension(as_dimension));
        ASSERT_EQ(right_dimension,new_site.rightDimension(as_dimension));
    }
}
//@+node:gcross.20110204201608.1747: *4* move constructable
TEST_CASE(move_constructable) {
    RNG random;

    REPEAT(10) {
        PhysicalDimension const physical_dimension(random);
        LeftDimension const left_dimension(random);
        RightDimension const right_dimension(random);

        StateSite<Middle> old_site
            (physical_dimension
            ,left_dimension
            ,right_dimension
            ,fillWithGenerator(random.randomComplexDouble)
            );

        ASSERT_TRUE(old_site.valid());
        ASSERT_EQ(physical_dimension,old_site.physicalDimension(as_dimension));
        ASSERT_EQ(left_dimension,old_site.leftDimension(as_dimension));
        ASSERT_EQ(right_dimension,old_site.rightDimension(as_dimension));

        StateSite<Middle> new_site(boost::move(old_site));

        ASSERT_FALSE(old_site.valid());
        ASSERT_EQ(0u,old_site.physicalDimension());
        ASSERT_EQ(0u,old_site.leftDimension());
        ASSERT_EQ(0u,old_site.rightDimension());

        ASSERT_TRUE(new_site.valid());
        ASSERT_EQ(physical_dimension,new_site.physicalDimension(as_dimension));
        ASSERT_EQ(left_dimension,new_site.leftDimension(as_dimension));
        ASSERT_EQ(right_dimension,new_site.rightDimension(as_dimension));
    }
}
//@+node:gcross.20110827234144.2626: *4* normalizable
TEST_CASE(normalizable) {
    RNG random;

    REPEAT(10) {
        State const state_1 = random.randomState();
        vector<StateSite<Middle> > state_2;
        BOOST_FOREACH(StateSiteAny const& state_site, state_1) {
            state_2.push_back(state_site.normalize());
        }
        Vector
            v1 = computeStateVector(state_1),
            v2 = computeStateVector(state_2);
        complex<double> ratio = v1[0]/v2[0];
        BOOST_FOREACH(unsigned int const index, irange<size_t>(1u,v1.size())) {
            ASSERT_NEAR_REL(v1[index]/v2[index],ratio,1e-13);
        }
    }
}
//@+node:gcross.20110829224358.2649: *4* serialization
TEST_SUITE(serialization) {

//@+others
//@+node:gcross.20110829224358.2653: *5* data
TEST_CASE(data) {
    RNG random;

    REPEAT(10) {
        PhysicalDimension const physical_dimension(random);
        LeftDimension const left_dimension(random);
        RightDimension const right_dimension(random);

        StateSite<None> state_site_tensor_1
            (physical_dimension
            ,left_dimension
            ,right_dimension
            ,fillWithGenerator(random.randomComplexDouble)
            );

        stringstream buffer;
        boost::archive::text_oarchive(buffer) << state_site_tensor_1;

        StateSite<None> state_site_tensor_2 = StateSite<None>
            (PhysicalDimension(random)
            ,LeftDimension(random)
            ,RightDimension(random)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        boost::archive::text_iarchive(buffer) >> state_site_tensor_2;

        checkSiteTensorsEqual(state_site_tensor_1,state_site_tensor_2);
    }
}
//@+node:gcross.20110829224358.2664: *5* normalization
struct NormalizationBuffer {
    string data;
    template<typename Side> NormalizationBuffer(Side* _) {
        ostringstream buffer;
        boost::archive::text_oarchive(buffer) << StateSite<Side>::trivial;
        data = buffer.str();
    }
    template<typename Item> void operator>>(Item& item) const {
        istringstream buffer(data);
        boost::archive::text_iarchive(buffer) >> item;
    }
};

TEST_CASE(normalization) {
    NormalizationBuffer const
          left_buffer((Left*)NULL)
        , middle_buffer((Middle*)NULL)
        , right_buffer((Right*)NULL)
        , none_buffer((None*)NULL)
        ;

    StateSite<Left> left_site;
    StateSite<Middle> middle_site;
    StateSite<Right> right_site;
    StateSite<None> none_site;

    left_buffer >> none_site;
    middle_buffer >> none_site;
    right_buffer >> none_site;

    left_buffer >> left_site;
    middle_buffer >> middle_site;
    right_buffer >> right_site;

    try {
        left_buffer >> middle_site;
        FAIL("A middle-normalized state site accepted a left-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        left_buffer >> right_site;
        FAIL("A right-normalized state site accepted a left-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        middle_buffer >> left_site;
        FAIL("A left-normalized state site accepted an middle-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        middle_buffer >> right_site;
        FAIL("A right-normalized state site accepted a middle-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        right_buffer >> left_site;
        FAIL("A left-normalized state site accepted a right-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        right_buffer >> middle_site;
        FAIL("A middle-normalized state site accepted a right-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        none_buffer >> left_site;
        FAIL("A left-normalized state site accepted an unnormalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        none_buffer >> middle_site;
        FAIL("A middle-normalized state site accepted an unnormalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        none_buffer >> right_site;
        FAIL("A right-normalized state site accepted an unnormalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}
}
//@-others

}
//@-others

}
//@+node:gcross.20110204201608.1733: *3* OperatorSite
TEST_SUITE(OperatorSite) {

//@+others
//@+node:gcross.20110204201608.1736: *4* move assignable
TEST_CASE(move_assignable) {
    RNG random;

    REPEAT(10) {
        unsigned int const number_of_matrices = random;
        PhysicalDimension const physical_dimension(random);
        LeftDimension const left_dimension(random);
        RightDimension const right_dimension(random);

        OperatorSite old_site
            (number_of_matrices
            ,physical_dimension
            ,left_dimension
            ,right_dimension
            ,fillWithGenerator(random.generateRandomIndices(left_dimension,right_dimension))
            ,fillWithGenerator(random.randomComplexDouble)
            );

        ASSERT_TRUE(old_site.valid());
        ASSERT_EQ(number_of_matrices,old_site.numberOfMatrices());
        ASSERT_EQ(physical_dimension,old_site.physicalDimension(as_dimension));
        ASSERT_EQ(left_dimension,old_site.leftDimension(as_dimension));
        ASSERT_EQ(right_dimension,old_site.rightDimension(as_dimension));

        OperatorSite new_site;

        ASSERT_FALSE(new_site.valid());
        ASSERT_EQ(0u,new_site.numberOfMatrices());
        ASSERT_EQ(0u,new_site.physicalDimension());
        ASSERT_EQ(0u,new_site.leftDimension());
        ASSERT_EQ(0u,new_site.rightDimension());

        new_site = boost::move(old_site);

        ASSERT_FALSE(old_site.valid());
        ASSERT_EQ(0u,old_site.numberOfMatrices());
        ASSERT_EQ(0u,old_site.physicalDimension());
        ASSERT_EQ(0u,old_site.leftDimension());
        ASSERT_EQ(0u,old_site.rightDimension());

        ASSERT_TRUE(new_site.valid());
        ASSERT_EQ(number_of_matrices,new_site.numberOfMatrices());
        ASSERT_EQ(physical_dimension,new_site.physicalDimension(as_dimension));
        ASSERT_EQ(left_dimension,new_site.leftDimension(as_dimension));
        ASSERT_EQ(right_dimension,new_site.rightDimension(as_dimension));
    }
}
//@+node:gcross.20110204201608.1734: *4* move constructable
TEST_CASE(move_constructable) {
    RNG random;

    REPEAT(10) {
        unsigned int const number_of_matrices = random;
        PhysicalDimension const physical_dimension(random);
        LeftDimension const left_dimension(random);
        RightDimension const right_dimension(random);

        OperatorSite old_site
            (number_of_matrices
            ,physical_dimension
            ,left_dimension
            ,right_dimension
            ,fillWithGenerator(random.generateRandomIndices(left_dimension,right_dimension))
            ,fillWithGenerator(random.randomComplexDouble)
            );

        ASSERT_TRUE(old_site.valid());
        ASSERT_EQ(number_of_matrices,old_site.numberOfMatrices());
        ASSERT_EQ(physical_dimension,old_site.physicalDimension(as_dimension));
        ASSERT_EQ(left_dimension,old_site.leftDimension(as_dimension));
        ASSERT_EQ(right_dimension,old_site.rightDimension(as_dimension));

        OperatorSite new_site(boost::move(old_site));

        ASSERT_FALSE(old_site.valid());
        ASSERT_EQ(0u,old_site.numberOfMatrices());
        ASSERT_EQ(0u,old_site.physicalDimension());
        ASSERT_EQ(0u,old_site.leftDimension());
        ASSERT_EQ(0u,old_site.rightDimension());

        ASSERT_TRUE(new_site.valid());
        ASSERT_EQ(number_of_matrices,new_site.numberOfMatrices());
        ASSERT_EQ(physical_dimension,new_site.physicalDimension(as_dimension));
        ASSERT_EQ(left_dimension,new_site.leftDimension(as_dimension));
        ASSERT_EQ(right_dimension,new_site.rightDimension(as_dimension));
    }
}
//@+node:gcross.20110204201608.1737: *4* random generator
TEST_CASE(random_generator) {
    RNG random;

    REPEAT(10) {
        PhysicalDimension physical_dimension(random);
        LeftDimension left_dimension(random);
        RightDimension right_dimension(random);
        OperatorSite operator_site_1(random.randomOperatorSite(physical_dimension,left_dimension,right_dimension));
        ASSERT_TRUE(operator_site_1.valid());
        OperatorSite operator_site_2;
        ASSERT_FALSE(operator_site_2.valid());
        operator_site_2 = random.randomOperatorSite(physical_dimension,left_dimension,right_dimension);
        ASSERT_TRUE(operator_site_2.valid());
    }
}
//@+node:gcross.20110829224358.2670: *4* serialization
TEST_CASE(encode_then_decode) {
    RNG random;

    REPEAT(10) {
        OperatorSite operator_site_tensor_1(random.randomOperatorSite());

        stringstream buffer;
        boost::archive::text_oarchive(buffer) << operator_site_tensor_1;

        OperatorSite operator_site_tensor_2(random.randomOperatorSite());
        boost::archive::text_iarchive(buffer) >> operator_site_tensor_2;

        checkOperatorSitesEqual(operator_site_tensor_1,operator_site_tensor_2);
    }
}
//@-others

}
//@+node:gcross.20110204201608.1759: *3* OverlapBoundary
TEST_SUITE(OverlapBoundary) {

//@+others
//@+node:gcross.20110204201608.1760: *4* move assignable
TEST_CASE(move_assignable) {
    RNG random;

    REPEAT(10) {
        OverlapDimension const overlap_dimension(random);
        StateDimension const state_dimension(random);

        OverlapBoundary<Left> old_boundary
            (overlap_dimension
            ,state_dimension
            ,fillWithGenerator(random.randomComplexDouble)
            );

        ASSERT_TRUE(old_boundary.valid());
        ASSERT_EQ(overlap_dimension,old_boundary.overlapDimension(as_dimension));
        ASSERT_EQ(state_dimension,old_boundary.stateDimension(as_dimension));

        OverlapBoundary<Left> new_boundary;

        ASSERT_FALSE(new_boundary.valid());
        ASSERT_EQ(0u,new_boundary.overlapDimension());
        ASSERT_EQ(0u,new_boundary.stateDimension());

        new_boundary = boost::move(old_boundary);

        ASSERT_FALSE(old_boundary.valid());
        ASSERT_EQ(0u,old_boundary.overlapDimension());
        ASSERT_EQ(0u,old_boundary.stateDimension());

        ASSERT_TRUE(new_boundary.valid());
        ASSERT_EQ(overlap_dimension,new_boundary.overlapDimension(as_dimension));
        ASSERT_EQ(state_dimension,new_boundary.stateDimension(as_dimension));
    }
}
//@+node:gcross.20110204201608.1761: *4* move constructable
TEST_CASE(move_constructable) {
    RNG random;

    REPEAT(10) {
        OverlapDimension const overlap_dimension(random);
        StateDimension const state_dimension(random);

        OverlapBoundary<Left> old_boundary
            (overlap_dimension
            ,state_dimension
            ,fillWithGenerator(random.randomComplexDouble)
            );

        ASSERT_TRUE(old_boundary.valid());
        ASSERT_EQ(overlap_dimension,old_boundary.overlapDimension(as_dimension));
        ASSERT_EQ(state_dimension,old_boundary.stateDimension(as_dimension));

        OverlapBoundary<Left> new_boundary(boost::move(old_boundary));

        ASSERT_FALSE(old_boundary.valid());
        ASSERT_EQ(0u,old_boundary.overlapDimension());
        ASSERT_EQ(0u,old_boundary.stateDimension());

        ASSERT_TRUE(new_boundary.valid());
        ASSERT_EQ(overlap_dimension,new_boundary.overlapDimension(as_dimension));
        ASSERT_EQ(state_dimension,new_boundary.stateDimension(as_dimension));
    }
}
//@-others

}
//@+node:gcross.20110204201608.2062: *3* OverlapSite
TEST_SUITE(OverlapSite) {

//@+others
//@+node:gcross.20110204201608.2063: *4* move assignable
TEST_CASE(move_assignable) {
    RNG random;

    REPEAT(10) {
        PhysicalDimension const physical_dimension(random);
        LeftDimension const left_dimension(random);
        RightDimension const right_dimension(random);

        OverlapSite<Middle> old_site
            (right_dimension
            ,physical_dimension
            ,left_dimension
            ,fillWithGenerator(random.randomComplexDouble)
            );

        ASSERT_TRUE(old_site.valid());
        ASSERT_EQ(physical_dimension,old_site.physicalDimension(as_dimension));
        ASSERT_EQ(left_dimension,old_site.leftDimension(as_dimension));
        ASSERT_EQ(right_dimension,old_site.rightDimension(as_dimension));

        OverlapSite<Middle> new_site;

        ASSERT_FALSE(new_site.valid());
        ASSERT_EQ(0u,new_site.physicalDimension());
        ASSERT_EQ(0u,new_site.leftDimension());
        ASSERT_EQ(0u,new_site.rightDimension());

        new_site = boost::move(old_site);

        ASSERT_FALSE(old_site.valid());
        ASSERT_EQ(0u,old_site.physicalDimension());
        ASSERT_EQ(0u,old_site.leftDimension());
        ASSERT_EQ(0u,old_site.rightDimension());

        ASSERT_TRUE(new_site.valid());
        ASSERT_EQ(physical_dimension,new_site.physicalDimension(as_dimension));
        ASSERT_EQ(left_dimension,new_site.leftDimension(as_dimension));
        ASSERT_EQ(right_dimension,new_site.rightDimension(as_dimension));
    }
}
//@+node:gcross.20110204201608.2064: *4* move constructable
TEST_CASE(move_constructable) {
    RNG random;

    REPEAT(10) {
        PhysicalDimension const physical_dimension(random);
        LeftDimension const left_dimension(random);
        RightDimension const right_dimension(random);

        OverlapSite<Middle> old_site
            (right_dimension
            ,physical_dimension
            ,left_dimension
            ,fillWithGenerator(random.randomComplexDouble)
            );

        ASSERT_TRUE(old_site.valid());
        ASSERT_EQ(physical_dimension,old_site.physicalDimension(as_dimension));
        ASSERT_EQ(left_dimension,old_site.leftDimension(as_dimension));
        ASSERT_EQ(right_dimension,old_site.rightDimension(as_dimension));

        OverlapSite<Middle> new_site(boost::move(old_site));

        ASSERT_FALSE(old_site.valid());
        ASSERT_EQ(0u,old_site.physicalDimension());
        ASSERT_EQ(0u,old_site.leftDimension());
        ASSERT_EQ(0u,old_site.rightDimension());

        ASSERT_TRUE(new_site.valid());
        ASSERT_EQ(physical_dimension,new_site.physicalDimension(as_dimension));
        ASSERT_EQ(left_dimension,new_site.leftDimension(as_dimension));
        ASSERT_EQ(right_dimension,new_site.rightDimension(as_dimension));
    }
}
//@-others

}
//@-others

}
//@-others
//@-leo
