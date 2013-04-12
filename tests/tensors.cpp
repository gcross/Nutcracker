#include <algorithm>
#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <complex>
#include <illuminate.hpp>
#include <sstream>

#include "nutcracker/boundaries.hpp"
#include "nutcracker/operators.hpp"
#include "nutcracker/states.hpp"
#include "nutcracker/utilities.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::equal;

using std::abs;
using std::endl;
using std::equal;
using std::istringstream;
using std::ostringstream;
using std::stringstream;

TEST_SUITE(Tensors) {

TEST_SUITE(ExpectationBoundary) {

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

}
TEST_SUITE(StateSite) {

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
TEST_SUITE(serialization) {

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

}

}
TEST_SUITE(OperatorSite) {

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

}
TEST_SUITE(OverlapBoundary) {

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

}
TEST_SUITE(OverlapSite) {

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

}

}

