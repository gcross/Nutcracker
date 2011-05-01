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
        ASSERT_EQ(operator_dimension,old_boundary.operatorDimension());
        ASSERT_EQ(state_dimension,old_boundary.stateDimension());

        ExpectationBoundary<Left> new_boundary;

        ASSERT_FALSE(new_boundary.valid());
        ASSERT_EQ(0,new_boundary.operatorDimension(as_unsigned_integer));
        ASSERT_EQ(0,new_boundary.stateDimension(as_unsigned_integer));

        new_boundary = boost::move(old_boundary);

        ASSERT_FALSE(old_boundary.valid());
        ASSERT_EQ(0,old_boundary.operatorDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_boundary.stateDimension(as_unsigned_integer));

        ASSERT_TRUE(new_boundary);
        ASSERT_EQ(operator_dimension,new_boundary.operatorDimension());
        ASSERT_EQ(state_dimension,new_boundary.stateDimension());
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
        ASSERT_EQ(operator_dimension,old_boundary.operatorDimension());
        ASSERT_EQ(state_dimension,old_boundary.stateDimension());

        ExpectationBoundary<Left> new_boundary(boost::move(old_boundary));

        ASSERT_FALSE(old_boundary.valid());
        ASSERT_EQ(0,old_boundary.operatorDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_boundary.stateDimension(as_unsigned_integer));

        ASSERT_TRUE(new_boundary.valid());
        ASSERT_EQ(operator_dimension,new_boundary.operatorDimension());
        ASSERT_EQ(state_dimension,new_boundary.stateDimension());
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
        ASSERT_EQ(physical_dimension,old_site.physicalDimension());
        ASSERT_EQ(left_dimension,old_site.leftDimension());
        ASSERT_EQ(right_dimension,old_site.rightDimension());

        StateSite<Middle> new_site;

        ASSERT_FALSE(new_site.valid());
        ASSERT_EQ(0,new_site.physicalDimension(as_unsigned_integer));
        ASSERT_EQ(0,new_site.leftDimension(as_unsigned_integer));
        ASSERT_EQ(0,new_site.rightDimension(as_unsigned_integer));

        new_site = boost::move(old_site);

        ASSERT_FALSE(old_site.valid());
        ASSERT_EQ(0,old_site.physicalDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.leftDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.rightDimension(as_unsigned_integer));

        ASSERT_TRUE(new_site.valid());
        ASSERT_EQ(physical_dimension,new_site.physicalDimension());
        ASSERT_EQ(left_dimension,new_site.leftDimension());
        ASSERT_EQ(right_dimension,new_site.rightDimension());
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
        ASSERT_EQ(physical_dimension,old_site.physicalDimension());
        ASSERT_EQ(left_dimension,old_site.leftDimension());
        ASSERT_EQ(right_dimension,old_site.rightDimension());

        StateSite<Middle> new_site(boost::move(old_site));

        ASSERT_FALSE(old_site.valid());
        ASSERT_EQ(0,old_site.physicalDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.leftDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.rightDimension(as_unsigned_integer));

        ASSERT_TRUE(new_site.valid());
        ASSERT_EQ(physical_dimension,new_site.physicalDimension());
        ASSERT_EQ(left_dimension,new_site.leftDimension());
        ASSERT_EQ(right_dimension,new_site.rightDimension());
    }
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
        ASSERT_EQ(physical_dimension,old_site.physicalDimension());
        ASSERT_EQ(left_dimension,old_site.leftDimension());
        ASSERT_EQ(right_dimension,old_site.rightDimension());

        OperatorSite new_site;

        ASSERT_FALSE(new_site.valid());
        ASSERT_EQ(0,new_site.numberOfMatrices());
        ASSERT_EQ(0,new_site.physicalDimension(as_unsigned_integer));
        ASSERT_EQ(0,new_site.leftDimension(as_unsigned_integer));
        ASSERT_EQ(0,new_site.rightDimension(as_unsigned_integer));

        new_site = boost::move(old_site);

        ASSERT_FALSE(old_site.valid());
        ASSERT_EQ(0,old_site.numberOfMatrices());
        ASSERT_EQ(0,old_site.physicalDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.leftDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.rightDimension(as_unsigned_integer));

        ASSERT_TRUE(new_site.valid());
        ASSERT_EQ(number_of_matrices,new_site.numberOfMatrices());
        ASSERT_EQ(physical_dimension,new_site.physicalDimension());
        ASSERT_EQ(left_dimension,new_site.leftDimension());
        ASSERT_EQ(right_dimension,new_site.rightDimension());
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
        ASSERT_EQ(physical_dimension,old_site.physicalDimension());
        ASSERT_EQ(left_dimension,old_site.leftDimension());
        ASSERT_EQ(right_dimension,old_site.rightDimension());

        OperatorSite new_site(boost::move(old_site));

        ASSERT_FALSE(old_site.valid());
        ASSERT_EQ(0,old_site.numberOfMatrices());
        ASSERT_EQ(0,old_site.physicalDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.leftDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.rightDimension(as_unsigned_integer));

        ASSERT_TRUE(new_site.valid());
        ASSERT_EQ(number_of_matrices,new_site.numberOfMatrices());
        ASSERT_EQ(physical_dimension,new_site.physicalDimension());
        ASSERT_EQ(left_dimension,new_site.leftDimension());
        ASSERT_EQ(right_dimension,new_site.rightDimension());
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
        ASSERT_EQ(overlap_dimension,old_boundary.overlapDimension());
        ASSERT_EQ(state_dimension,old_boundary.stateDimension());

        OverlapBoundary<Left> new_boundary;

        ASSERT_FALSE(new_boundary.valid());
        ASSERT_EQ(0,new_boundary.overlapDimension(as_unsigned_integer));
        ASSERT_EQ(0,new_boundary.stateDimension(as_unsigned_integer));

        new_boundary = boost::move(old_boundary);

        ASSERT_FALSE(old_boundary.valid());
        ASSERT_EQ(0,old_boundary.overlapDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_boundary.stateDimension(as_unsigned_integer));

        ASSERT_TRUE(new_boundary.valid());
        ASSERT_EQ(overlap_dimension,new_boundary.overlapDimension());
        ASSERT_EQ(state_dimension,new_boundary.stateDimension());
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
        ASSERT_EQ(overlap_dimension,old_boundary.overlapDimension());
        ASSERT_EQ(state_dimension,old_boundary.stateDimension());

        OverlapBoundary<Left> new_boundary(boost::move(old_boundary));

        ASSERT_FALSE(old_boundary.valid());
        ASSERT_EQ(0,old_boundary.overlapDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_boundary.stateDimension(as_unsigned_integer));

        ASSERT_TRUE(new_boundary.valid());
        ASSERT_EQ(overlap_dimension,new_boundary.overlapDimension());
        ASSERT_EQ(state_dimension,new_boundary.stateDimension());
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
        ASSERT_EQ(physical_dimension,old_site.physicalDimension());
        ASSERT_EQ(left_dimension,old_site.leftDimension());
        ASSERT_EQ(right_dimension,old_site.rightDimension());

        OverlapSite<Middle> new_site;

        ASSERT_FALSE(new_site.valid());
        ASSERT_EQ(0,new_site.physicalDimension(as_unsigned_integer));
        ASSERT_EQ(0,new_site.leftDimension(as_unsigned_integer));
        ASSERT_EQ(0,new_site.rightDimension(as_unsigned_integer));

        new_site = boost::move(old_site);

        ASSERT_FALSE(old_site.valid());
        ASSERT_EQ(0,old_site.physicalDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.leftDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.rightDimension(as_unsigned_integer));

        ASSERT_TRUE(new_site.valid());
        ASSERT_EQ(physical_dimension,new_site.physicalDimension());
        ASSERT_EQ(left_dimension,new_site.leftDimension());
        ASSERT_EQ(right_dimension,new_site.rightDimension());
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
        ASSERT_EQ(physical_dimension,old_site.physicalDimension());
        ASSERT_EQ(left_dimension,old_site.leftDimension());
        ASSERT_EQ(right_dimension,old_site.rightDimension());

        OverlapSite<Middle> new_site(boost::move(old_site));

        ASSERT_FALSE(old_site.valid());
        ASSERT_EQ(0,old_site.physicalDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.leftDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.rightDimension(as_unsigned_integer));

        ASSERT_TRUE(new_site.valid());
        ASSERT_EQ(physical_dimension,new_site.physicalDimension());
        ASSERT_EQ(left_dimension,new_site.leftDimension());
        ASSERT_EQ(right_dimension,new_site.rightDimension());
    }
}
//@-others

}
//@-others

}
//@-others
//@-leo
