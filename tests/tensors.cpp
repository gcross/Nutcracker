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
#include <illuminate.hpp>
#include <sstream>

#include "boundaries.hpp"
#include "operators.hpp"
#include "states.hpp"
#include "utilities.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::equal;

using std::endl;
using std::equal;
using std::istringstream;
using std::ostringstream;
//@-<< Includes >>

//@+others
//@+node:gcross.20110220141808.1989: ** Functions
//@+node:gcross.20110220141808.1990: *3* checkOperatorSitesEqual
void checkOperatorSitesEqual(OperatorSite const& operator_site_1,OperatorSite const& operator_site_2) {
    ASSERT_EQ(operator_site_1.physicalDimension(),operator_site_2.physicalDimension());
    ASSERT_EQ(operator_site_1.leftDimension(),operator_site_2.leftDimension());
    ASSERT_EQ(operator_site_1.rightDimension(),operator_site_2.rightDimension());
    ASSERT_EQ(operator_site_1.numberOfMatrices(),operator_site_2.numberOfMatrices());
    ASSERT_TRUE(equal(operator_site_1,operator_site_2));
    ASSERT_TRUE(equal((uint32_t const*)operator_site_1,((uint32_t const*)operator_site_1)+2*operator_site_1.numberOfMatrices(),(uint32_t const*)operator_site_2));
}
//@+node:gcross.20110220141808.1994: *3* checkOperatorsEqual
void checkOperatorsEqual(Operator const& operator_1,Operator const& operator_2) {
    ASSERT_EQ(operator_1.size(),operator_2.size());
    BOOST_FOREACH(unsigned int const i, irange(0u,(unsigned int)operator_1.size())) {
        checkOperatorSitesEqual(*operator_1[i],*operator_2[i]);
    }
}
//@+node:gcross.20110220093853.1987: ** Tests
//@+node:gcross.20110204201608.1727: *3* Tensors
TEST_SUITE(Tensors) {

//@+others
//@+node:gcross.20110204201608.1751: *4* ExpectationBoundary
TEST_SUITE(ExpectationBoundary) {

//@+others
//@+node:gcross.20110204201608.1752: *5* move assignable
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
        ASSERT_EQ_QUOTED(operator_dimension,old_boundary.operatorDimension());
        ASSERT_EQ_QUOTED(state_dimension,old_boundary.stateDimension());

        ExpectationBoundary<Left> new_boundary;

        ASSERT_FALSE(new_boundary.valid());
        ASSERT_EQ(0,new_boundary.operatorDimension(as_unsigned_integer));
        ASSERT_EQ(0,new_boundary.stateDimension(as_unsigned_integer));

        new_boundary = boost::move(old_boundary);

        ASSERT_FALSE(old_boundary.valid());
        ASSERT_EQ(0,old_boundary.operatorDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_boundary.stateDimension(as_unsigned_integer));

        ASSERT_TRUE(new_boundary);
        ASSERT_EQ_QUOTED(operator_dimension,new_boundary.operatorDimension());
        ASSERT_EQ_QUOTED(state_dimension,new_boundary.stateDimension());
    }
}
//@+node:gcross.20110204201608.1755: *5* move constructable
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
        ASSERT_EQ_QUOTED(operator_dimension,old_boundary.operatorDimension());
        ASSERT_EQ_QUOTED(state_dimension,old_boundary.stateDimension());

        ExpectationBoundary<Left> new_boundary(boost::move(old_boundary));

        ASSERT_FALSE(old_boundary.valid());
        ASSERT_EQ(0,old_boundary.operatorDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_boundary.stateDimension(as_unsigned_integer));

        ASSERT_TRUE(new_boundary.valid());
        ASSERT_EQ_QUOTED(operator_dimension,new_boundary.operatorDimension());
        ASSERT_EQ_QUOTED(state_dimension,new_boundary.stateDimension());
    }
}
//@-others

}
//@+node:gcross.20110204201608.1742: *4* StateSite
TEST_SUITE(StateSite) {

//@+others
//@+node:gcross.20110204201608.1743: *5* move assignable
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
        ASSERT_EQ_QUOTED(physical_dimension,old_site.physicalDimension());
        ASSERT_EQ_QUOTED(left_dimension,old_site.leftDimension());
        ASSERT_EQ_QUOTED(right_dimension,old_site.rightDimension());

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
        ASSERT_EQ_QUOTED(physical_dimension,new_site.physicalDimension());
        ASSERT_EQ_QUOTED(left_dimension,new_site.leftDimension());
        ASSERT_EQ_QUOTED(right_dimension,new_site.rightDimension());
    }
}
//@+node:gcross.20110204201608.1747: *5* move constructable
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
        ASSERT_EQ_QUOTED(physical_dimension,old_site.physicalDimension());
        ASSERT_EQ_QUOTED(left_dimension,old_site.leftDimension());
        ASSERT_EQ_QUOTED(right_dimension,old_site.rightDimension());

        StateSite<Middle> new_site(boost::move(old_site));

        ASSERT_FALSE(old_site.valid());
        ASSERT_EQ(0,old_site.physicalDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.leftDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.rightDimension(as_unsigned_integer));

        ASSERT_TRUE(new_site.valid());
        ASSERT_EQ_QUOTED(physical_dimension,new_site.physicalDimension());
        ASSERT_EQ_QUOTED(left_dimension,new_site.leftDimension());
        ASSERT_EQ_QUOTED(right_dimension,new_site.rightDimension());
    }
}
//@-others

}
//@+node:gcross.20110204201608.1733: *4* OperatorSite
TEST_SUITE(OperatorSite) {

//@+others
//@+node:gcross.20110204201608.1736: *5* move assignable
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
        ASSERT_EQ_QUOTED(number_of_matrices,old_site.numberOfMatrices());
        ASSERT_EQ_QUOTED(physical_dimension,old_site.physicalDimension());
        ASSERT_EQ_QUOTED(left_dimension,old_site.leftDimension());
        ASSERT_EQ_QUOTED(right_dimension,old_site.rightDimension());

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
        ASSERT_EQ_QUOTED(number_of_matrices,new_site.numberOfMatrices());
        ASSERT_EQ_QUOTED(physical_dimension,new_site.physicalDimension());
        ASSERT_EQ_QUOTED(left_dimension,new_site.leftDimension());
        ASSERT_EQ_QUOTED(right_dimension,new_site.rightDimension());
    }
}
//@+node:gcross.20110204201608.1734: *5* move constructable
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
        ASSERT_EQ_QUOTED(number_of_matrices,old_site.numberOfMatrices());
        ASSERT_EQ_QUOTED(physical_dimension,old_site.physicalDimension());
        ASSERT_EQ_QUOTED(left_dimension,old_site.leftDimension());
        ASSERT_EQ_QUOTED(right_dimension,old_site.rightDimension());

        OperatorSite new_site(boost::move(old_site));

        ASSERT_FALSE(old_site.valid());
        ASSERT_EQ(0,old_site.numberOfMatrices());
        ASSERT_EQ(0,old_site.physicalDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.leftDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.rightDimension(as_unsigned_integer));

        ASSERT_TRUE(new_site.valid());
        ASSERT_EQ_QUOTED(number_of_matrices,new_site.numberOfMatrices());
        ASSERT_EQ_QUOTED(physical_dimension,new_site.physicalDimension());
        ASSERT_EQ_QUOTED(left_dimension,new_site.leftDimension());
        ASSERT_EQ_QUOTED(right_dimension,new_site.rightDimension());
    }
}
//@+node:gcross.20110204201608.1737: *5* random generator
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
//@+node:gcross.20110204201608.1759: *4* OverlapBoundary
TEST_SUITE(OverlapBoundary) {

//@+others
//@+node:gcross.20110204201608.1760: *5* move assignable
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
        ASSERT_EQ_QUOTED(overlap_dimension,old_boundary.overlapDimension());
        ASSERT_EQ_QUOTED(state_dimension,old_boundary.stateDimension());

        OverlapBoundary<Left> new_boundary;

        ASSERT_FALSE(new_boundary.valid());
        ASSERT_EQ(0,new_boundary.overlapDimension(as_unsigned_integer));
        ASSERT_EQ(0,new_boundary.stateDimension(as_unsigned_integer));

        new_boundary = boost::move(old_boundary);

        ASSERT_FALSE(old_boundary.valid());
        ASSERT_EQ(0,old_boundary.overlapDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_boundary.stateDimension(as_unsigned_integer));

        ASSERT_TRUE(new_boundary.valid());
        ASSERT_EQ_QUOTED(overlap_dimension,new_boundary.overlapDimension());
        ASSERT_EQ_QUOTED(state_dimension,new_boundary.stateDimension());
    }
}
//@+node:gcross.20110204201608.1761: *5* move constructable
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
        ASSERT_EQ_QUOTED(overlap_dimension,old_boundary.overlapDimension());
        ASSERT_EQ_QUOTED(state_dimension,old_boundary.stateDimension());

        OverlapBoundary<Left> new_boundary(boost::move(old_boundary));

        ASSERT_FALSE(old_boundary.valid());
        ASSERT_EQ(0,old_boundary.overlapDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_boundary.stateDimension(as_unsigned_integer));

        ASSERT_TRUE(new_boundary.valid());
        ASSERT_EQ_QUOTED(overlap_dimension,new_boundary.overlapDimension());
        ASSERT_EQ_QUOTED(state_dimension,new_boundary.stateDimension());
    }
}
//@-others

}
//@+node:gcross.20110204201608.2062: *4* OverlapSite
TEST_SUITE(OverlapSite) {

//@+others
//@+node:gcross.20110204201608.2063: *5* move assignable
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
        ASSERT_EQ_QUOTED(physical_dimension,old_site.physicalDimension());
        ASSERT_EQ_QUOTED(left_dimension,old_site.leftDimension());
        ASSERT_EQ_QUOTED(right_dimension,old_site.rightDimension());

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
        ASSERT_EQ_QUOTED(physical_dimension,new_site.physicalDimension());
        ASSERT_EQ_QUOTED(left_dimension,new_site.leftDimension());
        ASSERT_EQ_QUOTED(right_dimension,new_site.rightDimension());
    }
}
//@+node:gcross.20110204201608.2064: *5* move constructable
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
        ASSERT_EQ_QUOTED(physical_dimension,old_site.physicalDimension());
        ASSERT_EQ_QUOTED(left_dimension,old_site.leftDimension());
        ASSERT_EQ_QUOTED(right_dimension,old_site.rightDimension());

        OverlapSite<Middle> new_site(boost::move(old_site));

        ASSERT_FALSE(old_site.valid());
        ASSERT_EQ(0,old_site.physicalDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.leftDimension(as_unsigned_integer));
        ASSERT_EQ(0,old_site.rightDimension(as_unsigned_integer));

        ASSERT_TRUE(new_site.valid());
        ASSERT_EQ_QUOTED(physical_dimension,new_site.physicalDimension());
        ASSERT_EQ_QUOTED(left_dimension,new_site.leftDimension());
        ASSERT_EQ_QUOTED(right_dimension,new_site.rightDimension());
    }
}
//@-others

}
//@-others

}
//@+node:gcross.20110220093853.1978: *3* I/O
TEST_SUITE(IO) {

//@+others
//@+node:gcross.20110220093853.1982: *4* OperatorSite
TEST_SUITE(OperatorSite) {

//@+others
//@+node:gcross.20110220093853.1984: *5* encode then decode
TEST_CASE(encode_then_decode) {

    RNG random;

    REPEAT(10) {
        OperatorSite operator_site_1 =
            random.randomOperatorSite(
                 PhysicalDimension(random(1,5))
                ,LeftDimension(random(1,5))
                ,RightDimension(random(1,5))
            );

        YAML::Emitter out;
        out << operator_site_1;
        string out_string(out.c_str());

        istringstream in(out_string);
        YAML::Parser parser(in);
        YAML::Node doc;
        parser.GetNextDocument(doc);

        OperatorSite operator_site_2;
        doc >> operator_site_2;

        checkOperatorSitesEqual(operator_site_1,operator_site_2);
    }

}
//@-others

}
//@+node:gcross.20110220141808.1987: *4* Operator
TEST_SUITE(Operator) {

//@+others
//@+node:gcross.20110220141808.1991: *5* examples
TEST_SUITE(examples) {

//@+others
//@+node:gcross.20110220141808.1992: *6* external field
TEST_CASE(external_field) {

    RNG random;

    REPEAT(10) {
        unsigned int const number_of_middle_sites = random;

        Operator operator_1 =
            constructExternalFieldOperator(
                 number_of_middle_sites+2
                ,Pauli::Z
            );

        ostringstream out;
        out << "sequence: [1,";
        REPEAT(number_of_middle_sites) { out << "2, "; }
        out << "3]" << endl;
        out << (
            "paulis:\n"
            "  - &I [1,0,0,1]\n"
            "  - &Z [1,0,0,-1]\n"
            "sites:\n"
            "  - physical dimension: 2\n"
            "    left dimension:     1\n"
            "    right dimension:    2\n"
            "    matrices:\n"
            "       - from: 1\n"
            "         to:   1\n"
            "         data: *I\n"
            "       - from: 1\n"
            "         to:   2\n"
            "         data: *Z\n"
            "  - physical dimension: 2\n"
            "    left dimension:     2\n"
            "    right dimension:    2\n"
            "    matrices:\n"
            "       - from: 1\n"
            "         to:   1\n"
            "         data: *I\n"
            "       - from: 1\n"
            "         to:   2\n"
            "         data: *Z\n"
            "       - from: 2\n"
            "         to:   2\n"
            "         data: *I\n"
            "  - physical dimension: 2\n"
            "    left dimension:     2\n"
            "    right dimension:    1\n"
            "    matrices:\n"
            "       - from: 1\n"
            "         to:   1\n"
            "         data: *Z\n"
            "       - from: 2\n"
            "         to:   1\n"
            "         data: *I\n"
        ) << endl;

        istringstream in(out.str());
        YAML::Parser parser(in);
        YAML::Node doc;
        parser.GetNextDocument(doc);

        Operator operator_2;
        doc >> operator_2;

        checkOperatorsEqual(operator_1,operator_2);
    }

}
//@-others

}
//@+node:gcross.20110220141808.1988: *5* encode then decode
TEST_CASE(encode_then_decode) {

    RNG random;

    REPEAT(10) {
        Operator operator_1 = random.randomOperator();

        YAML::Emitter out;
        out << operator_1;
        string out_string(out.c_str());

        istringstream in(out_string);
        YAML::Parser parser(in);
        YAML::Node doc;
        parser.GetNextDocument(doc);

        Operator operator_2;
        doc >> operator_2;

        checkOperatorsEqual(operator_1,operator_2);
    }

}
//@-others

}
//@-others

}
//@-others
//@-leo
