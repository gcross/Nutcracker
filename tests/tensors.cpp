//@+leo-ver=5-thin
//@+node:gcross.20110204201608.1725: * @thin tensors.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110204201608.1726: ** << Includes >>
#include <illuminate.hpp>

#include "utilities.hpp"

#include "test_utils.hpp"

using namespace boost;
using namespace Nutcracker;
using namespace std;
//@-<< Includes >>

//@+others
//@+node:gcross.20110204201608.1727: ** Tensors
TEST_SUITE(Tensors) {

//@+others
//@+node:gcross.20110204201608.1751: *3* ExpectationBoundary
TEST_SUITE(ExpectationBoundary) {

//@+others
//@+node:gcross.20110204201608.1752: *4* move assignable
TEST_CASE(move_assignable) {
    RNG random;

    REPEAT(10) {
        ExpectationBoundary<Left> old_boundary
            (OperatorDimension(random)
            ,StateDimension(random)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        ASSERT_TRUE(old_boundary);
        ExpectationBoundary<Left> new_boundary;
        ASSERT_FALSE(new_boundary);
        new_boundary = boost::move(old_boundary);
        ASSERT_FALSE(old_boundary);
        ASSERT_TRUE(new_boundary);
    }
}
//@+node:gcross.20110204201608.1755: *4* move constructable
TEST_CASE(move_constructable) {
    RNG random;

    REPEAT(10) {
        ExpectationBoundary<Left> old_boundary
            (OperatorDimension(random)
            ,StateDimension(random)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        ASSERT_TRUE(old_boundary);
        ExpectationBoundary<Left> new_boundary(boost::move(old_boundary));
        ASSERT_FALSE(old_boundary);
        ASSERT_TRUE(new_boundary);
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
        StateSite<Middle> old_site
            (PhysicalDimension(random)
            ,LeftDimension(random)
            ,RightDimension(random)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        ASSERT_TRUE(old_site);
        StateSite<Middle> new_site;
        ASSERT_FALSE(new_site);
        new_site = boost::move(old_site);
        ASSERT_FALSE(old_site);
        ASSERT_TRUE(new_site);
    }
}
//@+node:gcross.20110204201608.1747: *4* move constructable
TEST_CASE(move_constructable) {
    RNG random;

    REPEAT(10) {
        StateSite<Middle> old_site
            (PhysicalDimension(random)
            ,LeftDimension(random)
            ,RightDimension(random)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        ASSERT_TRUE(old_site);
        StateSite<Middle> new_site(boost::move(old_site));
        ASSERT_FALSE(old_site);
        ASSERT_TRUE(new_site);
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
        unsigned int const
             left_operator_dimension = random
            ,physical_dimension = random
            ,right_operator_dimension = random
            ,number_of_matrices = random
            ;
        RNG::IndexGenerator randomIndex(random,left_operator_dimension,right_operator_dimension);
        OperatorSite old_operator_site
            (number_of_matrices
            ,PhysicalDimension(physical_dimension)
            ,LeftDimension(left_operator_dimension)
            ,RightDimension(right_operator_dimension)
            ,fillWithGenerator(randomIndex)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        ASSERT_TRUE(old_operator_site);
        OperatorSite new_operator_site;
        ASSERT_FALSE(new_operator_site);
        new_operator_site = boost::move(old_operator_site);
        ASSERT_FALSE(old_operator_site);
        ASSERT_TRUE(new_operator_site);
    }
}
//@+node:gcross.20110204201608.1734: *4* move constructable
TEST_CASE(move_constructable) {
    RNG random;

    REPEAT(10) {
        unsigned int const
             left_operator_dimension = random
            ,physical_dimension = random
            ,right_operator_dimension = random
            ,number_of_matrices = random
            ;
        RNG::IndexGenerator randomIndex(random,left_operator_dimension,right_operator_dimension);
        OperatorSite old_operator_site
            (number_of_matrices
            ,PhysicalDimension(physical_dimension)
            ,LeftDimension(left_operator_dimension)
            ,RightDimension(right_operator_dimension)
            ,fillWithGenerator(randomIndex)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        ASSERT_TRUE(old_operator_site);
        OperatorSite new_operator_site(boost::move(old_operator_site));
        ASSERT_FALSE(old_operator_site);
        ASSERT_TRUE(new_operator_site);
    }
}
//@+node:gcross.20110204201608.1737: *4* random generator
TEST_CASE(random_generator) {
    RNG random;

    REPEAT(10) {
        PhysicalDimension physical_dimension(random);
        LeftDimension left_dimension(random);
        RightDimension right_dimension(random);
        OperatorSite operator_site_1(random.randomOperator(physical_dimension,left_dimension,right_dimension));
        ASSERT_TRUE(operator_site_1);
        OperatorSite operator_site_2;
        ASSERT_FALSE(operator_site_2);
        operator_site_2 = random.randomOperator(physical_dimension,left_dimension,right_dimension);
        ASSERT_TRUE(operator_site_2);
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
        OverlapBoundary<Left> old_boundary
            (OverlapDimension(random)
            ,StateDimension(random)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        ASSERT_TRUE(old_boundary);
        OverlapBoundary<Left> new_boundary;
        ASSERT_FALSE(new_boundary);
        new_boundary = boost::move(old_boundary);
        ASSERT_FALSE(old_boundary);
        ASSERT_TRUE(new_boundary);
    }
}
//@+node:gcross.20110204201608.1761: *4* move constructable
TEST_CASE(move_constructable) {
    RNG random;

    REPEAT(10) {
        OverlapBoundary<Left> old_boundary
            (OverlapDimension(random)
            ,StateDimension(random)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        ASSERT_TRUE(old_boundary);
        OverlapBoundary<Left> new_boundary(boost::move(old_boundary));
        ASSERT_FALSE(old_boundary);
        ASSERT_TRUE(new_boundary);
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
        OverlapSite<Middle> old_site
            (RightDimension(random)
            ,PhysicalDimension(random)
            ,LeftDimension(random)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        ASSERT_TRUE(old_site);
        OverlapSite<Middle> new_site;
        ASSERT_FALSE(new_site);
        new_site = boost::move(old_site);
        ASSERT_FALSE(old_site);
        ASSERT_TRUE(new_site);
    }
}
//@+node:gcross.20110204201608.2064: *4* move constructable
TEST_CASE(move_constructable) {
    RNG random;

    REPEAT(10) {
        OverlapSite<Middle> old_site
            (RightDimension(random)
            ,PhysicalDimension(random)
            ,LeftDimension(random)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        ASSERT_TRUE(old_site);
        OverlapSite<Middle> new_site(boost::move(old_site));
        ASSERT_FALSE(old_site);
        ASSERT_TRUE(new_site);
    }
}
//@-others

}
//@-others

}
//@-others
//@-leo
