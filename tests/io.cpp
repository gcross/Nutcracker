#include <illuminate.hpp>

#include <boost/assign/list_of.hpp>
#include <boost/range/algorithm/equal.hpp>

#include "nutcracker/io.hpp"

using namespace Nutcracker;

using boost::assign::list_of;
using boost::equal;
using boost::shared_ptr;

TEST_SUITE(IO) {

TEST_SUITE(correct_formats_are_installed) {

TEST_CASE(Input) {
    ASSERT_TRUE(equal(InputFormat::listNames(),list_of("hdf")("protobuf")("yaml")));
}
TEST_CASE(Output) {
    ASSERT_TRUE(equal(InputFormat::listNames(),list_of("hdf")("protobuf")("yaml")));
}

}
TEST_SUITE(correct_extensions_are_installed) {

TEST_SUITE(Input) {

TEST_CASE(h5) { ASSERT_EQ(InputFormat::lookupExtension("h5"),&InputFormat::lookupName("hdf")); }
TEST_CASE(hdf) { ASSERT_EQ(InputFormat::lookupExtension("hdf"),&InputFormat::lookupName("hdf")); }
TEST_CASE(hdf5) { ASSERT_EQ(InputFormat::lookupExtension("hdf5"),&InputFormat::lookupName("hdf")); }

TEST_CASE(prb) { ASSERT_EQ(InputFormat::lookupExtension("prb"),&InputFormat::lookupName("protobuf")); }

TEST_CASE(yaml) { ASSERT_EQ(InputFormat::lookupExtension("yaml"),&InputFormat::lookupName("yaml")); }

}
TEST_SUITE(Output) {

TEST_CASE(h5) { ASSERT_EQ(OutputFormat::lookupExtension("h5"),&OutputFormat::lookupName("hdf")); }
TEST_CASE(hdf) { ASSERT_EQ(OutputFormat::lookupExtension("hdf"),&OutputFormat::lookupName("hdf")); }
TEST_CASE(hdf5) { ASSERT_EQ(OutputFormat::lookupExtension("hdf5"),&OutputFormat::lookupName("hdf")); }

TEST_CASE(prb) { ASSERT_EQ(OutputFormat::lookupExtension("prb"),&OutputFormat::lookupName("protobuf")); }

TEST_CASE(yaml) { ASSERT_EQ(OutputFormat::lookupExtension("yaml"),&OutputFormat::lookupName("yaml")); }

}

}
TEST_SUITE(error_handling) {

TEST_CASE(no_such_operator_site_number) {
    try {
        constructOperatorFrom(
            list_of(shared_ptr<OperatorSite const>(new OperatorSite(0,PhysicalDimension(1),LeftDimension(1),RightDimension(1)))),
            vector<unsigned int>(1,1)
        );
        FAIL("Exception was not thrown.")
    } catch(NoSuchOperatorSiteNumberError const& e) {
        EXPECT_EQ_VAL(e.index,1u)
    }
}
TEST_CASE(left_dimension_must_be_one) {
    try {
        constructOperatorFrom(
            list_of(shared_ptr<OperatorSite const>(new OperatorSite(0,PhysicalDimension(1),LeftDimension(2),RightDimension(1)))),
            vector<unsigned int>(1,0)
        );
        FAIL("Exception was not thrown.")
    } catch(BoundaryDimensionNotOneError const& e) {
        EXPECT_EQ_VAL(e.boundary_name,"left")
        EXPECT_EQ_VAL(e.boundary_dimension,2u)
    }
}
TEST_CASE(right_dimension_must_be_one) {
    try {
        constructOperatorFrom(
            list_of(shared_ptr<OperatorSite const>(new OperatorSite(0,PhysicalDimension(1),LeftDimension(1),RightDimension(2)))),
            vector<unsigned int>(1,0)
        );
        FAIL("Exception was not thrown.")
    } catch(BoundaryDimensionNotOneError const& e) {
        EXPECT_EQ_VAL(e.boundary_name,"right")
        EXPECT_EQ_VAL(e.boundary_dimension,2u)
    }
}
TEST_CASE(mismatched_site_dimensions_error) {
    try {
        vector<unsigned int> sequence = list_of(0)(1);
        constructOperatorFrom(
            list_of
                (shared_ptr<OperatorSite const>(new OperatorSite(0,PhysicalDimension(1),LeftDimension(1),RightDimension(2))))
                (shared_ptr<OperatorSite const>(new OperatorSite(0,PhysicalDimension(1),LeftDimension(3),RightDimension(1))))
            ,
            sequence
        );
        FAIL("Exception was not thrown.")
    } catch(MismatchedSiteDimensionsError const& e) {
        EXPECT_EQ_VAL(e.site_number,1u)
        EXPECT_EQ_VAL(e.left_dimension,3u)
        EXPECT_EQ_VAL(e.right_dimension,2u)
    }
}
TEST_CASE(no_sites) {
    try {
        constructOperatorFrom(
            vector<shared_ptr<OperatorSite const> >(0),
            vector<unsigned int>(0)
        );
        FAIL("Exception was not thrown.")
    } catch(NoSitesError const& e) {}
}

}

}
