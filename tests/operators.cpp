//@+leo-ver=5-thin
//@+node:gcross.20110207005827.1776: * @thin operators.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110207005827.1777: ** << Includes >>
#include <boost/container/vector.hpp>
#include <boost/foreach.hpp>
#include <boost/format.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <boost/range/algorithm/generate.hpp>
#include <illuminate.hpp>
#include <iostream>
#include <sstream>
#include <utility>

#include "operators.hpp"
#include "utilities.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::container::vector;
using boost::equal;
using boost::format;
using boost::generate;

using std::cerr;
using std::endl;
using std::istringstream;
using std::ostringstream;
//@-<< Includes >>

//@+others
//@+node:gcross.20110220093853.1989: ** Tests
//@+node:gcross.20110207005827.1778: *3* Operators
TEST_SUITE(Operators) {

//@+others
//@+node:gcross.20110207005827.1779: *4* Pauli
TEST_SUITE(Pauli) {

    using namespace Pauli;

    #define TEST_PAULI(Pauli,_00,_01,_10,_11) \
        TEST_CASE(Pauli) { \
            EXPECT_EQ(2,Pauli.size1()); \
            EXPECT_EQ(2,Pauli.size2()); \
            EXPECT_EQ(_00,Pauli(0,0)); \
            EXPECT_EQ(_01,Pauli(0,1)); \
            EXPECT_EQ(_10,Pauli(1,0)); \
            EXPECT_EQ(_11,Pauli(1,1)); \
        }

    TEST_PAULI(I,c(1,0),c(0,0),c(0,0),c(1,0))
    TEST_PAULI(X,c(0,0),c(1,0),c(1,0),c(0,0))
    TEST_PAULI(Y,c(0,0),c(0,-1),c(0,1),c(0,0))
    TEST_PAULI(Z,c(1,0),c(0,0),c(0,0),c(-1,0))

    #undef TEST_PAULI
}
//@-others

}
//@+node:gcross.20110220093853.1988: *3* I/O
TEST_SUITE(IO) {

//@+others
//@+node:gcross.20110220093853.1954: *4* OperatorLink
TEST_SUITE(OperatorLink) {

//@+others
//@+node:gcross.20110220093853.1956: *5* decode
TEST_CASE(decode) {

    RNG random;

    REPEAT(10) {
        unsigned int const from = random, to = random, n = random;
        vector<complex<double> > data(n*n);
        generate(data,random.randomComplexDouble);

        ostringstream out;
        out << format("from: %1%\nto: %2%\ndata: [") % from % to;
        BOOST_FOREACH(complex<double> const x, make_pair(data.begin(),data.end()-1)) {
            out << format("[%|.20|,%|.20|],") % x.real() % x.imag();
        }
        out << format("[%|.20|,%|.20|]]") % data[n*n-1].real() % data[n*n-1].imag();
        istringstream in(out.str());
        YAML::Parser parser(in);
        YAML::Node doc;
        parser.GetNextDocument(doc);

        OperatorLink link;
        doc >> link;

        ASSERT_EQ(from,link.from);
        ASSERT_EQ(to,link.to);
        BOOST_FOREACH(unsigned int const i, irange(0u,n*n)) {
            ASSERT_EQ(data[i],link.matrix.data()[i]);
        }
    }

}
//@+node:gcross.20110220093853.1958: *5* encode then decode
TEST_CASE(encode_then_decode) {

    RNG random;

    REPEAT(10) {
        unsigned int n = random;
        Matrix data(n,n);
        generate(data.data(),random.randomComplexDouble);
        OperatorLink link1(random,random,data);

        YAML::Emitter out;
        out << link1;
        string out_string(out.c_str());

        istringstream in(out_string);
        YAML::Parser parser(in);
        YAML::Node doc;
        parser.GetNextDocument(doc);

        OperatorLink link2;
        doc >> link2;

        ASSERT_EQ(link1.from,link2.from);
        ASSERT_EQ(link1.to,link2.to);
        BOOST_FOREACH(unsigned int const i, irange(0u,n*n)) {
            ASSERT_EQ(link2.matrix.data()[i],link1.matrix.data()[i]);
        }
    }

}
//@-others

}
//@+node:gcross.20110220093853.1994: *4* OperatorSite
TEST_SUITE(OperatorSite) {

//@+others
//@+node:gcross.20110220093853.1995: *5* decode
TEST_CASE(decode) {

    RNG random;

    REPEAT(10) {
        unsigned int const
             physical_dimension = random
            ,left_dimension = random
            ,right_dimension = random
            ;
        vector<OperatorLink> links;
        REPEAT(random) {
            Matrix data(physical_dimension,physical_dimension);
            generate(data.data(),random.randomComplexDouble);
            links.emplace_back(random(1,left_dimension),random(1,right_dimension),data);
        }

        YAML::Emitter out;
        out << YAML::BeginMap;
        out << YAML::Key << "physical dimension" << YAML::Value << physical_dimension;
        out << YAML::Key << "left dimension" << YAML::Value << left_dimension;
        out << YAML::Key << "right dimension" << YAML::Value << right_dimension;
        out << YAML::Key << "matrices" << YAML::Value;
        {
            out << YAML::BeginSeq;
            BOOST_FOREACH(OperatorLink const& link, links) { out << link; }
            out << YAML::EndSeq;
        }
        out << YAML::EndMap;
        string out_string(out.c_str());

        istringstream in(out_string);
        YAML::Parser parser(in);
        YAML::Node doc;
        parser.GetNextDocument(doc);

        OperatorSite operator_site;
        doc >> operator_site;

        ASSERT_EQ(physical_dimension,operator_site.physicalDimension(as_unsigned_integer));
        ASSERT_EQ(left_dimension,operator_site.leftDimension(as_unsigned_integer));
        ASSERT_EQ(right_dimension,operator_site.rightDimension(as_unsigned_integer));
        ASSERT_EQ(links.size(),operator_site.numberOfMatrices());

        uint32_t const* index_data = operator_site;
        complex<double> const* matrix_data = operator_site;
        BOOST_FOREACH(OperatorLink const& link, links) {
            ASSERT_EQ(link.from,index_data[0]);
            ASSERT_EQ(link.to,index_data[1]);
            index_data += 2;
            ASSERT_TRUE(equal(link.matrix.data().begin(),link.matrix.data().end(),matrix_data));
            matrix_data += physical_dimension*physical_dimension;
        }
    }

}
//@-others

}
//@-others

}
//@-others
//@-leo
