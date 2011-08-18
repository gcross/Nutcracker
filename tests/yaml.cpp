//@+leo-ver=5-thin
//@+node:gcross.20110430221653.2171: * @file yaml.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110430221653.2173: ** << License >>
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
//@+node:gcross.20110430221653.2175: ** << Includes >>
#include <algorithm>
#include <boost/range/algorithm/generate.hpp>
#include <illuminate.hpp>
#include <iostream>

#include "io.hpp"
#include "yaml.hpp"

#include "test_utils.hpp"

using boost::generate;

using std::auto_ptr;
using std::endl;
using std::istringstream;
using std::ostringstream;
//@-<< Includes >>

//@+others
//@+node:gcross.20110726215559.2299: ** Functions
//@+node:gcross.20110726215559.2300: *3* parseYAMLString
auto_ptr<YAML::Node> parseYAMLString(string const& s) {
    auto_ptr<YAML::Node> result(new YAML::Node);
    istringstream in(s);
    YAML::Parser parser(in);
    parser.GetNextDocument(*result);
    return result;
}
//@+node:gcross.20110430221653.2176: ** Tests
TEST_SUITE(YAML) {

//@+others
//@+node:gcross.20110430221653.2178: *3* complex
TEST_SUITE(complex) {

//@+others
//@+node:gcross.20110220093853.1950: *4* complex double
TEST_SUITE(complex_double) {

//@+others
//@+node:gcross.20110220093853.1953: *5* decode
TEST_CASE(decode) {

    RNG random;

    REPEAT(10) {
        complex<double> x = random, y;

        ostringstream out;
        out << format("[%|.20|,%|.20|]") % x.real() % x.imag();
        istringstream in(out.str());
        YAML::Parser parser(in);
        YAML::Node doc;
        parser.GetNextDocument(doc);
        doc >> y;

        ASSERT_EQ(x,y);
    }

}
//@+node:gcross.20110220093853.1951: *5* encode then decode
TEST_CASE(encode_then_decode) {

    RNG random;

    REPEAT(10) {
        complex<double> x = random, y;

        YAML::Emitter out;
        out << x;
        string out_string(out.c_str());

        istringstream in(out_string);
        YAML::Parser parser(in);
        YAML::Node doc;
        parser.GetNextDocument(doc);
        doc >> y;

        ASSERT_EQ(x,y);
    }

}
//@-others

}
//@+node:gcross.20110220141808.1982: *4* real complex double
TEST_SUITE(real_complex_double) {

//@+others
//@+node:gcross.20110220141808.1983: *5* decode
TEST_CASE(decode) {

    RNG random;

    REPEAT(10) {
        double x = random.randomDouble();

        ostringstream out;
        out << format("%|.20|") % x;
        istringstream in(out.str());
        YAML::Parser parser(in);
        YAML::Node doc;
        parser.GetNextDocument(doc);
        complex<double> y;
        doc >> y;

        ASSERT_EQ(c(x,0),y);
    }

}
//@+node:gcross.20110220141808.1984: *5* encode then decode
TEST_CASE(encode_then_decode) {

    RNG random;

    REPEAT(10) {
        complex<double> x = c(random.randomDouble(),0), y;

        YAML::Emitter out;
        out << x;
        string out_string(out.c_str());

        istringstream in(out_string);
        YAML::Parser parser(in);
        YAML::Node doc;
        parser.GetNextDocument(doc);
        ASSERT_EQ(YAML::NodeType::Scalar,doc.Type());
        doc >> y;

        ASSERT_EQ(x,y);
    }

}
//@-others

}
//@-others

}
//@+node:gcross.20110220141808.1987: *3* Operator
TEST_SUITE(Operator) {

//@+others
//@+node:gcross.20110220141808.1988: *4* encode then decode
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
//@+node:gcross.20110220141808.1991: *4* examples
TEST_SUITE(examples) {

//@+others
//@+node:gcross.20110220141808.1992: *5* external field
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
//@+node:gcross.20110726215559.2345: *4* error handling
TEST_SUITE(error_handling) {

//@+others
//@+node:gcross.20110726215559.2346: *5* no such operator site number
TEST_CASE(no_such_operator_site_number) {
    auto_ptr<YAML::Node> result = parseYAMLString(
        "---\n"
        "sites:\n"
        "sequence: [1]\n"
    );
    Operator o;
    try {
        *result >> o;
        FAIL("No exception was thrown.")
    } catch(NoSuchOperatorSiteNumberError const& e) {
        EXPECT_EQ_VAL(e.index,1u)
    }
}
//@-others

}
//@-others

}
//@+node:gcross.20110220093853.1954: *3* OperatorLink
TEST_SUITE(OperatorLink) {

//@+others
//@+node:gcross.20110220093853.1956: *4* decode
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
            ASSERT_EQ(data[i],link.matrix->data()[i]);
        }
    }

}
//@+node:gcross.20110220093853.1958: *4* encode then decode
TEST_CASE(encode_then_decode) {

    RNG random;

    REPEAT(10) {
        unsigned int n = random;
        MatrixPtr data(new Matrix(n,n));
        generate(data->data(),random.randomComplexDouble);
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
            ASSERT_EQ(link2.matrix->data()[i],link1.matrix->data()[i]);
        }
    }

}
//@+node:gcross.20110726215559.2297: *4* error handling
TEST_SUITE(error_handling) {

//@+others
//@+node:gcross.20110726215559.2298: *5* non-square matrix
TEST_CASE(non_square_matrix) {
    auto_ptr<YAML::Node> result = parseYAMLString(
        "---\n"
        "from: 1\n"
        "to: 1\n"
        "data: [1,2,3]\n"
    );
    OperatorLink link;
    try {
        *result >> link;
        FAIL("No exception was thrown.")
    } catch(NonSquareMatrixYAMLInputError const& e) {
        EXPECT_EQ_VAL(e.mark.line,3)
        EXPECT_EQ_VAL(e.mark.column,6)
        EXPECT_EQ_VAL(e.length,3u)
    }
}
//@-others

}
//@-others

}
//@+node:gcross.20110220093853.1982: *3* OperatorSite
TEST_SUITE(OperatorSite) {

//@+others
//@+node:gcross.20110220093853.1995: *4* decode
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
            MatrixPtr data(new Matrix(physical_dimension,physical_dimension));
            generate(data->data(),random.randomComplexDouble);
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

        ASSERT_EQ(physical_dimension,operator_site.physicalDimension());
        ASSERT_EQ(left_dimension,operator_site.leftDimension());
        ASSERT_EQ(right_dimension,operator_site.rightDimension());
        ASSERT_EQ(links.size(),operator_site.numberOfMatrices());

        uint32_t const* index_data = operator_site;
        complex<double> const* matrix_data = operator_site;
        BOOST_FOREACH(OperatorLink const& link, links) {
            ASSERT_EQ(link.from,index_data[0]);
            ASSERT_EQ(link.to,index_data[1]);
            index_data += 2;
            ASSERT_TRUE(equal(link.matrix->data().begin(),link.matrix->data().end(),matrix_data));
            matrix_data += physical_dimension*physical_dimension;
        }
    }

}
//@+node:gcross.20110220093853.1984: *4* encode then decode
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
//@+node:gcross.20110726215559.2320: *4* error handling
TEST_SUITE(error_handling) {

//@+others
//@+node:gcross.20110726215559.2321: *5* from index too low
TEST_CASE(from_index_too_low) {
    auto_ptr<YAML::Node> result = parseYAMLString(
        "---\n"
        "physical dimension: 2\n"
        "left dimension: 2\n"
        "right dimension: 2\n"
        "matrices:\n"
        "  - from: 0\n"
        "    to: 1\n"
        "    data: [1,2,3,4]\n"
    );
    OperatorSite site;
    try {
        *result >> site;
        FAIL("No exception was thrown.")
    } catch(IndexTooLowYAMLInputError const& e) {
        EXPECT_EQ_VAL(e.mark.line,5)
        EXPECT_EQ_VAL(e.mark.column,10)
        EXPECT_EQ_VAL(e.name,"from")
        EXPECT_EQ_VAL(e.index,0u)
    }
}
//@+node:gcross.20110726215559.2323: *5* from index too high
TEST_CASE(from_index_too_high) {
    auto_ptr<YAML::Node> result = parseYAMLString(
        "---\n"
        "physical dimension: 2\n"
        "left dimension: 2\n"
        "right dimension: 2\n"
        "matrices:\n"
        "  - from: 3\n"
        "    to: 1\n"
        "    data: [1,2,3,4]\n"
    );
    OperatorSite site;
    try {
        *result >> site;
        FAIL("No exception was thrown.")
    } catch(IndexTooHighYAMLInputError const& e) {
        EXPECT_EQ_VAL(e.mark.line,5)
        EXPECT_EQ_VAL(e.mark.column,10)
        EXPECT_EQ_VAL(e.name,"from")
        EXPECT_EQ_VAL(e.index,3u)
        EXPECT_EQ_VAL(e.dimension,2u)
    }
}
//@+node:gcross.20110726215559.2325: *5* to index too low
TEST_CASE(to_index_too_low) {
    auto_ptr<YAML::Node> result = parseYAMLString(
        "---\n"
        "physical dimension: 2\n"
        "left dimension: 2\n"
        "right dimension: 2\n"
        "matrices:\n"
        "  - from: 1\n"
        "    to: 0\n"
        "    data: [1,2,3,4]\n"
    );
    OperatorSite site;
    try {
        *result >> site;
        FAIL("No exception was thrown.")
    } catch(IndexTooLowYAMLInputError const& e) {
        EXPECT_EQ_VAL(e.mark.line,6)
        EXPECT_EQ_VAL(e.mark.column,8)
        EXPECT_EQ_VAL(e.name,"to")
        EXPECT_EQ_VAL(e.index,0u)
    }
}
//@+node:gcross.20110726215559.2327: *5* to index too high
TEST_CASE(to_index_too_high) {
    auto_ptr<YAML::Node> result = parseYAMLString(
        "---\n"
        "physical dimension: 2\n"
        "left dimension: 2\n"
        "right dimension: 2\n"
        "matrices:\n"
        "  - from: 1\n"
        "    to: 3\n"
        "    data: [1,2,3,4]\n"
    );
    OperatorSite site;
    try {
        *result >> site;
        FAIL("No exception was thrown.")
    } catch(IndexTooHighYAMLInputError const& e) {
        EXPECT_EQ_VAL(e.mark.line,6)
        EXPECT_EQ_VAL(e.mark.column,8)
        EXPECT_EQ_VAL(e.name,"to")
        EXPECT_EQ_VAL(e.index,3u)
        EXPECT_EQ_VAL(e.dimension,2u)
    }
}
//@+node:gcross.20110726215559.2333: *5* wrong data length
TEST_CASE(wrong_data_length) {
    auto_ptr<YAML::Node> result = parseYAMLString(
        "---\n"
        "physical dimension: 2\n"
        "left dimension: 2\n"
        "right dimension: 2\n"
        "matrices:\n"
        "  - from: 1\n"
        "    to: 1\n"
        "    data: [1,2,3]\n"
    );
    OperatorSite site;
    try {
        *result >> site;
        FAIL("No exception was thrown.")
    } catch(WrongDataLengthYAMLInputError const& e) {
        EXPECT_EQ_VAL(e.mark.line,7)
        EXPECT_EQ_VAL(e.mark.column,10)
        EXPECT_EQ_VAL(e.length,3u)
        EXPECT_EQ_VAL(e.correct_length,4u)
    }
}
//@-others

}
//@-others

}
//@-others

}
//@-others
//@-leo
