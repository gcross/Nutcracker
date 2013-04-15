#include <algorithm>
#include <boost/range/algorithm/generate.hpp>
#include <illuminate.hpp>
#include <iostream>

#include "nutcracker/io.hpp"
#include "nutcracker/yaml.hpp"

#include "test_utils.hpp"

using boost::generate;

using std::auto_ptr;
using std::endl;
using std::istringstream;
using std::ostringstream;

YAML::Node parseYAMLString(string const& s) {
    istringstream in(s);
    return YAML::Load(in);
}
TEST_SUITE(YAML) {

TEST_SUITE(complex) {

TEST_SUITE(complex_double) {

TEST_CASE(decode) {

    RNG random;

    REPEAT(10) {
        complex<double> x = random, y;

        ostringstream out;
        out << format("[%|.20|,%|.20|]") % x.real() % x.imag();
        istringstream in(out.str());
        YAML::Node doc = YAML::Load(in);
        doc >> y;

        ASSERT_EQ(x,y);
    }

}
TEST_CASE(encode_then_decode) {

    RNG random;

    REPEAT(10) {
        complex<double> x = random, y;

        YAML::Emitter out;
        out << x;
        string out_string(out.c_str());

        istringstream in(out_string);
        YAML::Node doc = YAML::Load(in);
        doc >> y;

        ASSERT_EQ(x,y);
    }

}

}
TEST_SUITE(real_complex_double) {

TEST_CASE(decode) {

    RNG random;

    REPEAT(10) {
        double x = random.randomDouble();

        ostringstream out;
        out << format("%|.20|") % x;
        istringstream in(out.str());
        YAML::Node doc = YAML::Load(in);
        complex<double> y;
        doc >> y;

        ASSERT_EQ(c(x,0),y);
    }

}
TEST_CASE(encode_then_decode) {

    RNG random;

    REPEAT(10) {
        complex<double> x = c(random.randomDouble(),0), y;

        YAML::Emitter out;
        out << x;
        string out_string(out.c_str());

        istringstream in(out_string);
        YAML::Node doc = YAML::Load(in);
        ASSERT_EQ(YAML::NodeType::Scalar,doc.Type());
        doc >> y;

        ASSERT_EQ(x,y);
    }

}

}

}
TEST_SUITE(Operator) {

TEST_CASE(encode_then_decode) {

    RNG random;

    REPEAT(10) {
        Operator operator_1 = random.randomOperator();

        YAML::Emitter out;
        out << operator_1;
        string out_string(out.c_str());

        istringstream in(out_string);
        YAML::Node doc = YAML::Load(in);

        Operator operator_2;
        doc >> operator_2;

        checkOperatorsEqual(operator_1,operator_2);
    }

}
TEST_SUITE(examples) {

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
        YAML::Node doc = YAML::Load(in);

        Operator operator_2;
        doc >> operator_2;

        checkOperatorsEqual(operator_1,operator_2);
    }

}

}
TEST_SUITE(error_handling) {

TEST_CASE(no_such_operator_site_number) {
    YAML::Node result = parseYAMLString(
        "---\n"
        "sites:\n"
        "sequence: [1]\n"
    );
    Operator o;
    try {
        result >> o;
        FAIL("No exception was thrown.")
    } catch(NoSuchOperatorSiteNumberError const& e) {
        EXPECT_EQ_VAL(e.index,1u)
    }
}

}

}
TEST_SUITE(OperatorSiteLink) {

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
        YAML::Node doc = YAML::Load(in);

        OperatorSiteLink link;
        doc >> link;

        ASSERT_EQ(from,link.from);
        ASSERT_EQ(to,link.to);
        BOOST_FOREACH(unsigned int const i, irange(0u,n*n)) {
            ASSERT_EQ(data[i],link.label->data()[i]);
        }
    }

}
TEST_CASE(encode_then_decode) {

    RNG random;

    REPEAT(10) {
        unsigned int n = random;
        MatrixPtr data(new Matrix(n,n));
        generate(data->data(),random.randomComplexDouble);
        OperatorSiteLink link1(random,random,data);

        YAML::Emitter out;
        out << link1;
        string out_string(out.c_str());

        istringstream in(out_string);
        YAML::Node doc = YAML::Load(in);

        OperatorSiteLink link2;
        doc >> link2;

        ASSERT_EQ(link1.from,link2.from);
        ASSERT_EQ(link1.to,link2.to);
        BOOST_FOREACH(unsigned int const i, irange(0u,n*n)) {
            ASSERT_EQ(link2.label->data()[i],link1.label->data()[i]);
        }
    }

}
TEST_SUITE(error_handling) {

TEST_CASE(non_square_matrix) {
    YAML::Node result = parseYAMLString(
        "---\n"
        "from: 1\n"
        "to: 1\n"
        "data: [1,2,3]\n"
    );
    OperatorSiteLink link;
    try {
        result >> link;
        FAIL("No exception was thrown.")
    } catch(NonSquareMatrixYAMLInputError const& e) {
        EXPECT_EQ_VAL(e.length,3u)
    }
}

}

}
TEST_SUITE(OperatorSite) {

TEST_CASE(decode) {

    RNG random;

    REPEAT(10) {
        unsigned int const
             physical_dimension = random
            ,left_dimension = random
            ,right_dimension = random
            ;
        vector<OperatorSiteLink> links;
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
            BOOST_FOREACH(OperatorSiteLink const& link, links) { out << link; }
            out << YAML::EndSeq;
        }
        out << YAML::EndMap;
        string out_string(out.c_str());

        istringstream in(out_string);
        YAML::Node doc = YAML::Load(in);

        OperatorSite operator_site;
        doc >> operator_site;

        ASSERT_EQ(physical_dimension,operator_site.physicalDimension());
        ASSERT_EQ(left_dimension,operator_site.leftDimension());
        ASSERT_EQ(right_dimension,operator_site.rightDimension());
        ASSERT_EQ(links.size(),operator_site.numberOfMatrices());

        uint32_t const* index_data = operator_site;
        complex<double> const* matrix_data = operator_site;
        BOOST_FOREACH(OperatorSiteLink const& link, links) {
            ASSERT_EQ(link.from,index_data[0]);
            ASSERT_EQ(link.to,index_data[1]);
            index_data += 2;
            ASSERT_TRUE(equal(link.label->data().begin(),link.label->data().end(),matrix_data));
            matrix_data += physical_dimension*physical_dimension;
        }
    }

}
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
        YAML::Node doc = YAML::Load(in);

        OperatorSite operator_site_2;
        doc >> operator_site_2;

        checkOperatorSitesEqual(operator_site_1,operator_site_2);
    }

}
TEST_SUITE(error_handling) {

TEST_CASE(from_index_too_low) {
    YAML::Node result = parseYAMLString(
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
        result >> site;
        FAIL("No exception was thrown.")
    } catch(IndexTooLowYAMLInputError const& e) {
        EXPECT_EQ_VAL(e.name,"from")
        EXPECT_EQ_VAL(e.index,0u)
    }
}
TEST_CASE(from_index_too_high) {
    YAML::Node result = parseYAMLString(
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
        result >> site;
        FAIL("No exception was thrown.")
    } catch(IndexTooHighYAMLInputError const& e) {
        EXPECT_EQ_VAL(e.name,"from")
        EXPECT_EQ_VAL(e.index,3u)
        EXPECT_EQ_VAL(e.dimension,2u)
    }
}
TEST_CASE(to_index_too_low) {
    YAML::Node result = parseYAMLString(
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
        result >> site;
        FAIL("No exception was thrown.")
    } catch(IndexTooLowYAMLInputError const& e) {
        EXPECT_EQ_VAL(e.name,"to")
        EXPECT_EQ_VAL(e.index,0u)
    }
}
TEST_CASE(to_index_too_high) {
    YAML::Node result = parseYAMLString(
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
        result >> site;
        FAIL("No exception was thrown.")
    } catch(IndexTooHighYAMLInputError const& e) {
        EXPECT_EQ_VAL(e.name,"to")
        EXPECT_EQ_VAL(e.index,3u)
        EXPECT_EQ_VAL(e.dimension,2u)
    }
}
TEST_CASE(wrong_data_length) {
    YAML::Node result = parseYAMLString(
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
        result >> site;
        FAIL("No exception was thrown.")
    } catch(WrongDataLengthYAMLInputError const& e) {
        EXPECT_EQ_VAL(e.length,3u)
        EXPECT_EQ_VAL(e.correct_length,4u)
    }
}

}

}

}
