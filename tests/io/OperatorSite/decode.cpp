//@+leo-ver=5-thin
//@+node:gcross.20110307093706.3105: * @file decode.cpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.3106: ** << License >>
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
//@+node:gcross.20110307093706.3107: ** << Includes >>
#include <boost/container/vector.hpp>
#include <boost/foreach.hpp>
#include <boost/format.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <boost/range/algorithm/generate.hpp>
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
int main() {
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

        ASSERT_EQUAL(physical_dimension,operator_site.physicalDimension(as_unsigned_integer));
        ASSERT_EQUAL(left_dimension,operator_site.leftDimension(as_unsigned_integer));
        ASSERT_EQUAL(right_dimension,operator_site.rightDimension(as_unsigned_integer));
        ASSERT_EQUAL(links.size(),operator_site.numberOfMatrices());

        uint32_t const* index_data = operator_site;
        complex<double> const* matrix_data = operator_site;
        BOOST_FOREACH(OperatorLink const& link, links) {
            ASSERT_EQUAL(link.from,index_data[0]);
            ASSERT_EQUAL(link.to,index_data[1]);
            index_data += 2;
            ASSERT_TRUE(equal(link.matrix.data().begin(),link.matrix.data().end(),matrix_data));
            matrix_data += physical_dimension*physical_dimension;
        }
    }

    return 0;
}
//@-leo
