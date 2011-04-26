//@+leo-ver=5-thin
//@+node:gcross.20110307093706.3117: * @file encode_than_decode.cpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.3118: ** << License >>
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
//@+node:gcross.20110307093706.3119: ** << Includes >>
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

        ASSERT_EQUAL(link1.from,link2.from);
        ASSERT_EQUAL(link1.to,link2.to);
        BOOST_FOREACH(unsigned int const i, irange(0u,n*n)) {
            ASSERT_EQUAL(link2.matrix.data()[i],link1.matrix.data()[i]);
        }
    }

    return 0;
}
//@-leo
