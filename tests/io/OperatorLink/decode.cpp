//@+leo-ver=5-thin
//@+node:gcross.20110307093706.3111: * @file decode.cpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.3112: ** << License >>
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
//@+node:gcross.20110307093706.3113: ** << Includes >>
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

        ASSERT_EQUAL(from,link.from);
        ASSERT_EQUAL(to,link.to);
        BOOST_FOREACH(unsigned int const i, irange(0u,n*n)) {
            ASSERT_EQUAL(data[i],link.matrix.data()[i]);
        }
    }

    return 0;
}
//@-leo
