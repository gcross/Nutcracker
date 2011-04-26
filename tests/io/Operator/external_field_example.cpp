//@+leo-ver=5-thin
//@+node:gcross.20110307093706.2449: * @file external_field_example.cpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.2450: ** << License >>
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
//@+node:gcross.20110307093706.2451: ** << Includes >>
#include <algorithm>
#include <boost/range/algorithm/equal.hpp>
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
int main() {
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

        assertOperatorsEqual(operator_1,operator_2);
    }

    return 0;
}
//@-leo
