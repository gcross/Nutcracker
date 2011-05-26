//@+leo-ver=5-thin
//@+node:gcross.20110525201928.2433: * @file io.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110525200457.2437: ** << License >>
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
//@+node:gcross.20110525200457.2438: ** << Includes >>
#include <illuminate.hpp>

#include <boost/assign/list_of.hpp>
#include <boost/range/algorithm/equal.hpp>

#include "io.hpp"

using Nutcracker::InputFormat;
using Nutcracker::OutputFormat;

using boost::assign::list_of;
using boost::equal;
//@-<< Includes >>

//@+others
//@+node:gcross.20110525200457.2439: ** Tests
TEST_SUITE(IO) {

//@+others
//@+node:gcross.20110525200457.2440: *3* correct_formats_are_installed
TEST_SUITE(correct_formats_are_installed) {

//@+others
//@+node:gcross.20110525200457.2441: *4* input
TEST_CASE(Input) {
    ASSERT_TRUE(equal(InputFormat::listNames(),list_of("hdf")("yaml")));
}
//@+node:gcross.20110525200457.2443: *4* output
TEST_CASE(Output) {
    ASSERT_TRUE(equal(InputFormat::listNames(),list_of("hdf")("yaml")));
}
//@-others

}
//@+node:gcross.20110525201928.2443: *3* correct_extensions_are_identified
TEST_SUITE(correct_extensions_are_installed) {

//@+others
//@+node:gcross.20110525201928.2444: *4* input
TEST_SUITE(Input) {

TEST_CASE(h5) { ASSERT_EQ(InputFormat::lookupExtension("h5"),&InputFormat::lookupName("hdf")); }
TEST_CASE(hdf) { ASSERT_EQ(InputFormat::lookupExtension("hdf"),&InputFormat::lookupName("hdf")); }
TEST_CASE(hdf5) { ASSERT_EQ(InputFormat::lookupExtension("hdf5"),&InputFormat::lookupName("hdf")); }

TEST_CASE(yaml) { ASSERT_EQ(InputFormat::lookupExtension("yaml"),&InputFormat::lookupName("yaml")); }

}
//@+node:gcross.20110525201928.2445: *4* output
TEST_SUITE(Output) {

TEST_CASE(h5) { ASSERT_EQ(OutputFormat::lookupExtension("h5"),&OutputFormat::lookupName("hdf")); }
TEST_CASE(hdf) { ASSERT_EQ(OutputFormat::lookupExtension("hdf"),&OutputFormat::lookupName("hdf")); }
TEST_CASE(hdf5) { ASSERT_EQ(OutputFormat::lookupExtension("hdf5"),&OutputFormat::lookupName("hdf")); }

TEST_CASE(yaml) { ASSERT_EQ(OutputFormat::lookupExtension("yaml"),&OutputFormat::lookupName("yaml")); }

}
//@-others

}
//@-others

}
//@-others
//@-leo