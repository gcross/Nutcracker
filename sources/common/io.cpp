//@+leo-ver=5-thin
//@+node:gcross.20110511190907.3549: * @file io.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110511190907.3551: ** << License >>
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
//@+node:gcross.20110511190907.3552: ** << Includes >>
#include "io.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110511190907.3553: ** << Usings >>
//@-<< Usings >>

//@+others
//@+node:gcross.20110511190907.3615: ** Values
const char* input_format_type_name = "input";
const char* output_format_type_name = "output";
//@+node:gcross.20110511190907.3818: ** Exceptions
//@+node:gcross.20110511190907.3819: *3* FormatTypeException
vector<string> FormatTypeException::getAcceptedFormatNames() const {
    if(format_type_name == "input") return InputFormat::listNames();
    if(format_type_name == "output") return OutputFormat::listNames();
    return vector<string>();
}
//@+node:gcross.20110511190907.3554: ** Functions
//@+node:gcross.20110511190907.3556: *3* constructOperatorFrom
Operator constructOperatorFrom(
    vector<shared_ptr<OperatorSite const> > unique_operator_sites
  , vector<unsigned int> sequence
) {
    Operator operator_sites;
    operator_sites.reserve(sequence.size());

    BOOST_FOREACH(unsigned int index, sequence) {
        if(index >= unique_operator_sites.size())
            throw NoSuchOperatorSiteNumber(index);
        shared_ptr<OperatorSite const> operator_site_ptr = unique_operator_sites[index];
        if(operator_sites.empty()) {
            assert(operator_site_ptr->leftDimension(as_unsigned_integer) == 1);
        } else {
            assert(operator_site_ptr->leftDimension(as_unsigned_integer) == operator_sites.back()->rightDimension(as_unsigned_integer));
        }
        operator_sites.emplace_back(operator_site_ptr);
    }

    return boost::move(operator_sites);
}
//@+node:gcross.20110511190907.3560: *3* deconstructOperatorTo
void deconstructOperatorTo(
    Operator const& operator_sites
  , vector<shared_ptr<OperatorSite const> >& unique_operator_sites
  , vector<unsigned int>& sequence
) {
    unique_operator_sites.clear();
    sequence.clear();
    sequence.reserve(operator_sites.size());

    typedef map<shared_ptr<OperatorSite const>,unsigned int> SequenceNumberMap;
    SequenceNumberMap sequence_number_map;

    unsigned int next_sequence_number = 0;
    BOOST_FOREACH(shared_ptr<OperatorSite const> const operator_site_ptr, operator_sites) {
        SequenceNumberMap::iterator sequence_number_iterator = sequence_number_map.find(operator_site_ptr);
        if(sequence_number_iterator == sequence_number_map.end()) {
            sequence.push_back(next_sequence_number);
            sequence_number_map.insert(make_pair(operator_site_ptr,next_sequence_number++));
            unique_operator_sites.push_back(operator_site_ptr);
        } else {
            sequence.push_back(sequence_number_iterator->second);
        }
    }
}
//@+node:gcross.20110511190907.3759: *3* installFormats
namespace HDF { void installFormat(); }
void installYAMLFormat();

void installFormats() {
    static bool called = false;
    if(called) return;
    called = true;
    HDF::installFormat();
    installYAMLFormat();
    InputFormat::default_format = &InputFormat::lookupName("yaml");
    OutputFormat::default_format = &OutputFormat::lookupName("yaml");
}
//@-others

}
//@-leo
