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
#include "nutcracker/io.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110511190907.3553: ** << Usings >>
//@-<< Usings >>

//@+others
//@+node:gcross.20110511190907.3615: ** Values
const char* input_format_type_name = "input";
const char* output_format_type_name = "output";

FormatInstaller const FormatInstaller::_;
//@+node:gcross.20110525201928.2448: ** Format installation
namespace HDF { void installFormat(); }
namespace Protobuf { void installFormat(); }
void installYAMLFormat();

FormatInstaller::FormatInstaller() {
    HDF::installFormat();
    Protobuf::installFormat();
    installYAMLFormat();
    InputFormat::default_format = &InputFormat::lookupName("yaml");
    OutputFormat::default_format = &OutputFormat::lookupName("yaml");
}
//@+node:gcross.20110511190907.3818: ** Exceptions
//@+node:gcross.20110511190907.3819: *3* FormatTypeException
vector<string> FormatTypeException::getAcceptedFormatNames() const {
    if(format_type_name == "input") return InputFormat::listNames();
    if(format_type_name == "output") return OutputFormat::listNames();
    return vector<string>();
}
//@+node:gcross.20110511190907.3554: ** Functions
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
//@-others

}
//@-leo
