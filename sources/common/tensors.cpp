//@+leo-ver=5-thin
//@+node:gcross.20110215235924.1990: * @file tensors.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2040: ** << License >>
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
//@+node:gcross.20110215235924.1991: ** << Includes >>
#include <map>
#include <yaml-cpp/yaml.h>

#include "tensors.hpp"
#include "utilities.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110215235924.1992: ** << Usings >>
using std::make_pair;
using std::map;
//@-<< Usings >>

//@+others
//@+node:gcross.20110215235924.2000: ** Values
OperatorSite const OperatorSite::trivial(make_trivial);

DEFINE_DUMMY_PARAMETER(MakeTrivial,make_trivial)
DEFINE_DUMMY_PARAMETER(AsUnsignedInteger,as_unsigned_integer)

optional<string> const normalizationOf<Left>::value("left");
optional<string> const normalizationOf<Middle>::value("middle");
optional<string> const normalizationOf<Right>::value("right");
optional<string> const normalizationOf<None>::value(none);
//@-others

}
//@-leo
