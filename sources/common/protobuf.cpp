//@+leo-ver=5-thin
//@+node:gcross.20110901221152.2671: * @file protobuf.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110901221152.2672: ** << License >>
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
//@+node:gcross.20110901221152.2673: ** << Includes >>
#include "protobuf.hpp"
#include "states.hpp"
//@-<< Includes >>

//@+<< Usings >>
//@+node:gcross.20110901221152.2674: ** << Usings >>
using boost::container::vector;
using boost::optional;
//@-<< Usings >>

//@+others
//@+node:gcross.20110901221152.2675: ** I/O Operators
//@+node:gcross.20110902105950.2689: *3* State
void operator<<(Nutcracker::Protobuf::State& buffer, Nutcracker::State const& state) {
    using namespace Nutcracker;
    buffer.clear_sites();
    static_cast<Protobuf::StateSite&>(*buffer.add_sites()) << static_cast<StateSite<Middle> const&>(state.getFirstSite());
    BOOST_FOREACH(StateSite<Right> const& state_site, state.getRestSites()) {
        (*buffer.add_sites()) << state_site;
    }
}

void operator>>(Nutcracker::Protobuf::State const& buffer, Nutcracker::State& tensor) {
    using namespace Nutcracker;
    StateSite<Middle> first_site;
    vector<StateSite<Right> > rest_sites(buffer.sites_size()-1);
    buffer.sites(0) >> first_site;
    BOOST_FOREACH(unsigned int const index, irange<unsigned int>(0u,rest_sites.size())) {
        buffer.sites(index+1) >> rest_sites[index];
    }
    tensor = State(boost::move(first_site),boost::move(rest_sites));
}
//@-others
//@-leo
