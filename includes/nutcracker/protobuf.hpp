//@+leo-ver=5-thin
//@+node:gcross.20110901221152.2649: * @file protobuf.hpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110901221152.2650: ** << License >>
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

#ifndef NUTCRACKER_PROTOBUF_HPP
#define NUTCRACKER_PROTOBUF_HPP

//@+<< Includes >>
//@+node:gcross.20110901221152.2651: ** << Includes >>
#include <boost/none.hpp>
#include "nutcracker.pb.h"

#include "nutcracker/chain.hpp"
#include "nutcracker/tensors.hpp"
//@-<< Includes >>

namespace Nutcracker { namespace Protobuf {

//@+others
//@+node:gcross.20110901221152.2653: ** I/O Operators
//@+others
//@+node:gcross.20110902105950.2692: *3* OperatorSite
void operator<<(OperatorSiteBuffer& buffer, OperatorSite const& tensor);
void operator>>(OperatorSiteBuffer const& buffer, OperatorSite& tensor);
//@+node:gcross.20110902105950.2700: *3* Operator
void operator<<(OperatorBuffer& buffer, Operator const& op);
void operator>>(OperatorBuffer const& buffer, Operator& op);
//@+node:gcross.20110901221152.2676: *3* State
void operator<<(StateBuffer& buffer, Chain const& chain);
void operator<<(StateBuffer& buffer, State const& state);
void operator>>(StateBuffer const& buffer, State& tensor);
//@+node:gcross.20110901221152.2654: *3* StateSite
template<typename Side> struct normalizationForProtobufOf {};

template<> struct normalizationForProtobufOf<Left> {
    static boost::optional<Normalization> const get() { return LEFT; }
};

template<> struct normalizationForProtobufOf<Middle> {
    static boost::optional<Normalization> const get() { return MIDDLE; }
};

template<> struct normalizationForProtobufOf<Right> {
    static boost::optional<Normalization> const get() { return RIGHT; }
};

template<> struct normalizationForProtobufOf<None> {
    static boost::optional<Normalization> const get() { return none; }
};


template<typename side> void storeNormalizationInto(StateSiteBuffer& buffer) {
    boost::optional<Normalization> const& normalization = normalizationForProtobufOf<side>::get();
    if(normalization) {
        buffer.set_normalization(*normalization);
    } else {
        buffer.clear_normalization();
    }
}

inline boost::optional<std::string> const& loadNormalizationFrom(StateSiteBuffer const& buffer) {
    if(buffer.has_normalization()) {
        switch(buffer.normalization()) {
            case MIDDLE: return normalizationOf<Middle>::value;
            case LEFT: return normalizationOf<Left>::value;
            case RIGHT: return normalizationOf<Right>::value;
        }
    }
    return normalizationOf<None>::value;
}

template<typename side> void operator<<(StateSiteBuffer& buffer, StateSite<side> const& tensor) {
    storeNormalizationInto<side>(buffer);
    buffer.set_physical_dimension(tensor.physicalDimension());
    buffer.set_left_dimension(tensor.leftDimension());
    buffer.set_right_dimension(tensor.rightDimension());
    google::protobuf::RepeatedField<double>& data = *buffer.mutable_data();
    data.Clear();
    data.Reserve(2*tensor.size());
    BOOST_FOREACH(complex<double> const& x, tensor) {
        (*data.AddAlreadyReserved()) = x.real();
        (*data.AddAlreadyReserved()) = x.imag();
    }
}

template<typename side> void operator>>(StateSiteBuffer const& buffer, StateSite<side>& tensor) {
    assertNormalizationIs<side>(loadNormalizationFrom(buffer));
    PhysicalDimension const physical_dimension(buffer.physical_dimension());
    LeftDimension     const left_dimension    (buffer.left_dimension());
    RightDimension    const right_dimension   (buffer.right_dimension());
    StateSite<side> state_site_tensor(physical_dimension,left_dimension,right_dimension);
    google::protobuf::RepeatedField<double> const& data = buffer.data();
    assert(state_site_tensor.size()*2 == (unsigned int)data.size());
    google::protobuf::RepeatedField<double>::const_iterator iter = data.begin();
    BOOST_FOREACH(std::complex<double>& x, state_site_tensor) {
        x.real() = *(iter++);
        x.imag() = *(iter++);
    }
    tensor = boost::move(state_site_tensor);
}
//@-others
//@+node:gcross.20110903120540.2687: ** Functions
//@+others
//@+node:gcross.20110903120540.2688: *3* setState
template<typename RestSites> void setState(StateBuffer& buffer,StateSite<Nutcracker::Middle> const& first_site, RestSites const& rest_sites) {
    buffer.clear_sites();
    static_cast<StateSiteBuffer&>(*buffer.add_sites()) << first_site;
    BOOST_FOREACH(StateSite<Right> const& state_site, rest_sites) {
        (*buffer.add_sites()) << state_site;
    }
}
//@-others
//@-others

} }

#endif
//@-leo
