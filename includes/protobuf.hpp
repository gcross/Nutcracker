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

#include "tensors.hpp"
//@-<< Includes >>

//@+others
//@+node:gcross.20110901221152.2653: ** I/O Operators
//@+node:gcross.20110901221152.2676: *3* State
void operator<<(Nutcracker::Protobuf::State& buffer, Nutcracker::State const& state);
void operator>>(Nutcracker::Protobuf::State const& buffer, Nutcracker::State& tensor);
//@+node:gcross.20110901221152.2654: *3* StateSite
namespace Nutcracker {

    namespace Protobuf {

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


        template<typename side> void storeNormalizationInto(StateSite& buffer) {
            boost::optional<Normalization> const& normalization = normalizationForProtobufOf<side>::get();
            if(normalization) {
                buffer.set_normalization(*normalization);
            } else {
                buffer.clear_normalization();
            }
        }

        inline boost::optional<std::string> const& loadNormalizationFrom(StateSite const& buffer) {
            if(buffer.has_normalization()) {
                switch(buffer.normalization()) {
                    case MIDDLE: return normalizationOf<Middle>::value;
                    case LEFT: return normalizationOf<Left>::value;
                    case RIGHT: return normalizationOf<Right>::value;
                }
            }
            return normalizationOf<None>::value;
        }

    }

}

template<typename side> void operator<<(Nutcracker::Protobuf::StateSite& buffer, Nutcracker::StateSite<side> const& tensor) {
    using namespace Nutcracker;
    Protobuf::storeNormalizationInto<side>(buffer);
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

template<typename side> void operator>>(Nutcracker::Protobuf::StateSite const& buffer, Nutcracker::StateSite<side>& tensor) {
    using namespace Nutcracker;
    assertNormalizationIs<side>(Protobuf::loadNormalizationFrom(buffer));
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

#endif
//@-leo
