//@+leo-ver=5-thin
//@+node:gcross.20110430163445.2593: * @file yaml.hpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110430163445.2595: ** << License >>
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

//@+<< Documentation >>
//@+node:gcross.20110430163445.2597: ** << Documentation >>
/*!
\file yaml.hpp
\brief YAML serialization functions
*/
//@-<< Documentation >>

#ifndef NUTCRACKER_YAML_HPP
#define NUTCRACKER_YAML_HPP

//@+<< Includes >>
//@+node:gcross.20110430163445.2598: ** << Includes >>
#include <complex>
#include <yaml-cpp/yaml.h>

#include "operators.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110430163445.2599: ** << Usings >>
//@-<< Usings >>

//@+others
//@-others

}

//@+<< Outside namespace >>
//@+node:gcross.20110511190907.3542: ** << Outside namespace >>
//@+others
//@+node:gcross.20110430163445.2622: *3* I/O Operators
//! \defgroup YAMLSerializationOperators YAML serialization operators
//! @{

//@+others
//@+node:gcross.20110430163445.2653: *4* Reading
//! \defgroup YAMLSerializationOperatorsReading Reading
//! @{

//@+others
//@+node:gcross.20110430163445.2629: *5* complex<T>
//! Reads a complex number from a YAML scalar (real part only) or sequence (real part then imaginary part).
template<typename T> inline void operator>>(YAML::Node const& node,std::complex<T>& x) {
    switch(node.Type()) {
        case YAML::NodeType::Scalar:
            node >> x.real();
            x.imag() = 0;
            return;
        case YAML::NodeType::Sequence:
            assert(node.size() == 2);
            node[0] >> x.real();
            node[1] >> x.imag();
            return;
        default: assert(!"bad node type");
    }
}
//@-others

//! Reads an Operator from a YAML node.
void operator >> (const YAML::Node& node, Nutcracker::Operator& operator_site);

//! Reads an OperatorLink from a YAML node.
void operator >> (const YAML::Node& node, Nutcracker::OperatorLink& link);

//! Reads an OperatorSite from a YAML node.
void operator >> (const YAML::Node& node, Nutcracker::OperatorSite& operator_site);


//! @}
//@+node:gcross.20110430163445.2654: *4* Writing
//! \defgroup YAMLSerializationOperatorsWriting Writing
//! @{

//@+others
//@+node:gcross.20110430163445.2630: *5* complex<T>
//! Write a complex number to a YAML document.
template<typename T> inline YAML::Emitter& operator<<(YAML::Emitter& out,std::complex<T> const& x) {
    using boost::format;
    if(x.imag() == 0) {
        return out << (format("%|.20|") % x.real()).str();
    } else {
        return out
            << YAML::Flow << YAML::BeginSeq
                << (format("%|.20|") % x.real()).str()
                << (format("%|.20|") % x.imag()).str()
            << YAML::EndSeq;
    }
}
//@-others

//! Write an Operator to a YAML document.
YAML::Emitter& operator << (YAML::Emitter& emitter, Nutcracker::Operator const& operator_site);

//! Write an OperatorLink to a YAML document.
YAML::Emitter& operator << (YAML::Emitter& emitter, Nutcracker::OperatorLink const& link);

//! Write an OperatorSite to a YAML document.
YAML::Emitter& operator << (YAML::Emitter& emitter, Nutcracker::OperatorSite const& operator_site);

//! @}
//@-others

//! @}
//@+node:gcross.20110511190907.3543: *3* Boost
namespace boost {

template<> struct range_iterator<YAML::Node> { typedef YAML::Iterator type; };
template<> struct range_const_iterator<YAML::Node> { typedef YAML::Iterator type; };

}
//@+node:gcross.20110511190907.3544: *3* std
namespace std {

template<> struct iterator_traits<YAML::Iterator> {
    typedef YAML::Node const value_type;
    typedef value_type* pointer;
    typedef value_type& reference;
    typedef input_iterator_tag iterator_category;
};

}
//@-others
//@-<< Outside namespace >>

#endif
//@-leo
