//@+leo-ver=5-thin
//@+node:gcross.20110430163445.2593: * @file yaml.hpp
//@@language cplusplus
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
#include <boost/format.hpp>
#include <complex>
#include <yaml-cpp/yaml.h>

#include "nutcracker/operators.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110430163445.2599: ** << Usings >>
using boost::format;

using std::string;
//@-<< Usings >>

//@+others
//@+node:gcross.20110726215559.2294: ** Exceptions
//@+node:gcross.20110726215559.2295: *3* YAMLInputError
struct YAMLInputError: public std::runtime_error {
    YAML::Mark mark;
    YAMLInputError(YAML::Mark const& mark, string const& message);
};
//@+node:gcross.20110726215559.2296: *3* NonSquareMatrixYAMLInputError
struct NonSquareMatrixYAMLInputError: public YAMLInputError {
    unsigned int length;
    NonSquareMatrixYAMLInputError(YAML::Mark const& mark, unsigned int const length);
};
//@+node:gcross.20110726215559.2307: *3* IndexTooLowYAMLInputError
struct IndexTooLowYAMLInputError: public YAMLInputError {
    string name;
    unsigned int index;
    IndexTooLowYAMLInputError(YAML::Mark const& mark, string const& name, int const index);
    virtual ~IndexTooLowYAMLInputError() throw ();
};
//@+node:gcross.20110726215559.2313: *3* IndexTooHighYAMLInputError
struct IndexTooHighYAMLInputError: public YAMLInputError {
    string name;
    unsigned int index, dimension;
    IndexTooHighYAMLInputError(YAML::Mark const& mark, string const& name, unsigned int const index, unsigned int const dimension);
    virtual ~IndexTooHighYAMLInputError() throw ();
};
//@+node:gcross.20110726215559.2329: *3* WrongDataLengthYAMLInputError
struct WrongDataLengthYAMLInputError: public YAMLInputError {
    unsigned int length, correct_length;
    WrongDataLengthYAMLInputError(YAML::Mark const& mark, unsigned int const length, unsigned int const correct_length);
};
//@-others

}

//@+<< Outside namespace >>
//@+node:gcross.20110511190907.3542: ** << Outside namespace >>
//@+others
//@+node:gcross.20110430163445.2622: *3* I/O Operators
//! \defgroup YAMLSerializationOperators YAML serialization operators
//! @{

namespace YAML {

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

//! Reads an OperatorSiteLink from a YAML node.
void operator >> (const YAML::Node& node, Nutcracker::OperatorSiteLink& link);

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

//! Write an OperatorSiteLink to a YAML document.
YAML::Emitter& operator << (YAML::Emitter& emitter, Nutcracker::OperatorSiteLink const& link);

//! Write an OperatorSite to a YAML document.
YAML::Emitter& operator << (YAML::Emitter& emitter, Nutcracker::OperatorSite const& operator_site);

//! @}
//@-others

}

//! @}
//@+node:gcross.20110511190907.3543: *3* Boost
namespace boost {

template<> struct range_iterator<YAML::Node> { typedef YAML::Iterator type; };
template<> struct range_const_iterator<YAML::Node> { typedef YAML::Iterator type; };

namespace foreach { template<> struct is_noncopyable<YAML::Node> : mpl::true_ {}; }

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
