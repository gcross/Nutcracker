/*!
\file yaml.hpp
\brief YAML serialization functions
*/

#ifndef NUTCRACKER_YAML_HPP
#define NUTCRACKER_YAML_HPP

#include <boost/format.hpp>
#include <complex>
#include <yaml-cpp/yaml.h>

#include "nutcracker/operators.hpp"

namespace Nutcracker {

using boost::format;

using std::string;

struct YAMLInputError: public std::runtime_error {
    YAMLInputError(string const& message);
};
struct NonSquareMatrixYAMLInputError: public YAMLInputError {
    unsigned int length;
    NonSquareMatrixYAMLInputError(unsigned int const length);
};
struct IndexTooLowYAMLInputError: public YAMLInputError {
    string name;
    unsigned int index;
    IndexTooLowYAMLInputError(string const& name, int const index);
    virtual ~IndexTooLowYAMLInputError() throw ();
};
struct IndexTooHighYAMLInputError: public YAMLInputError {
    string name;
    unsigned int index, dimension;
    IndexTooHighYAMLInputError(string const& name, unsigned int const index, unsigned int const dimension);
    virtual ~IndexTooHighYAMLInputError() throw ();
};
struct WrongDataLengthYAMLInputError: public YAMLInputError {
    unsigned int length, correct_length;
    WrongDataLengthYAMLInputError(unsigned int const length, unsigned int const correct_length);
    virtual ~WrongDataLengthYAMLInputError() throw ();
};

}

//! \defgroup YAMLSerializationOperators YAML serialization operators
//! @{

namespace YAML {

//! \defgroup YAMLSerializationOperatorsReading Reading
//! @{

//! Reads a complex number from a YAML scalar (real part only) or sequence (real part then imaginary part).
template<typename T> inline void operator>>(YAML::Node const& node,std::complex<T>& x) {
    if(node.IsScalar()) {
        x = node.as<T>();
    } else if(node.IsSequence()) {
        x.real() = node[0].as<T>();
        x.imag() = node[1].as<T>();
    } else if(node.IsMap()) {
        x.real() = node["real"].as<T>();
        x.imag() = node["imag"].as<T>();
    } else {
        assert(!"bad node type for complex number");
    }
}

//! Reads an Operator from a YAML node.
void operator >> (const YAML::Node& node, Nutcracker::Operator& operator_site);

//! Reads an OperatorSiteLink from a YAML node.
void operator >> (const YAML::Node& node, Nutcracker::OperatorSiteLink& link);

//! Reads an OperatorSite from a YAML node.
void operator >> (const YAML::Node& node, Nutcracker::OperatorSite& operator_site);


//! @}
//! \defgroup YAMLSerializationOperatorsWriting Writing
//! @{

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

//! Write an Operator to a YAML document.
YAML::Emitter& operator << (YAML::Emitter& emitter, Nutcracker::Operator const& operator_site);

//! Write an OperatorSiteLink to a YAML document.
YAML::Emitter& operator << (YAML::Emitter& emitter, Nutcracker::OperatorSiteLink const& link);

//! Write an OperatorSite to a YAML document.
YAML::Emitter& operator << (YAML::Emitter& emitter, Nutcracker::OperatorSite const& operator_site);

//! @}

}

//! @}
namespace boost {

template<> struct range_iterator<YAML::Node> { typedef YAML::Node::iterator type; };
template<> struct range_const_iterator<YAML::Node> { typedef YAML::Node::const_iterator type; };

namespace foreach { template<> struct is_noncopyable<YAML::Node> : mpl::true_ {}; }

}
namespace std {

template<> struct iterator_traits<YAML::Node::iterator> {
    typedef YAML::Node const value_type;
    typedef value_type* pointer;
    typedef value_type& reference;
    typedef input_iterator_tag iterator_category;
};

}

#endif
