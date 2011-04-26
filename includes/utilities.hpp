//@+leo-ver=5-thin
//@+node:gcross.20110125202132.2156: * @thin utilities.hpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2022: ** << License >>
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

#ifndef NUTCRACKER_UTILITIES_HPP
#define NUTCRACKER_UTILITIES_HPP

//@+<< Includes >>
//@+node:gcross.20110125202132.2157: ** << Includes >>
#include <boost/concept_check.hpp>
#include <boost/container/vector.hpp>
#include <boost/foreach.hpp>
#include <boost/format.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <boost/range/algorithm/reverse.hpp>
#include <boost/range/algorithm/reverse_copy.hpp>
#include <boost/range/concepts.hpp>
#include <boost/range/irange.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/move/move.hpp>
#include <boost/none_t.hpp>
#include <complex>
#include <exception>
#include <functional>
#include <iterator>
#include <numeric>
#include <sstream>
#include <stdint.h>
#include <string>
#include <typeinfo>
#include <yaml-cpp/yaml.h>
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110125202132.2158: ** << Usings >>
using boost::adaptors::reversed;
using boost::container::vector;
using boost::format;
using boost::forward_traversal_tag;
using boost::irange;
using boost::iterator_facade;
using boost::iterator_range;
using boost::make_iterator_range;
using boost::RandomAccessRangeConcept;
using boost::reverse;
using boost::reverse_copy;

using std::abs;
using std::accumulate;
using std::complex;
using std::iterator_traits;
using std::min;
using std::multiplies;
using std::numeric_limits;
using std::string;
using std::type_info;
//@-<< Usings >>

//@+others
//@+node:gcross.20110215135633.1864: ** Type aliases
typedef boost::none_t None;
//@+node:gcross.20110202200838.1710: ** Exceptions
//@+node:gcross.20110125202132.2159: *3* Exception
struct Exception : public std::exception {
    string const message;
    Exception(string const& message);
    virtual char const* what() const throw();
    virtual ~Exception() throw();
};
//@+node:gcross.20110202200838.1709: *3* BadProgrammerException
struct BadProgrammerException : public Exception {
    BadProgrammerException(string const& message) : Exception("BAD PROGRAMMER!!! --- " + message) {}
};
//@+node:gcross.20110206185121.1786: *3* BadLabelException
struct BadLabelException : public BadProgrammerException {
    BadLabelException(string const& symbol, type_info const& type)
      : BadProgrammerException((
          format("Attempted to access templated symbol %1% with invalid type label %2%.")
            % symbol
            % type.name()
        ).str())
    {}
};
//@+node:gcross.20110217175626.1943: *3* RequestedBandwidthDimensionTooLargeError
struct RequestedBandwidthDimensionTooLargeError : public Exception {
    unsigned int const
          requested_bandwidth_dimension
        , greatest_possible_bandwidth_dimension
        ;
    RequestedBandwidthDimensionTooLargeError(
        unsigned int const requested_bandwidth_dimension
      , unsigned int const greatest_possible_bandwidth_dimension
    ) : Exception((
            format("Requested bandwidth dimension %1% is too large;  the highest possible with the given physical dimensions is %2%.")
                % requested_bandwidth_dimension
                % greatest_possible_bandwidth_dimension
        ).str())
      , requested_bandwidth_dimension(requested_bandwidth_dimension)
      , greatest_possible_bandwidth_dimension(greatest_possible_bandwidth_dimension)
    {}
};
//@+node:gcross.20110211120708.1791: ** Classes
//@+node:gcross.20110211120708.1793: *3* ProductIterator
template<typename T> class ProductIterator
 : public iterator_facade<ProductIterator<T>,typename iterator_traits<T>::value_type const,forward_traversal_tag>
{
protected:
    T nested_iterator;
    typename iterator_traits<T>::value_type product;
    mutable typename iterator_traits<T>::value_type cached_product;
public:
    ProductIterator() {}

    ProductIterator(T const nested_iterator)
      : nested_iterator(nested_iterator)
      , product(1)
    {}

    typename iterator_traits<T>::value_type const& dereference() const {
        cached_product = product * (*nested_iterator);
        return cached_product;
    }

    bool equal(ProductIterator const other) const {
        return nested_iterator == other.nested_iterator;
    }

    void increment() {
        product *= (*nested_iterator);
        ++nested_iterator;
    }
};
//@+node:gcross.20110127123226.2857: ** Functions
unsigned long long choose(unsigned int n, unsigned int k);

vector<unsigned int> computeBandwidthDimensionSequence(
    unsigned int const requested_bandwidth_dimension
   ,vector<unsigned int> const& physical_dimensions
);

//@+others
//@+node:gcross.20110211120708.1786: *3* c
inline complex<double> c(double x, double y) { return complex<double>(x,y); }
//@+node:gcross.20110211120708.1787: *3* copyAndReset
template<typename T> inline T copyAndReset(T& x) {
    T const old_x = x;
    x = 0;
    return old_x;
}
//@+node:gcross.20110211120708.1789: *3* dznrm2
extern "C" double dznrm2_(uint32_t const* n, complex<double>* const x, uint32_t const* incx);
inline double dznrm2(uint32_t const n, complex<double>* const x, uint32_t const incx=1) { return dznrm2_(&n,x,&incx); }
//@+node:gcross.20110215135633.1856: *3* flatIndexToTensorIndex
template<typename DimensionRange> vector<unsigned int> flatIndexToTensorIndex(DimensionRange const& dimensions, unsigned long long flat_index) {
    vector<unsigned int> tensor_index;
    tensor_index.reserve(dimensions.size());
    BOOST_FOREACH(unsigned int const dimension, dimensions | reversed) {
        tensor_index.push_back(flat_index % dimension);
        flat_index /= dimension;
    }
    assert(flat_index == 0);
    reverse(tensor_index);
    return boost::move(tensor_index);
}
//@+node:gcross.20110211120708.1798: *3* makeProductIterator
template<typename T> ProductIterator<T> makeProductIterator(T const x) { return ProductIterator<T>(x); }
//@+node:gcross.20110217175626.1941: *3* maximumBandwidthDimension
template<typename PhysicalDimensionRange> unsigned int maximumBandwidthDimension(
    PhysicalDimensionRange const& physical_dimensions
) {
    BOOST_CONCEPT_ASSERT((RandomAccessRangeConcept<PhysicalDimensionRange const>));
    if(physical_dimensions.size() == 1) return 1;
    size_t middle_index = (physical_dimensions.size()+1)/2;

    return min(
        accumulate(
             physical_dimensions.begin()
            ,physical_dimensions.begin()+middle_index
            ,1
            ,multiplies<unsigned int>()
        )
       ,accumulate(
             physical_dimensions.rbegin()
            ,physical_dimensions.rbegin()+middle_index
            ,1
            ,multiplies<unsigned int>()
        )
    );
}
//@+node:gcross.20110211120708.1788: *3* moveArrayToFrom
template<typename T> inline void moveArrayToFrom(T*& to, T*& from) {
    if(to) delete[] to;
    to = copyAndReset(from);
}
//@+node:gcross.20110219083229.2603: *3* outsideTolerance
template<typename A, typename B, typename C> bool outsideTolerance(A a, B b, C tolerance) {
    return ((abs(a)+abs(b))/2 > tolerance) && (abs(a-b)/(abs(a)+abs(b)+tolerance) > tolerance);
}
//@+node:gcross.20110213233103.3637: *3* rethrow
template<typename Exception> void rethrow(Exception& e) { throw e; }
//@+node:gcross.20110215135633.1858: *3* tensorIndexToFlatIndex
template<typename DimensionRange> unsigned long long tensorIndexToFlatIndex(DimensionRange const& dimensions, vector<unsigned int> const& tensor_index) {
    assert(dimensions.size() == tensor_index.size());
    unsigned long long flat_index = 0;
    BOOST_FOREACH(unsigned int const i, irange(0u,(unsigned int)dimensions.size())) {
        assert(tensor_index[i] < dimensions[i]);
        flat_index *= dimensions[i];
        flat_index += tensor_index[i];
    }
    return flat_index;
}
//@+node:gcross.20110215135633.1901: *3* zgemm
extern "C" void zgemm_(
    char const* transa, char const* transb,
    uint32_t const* m, uint32_t const* n, uint32_t const* k,
    complex<double> const* alpha,
    complex<double> const* a, uint32_t const* lda,
    complex<double> const* b, uint32_t const* ldb,
    complex<double> const* beta,
    complex<double>* c, uint32_t const* ldc
);
inline void zgemm(
    char const* transa, char const* transb,
    uint32_t const m, uint32_t const n, uint32_t const k,
    complex<double> const alpha,
    complex<double> const* a, uint32_t const lda,
    complex<double> const* b, uint32_t const ldb,
    complex<double> const beta,
    complex<double>* c, uint32_t const ldc
) {
    zgemm_(
        transa,transb,
        &m,&n,&k,
        &alpha,
        a,&lda,
        b,&ldb,
        &beta,
        c,&ldc
    );        
}
//@+node:gcross.20110215135633.1903: *3* zgemv
extern "C" void zgemv_(
    char const* trans,
    uint32_t const* m, uint32_t const* n,
    complex<double> const* alpha,
    complex<double> const* a, uint32_t const* lda,
    complex<double> const* x, uint32_t const* incx,
    complex<double> const* beta,
    complex<double>* y, uint32_t const* incy
);
inline void zgemv(
    char const* trans,
    uint32_t const m, uint32_t const n,
    complex<double> const alpha,
    complex<double> const* a, uint32_t const lda,
    complex<double> const* x, uint32_t const incx,
    complex<double> const beta,
    complex<double>* y, uint32_t const incy
) {
    zgemv_(
        trans,
        &m,&n,
        &alpha,
        a,&lda,
        x,&incx,
        &beta,
        y,&incy
    );        
}
//@-others
//@+node:gcross.20110219101843.2618: ** I/O
//@+node:gcross.20110220093853.1990: *3* complex<T>
//@+node:gcross.20110219101843.2619: *4* >>
template<typename T> inline void operator>>(YAML::Node const& node,complex<T>& x) {
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
//@+node:gcross.20110219101843.2623: *4* <<
template<typename T> inline YAML::Emitter& operator<<(YAML::Emitter& out,complex<T> const& x) {
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
//@+node:gcross.20110129220506.1652: ** Macros
#define REPEAT(n) for(unsigned int _counter##__LINE__ = 0; _##counter##__LINE__ < n; ++_##counter##__LINE__)
//@-others

}

#endif
//@-leo
