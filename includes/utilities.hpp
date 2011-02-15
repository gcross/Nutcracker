//@+leo-ver=5-thin
//@+node:gcross.20110125202132.2156: * @thin utilities.hpp
//@@language cplusplus

#ifndef NUTCRACKER_UTILITIES_HPP
#define NUTCRACKER_UTILITIES_HPP

//@+<< Includes >>
//@+node:gcross.20110125202132.2157: ** << Includes >>
#include <boost/container/vector.hpp>
#include <boost/foreach.hpp>
#include <boost/format.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <boost/range/algorithm/reverse.hpp>
#include <boost/range/irange.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/move/move.hpp>
#include <complex>
#include <exception>
#include <iterator>
#include <stdint.h>
#include <string>
#include <typeinfo>
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
using boost::reverse;

using std::complex;
using std::iterator_traits;
using std::string;
using std::type_info;
//@-<< Usings >>

//@+others
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
//@+node:gcross.20110211120708.1788: *3* moveArrayToFrom
template<typename T> inline void moveArrayToFrom(T*& to, T*& from) {
    if(to) delete[] to;
    to = copyAndReset(from);
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
//@-others
//@+node:gcross.20110129220506.1652: ** Macros
#define REPEAT(n) for(unsigned int _counter##__LINE__ = 0; _##counter##__LINE__ < n; ++_##counter##__LINE__)
//@-others

}

#endif
//@-leo
