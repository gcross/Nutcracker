//@+leo-ver=5-thin
//@+node:gcross.20110125202132.2156: * @file utilities.hpp
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

//@+<< Documentation >>
//@+node:gcross.20110429225820.2525: ** << Documentation >>
/*!
\file utilities.hpp
\brief Utility classes and functions
*/
//@-<< Documentation >>

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

//! \defgroup Utilities Utilities
//! @{

//@+others
//@+node:gcross.20110215135633.1864: ** Type aliases
//! None type tag
typedef boost::none_t None;
//@+node:gcross.20110202200838.1710: ** Exceptions
//@+node:gcross.20110125202132.2159: *3* Exception
//! A convenient base class for exceptions.
/*!
The feature that this class provides over std::exception is that rather than making you override the what() method yourself its constructor takes a string message that it then returns when the user calls what(), saving you from having to write some boilerplate code.
*/
struct Exception : public std::exception {
    //! The user-supplied message.
    string const message;
    //! The message to be stored in this exception.
    Exception(string const& message);
    //! Get a read-only pointer to the character string of this message.
    virtual char const* what() const throw();
    //! Destroy this message.
    virtual ~Exception() throw();
};
//@+node:gcross.20110202200838.1709: *3* BadProgrammerException
//! Exception indicating a programmer error.
struct BadProgrammerException : public Exception {
    //! Constructs this exception with the given message.
    BadProgrammerException(string const& message) : Exception("BAD PROGRAMMER!!! --- " + message) {}
};
//@+node:gcross.20110206185121.1786: *3* BadLabelException
//! Exception thrown when an generic symbol is accessed with an invalid type tag.
struct BadLabelException : public BadProgrammerException {
    //! Constructs this exception with the given symbol name and type tag.
    BadLabelException(string const& symbol, type_info const& type)
      : BadProgrammerException((
          format("Attempted to access templated symbol %1% with invalid type label %2%.")
            % symbol
            % type.name()
        ).str())
    {}
};
//@+node:gcross.20110217175626.1943: *3* RequestedBandwidthDimensionTooLargeError
//! This exception is thrown when the user requests a bandwidth dimension that is larger than the maximum possible given the physical dimension sequence.
struct RequestedBandwidthDimensionTooLargeError : public Exception {
    unsigned int const
          requested_bandwidth_dimension //!< requested bandwidth dimension
        , greatest_possible_bandwidth_dimension //!< greatest possible bandwidth dimension
        ;

    //! Constructs this exception with the given requested and greatest possible bandwidth dimensions.
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
//@+node:gcross.20110511190907.3636: *3* Destructable
struct Destructable { virtual ~Destructable() {} };
//@+node:gcross.20110211120708.1793: *3* ProductIterator
//@+<< Description >>
//@+node:gcross.20110429225820.2541: *4* << Description >>
//! Iterator class which accumulates the product of the values of a nested iterator.
/*!
This class acts like a STL iterator and specifically implements the Boost ForwardTraversal iterator concept, but most of the iterator methods might not be visible in the documentation because they are inherited from \c iterator_facade (which generates the requisite boilerplate code from the methods listed in "Iterator implementation").
*/
//@-<< Description >>
template<typename T> class ProductIterator
 : public iterator_facade<ProductIterator<T>,typename iterator_traits<T>::value_type const,forward_traversal_tag>
{
    //@+others
    //@+node:gcross.20110429225820.2542: *4* Constructors
    //! @name Constructor

    //! @{

    public:

    //! Null constructor
    ProductIterator() {}

    //! Constructs this class with the given nested iterator;  it first value will be the first value of the iterator.
    ProductIterator(T const nested_iterator)
      : nested_iterator(nested_iterator)
      , product(1)
    {}

    //! @}
    //@+node:gcross.20110429225820.2543: *4* Fields
    protected:

    //! The nested iterator
    T nested_iterator;

    //! The value of the accumulator divided by the current iterator element.
    typename iterator_traits<T>::value_type product;

    //! If current, the value of the accumulator.
    /*! This field is present so that we can return a reference rather than a value. */
    mutable typename iterator_traits<T>::value_type cached_product;
    //@+node:gcross.20110429225820.2544: *4* Iterator implementation
    //! @name Iterator implementation
    /*!
    These methods are not meant to be called by users directly, but rather are supplied to provide the implementation details of the iterator so that the \c iterator_facade superclass can construct an iterator facade that makes this class act like a STL iterator.
    */

    //! @{

    public:

    //! Returns the current value of the accumulator.
    typename iterator_traits<T>::value_type const& dereference() const {
        cached_product = product * (*nested_iterator);
        return cached_product;
    }

    //! Returns if two ProductIterators are equal.
    bool equal(ProductIterator const other) const {
        return nested_iterator == other.nested_iterator;
    }

    //! Advance this constructor.
    void increment() {
        product *= (*nested_iterator);
        ++nested_iterator;
    }

    //! @}
    //@-others
};
//@+node:gcross.20110127123226.2857: ** Functions
//@+others
//@+node:gcross.20110429225820.2535: *3* Bandwidth dimensions
/*!
\defgroup BandwidthDimensionFunctions Bandwidth dimensions

These functions provide useful functionality for computing bandwidth dimensions.
*/

//! @{

//@+others
//@+node:gcross.20110429225820.2537: *4* computeBandwidthDimensions
//! Computes the bandwidth dimension sequence given the requested bandwidth dimension and the sequence of physical dimensions.
vector<unsigned int> computeBandwidthDimensionSequence(
    unsigned int const requested_bandwidth_dimension
   ,vector<unsigned int> const& physical_dimensions
);
//@+node:gcross.20110429225820.2539: *4* maximumBandwidthDimension
//! Computes the maximum bandwidth dimension attainable given the sequence of physical dimensions.
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
//@-others

//! @}
//@+node:gcross.20110429225820.2534: *3* BLAS wrappers
/*!
\defgroup BLASWrappers BLAS wrappers

These functions provide wrappers around some of the FORTRAN BLAS routines.
*/

//! @{

//@+others
//@+node:gcross.20110211120708.1789: *4* dznrm2
//! \cond
extern "C" double dznrm2_(uint32_t const* n, complex<double>* const x, uint32_t const* incx);
//! \endcond

//! Compute the 2-norm of a vector.
/*!
This function is just a convenience wrapper around the BLAS dznrm2 function.
\param n the size of the vector
\param x a pointer to the vector data
\param incx the increment between vector entries (usually 1)
\return the 2-norm of the vector --- that is, the sum of the absolute-square of the entries
*/
inline double dznrm2(uint32_t const n, complex<double>* const x, uint32_t const incx=1) { return dznrm2_(&n,x,&incx); }
//@+node:gcross.20110215135633.1901: *4* zgemm
//! \cond
extern "C" void zgemm_(
    char const* transa, char const* transb,
    uint32_t const* m, uint32_t const* n, uint32_t const* k,
    complex<double> const* alpha,
    complex<double> const* a, uint32_t const* lda,
    complex<double> const* b, uint32_t const* ldb,
    complex<double> const* beta,
    complex<double>* c, uint32_t const* ldc
);
//! \endcond

//! Multiplies two matrices using the BLAS \c zgemm call.
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
//@+node:gcross.20110215135633.1903: *4* zgemv
//! \cond
extern "C" void zgemv_(
    char const* trans,
    uint32_t const* m, uint32_t const* n,
    complex<double> const* alpha,
    complex<double> const* a, uint32_t const* lda,
    complex<double> const* x, uint32_t const* incx,
    complex<double> const* beta,
    complex<double>* y, uint32_t const* incy
);
//! \endcond

//! Multiplies a matrix by a vector using the BLAS \c zgemv call.
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

//! @}
//@+node:gcross.20110429225820.2540: *3* Miscellaneous
//@+node:gcross.20110211120708.1786: *4* c
//! Convenience function that constructs the complex number x + iy.
inline complex<double> c(double x, double y) { return complex<double>(x,y); }
//@+node:gcross.20110429225820.2536: *4* choose
//! Implements the statistical combinatorics function n choose k.
/*!
Returns the number of ways to choose \c k elements from \c n elements.
\param n the total number of elements
\param k the number of elements to be chosen
\return the number of ways to choose \c k elements from \c n elements.
*/
unsigned long long choose(unsigned int n, unsigned int k);
//@+node:gcross.20110211120708.1798: *4* makeProductIterator
//! Convenience function for constructing instance of ProductIterator.
template<typename T> ProductIterator<T> makeProductIterator(T const x) { return ProductIterator<T>(x); }
//@+node:gcross.20110219083229.2603: *4* outsideTolerance
//! Returns whether \c a and \c b do not match within the specified relative \c tolerance.
template<typename A, typename B, typename C> bool outsideTolerance(A a, B b, C tolerance) {
    return ((abs(a)+abs(b))/2 > tolerance) && (abs(a-b)/(abs(a)+abs(b)+tolerance) > tolerance);
}
//@+node:gcross.20110213233103.3637: *4* rethrow
//! Throws the argument passed to it; useful as part of a signal handler.
template<typename Exception> void rethrow(Exception& e) { throw e; }
//@+node:gcross.20110429225820.2533: *3* Move assistants
/*!
\defgroup MoveAssistantFunctions Move assistants

The functions in this module assist in implementing move semantics.
*/

//! @{

//@+others
//@+node:gcross.20110211120708.1787: *4* copyAndReset
//! Returns the value in \c x, and sets \c x to zero.
template<typename T> inline T copyAndReset(T& x) {
    T const old_x = x;
    x = 0;
    return old_x;
}
//@+node:gcross.20110211120708.1788: *4* moveArrayToFrom
//! Deallocates the array at \c to (if \c to is non-null), copies the pointer from \c from to \c to, and then sets \c from to null.
template<typename T> inline void moveArrayToFrom(T*& to, T*& from) {
    if(to) delete[] to;
    to = copyAndReset(from);
}
//@-others

//! @}
//@-others
//@+node:gcross.20110129220506.1652: ** Macros
//! Repeats a statement or block \c n times.
#define REPEAT(n) for(unsigned int _counter##__LINE__ = 0; _##counter##__LINE__ < n; ++_##counter##__LINE__)
//@-others

//! @}

}

#endif
//@-leo
