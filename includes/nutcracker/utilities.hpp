/*!
\file utilities.hpp
\brief Utility classes and functions
*/

#ifndef NUTCRACKER_UTILITIES_HPP
#define NUTCRACKER_UTILITIES_HPP

#include <boost/concept_check.hpp>
#include <boost/container/vector.hpp>
#include <boost/foreach.hpp>
#include <boost/format.hpp>
#include <boost/functional/hash.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <boost/range/algorithm/copy.hpp>
#include <boost/range/algorithm/fill.hpp>
#include <boost/range/algorithm/reverse.hpp>
#include <boost/range/algorithm/reverse_copy.hpp>
#include <boost/range/concepts.hpp>
#include <boost/range/irange.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/make_shared.hpp>
#include <boost/move/move.hpp>
#include <boost/none_t.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include <complex>
#include <exception>
#include <functional>
#include <iterator>
#include <iostream>
#include <numeric>
#include <sstream>
#include <stdexcept>
#include <stdint.h>
#include <string>
#include <typeinfo>

namespace Nutcracker {

using boost::adaptors::reversed;
using boost::container::vector;
using boost::copy;
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
using std::complex;
using std::iterator_traits;
using std::min;
using std::multiplies;
using std::numeric_limits;
using std::string;
using std::type_info;

//! \defgroup Utilities Utilities
//! @{

//! None type tag
typedef boost::none_t None;

typedef boost::numeric::ublas::matrix<complex<double> > Matrix;
typedef boost::shared_ptr<Matrix> MatrixPtr;
typedef boost::shared_ptr<Matrix const> MatrixConstPtr;

typedef boost::numeric::ublas::vector<complex<double> > Vector;
typedef boost::shared_ptr<Vector> VectorPtr;
typedef boost::shared_ptr<Vector const> VectorConstPtr;
//! Repeats a statement or block \c n times.
#define REPEAT(n) for(unsigned int _counter##__LINE__ = 0; _counter##__LINE__ < n; ++_counter##__LINE__)
//! Exception indicating a programmer error.
struct BadProgrammerException : public std::logic_error {
    //! Constructs this exception with the given message.
    BadProgrammerException(string const& message) : std::logic_error("BAD PROGRAMMER!!! --- " + message) {}
};
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
//! This exception is thrown when the user requests a bandwidth dimension that is larger than the maximum possible given the physical dimension sequence.
struct RequestedBandwidthDimensionTooLargeError : public std::logic_error {
    unsigned int const
          requested_bandwidth_dimension //!< requested bandwidth dimension
        , greatest_possible_bandwidth_dimension //!< greatest possible bandwidth dimension
        ;

    //! Constructs this exception with the given requested and greatest possible bandwidth dimensions.
    RequestedBandwidthDimensionTooLargeError(
        unsigned int const requested_bandwidth_dimension
      , unsigned int const greatest_possible_bandwidth_dimension
    ) : std::logic_error((
            format("Requested bandwidth dimension %1% is too large;  the highest possible with the given physical dimensions is %2%.")
                % requested_bandwidth_dimension
                % greatest_possible_bandwidth_dimension
        ).str())
      , requested_bandwidth_dimension(requested_bandwidth_dimension)
      , greatest_possible_bandwidth_dimension(greatest_possible_bandwidth_dimension)
    {}
};
struct Destructable { virtual ~Destructable() {} };
template<typename Label> struct Link {
    unsigned int from, to;
    Label label;

    Link() {}

    Link(
          unsigned int const from
        , unsigned int const to
        , Label const& label
    ) : from(from)
      , to(to)
      , label(label)
    { }

    bool operator<(Link const& other) const {
        return from < other.from ||
              (from == other.from && (
                 to < other.to ||
                (to == other.to && (
                   label < other.label
                ))
              ));
    }

};
//! Iterator class which accumulates the product of the values of a nested iterator.
/*!
This class acts like a STL iterator and specifically implements the Boost ForwardTraversal iterator concept, but most of the iterator methods might not be visible in the documentation because they are inherited from \c iterator_facade (which generates the requisite boilerplate code from the methods listed in "Iterator implementation").
*/
template<typename T> class ProductIterator
 : public iterator_facade<ProductIterator<T>,typename iterator_traits<T>::value_type const,forward_traversal_tag>
{
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
    protected:

    //! The nested iterator
    T nested_iterator;

    //! The value of the accumulator divided by the current iterator element.
    typename iterator_traits<T>::value_type product;

    //! If current, the value of the accumulator.
    /*! This field is present so that we can return a reference rather than a value. */
    mutable typename iterator_traits<T>::value_type cached_product;
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
};
/*!
\defgroup BandwidthDimensionFunctions Bandwidth dimensions

These functions provide useful functionality for computing bandwidth dimensions.
*/

//! @{

//! Computes the bandwidth dimension sequence given the requested bandwidth dimension and the sequence of physical dimensions.
vector<unsigned int> computeBandwidthDimensionSequence(
    unsigned int const requested_bandwidth_dimension
   ,vector<unsigned int> const& physical_dimensions
);

template<typename Iterator> unsigned int computeOverflowingProduct(Iterator const begin, Iterator const end) {
    unsigned int result = 1;
    BOOST_FOREACH(
        unsigned int const element,
        make_iterator_range(begin,end)
    ) {
        if(UINT_MAX/element > result) {
            result *= element;
        } else {
            return UINT_MAX;
        }
    }
    return result;
}

//! Computes the maximum bandwidth dimension attainable given the sequence of physical dimensions.
template<typename PhysicalDimensionRange> unsigned int maximumBandwidthDimension(
    PhysicalDimensionRange const& physical_dimensions
) {
    BOOST_CONCEPT_ASSERT((RandomAccessRangeConcept<PhysicalDimensionRange const>));
    if(physical_dimensions.size() == 1) return 1;
    size_t middle_index = (physical_dimensions.size()+1)/2;

    return min(
        computeOverflowingProduct(
            physical_dimensions.begin(),
            physical_dimensions.begin()+middle_index
        ),
        computeOverflowingProduct(
            physical_dimensions.rbegin(),
            physical_dimensions.rbegin()+middle_index
        )
    );
}

//! @}
/*!
\defgroup BLASWrappers BLAS wrappers

These functions provide wrappers around some of the FORTRAN BLAS routines.
*/

//! @{

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

//! @}
template<typename Range> inline typename Range::iterator getFirstLoopIterator(bool forward,Range& range) {
    return forward ? range.begin() : range.end();
}
template<typename Iterator> inline typename std::iterator_traits<Iterator>::reference updateLoopIterator(bool forward,Iterator& iter) {
    return forward ? *(iter++) : *(--iter);
}
template<typename Iterator> inline typename std::iterator_traits<Iterator>::reference dereferenceLoopIterator(bool forward,Iterator iter) {
    return forward ? *iter : *(--iter);
}
//! Convenience function that constructs the complex number x + iy.
inline complex<double> c(double x, double y) { return complex<double>(x,y); }
//! Implements the statistical combinatorics function n choose k.
/*!
Returns the number of ways to choose \c k elements from \c n elements.
\param n the total number of elements
\param k the number of elements to be chosen
\return the number of ways to choose \c k elements from \c n elements.
*/
unsigned long long choose(unsigned int n, unsigned int k);
unsigned int computeDigitsOfPrecision(double const tolerance);
//! Convenience function for constructing instance of ProductIterator.
template<typename T> ProductIterator<T> makeProductIterator(T const x) { return ProductIterator<T>(x); }
//! Returns whether \c a and \c b do not match within the specified relative \c tolerance.
template<typename A, typename B, typename C> bool outsideTolerance(A a, B b, C tolerance) {
    return ((abs(a)+abs(b))/2 > tolerance) && (abs(a-b)/(abs(a)+abs(b)+tolerance) > tolerance);
}
template<typename RangeType> std::string rangeToString(RangeType const& range) {
    typedef typename iterator_traits<typename boost::range_iterator<RangeType const>::type>::reference ElementReference;
    std::ostringstream s;
    s << '[';
    bool first = true;
    BOOST_FOREACH(ElementReference element, range) {
        if(first) {
            first = false;
        } else {
            s << ',';
        }
        s << element;
    }
    s << ']';
    return s.str();
}
//! Throws the argument passed to it; useful as part of a signal handler.
template<typename Exception> void rethrow(Exception& e) { throw e; }
template<typename T> MatrixPtr diagonalMatrix(T const& data) {
    unsigned int const n = data.size();
    Matrix* matrix = new Matrix(n,n,c(0,0));
    unsigned int i = 0;
    typename boost::range_iterator<T const>::type px = boost::begin(data);
    while(px != boost::end(data)) {
        (*matrix)(i,i) = *px++;
        ++i;
    }
    return MatrixPtr(matrix);
}
MatrixPtr identityMatrix(unsigned int const n);
template<typename T> MatrixPtr squareMatrix(T const& data) {
    unsigned int const n = (unsigned int)sqrt(data.size());
    assert(n*n == data.size());
    Matrix* matrix = new Matrix(n,n);
    copy(data,matrix->data().begin());
    return MatrixPtr(matrix);
}
/*!
\defgroup MoveAssistantFunctions Move assistants

The functions in this module assist in implementing move semantics.
*/

//! @{

//! Returns the value in \c x, and sets \c x to zero.
template<typename T> inline T copyAndReset(T& x) {
    T const old_x = x;
    x = 0;
    return old_x;
}
//! Deallocates the array at \c to (if \c to is non-null), copies the pointer from \c from to \c to, and then sets \c from to null.
template<typename T> inline void moveArrayToFrom(T*& to, T*& from) {
    if(to) delete[] to;
    to = copyAndReset(from);
}

//! @}
inline VectorConstPtr basisVector(unsigned int physical_dimension, unsigned int observation) {
    VectorPtr vector = boost::make_shared<Vector>(physical_dimension);
    boost::fill(vector->data(),c(0,0));
    (*vector)[observation] = 1;
    return vector;
}
namespace Pauli {
    extern MatrixConstPtr const I, X, Y, Z;
}

namespace Qubit {
    extern VectorConstPtr const Up, Down;
}
inline MatrixConstPtr operator+(MatrixConstPtr const& a, MatrixConstPtr const& b) {
    return boost::make_shared<Matrix>(*a + *b);
}

inline MatrixConstPtr operator*(complex<double> c, MatrixConstPtr const& x) {
    return boost::make_shared<Matrix>(c*(*x));
}

inline MatrixConstPtr operator*(MatrixConstPtr const& x, complex<double> c) {
    return boost::make_shared<Matrix>(c*(*x));
}

inline VectorConstPtr operator+(VectorConstPtr const& a, VectorConstPtr const& b) {
    return boost::make_shared<Vector>(*a + *b);
}

inline VectorConstPtr operator*(complex<double> c, VectorConstPtr const& x) {
    return boost::make_shared<Vector>(c*(*x));
}

inline VectorConstPtr operator*(VectorConstPtr const& x, complex<double> c) {
    return boost::make_shared<Vector>(c*(*x));
}

//! @}

}

template<typename T> inline T& operator>> (T& in, boost::none_t& _) { return in; }
template<typename T> inline T& operator<< (T& out, const boost::none_t& _) { return out; }

#endif
