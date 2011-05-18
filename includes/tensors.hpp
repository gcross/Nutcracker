//@+leo-ver=5-thin
//@+node:gcross.20110124161335.2009: * @file tensors.hpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2020: ** << License >>
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

#ifndef NUTCRACKER_TENSORS_HPP
#define NUTCRACKER_TENSORS_HPP

//@+<< Includes >>
//@+node:gcross.20110124161335.2010: ** << Includes >>
#include <boost/concept_check.hpp>
#include <boost/container/vector.hpp>
#include <boost/foreach.hpp>
#include <boost/format.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/move/move.hpp>
#include <boost/optional.hpp>
#include <boost/range/algorithm/copy.hpp>
#include <boost/range/concepts.hpp>
#include <boost/range/irange.hpp>
#include <boost/smart_ptr/shared_ptr.hpp>
#include <boost/utility.hpp>
#include <complex>
#include <exception>
#include <ostream>
#include <stdint.h>

#include "utilities.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110124161335.2011: ** << Usings >>
using boost::container::vector;
using boost::copy;
using boost::Generator;
using boost::format;
using boost::iterator_facade;
using boost::none;
using boost::optional;
using boost::RandomAccessRangeConcept;
using boost::random_access_traversal_tag;
using boost::shared_ptr;

using std::copy;
using std::fill_n;
using std::ostream;
//@-<< Usings >>

//@+others
//@+node:gcross.20110214164734.1918: ** Exceptions
//@+node:gcross.20110215235924.1979: *3* DimensionMismatch
struct DimensionMismatch : public Exception {
    DimensionMismatch(
          const char* n1
        , unsigned int const d1
        , const char* n2
        , unsigned int const d2
    ) : Exception((format("%1% dimension (%2%) does not match %3% dimension (%4%)") % n1 % d1 % n2 % d2).str())
    { }
};
//@+node:gcross.20110202172517.1694: *3* InvalidTensorException
struct InvalidTensorException : public Exception {
    InvalidTensorException() : Exception("Attempt to dereference an invalid tensor") {}
};
//@+node:gcross.20110215235924.1980: ** Functions
//@+node:gcross.20110215235924.1982: *3* function connectDimensions
inline unsigned int connectDimensions(
      const char* n1
    , unsigned int const d1
    , const char* n2
    , unsigned int const d2
) {
    if(d1 != d2) throw DimensionMismatch(n1,d1,n2,d2);
    return d1;
}
//@+node:gcross.20110215231246.1878: ** Type aliases
class OperatorSite;
typedef vector<shared_ptr<OperatorSite const> > Operator;
//@+node:gcross.20110215231246.1877: ** Dummy/Forward classes
class Left;
class Middle;
class Overlap;
class Physical;
class Right;
class State;
//@+node:gcross.20110511190907.2252: ** Normalization traits
template<typename Side> struct normalizationOf {};

template<> struct normalizationOf<Left> {
    static optional<string> const value;
};

template<> struct normalizationOf<Middle> {
    static optional<string> const value;
};

template<> struct normalizationOf<Right> {
    static optional<string> const value;
};

template<> struct normalizationOf<None> {
    static optional<string> const value;
};
//@+node:gcross.20110127123226.2852: ** Constructor parameters wrappers
/*! \defgroup ConstructorParameters Constructor parameter wrappers

C++ constructors can only take the name of the class itself, so it is impossible
to distinguish different ways in which a class can be constructed by using
constructors with different names.  Fortunately, C++ allows one to create
several methods in a class that share the same name but that have different
types and numbers of arguments.  Thus, several "wrapper" classes have been
created that are nothing more than a simple wrapper around an argument in
order to explicitly specify a particular constructor.  Associated with each such
class is also a convenience function for wrapping a value in this class, so that
for example one can construct a tensor \c B that as a copy of a tensor \c A
via code like the following:

\code
OperatorTensor B(copyFrom(A));
\endcode

where \c copyFrom(A) constructs a value of type CopyFrom<OperatorSite> and passes
it to the constructor, which causes C++ (using the argument-dependent lookup rules)
to select the constructor that we want --- the one that will make a copy of the
data in \c A.
*/

// @{
//@+<< Base class >>
//@+node:gcross.20110427160525.2450: *3* << Base class >>
/*! \brief The base class of the constructor parameter wrappers.

See the \ref ConstructorParameter "Constructor parameter wrappers" section for a list of these and explanation of their purpose.

*/
template<typename T> class ConstructorParameter {
private:
    //! The data wrapped by this class.
    T& data;
protected:
    //! Wrap \c data in this class.
    explicit ConstructorParameter(T& data) : data(data) {}
public:
    //! Get a reference to the wrapped data.
    T& operator*() const { return data; }
    //! Access a field of the wrapped data.
    T* operator->() const { return &data; }
};
//@-<< Base class >>

//@+<< DEFINE_TEMPLATIZED_PARAMETER >>
//@+node:gcross.20110427160525.2451: *3* << DEFINE_TEMPLATIZED_PARAMETER >>
/*! \brief A convenience macro for constructing constructor parameter wrappers.

For example, the CopyFrom class is defined using the following line of code:

\code
DEFINE_TEMPLATIZED_PARAMETER(CopyFrom,copyFrom)
\endcode

(Note that the two parameters are essentially the same thing, but using different capitalization.)

\param ParameterName the name of the parameter class
\param parameterName the name of the convenience function used to wrap values in the class

*/
#define DEFINE_TEMPLATIZED_PARAMETER(ParameterName,parameterName) \
    template<typename T> struct ParameterName : public ConstructorParameter<T> { \
        explicit ParameterName(T& data) : ConstructorParameter<T>(data) {} \
        template<typename U> ParameterName(ParameterName<U>& other) : ConstructorParameter<T>(*other) {} \
        template<typename U> operator ParameterName<U>() const { return ParameterName<U>(static_cast<U&>(**this)); } \
    }; \
    template<typename T> ParameterName<T> parameterName(T& data) { return ParameterName<T>(data); } \
    template<typename T> ParameterName<T const> parameterName(T const& data) { return ParameterName<T const>(data); }
//@-<< DEFINE_TEMPLATIZED_PARAMETER >>

//@+others
//@+node:gcross.20110427160525.2452: *3* CopyFrom
/*! \brief A parameter wrapper class that indicates a tensor should be constructed by making a copy of the data in the wrapped tensor.

It is easiest to wrap a tensor in this class by using the convenience functions \p copyFrom.

\sa ConstructorParameters
*/
DEFINE_TEMPLATIZED_PARAMETER(CopyFrom,copyFrom)

/*!
\fn CopyFrom::CopyFrom(T &data)
\brief Construct a new CopyFrom object wrapping data.

\fn template<typename U> CopyFrom::CopyFrom(CopyFrom<U> &other) 
\brief Constructs (possibly implicitly) a CopyFrom<T> from a CopyFrom<U> iff U can be cast to T.

\fn template<typename U> CopyFrom::operator CopyFrom<U>() const
\brief Performs a (possibly implicit) cast from CopyFrom<U> to CopyFrom<T> iff U can be cast to T.

\fn template<typename T> CopyFrom<T> copyFrom (T &data)
\brief A convenience function for wrapping values in CopyFrom.

\fn template<typename T> CopyFrom<T const> copyFrom (T const &data)
\brief A convenience function for wrapping constant values in CopyFrom.
*/
//@+node:gcross.20110427160525.2453: *3* DimensionsOf
/*! \brief A parameter wrapper class that indicates a tensor should be constructed with the same dimensions as the wrapped tensor.

It is easiest to wrap a tensor in this class by using the convenience functions \p dimensionsOf.

\sa ConstructorParameters
*/
DEFINE_TEMPLATIZED_PARAMETER(DimensionsOf,dimensionsOf)

/*!
\fn DimensionsOf::DimensionsOf(T &data)
\brief Construct a new DimensionsOf object wrapping data.

\fn template<typename U> DimensionsOf::DimensionsOf(DimensionsOf<U> &other) 
\brief Constructs (possibly implicitly) a DimensionsOf<T> from a DimensionsOf<U> iff U can be cast to T.

\fn template<typename U> DimensionsOf::operator DimensionsOf<U>() const
\brief Performs a (possibly implicit) cast from DimensionsOf<U> to DimensionsOf<T> iff U can be cast to T.

\fn template<typename T> DimensionsOf<T> dimensionsOf (T &data)
\brief A convenience function for wrapping values in DimensionsOf.

\fn template<typename T> DimensionsOf<T const> dimensionsOf (T const &data)
\brief A convenience function for wrapping constant values in DimensionsOf.
*/
//@+node:gcross.20110427160525.2457: *3* FillWithGenerator
/*! \brief A parameter wrapper class that indicates a tensor should be constructed using data from a generator.

It is easiest to wrap a tensor in this class by using the convenience functions \p fillWithGenerator.

\sa ConstructorParameters
*/
DEFINE_TEMPLATIZED_PARAMETER(FillWithGenerator,fillWithGenerator)

/*!
\fn FillWithGenerator::FillWithGenerator(T &data)
\brief Construct a new FillWithGenerator object wrapping data.

\fn template<typename U> FillWithGenerator::FillWithGenerator(FillWithGenerator<U> &other) 
\brief Constructs (possibly implicitly) a FillWithGenerator<T> from a FillWithGenerator<U> iff U can be cast to T.

\fn template<typename U> FillWithGenerator::operator FillWithGenerator<U>() const
\brief Performs a (possibly implicit) cast from FillWithGenerator<U> to FillWithGenerator<T> iff U can be cast to T.

\fn template<typename T> FillWithGenerator<T> fillWithGenerator (T &data)
\brief A convenience function for wrapping values in FillWithGenerator.

\fn template<typename T> FillWithGenerator<T const> fillWithGenerator (T const &data)
\brief A convenience function for wrapping constant values in FillWithGenerator.
*/
//@+node:gcross.20110427160525.2459: *3* FillWithRange
/*! \brief A parameter wrapper class that indicates a tensor should be constructed using data from a range.

It is easiest to wrap a tensor in this class by using the convenience functions \p fillWithRange.

\sa ConstructorParameters
*/
DEFINE_TEMPLATIZED_PARAMETER(FillWithRange,fillWithRange)

/*!
\fn FillWithRange::FillWithRange(T &data)
\brief Construct a new FillWithRange object wrapping data.

\fn template<typename U> FillWithRange::FillWithRange(FillWithRange<U> &other) 
\brief Constructs (possibly implicitly) a FillWithRange<T> from a FillWithRange<U> iff U can be cast to T.

\fn template<typename U> FillWithRange::operator FillWithRange<U>() const
\brief Performs a (possibly implicit) cast from FillWithRange<U> to FillWithRange<T> iff U can be cast to T.

\fn template<typename T> FillWithRange<T> fillWithRange (T &data)
\brief A convenience function for wrapping values in FillWithRange.

\fn template<typename T> FillWithRange<T const> fillWithRange (T const &data)
\brief A convenience function for wrapping constant values in FillWithRange.
*/
//@-others

// @}
//@+node:gcross.20110127123226.2856: ** Dimension wrappers
template<typename label> class Dimension {
private:
    BOOST_COPYABLE_AND_MOVABLE(Dimension)
    unsigned int dimension;
public:
    Dimension() : dimension(0) { }
    explicit Dimension(unsigned int const dimension) : dimension(dimension) { }
    Dimension(BOOST_RV_REF(Dimension) other) : dimension(copyAndReset(other.dimension)) { }
    Dimension(Dimension const& other) : dimension(other.dimension) { } \
    Dimension& operator=(Dimension const& other) { dimension = other.dimension; return *this; }
    Dimension& operator=(BOOST_RV_REF(Dimension) other) { dimension = copyAndReset(other.dimension); return *this; }
    unsigned int operator *() const { return dimension; }
    bool operator==(Dimension const other) const { return dimension == other.dimension; }
    bool operator!=(Dimension const other) const { return dimension != other.dimension; }
};
template<typename label> inline ostream& operator<<(ostream& out, Dimension<label> const d) { return (out << *d); }

#define DEFINE_DIMENSION(Name) \
    typedef Dimension<Name> Name##Dimension;

DEFINE_DIMENSION(Left);
DEFINE_DIMENSION(Operator);
DEFINE_DIMENSION(Overlap);
DEFINE_DIMENSION(Physical);
DEFINE_DIMENSION(Right);
DEFINE_DIMENSION(State);
//@+node:gcross.20110129220506.1661: ** Dummy arguments
//@+<< Description >>
//@+node:gcross.20110428160636.2465: *3* << Description >>
/*! \defgroup DummyArguments Dummy arguments

The classes in these groups are trivial (i.e., empty) structs that exist solely for the purpose of creating a singleton with a unique type in order to distinguish between overloaded methods using argument-dependent lookup.

*/
//@-<< Description >>

//! @{

//@+<< Macros >>
//@+node:gcross.20110428160636.2466: *3* << Macros >>
//@+others
//@+node:gcross.20110428160636.2467: *4* DECLARE_DUMMY_PARAMETER
//! A convenience macro for defining a dummy parameter class and the singleton object associated with it.
/*!
\note The singleton is declared with external linkage, so it needs to actually be defined in one of the source files using the DEFINE_DUMMY_PARAMETER macro.

\param Parameter the name of the dummy class
\param parameter the name of the singleton object

\see DEFINE_DUMMY_PARAMETER
*/
#define DECLARE_DUMMY_PARAMETER(Parameter,parameter) struct Parameter {}; extern Parameter const parameter;
//@+node:gcross.20110428160636.2468: *4* DEFINE_DUMMY_PARAMETER
//! A convenience macro for defining the singleton associated with a dummy parameter class.
/*!
\param Parameter the name of the dummy class
\param parameter the name of the singleton object

\see DECLARE_DUMMY_PARAMETER
*/
#define DEFINE_DUMMY_PARAMETER(Parameter,parameter) Parameter const parameter = {};
//@-others
//@-<< Macros >>

//@+others
//@+node:gcross.20110428160636.2470: *3* AsUnsignedInteger
//! Dummy class used to specify that the integer inside a dimension wrapper class should be unwrapped and returned directly.
/*! Its associated singleton is as_unsigned_integer.*/
DECLARE_DUMMY_PARAMETER(AsUnsignedInteger,as_unsigned_integer)
//!< The singleton instance of AsUnsignedInteger.
//@+node:gcross.20110428160636.2469: *3* MakeTrivial
//! Dummy class used to specify that a tensor should be constructed as the "trivial" tensor.
/*!
Specifically, it indicates that a tensor should be constructed to have an array with all dimensions 1 and initialized to contain the value 1.

Its associated singleton is make_trivial.
*/
DECLARE_DUMMY_PARAMETER(MakeTrivial,make_trivial)
//!< The singleton instance of MakeTrivial.
//@-others

//! @}
//@+node:gcross.20110124175241.1520: ** type function Other
template<typename other_side> struct Other { };
template<> struct Other<Left> { typedef Right value; };
template<> struct Other<Right> { typedef Left value; };
//@+node:gcross.20110124161335.2012: ** Tensors
//! \defgroup Tensors Tensors
//! @{

//@+others
//@+node:gcross.20110215235924.2007: *3* Base
//@+node:gcross.20110126150230.1601: *4* BaseTensor
//@+<< Description >>
//@+node:gcross.20110428160636.2462: *5* << Description >>
/*! The base class of all tensors.

All tensors in Nutcracker share in common the trait that they are either invalid or they contain a pointer to a chunk of contiguous \c complex<double> data of known size.  This base class implements some natural functionality that follows from this trait (such as the ability to query the size of the data, or to get a pointer to it) that is shared amongst all of the tensors classes.

A \c BaseTensor object holds sole ownership over its data and will automatically free it when the destructor is called.  Two objects cannot share data, but you can move data from one object to another.  For example, the code
\code
a = boost::move(b);
\endcode
moves the data from \c b to \c a and then invalidates b.  Of course, this "move" only involves moving the data pointer and size fields and not the data itself, so it is a very cheap operation.  The \c boost::move function (part of the Boost.Move library) does some magic that you don't have to know about, so you can just think about it as being required to confirm that you know that you are moving the data out of \c b and so \c b will be invalid after this statement.

In many cases a function will return a value that is an instance of \c BaseTensor.  The Boost.Move library does magic behind the scenes so that if \c f is a function returning an \c OperatorTensor then you can write
\code
OperatorTensor a(f(...));
\endcode
to indicate that the result of \c f should be \a moved into \c a.

Note that the interface of \c BaseTensor has been designed so that copies will never be made unless you explicitly ask for them, so if you fail to use the correct syntax to move the data between objects then you will get a compiler error rather than having a copy be silently made.
*/
//@-<< Description >>
class BaseTensor {
    //@+others
    //@+node:gcross.20110428160636.2509: *5* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(BaseTensor)
    //@+node:gcross.20110428124342.2459: *5* Assignment
    //! \name Assignment
    //! @{

    protected:

    //! Moves the data from \c other to \c this and invalidates \c other.
    void operator=(BOOST_RV_REF(BaseTensor) other) {
        data_size = copyAndReset(other.data_size);
        moveArrayToFrom(data,other.data);
    }

    //! Swaps the data in \c other and \c this.
    void swap(BaseTensor& other) {
        std::swap(data_size,other.data_size);
        std::swap(data,other.data);
    }

    //! @}
    //@+node:gcross.20110428124342.2457: *5* Constructors
    //! \name Constructors
    //! @{

    protected:

    //! Construct an invalid tensor (presumably into which you will eventually move data from elsewhere).
    BaseTensor() : data_size(0), data(NULL) {}

    //! Move the data from \c other into \c this and invalidate \c other.
    BaseTensor(BOOST_RV_REF(BaseTensor) other)
      : data_size(copyAndReset(other.data_size))
      , data(copyAndReset(other.data))
    { }

    //! Allocate memory for an array of size \c size.
    BaseTensor(unsigned int const size)
      : data_size(size)
      , data(new complex<double>[data_size])
    { }

    //! Make a copy of the data in \c other.
    /*!
    \note
    I could have used a normal copy constructor for this rather than using parameter wrapper, but since it is very rare that this is actually what one wants I decided to use a parameter wrapper in order to force the it to be stated explicitly that one intends to make a copy of the data.
    */
    template<typename Tensor> BaseTensor(CopyFrom<Tensor> const other)
      : data_size(other->data_size)
      , data(new complex<double>[data_size])
    {
        copy(*other,begin());
    }

    //! Fill this tensor with data supplied from a generator.
    /*!
    The generator must satisfy the STL Generator concept, which is really just a formal way of stating that \c generator must be something that we can call to generate new \c complex<double> values on demand.

    \param size the size of the data array
    \param generator the generator supplying the data
    */
    template<typename G> BaseTensor
        ( unsigned int const size
        , FillWithGenerator<G> const generator
        )
      : data_size(size)
      , data(new complex<double>[data_size])
    {
        BOOST_CONCEPT_ASSERT(( Generator<G,complex<double> > ));
        generate_n(begin(),size,*generator);
    }

    //! Constructs a tensor using data supplied from a range.
    /*!
    The range must satisfy the Boost RandomAccessRangeConcept concept. The size of the data array will be inferred from the range by
    calling its \c size method.

    \param init the range supplying the data for this tensor
    */
    template<typename Range> BaseTensor(FillWithRange<Range> const init)
      : data_size(init->size())
      , data(new complex<double>[data_size])
    {
        BOOST_CONCEPT_ASSERT(( RandomAccessRangeConcept<Range const> ));
        copy(*init,begin());
    }

    //! Allocates an array of size one and fills it with the value 1.
    BaseTensor(MakeTrivial const make_trivial)
      : data_size(1)
      , data(new complex<double>[1])
    {
        data[0] = c(1,0);
    }
    //! @}
    //@+node:gcross.20110428124342.2461: *5* Data access
    //! \name Data access
    /*!
    \note
    These methods \a assume that the tensor is valid.  If it is not, then the
    result of using them is undefined.
    */
    //! @{

    public:

    //! An implicit cast to \c complex<double> that obtains a direct pointer to the beginning of the data.
    operator complex<double>*() { return begin(); }
    //! An implicit cast to \c complex<double> that obtains a read-only pointer to the beginning of the data.
    operator complex<double> const*() const { return begin(); }

    //! Array-style direct access to an individual component of the data.
    complex<double>& operator[](unsigned int const index) { return *(begin()+index); }
    //! Array-style read-only access to an individual component of the data.
    complex<double> operator[](unsigned int const index) const { return *(begin()+index); }

    //! @}
    //@+node:gcross.20110428160636.2461: *5* Fields
    private:

    //! The size of the stored data array.
    unsigned int data_size;

    //! The pointer to the stored data array.
    complex<double>* data;
    //@+node:gcross.20110428124342.2460: *5* Informational
    //! \name Informational
    //! @{

    public:

    //! Returns the size of the data.
    unsigned int size() const { return data_size; }

    //! Returns true iff this tensor is valid.
    bool valid() const { return data; }
    //! Returns true iff this tensor is invalid.
    bool invalid() const { return !valid(); }

    //! @}
    //@+node:gcross.20110428124342.2458: *5* Iteration support
    /*! \name Iteration support
    The following public methods and associated types are provided in order to make
    it easier to apply STL-style generic algorithms to the contained data.
    */
    //! @{

    public:

    typedef complex<double> value_type;
    typedef value_type* iterator;
    typedef value_type const* const_iterator;
    typedef value_type& reference;
    typedef value_type const& const_reference;

    //! Returns a pointer to the beginning of the data.
    complex<double>* begin() { if(invalid()) throw InvalidTensorException(); return data; }
    //! Returns a read-only pointer to the beginning of the data.
    complex<double> const* begin() const { if(invalid()) throw InvalidTensorException(); return data; }

    //! Returns a pointer just after the end of the data.
    complex<double>* end() { return begin()+size(); }
    //! Returns a read-only pointer just after the end of the data.
    complex<double> const* end() const { return begin()+size(); }

    //! @}
    //@+node:gcross.20110428160636.2464: *5* Miscellaneous
    public:

    //! If this tensor is valid, the data is destroyed;  otherwise nothing is done.
    ~BaseTensor() { if(valid()) delete[] data; }

    protected:

    //! Returns the Frobenius 2-norm of this tensor --- i.e., the sum of the absolute value squared of all components.
    double norm() const { return dznrm2(data_size,data); }
    //@-others
};
//@+node:gcross.20110214164734.1934: *4* SiteBaseTensor
//@+<< Description >>
//@+node:gcross.20110428160636.2471: *5* << Description >>
/*! The base class of all tensors.

All tensors in Nutcracker that are associated with sites share in common the trait that they have tensor connected to their left and therefore a \a left dimension, a tensor connected to their right and therfore a \a right dimension, and a tensor connected above and/or below them and therefore one or two indices with the \a physical dimension (that is, the dimension of the qudit) associated with the site.  This base class implements the common functionality following from this shared trait by maintaining fields for these three dimesions.

\note
See the documentation in BaseTensor for a description of the policy of how data ownership in tensors works.  (Short version: tensors own their data, which can be moved but not copied unless you explicitly ask for a copy to be made.)

\see BaseTensor
*/
//@-<< Description >>

class SiteBaseTensor : public BaseTensor {
    //@+others
    //@+node:gcross.20110428160636.2510: *5* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(SiteBaseTensor)
    //@+node:gcross.20110428160636.2474: *5* Assignment
    //! \name Assignment
    //! @{

    protected:

    //! Moves the data (and dimensions) from \c other to \c this and invalidates \c other.
    void operator=(BOOST_RV_REF(SiteBaseTensor) other) {
        BaseTensor::operator=(static_cast<BOOST_RV_REF(BaseTensor)>(other));
        physical_dimension = boost::move(other.physical_dimension);
        left_dimension = boost::move(other.left_dimension);
        right_dimension = boost::move(other.right_dimension);
    }

    //! Swaps the data (and dimensions) in \c other and \c this.
    void swap(SiteBaseTensor& other) {
        BaseTensor::swap(other);
        std::swap(physical_dimension,other.physical_dimension);
        std::swap(left_dimension,other.left_dimension);
        std::swap(right_dimension,other.right_dimension);
    }

    //! @}
    //@+node:gcross.20110428160636.2473: *5* Constructors
    //! \name Constructors
    //! @{

    protected:

    //! Construct an invalid tensor (presumably into which you will eventually move data from elsewhere).
    SiteBaseTensor() {}

    //! Move the data (and dimensions) from \c other into \c this and invalidate \c other.
    SiteBaseTensor(BOOST_RV_REF(SiteBaseTensor) other)
      : BaseTensor(static_cast<BOOST_RV_REF(BaseTensor)>(other))
      , physical_dimension(boost::move(other.physical_dimension))
      , left_dimension(boost::move(other.left_dimension))
      , right_dimension(boost::move(other.right_dimension))
    { }

    //! Initialize the dimensions with the given values and allocate memory for an array of size \c size.
    SiteBaseTensor(
          PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
        , unsigned int const size
    ) : BaseTensor(size)
      , physical_dimension(physical_dimension)
      , left_dimension(left_dimension)
      , right_dimension(right_dimension)
    { }

    //! Construct \c this by making a copy of \c other.
    /*! \see BaseTensor(CopyFrom<Tensor> const other) */
    template<typename Tensor> SiteBaseTensor(CopyFrom<Tensor const> const other)
      : BaseTensor(other)
      , physical_dimension(other->physicalDimension())
      , left_dimension(other->leftDimension())
      , right_dimension(other->rightDimension())
    { }

    //! Construct \c this using the dimensions and size of \c other but leaving the data uninitialized.
    template<typename Tensor> SiteBaseTensor(DimensionsOf<Tensor const> const other)
      : BaseTensor(other->size())
      , physical_dimension(other->physicalDimension())
      , left_dimension(other->leftDimension())
      , right_dimension(other->rightDimension())
    { }

    //! Constructs a tensor using data supplied from a generator.
    /*! \see BaseTensor( unsigned int const size, FillWithGenerator<G> const generator) */
    template<typename G> SiteBaseTensor(
          PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
        , unsigned int const size
        , FillWithGenerator<G> const generator
    ) : BaseTensor(size,generator)
      , physical_dimension(physical_dimension)
      , left_dimension(left_dimension)
      , right_dimension(right_dimension)
    { }

    //! Constructs a tensor using data supplied from a range.
    //! \see BaseTensor(FillWithRange<Range> const init)
    template<typename Range> SiteBaseTensor(
          PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
        , FillWithRange<Range> const init
    ) : BaseTensor(init)
      , physical_dimension(physical_dimension)
      , left_dimension(left_dimension)
      , right_dimension(right_dimension)
    { }

    //! Sets all dimensions to 1, and then allocates an array of size one and fills it with the value 1.
    SiteBaseTensor(MakeTrivial const make_trivial)
      : BaseTensor(make_trivial)
      , physical_dimension(1)
      , left_dimension(1)
      , right_dimension(1)
    { }

    //! @}
    //@+node:gcross.20110428160636.2475: *5* Dimension information
    //! \name Dimension information
    //! @{

    public:

    //! Returns the physical dimension of this tensor.
    PhysicalDimension physicalDimension() const { return physical_dimension; }
    //! Unwraps the physical dimension of this tensor and directly returns an unsigned integer.
    unsigned int physicalDimension(AsUnsignedInteger _) const { return *physical_dimension; }

    //! Returns the left dimension of this tensor.
    LeftDimension leftDimension() const { return left_dimension; }
    //! Unwraps the left dimension of this tensor and directly returns an unsigned integer.
    unsigned int leftDimension(AsUnsignedInteger _) const { return *left_dimension; }

    //! Returns the right dimension of this tensor.
    RightDimension rightDimension() const { return right_dimension; }
    //! Unwraps the right dimension of this tensor and directly returns an unsigned integer.
    unsigned int rightDimension(AsUnsignedInteger _) const { return *right_dimension; }

    //! @}
    //@+node:gcross.20110428160636.2472: *5* Fields
    private:

    //! The physical dimension of the site.
    PhysicalDimension physical_dimension;
    //! The left dimension of the site.
    LeftDimension left_dimension;
    //! The right dimension of the site.
    RightDimension right_dimension;
    //@-others
};
//@+node:gcross.20110215235924.2004: *3* Boundary
//@+node:gcross.20110215235924.2005: *4* ExpectationBoundary
//@+<< Description >>
//@+node:gcross.20110428160636.2519: *5* << Description >>
/*! Boundaries for expectation tensor network chains.

\image html expectation_boundary_tensors.png
\image latex expectation_boundary_tensors.eps

These tensors provide the left and right boundaries for the tensor network chains used to compute expectation values.  They have three ranks, two of which connect to the neighboring (i.e., left- or right-most) state site tensor and its complex conjugate (the "state" dimension), and one of which connects to the operator site tensor (the "operator" dimension).

The type tag on this class should be Left or Right, corresponding to whether this is respectively the left or the right boundary of the chain.

\note
See the documentation in BaseTensor for a description of the policy of how data ownership in tensors works.  (Short version: tensors own their data, which can be moved but not copied unless you explicitly ask for a copy to be made.)

\see StateSiteAny BaseTensor
*/
//@-<< Description >>
template<typename side> class ExpectationBoundary : public BaseTensor {
    //@+others
    //@+node:gcross.20110428160636.2521: *5* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(ExpectationBoundary)
    //@+node:gcross.20110428160636.2524: *5* Assignment
    //! \name Assignment
    //! @{

    public:

    //! Moves the data (and dimensions) from \c other to \c this and invalidates \c other.
    ExpectationBoundary& operator=(BOOST_RV_REF(ExpectationBoundary) other) {
        if(this == &other) return *this;
        BaseTensor::operator=(static_cast<BOOST_RV_REF(BaseTensor)>(other));
        operator_dimension = boost::move(other.operator_dimension);
        state_dimension = boost::move(other.state_dimension);
        return *this;
    }

    //! Swaps the data (and dimensions) in \c other and \c this.
    void swap(ExpectationBoundary& other) {
        if(this == &other) return;
        BaseTensor::swap(other);
        std::swap(operator_dimension,other.operator_dimension);
        std::swap(state_dimension,other.state_dimension);
    }

    //! @}
    //@+node:gcross.20110428160636.2523: *5* Constructors
    //! \name Constructors
    //! @{

    public:

    //! Construct an invalid tensor (presumably into which you will eventually move data from elsewhere).
    ExpectationBoundary() {}

    //! Move the data (and dimensions) from \c other into \c this and invalidate \c other.
    ExpectationBoundary(BOOST_RV_REF(ExpectationBoundary) other)
      : BaseTensor(static_cast<BOOST_RV_REF(BaseTensor)>(other))
      , operator_dimension(boost::move(other.operator_dimension))
      , state_dimension(boost::move(other.state_dimension))
    { }

    //! Initialize the dimensions with the given values and allocate memory for an array of the appropriate size.
    ExpectationBoundary(
          OperatorDimension const operator_dimension
        , StateDimension const state_dimension
    ) : BaseTensor((*operator_dimension)*(*state_dimension)*(*state_dimension))
      , operator_dimension(operator_dimension)
      , state_dimension(state_dimension)
    { }

    //! Construct \c this by making a copy of \c other.
    /*! \see BaseTensor(CopyFrom<Tensor> const other) */
    template<typename other_side> ExpectationBoundary(
          CopyFrom<ExpectationBoundary<other_side> const> const other
    ) : BaseTensor(other)
      , operator_dimension(other->operator_dimension)
      , state_dimension(other->state_dimension)
    { }

    //! Constructs a tensor using data supplied from a generator.
    /*! \see BaseTensor( unsigned int const size, FillWithGenerator<G> const generator) */
    template<typename G> ExpectationBoundary(
          OperatorDimension const operator_dimension
        , StateDimension const state_dimension
        , FillWithGenerator<G> const generator
    ) : BaseTensor((*operator_dimension)*(*state_dimension)*(*state_dimension),generator)
      , operator_dimension(operator_dimension)
      , state_dimension(state_dimension)
    { }

    //! Constructs a tensor using data supplied from a range.
    /*!
    \note The state dimension is inferred automatically from the size of the range and the operator dimension.
    \see BaseTensor(FillWithRange<Range> const init)
    */
    template<typename Range> ExpectationBoundary(
          OperatorDimension const operator_dimension
        , FillWithRange<Range> const init
    ) : BaseTensor(init)
      , operator_dimension(operator_dimension)
      , state_dimension((unsigned int)sqrt(size()/(*operator_dimension)))
    { }

    //! Sets all dimensions to 1, and then allocates an array of size one and fills it with the value 1.
    ExpectationBoundary(
          MakeTrivial const make_trivial
    ) : BaseTensor(make_trivial)
      , operator_dimension(1)
      , state_dimension(1)
    { }

    //! @}
    //@+node:gcross.20110428160636.2526: *5* Dimension information
    //! \name Dimension information
    //! @{

    public:

    //! Returns the operator dimension of this tensor.
    OperatorDimension operatorDimension() const { return operator_dimension; }
    //! Unwraps the operator dimension of this tensor and directly returns an unsigned integer.
    unsigned int operatorDimension(AsUnsignedInteger _) const { return *operator_dimension; }

    //! Returns the state dimension of this tensor.
    StateDimension stateDimension() const { return state_dimension; }
    //! Unwraps the state dimension of this tensor and directly returns an unsigned integer.
    unsigned int stateDimension(AsUnsignedInteger _) const { return *state_dimension; }

    //! @}
    //@+node:gcross.20110428160636.2522: *5* Fields
    private:

    //! The operator dimension.
    OperatorDimension operator_dimension;
    //! The state dimension.
    StateDimension state_dimension;
    //@+node:gcross.20110428160636.2525: *5* Miscellaneous
    public:

    //! The trivial state site tensor with all dimensions one and containing the single value 1.
    static ExpectationBoundary const trivial;
    //@-others
};

template<typename side> ExpectationBoundary<side> const ExpectationBoundary<side>::trivial(make_trivial);
//@+node:gcross.20110215235924.2006: *4* OverlapBoundary
//@+<< Description >>
//@+node:gcross.20110428160636.2528: *5* << Description >>
/*! Boundaries for overlap tensor network chains.

\image html overlap_boundary_tensors.png
\image latex overlap_boundary_tensors.eps

These tensors provide the left and right boundaries for the tensor network chains used to compute overlaps between states.  They have two ranks, one of which connects to an overlap site tensor (the "ovelap" dimension) and one of which connects to a state site tensor (the "state" dimension).

The type tag on this class should be Left or Right, corresponding to whether this is respectively the left or the right boundary of the chain.

\note
See the documentation in BaseTensor for a description of the policy of how data ownership in tensors works.  (Short version: tensors own their data, which can be moved but not copied unless you explicitly ask for a copy to be made.)

\see StateSiteAny BaseTensor
*/
//@-<< Description >>
template<typename side> class OverlapBoundary : public BaseTensor {
    //@+others
    //@+node:gcross.20110428160636.2529: *5* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(OverlapBoundary)
    //@+node:gcross.20110428160636.2532: *5* Assignment
    //! \name Assignment
    //! @{

    public:

    //! Moves the data (and dimensions) from \c other to \c this and invalidates \c other.
    OverlapBoundary& operator=(BOOST_RV_REF(OverlapBoundary) other) {
        if(this == &other) return *this;
        BaseTensor::operator=(static_cast<BOOST_RV_REF(BaseTensor)>(other));
        overlap_dimension = boost::move(other.overlap_dimension);
        state_dimension = boost::move(other.state_dimension);
        return *this;
    }

    //! Swaps the data (and dimensions) in \c other and \c this.
    void swap(OverlapBoundary& other) {
        if(this == &other) return;
        BaseTensor::swap(other);
        std::swap(overlap_dimension,other.overlap_dimension);
        std::swap(state_dimension,other.state_dimension);
    }

    //! @}
    //@+node:gcross.20110428160636.2531: *5* Constructors
    //! \name Constructors
    //! @{

    public:

    //! Construct an invalid tensor (presumably into which you will eventually move data from elsewhere).
    OverlapBoundary() {}

    //! Move the data (and dimensions) from \c other into \c this and invalidate \c other.
    OverlapBoundary(BOOST_RV_REF(OverlapBoundary) other)
      : BaseTensor(static_cast<BOOST_RV_REF(BaseTensor)>(other))
      , overlap_dimension(boost::move(other.overlap_dimension))
      , state_dimension(boost::move(other.state_dimension))
    { }

    //! Initialize the dimensions with the given values and allocate memory for an array of the appropriate size.
    OverlapBoundary(
          OverlapDimension const overlap_dimension
        , StateDimension const state_dimension
    ) : BaseTensor((*overlap_dimension)*(*state_dimension))
      , overlap_dimension(overlap_dimension)
      , state_dimension(state_dimension)
    { }

    //! Construct \c this by making a copy of \c other.
    /*! \see BaseTensor(CopyFrom<Tensor> const other) */
    template<typename other_side> OverlapBoundary(
          CopyFrom<OverlapBoundary<other_side> const> const other
    ) : BaseTensor(other)
      , overlap_dimension(other->overlap_dimension)
      , state_dimension(other->state_dimension)
    { }

    //! Constructs a tensor using data supplied from a generator.
    /*! \see BaseTensor( unsigned int const size, FillWithGenerator<G> const generator) */
    template<typename G> OverlapBoundary(
          OverlapDimension const overlap_dimension
        , StateDimension const state_dimension
        , FillWithGenerator<G> const generator
    ) : BaseTensor((*overlap_dimension)*(*state_dimension),generator)
      , overlap_dimension(overlap_dimension)
      , state_dimension(state_dimension)
    { }

    //! Constructs a tensor using data supplied from a range.
    /*!
    \note The state dimension is inferred automatically from the size of the range and the overlap dimension.
    \see BaseTensor(FillWithRange<Range> const init)
    */
    template<typename Range> OverlapBoundary(
          OverlapDimension const overlap_dimension
        , FillWithRange<Range> const init
    ) : BaseTensor(init)
      , overlap_dimension(overlap_dimension)
      , state_dimension(size()/(*overlap_dimension))
    { }

    //! Sets all dimensions to 1, and then allocates an array of size one and fills it with the value 1.
    OverlapBoundary(
          MakeTrivial const make_trivial
    ) : BaseTensor(make_trivial)
      , overlap_dimension(1)
      , state_dimension(1)
    { }

    //! @}
    //@+node:gcross.20110428160636.2533: *5* Dimension information
    //! \name Dimension information
    //! @{

    public:

    //! Returns the overlap dimension of this tensor.
    OverlapDimension overlapDimension() const { return overlap_dimension; }
    //! Unwraps the overlap dimension of this tensor and directly returns an unsigned integer.
    unsigned int overlapDimension(AsUnsignedInteger _) const { return *overlap_dimension; }

    //! Returns the state dimension of this tensor.
    StateDimension stateDimension() const { return state_dimension; }
    //! Unwraps the state dimension of this tensor and directly returns an unsigned integer.
    unsigned int stateDimension(AsUnsignedInteger _) const { return *state_dimension; }

    //! @}
    //@+node:gcross.20110428160636.2530: *5* Fields
    private:

    //! The overlap dimension.
    OverlapDimension overlap_dimension;
    //! The state dimension.
    StateDimension state_dimension;
    //@+node:gcross.20110428160636.2534: *5* Miscellaneous
    public:

    //! The trivial state site tensor with all dimensions one and containing the single value 1.
    static OverlapBoundary const trivial;
    //@-others
};

template<typename side> OverlapBoundary<side> const OverlapBoundary<side>::trivial(make_trivial);
//@+node:gcross.20110215235924.1926: *3* State
//@+node:gcross.20110215235924.1927: *4* StateSiteAny
//@+<< Description >>
//@+node:gcross.20110428160636.2478: *5* << Description >>
/*! Base class for state site tensors.

\image html state_site_tensor.png
\image latex state_site_tensor.eps

Each state site tensor is associated with a qudit and has three ranks.  Two of the three ranks correspond to its connections to its neighbors (i.e., its "left" and "right" dimensions) and the last rank corresponds to the state space of the qudit at the site (i.e., its "physical" dimension).

The \c Any suffix on the base class means that no assertion has been made about the normalization of this tensor.

\note
See the documentation in BaseTensor for a description of the policy of how data ownership in tensors works.  (Short version: tensors own their data, which can be moved but not copied unless you explicitly ask for a copy to be made.)

\see BaseTensor
*/
//@-<< Description >>
class StateSiteAny : public SiteBaseTensor {
    //@+others
    //@+node:gcross.20110428160636.2479: *5* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(StateSiteAny)
    //@+node:gcross.20110428160636.2483: *5* Assignment
    //! \name Assignment
    //! @{

    protected:

    //! Moves the data (and dimensions) from \c other to \c this and invalidates \c other.
    void operator=(BOOST_RV_REF(StateSiteAny) other) {
        SiteBaseTensor::operator=(static_cast<BOOST_RV_REF(SiteBaseTensor)>(other));
    }

    //! Swaps the data (and dimensions) in \c other and \c this.
    void swap(StateSiteAny& other) {
        SiteBaseTensor::swap(other);
    }

    //! @}
    //@+node:gcross.20110428160636.2482: *5* Constructors
    //! \name Constructors
    //! @{

    protected:

    //! Construct an invalid tensor (presumably into which you will eventually move data from elsewhere).
    StateSiteAny() {}

    //! Move the data (and dimensions) from \c other into \c this and invalidate \c other.
    StateSiteAny(BOOST_RV_REF(StateSiteAny) other)
      : SiteBaseTensor(static_cast<BOOST_RV_REF(SiteBaseTensor)>(other))
    { }

    //! Initialize the dimensions with the given values and allocate memory for an array of the appropriate size.
    StateSiteAny(
          PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
    ) : SiteBaseTensor(
             physical_dimension
            ,left_dimension
            ,right_dimension
            ,(*physical_dimension)*(*left_dimension)*(*right_dimension)
        )
    { }

    //! Construct \c this by making a copy of \c other.
    /*! \see BaseTensor(CopyFrom<Tensor> const other) */
    StateSiteAny(
          CopyFrom<StateSiteAny const> const other_site
    ) : SiteBaseTensor(other_site)
    { }

    //! Construct \c this using the dimensions and size of \c other but leaving the data uninitialized.
    StateSiteAny(
          DimensionsOf<StateSiteAny const> const other_site
    ) : SiteBaseTensor(other_site)
    { }

    //! Constructs a tensor using data supplied from a generator.
    /*! \see BaseTensor( unsigned int const size, FillWithGenerator<G> const generator) */
    template<typename G> StateSiteAny(
          PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
        , FillWithGenerator<G> const generator
    ) : SiteBaseTensor(
             physical_dimension
            ,left_dimension
            ,right_dimension
            ,(*physical_dimension)*(*left_dimension)*(*right_dimension)
            ,generator
        )
    { }

    //! Constructs a tensor using data supplied from a range.
    /*!
    \note The physical dimension is inferred automatically from the size of the range and the left and right dimensions.
    \see BaseTensor(FillWithRange<Range> const init)
    */
    template<typename Range> StateSiteAny(
          LeftDimension const left_dimension
        , RightDimension const right_dimension
        , FillWithRange<Range> const init
    ) : SiteBaseTensor(
             PhysicalDimension(init->size()/((*left_dimension)*(*right_dimension)))
            ,left_dimension
            ,right_dimension
            ,init
        )
    { }

    //! Sets all dimensions to 1, and then allocates an array of size one and fills it with the value 1.
    StateSiteAny(MakeTrivial const make_trivial) : SiteBaseTensor(make_trivial) {}

    //! @}
    //@+node:gcross.20110510004855.3255: *5* Dimension information
    //! \name Dimension information
    //! @{

    public:

    //! Returns a vector with the dimensions of the data stored in this tensor in row-major order
    vector<unsigned int> dataDimensions() const {
        vector<unsigned int> dimensions;
        dimensions.reserve(3);
        dimensions.push_back(physicalDimension(as_unsigned_integer));
        dimensions.push_back(leftDimension(as_unsigned_integer));
        dimensions.push_back(rightDimension(as_unsigned_integer));
        return boost::move(dimensions);
    }

    //! @}
    //@+node:gcross.20110428160636.2484: *5* Observation transition matrices
    //! \name Observation transition matrices
    //! @{

    public:

    //! Computes the offset into the data array of the transition matrix corresponding to the given observation value.
    unsigned int observationValueOffset(unsigned int const observation_value) const {
        assert(observation_value < physicalDimension(as_unsigned_integer));
        return observation_value*leftDimension(as_unsigned_integer)*rightDimension(as_unsigned_integer);
    }

    //! Returns a pointer to the transition matrix for the given observation value.
    complex<double>* transitionMatrixForObservation(unsigned int observation_value) {
        return begin() + observationValueOffset(observation_value);
    }

    //! Returns a read-only pointer to the transition matrix for the given observation value.
    complex<double> const* transitionMatrixForObservation(unsigned int observation_value) const {
        return begin() + observationValueOffset(observation_value);
    }

    //! @}
    //@-others
};
//@+node:gcross.20110215235924.1928: *4* StateSite
//@+<< Description >>
//@+node:gcross.20110428160636.2485: *5* << Description >>
/*! State site tensors.

\image html state_site_tensor.png
\image latex state_site_tensor.eps

Each state site tensor is associated with a qudit,and has three ranks.  Two of the three ranks correspond to its connections to its neighbors (i.e., its "left" and "right" dimensions) and the last rank corresponds to the state space of the qudit at the site (i.e., its "physical" dimension).

The type tag \c side on the class uses the type system to embed information about the normalization of the tensor in the type, so that for example a left-normalized state site tensor cannot be used in the place of a right-normalized tensor.

\tparam side the normalization of this state site tensor

\note
See the documentation in BaseTensor for a description of the policy of how data ownership in tensors works.  (Short version: tensors own their data, which can be moved but not copied unless you explicitly ask for a copy to be made.)

\see BaseTensor
*/
//@-<< Description >>
template<typename side> class StateSite : public StateSiteAny {
    //@+others
    //@+node:gcross.20110428160636.2486: *5* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(StateSite)
    //@+node:gcross.20110428160636.2489: *5* Assignment
    //! \name Assignment
    //! @{

    public:

    //! Moves the data (and dimensions) from \c other to \c this and invalidates \c other.
    StateSite& operator=(BOOST_RV_REF(StateSite) other) {
        if(this == &other) return *this;
        StateSiteAny::operator=(static_cast<BOOST_RV_REF(StateSiteAny)>(other));
        return *this;
    }

    //! Swaps the data (and dimensions) in \c other and \c this.
    void swap(StateSite& other) {
        if(this == &other) return;
        StateSiteAny::swap(other);
    }

    //! @}
    //@+node:gcross.20110428160636.2487: *5* Constructors
    //! \name Constructors
    //! @{

    public:

    //! Construct an invalid tensor (presumably into which you will eventually move data from elsewhere).
    StateSite() {}

    //! Move the data (and dimensions) from \c other into \c this and invalidate \c other.
    StateSite(BOOST_RV_REF(StateSite) other)
      : StateSiteAny(static_cast<BOOST_RV_REF(StateSiteAny)>(other))
    { }

    //! Initialize the dimensions with the given values and allocate memory for an array of the appropriate size.
    StateSite(
          PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
    ) : StateSiteAny(
             physical_dimension
            ,left_dimension
            ,right_dimension
        )
    { }

    //! Construct \c this by making a copy of \c other.
    /*! \see BaseTensor(CopyFrom<Tensor> const other) */
    StateSite(
          CopyFrom<StateSiteAny const> const other_site
    ) : StateSiteAny(other_site)
    { }

    //! Construct \c this using the dimensions and size of \c other but leaving the data uninitialized.
    StateSite(
          DimensionsOf<StateSiteAny const> const other_site
    ) : StateSiteAny(other_site)
    { }

    //! Constructs a tensor using data supplied from a generator.
    /*! \see BaseTensor( unsigned int const size, FillWithGenerator<G> const generator) */
    template<typename G> StateSite(
          PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
        , FillWithGenerator<G> const generator
    ) : StateSiteAny(
             physical_dimension
            ,left_dimension
            ,right_dimension
            ,generator
        )
    { }

    //! Constructs a tensor using data supplied from a range.
    /*!
    \note The physical dimension is inferred automatically from the size of the range and the left and right dimensions.
    \see BaseTensor(FillWithRange<Range> const init)
    */
    template<typename Range> StateSite(
          LeftDimension const left_dimension
        , RightDimension const right_dimension
        , FillWithRange<Range> const init
    ) : StateSiteAny(
             left_dimension
            ,right_dimension
            ,init
        )
    { }

    //! Sets all dimensions to 1, and then allocates an array of size one and fills it with the value 1.
    StateSite(MakeTrivial const make_trivial) : StateSiteAny(make_trivial) {}

    //! @}
    //@+node:gcross.20110428160636.2488: *5* Miscellaneous
    public:

    //! Returns the Frobenius 2-norm of this tensor --- i.e., the sum of the absolute value squared of all components.
    double norm() const { return BaseTensor::norm(); }

    //! The trivial state site tensor with all dimensions one and containing the single value 1.
    static StateSite const trivial;
    //@-others
};

template<typename side> StateSite<side> const StateSite<side>::trivial(make_trivial);
//@+node:gcross.20110216193817.1919: *3* Overlap
//@+node:gcross.20110216193817.1921: *4* OverlapSiteAny
//@+<< Description >>
//@+node:gcross.20110428160636.2491: *5* << Description >>
/*! Base class for overlap site tensors.

\image html overlap_site_tensor.png
\image latex overlap_site_tensor.eps

An overlap site tensor is the mathematical dual of a state site tensor that is used when computing the overlap of two quantum states.  Like a state site tensor, it is associated with a qudit, and has three ranks.  Two of the three ranks correspond to its connections to its neighbors (i.e., its "left" and "right" dimensions) and the last rank corresponds to the state space of the qudit at the site (i.e., its "physical" dimension).

The \c Any suffix on the base class means that no assertion has been made about the normalization of this tensor.

\note
See the documentation in BaseTensor for a description of the policy of how data ownership in tensors works.  (Short version: tensors own their data, which can be moved but not copied unless you explicitly ask for a copy to be made.)

\see StateSiteAny BaseTensor
*/
//@-<< Description >>
class OverlapSiteAny : public SiteBaseTensor {
    //@+others
    //@+node:gcross.20110428160636.2494: *5* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(OverlapSiteAny)
    //@+node:gcross.20110428160636.2498: *5* Assignment
    //! \name Assignment
    //! @{

    protected:

    //! Moves the data (and dimensions) from \c other to \c this and invalidates \c other.
    void operator=(BOOST_RV_REF(OverlapSiteAny) other) {
        SiteBaseTensor::operator=(static_cast<BOOST_RV_REF(SiteBaseTensor)>(other));
    }

    //! Swaps the data (and dimensions) in \c other and \c this.
    void swap(OverlapSiteAny& other) {
        SiteBaseTensor::swap(other);
    }

    //! @}
    //@+node:gcross.20110428160636.2497: *5* Constructors
    //! \name Constructors
    //! @{

    protected:

    //! Construct an invalid tensor (presumably into which you will eventually move data from elsewhere).
    OverlapSiteAny() {}

    //! Move the data (and dimensions) from \c other into \c this and invalidate \c other.
    OverlapSiteAny(BOOST_RV_REF(OverlapSiteAny) other)
      : SiteBaseTensor(static_cast<BOOST_RV_REF(SiteBaseTensor)>(other))
    { }

    //! Initialize the dimensions with the given values and allocate memory for an array of the appropriate size.
    OverlapSiteAny(
          RightDimension const right_dimension
        , PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
    ) : SiteBaseTensor(
             physical_dimension
            ,left_dimension
            ,right_dimension
            ,(*physical_dimension)*(*left_dimension)*(*right_dimension)
        )
    { }

    //! Construct \c this by making a copy of \c other.
    /*! \see BaseTensor(CopyFrom<Tensor> const other) */
    OverlapSiteAny(
          CopyFrom<OverlapSiteAny const> const other_site
    ) : SiteBaseTensor(other_site)
    { }

    //! Construct \c this using the dimensions and size of \c other but leaving the data uninitialized.
    OverlapSiteAny(
          DimensionsOf<StateSiteAny const> const other_site
    ) : SiteBaseTensor(other_site)
    { }

    //! Constructs a tensor using data supplied from a generator.
    /*! \see BaseTensor( unsigned int const size, FillWithGenerator<G> const generator) */
    template<typename G> OverlapSiteAny(
          RightDimension const right_dimension
        , PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , FillWithGenerator<G> const generator
    ) : SiteBaseTensor(
             physical_dimension
            ,left_dimension
            ,right_dimension
            ,(*physical_dimension)*(*left_dimension)*(*right_dimension)
            ,generator
        )
    { }

    //! Constructs a tensor using data supplied from a range.
    /*!
    \note The physical dimension is inferred automatically from the size of the range and the left and right dimensions.
    \see BaseTensor(FillWithRange<Range> const init)
    */
    template<typename Range> OverlapSiteAny(
          RightDimension const right_dimension
        , LeftDimension const left_dimension
        , FillWithRange<Range> const init
    ) : SiteBaseTensor(
             PhysicalDimension(init->size()/((*left_dimension)*(*right_dimension)))
            ,left_dimension
            ,right_dimension
            ,init
        )
    { }

    //! Sets all dimensions to 1, and then allocates an array of size one and fills it with the value 1.
    OverlapSiteAny(MakeTrivial const make_trivial) : SiteBaseTensor(make_trivial) {}

    //! @}
    //@-others
};
//@+node:gcross.20110215235924.1929: *4* OverlapSite
//@+<< Description >>
//@+node:gcross.20110428160636.2493: *5* << Description >>
/*! Overlap site tensors.

\image html overlap_site_tensor.png
\image latex overlap_site_tensor.eps

An overlap site tensor is the mathematical dual of a state site tensor that is used when computing the overlap of two quantum states.  Like a state site tensor, it is associated with a qudit, and has three ranks.  Two of the three ranks correspond to its connections to its neighbors (i.e., its "left" and "right" dimensions) and the last rank corresponds to the state space of the qudit at the site (i.e., its "physical" dimension).

The type tag \c side on the class uses the type system to embed information about the normalization of the tensor in the type, so that for example a left-normalized state site tensor cannot be used in the place of a right-normalized tensor.

\tparam side the normalization of this state site tensor

\note
See the documentation in BaseTensor for a description of the policy of how data ownership in tensors works.  (Short version: tensors own their data, which can be moved but not copied unless you explicitly ask for a copy to be made.)

\see StateSite BaseTensor
*/
//@-<< Description >>
template<typename side> class OverlapSite : public OverlapSiteAny {
    //@+others
    //@+node:gcross.20110428160636.2496: *5* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(OverlapSite)
    //@+node:gcross.20110428160636.2501: *5* Assignment
    //! \name Assignment
    //! @{

    public:

    //! Moves the data (and dimensions) from \c other to \c this and invalidates \c other.
    OverlapSite& operator=(BOOST_RV_REF(OverlapSite) other) {
        if(this == &other) return *this;
        OverlapSiteAny::operator=(static_cast<BOOST_RV_REF(OverlapSiteAny)>(other));
        return *this;
    }

    //! Swaps the data (and dimensions) in \c other and \c this.
    void swap(OverlapSite& other) {
        if(this == &other) return;
        OverlapSiteAny::swap(other);
    }

    //! @}
    //@+node:gcross.20110428160636.2500: *5* Constructors
    //! \name Constructors
    //! @{

    public:

    //! Construct an invalid tensor (presumably into which you will eventually move data from elsewhere).
    OverlapSite() {}

    //! Move the data (and dimensions) from \c other into \c this and invalidate \c other.
    OverlapSite(BOOST_RV_REF(OverlapSite) other)
      : OverlapSiteAny(static_cast<BOOST_RV_REF(OverlapSiteAny)>(other))
    { }

    //! Initialize the dimensions with the given values and allocate memory for an array of the appropriate size.
    OverlapSite(
          RightDimension const right_dimension
        , PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
    ) : OverlapSiteAny(
             right_dimension
            ,physical_dimension
            ,left_dimension
        )
    { }

    //! Construct \c this by making a copy of \c other.
    /*! \see BaseTensor(CopyFrom<Tensor> const other) */
    OverlapSite(
          CopyFrom<OverlapSiteAny const> const other_site
    ) : OverlapSiteAny(other_site)
    { }

    //! Construct \c this using the dimensions and size of \c other but leaving the data uninitialized.
    OverlapSite(
          DimensionsOf<StateSiteAny const> const other_site
    ) : OverlapSiteAny(other_site)
    { }

    //! Constructs a tensor using data supplied from a generator.
    /*! \see BaseTensor( unsigned int const size, FillWithGenerator<G> const generator) */
    template<typename G> OverlapSite(
          RightDimension const right_dimension
        , PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , FillWithGenerator<G> const generator
    ) : OverlapSiteAny(
             right_dimension
            ,physical_dimension
            ,left_dimension
            ,generator
        )
    { }

    //! Constructs a tensor using data supplied from a range.
    /*!
    \note The physical dimension is inferred automatically from the size of the range and the left and right dimensions.
    \see BaseTensor(FillWithRange<Range> const init)
    */
    template<typename Range> OverlapSite(
          RightDimension const right_dimension
        , LeftDimension const left_dimension
        , FillWithRange<Range> const init
    ) : OverlapSiteAny(
             right_dimension
            ,left_dimension
            ,init
        )
    { }

    //! Sets all dimensions to 1, and then allocates an array of size one and fills it with the value 1.
    OverlapSite(MakeTrivial const make_trivial) : OverlapSiteAny(make_trivial) {}

    //! @}
    //@+node:gcross.20110428160636.2502: *5* Miscellaneous
    public:

    //! The trivial overlap site tensor with all dimensions one and containing the single value 1.
    static OverlapSite const trivial;
    //@-others
};

template<typename side> OverlapSite<side> const OverlapSite<side>::trivial(make_trivial);
//@+node:gcross.20110215235924.1933: *3* OperatorSite
//@+<< Description >>
//@+node:gcross.20110428160636.2507: *4* << Description >>
/*! Operator site tensor.

\image html operator_site_tensor.png
\image latex operator_site_tensor.eps

Each operator site tensor is associated with a qudit and has four ranks.  Two of the four ranks correspond to its connections to its neighbors (i.e., its "left" and "right" dimensions) and the last two ranks corresponds to the space of operators acting on the qudit at the site (i.e., its "physical" dimension);  of the last two ranks, one connects to a state site tensor, and the other connects to an overlap site tensor.

Since operator site tensor are usually sparse, a sparse representation is used to store them.  This tensor class contains two kinds of data:  an array of single-qudit matrices, and an array of pair of indices.

\note
See the documentation in BaseTensor for a description of the policy of how data ownership in tensors works.  (Short version: tensors own their data, which can be moved but not copied unless you explicitly ask for a copy to be made.)

\see BaseTensor
*/
//@-<< Description >>
class OperatorSite : public SiteBaseTensor {
    //@+others
    //@+node:gcross.20110428160636.2508: *4* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(OperatorSite)
    //@+node:gcross.20110428160636.2514: *4* Assignment
    //! \name Assignment
    //! @{

    public:

    //! Moves the data (and dimensions) from \c other to \c this and invalidates \c other.
    OperatorSite& operator=(BOOST_RV_REF(OperatorSite) other) {
        if(this == &other) return *this;
        SiteBaseTensor::operator=(static_cast<BOOST_RV_REF(SiteBaseTensor)>(other));
        number_of_matrices = copyAndReset(other.number_of_matrices);
        moveArrayToFrom(index_data,other.index_data); 
        return *this;
    }

    //! Swaps the data (and dimensions) in \c other and \c this.
    void swap(OperatorSite& other) {
        if(this == &other) return;
        SiteBaseTensor::swap(other);
        std::swap(number_of_matrices,other.number_of_matrices);
        std::swap(index_data,other.index_data);
    }

    //! @}
    //@+node:gcross.20110428160636.2513: *4* Constructors
    //! \name Constructors
    //! @{

    public:

    //! Construct an invalid tensor (presumably into which you will eventually move data from elsewhere).
    OperatorSite()
      : number_of_matrices(0)
      , index_data(NULL)
    { }

    //! Move the data (and dimensions) from \c other into \c this and invalidate \c other.
    OperatorSite(BOOST_RV_REF(OperatorSite) other)
      : SiteBaseTensor(boost::move(static_cast<SiteBaseTensor&>(other)))
      , number_of_matrices(copyAndReset(other.number_of_matrices))
      , index_data(copyAndReset(other.index_data))
    { }

    //! Initialize the dimensions and number of transition matrices with the given values and allocate arrays of the appropriate sizes.
    OperatorSite(
          unsigned int const number_of_matrices
        , PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
    ) : SiteBaseTensor(
             physical_dimension
            ,left_dimension
            ,right_dimension
            ,number_of_matrices*(*physical_dimension)*(*physical_dimension)
        )
      , number_of_matrices(number_of_matrices)
      , index_data(new uint32_t[number_of_matrices*2])
    { }

    //! Construct \c this by making a copy of \c other.
    /*! \see BaseTensor(CopyFrom<Tensor> const other) */
    OperatorSite(
          CopyFrom<OperatorSite const> const other
    ) : SiteBaseTensor(other)
      , number_of_matrices(other->number_of_matrices)
      , index_data(new uint32_t[number_of_matrices*2])
    {
        copy(other->index_data,other->index_data+2*number_of_matrices,index_data);
    }

    //! Constructs a tensor with data supplied from generators.
    /*! \see BaseTensor( unsigned int const size, FillWithGenerator<G> const generator) */
    template<typename G1, typename G2> OperatorSite(
          unsigned int const number_of_matrices
        , PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
        , FillWithGenerator<G1> const index_generator
        , FillWithGenerator<G2> const matrix_generator
    ) : SiteBaseTensor(
             physical_dimension
            ,left_dimension
            ,right_dimension
            ,number_of_matrices*(*physical_dimension)*(*physical_dimension)
            ,matrix_generator
        )
      , number_of_matrices(number_of_matrices)
      , index_data(new uint32_t[number_of_matrices*2])
    {
        BOOST_CONCEPT_ASSERT(( Generator<G1,uint32_t> ));
        uint32_t* index = index_data;
        REPEAT(number_of_matrices) {
            uint32_t left_index = (*index_generator)()
                   , right_index = (*index_generator)()
                   ;
            assert(left_index >= 1 && left_index <= *leftDimension());
            assert(right_index >= 1 && right_index <= *rightDimension());
            *index++ = left_index;
            *index++ = right_index;
        }
        //generate_n(index_data.get(),size,*index_generator);
    }

    //! Constructs a tensor using data supplied from ranges.
    //! \see BaseTensor(FillWithRange<Range> const init)
    template<typename Range1, typename Range2> OperatorSite(
          LeftDimension const left_dimension
        , RightDimension const right_dimension
        , FillWithRange<Range1> const index_init
        , FillWithRange<Range2> const matrix_init
    ) : SiteBaseTensor(
             PhysicalDimension((unsigned int)sqrt(matrix_init->size()/(index_init->size()/2)))
            ,left_dimension
            ,right_dimension
            ,matrix_init
        )
      , number_of_matrices(index_init->size()/2)
      , index_data(new uint32_t[index_init->size()])
    {
        BOOST_CONCEPT_ASSERT(( RandomAccessRangeConcept<Range1 const> ));
        copy(*index_init,index_data);
    }

    //! Sets all dimensions and the number of matrices to 1, and sets the only matrix to the identity.
    OperatorSite(MakeTrivial const make_trivial)
      : SiteBaseTensor(make_trivial)
      , number_of_matrices(1)
      , index_data(new uint32_t[2])
    {
        fill_n(index_data,2,1);
    }

    //! @}
    //@+node:gcross.20110428160636.2516: *4* Data access
    //! \name Data access
    /*!
    \note
    These methods \a assume that the tensor is valid.  If it is not, then the
    result of using them is undefined.
    */
    //! @{

    //! An implicit cast to \c complex<double> that obtains a direct pointer to the beginning of the transition data.
    operator uint32_t*() { return index_data; }

    //! An implicit cast to \c complex<double> that obtains a read-only pointer to the beginning of the transition data.
    operator uint32_t const*() const { return index_data; }


    //! @}
    //@+node:gcross.20110510004855.3261: *4* Dimension information
    //! \name Dimension information
    //! @{

    public:

    //! Returns a vector with the dimensions of the matrix data stored in this tensor in row-major order
    vector<unsigned int> matrixDataDimensions() const {
        vector<unsigned int> dimensions;
        dimensions.reserve(3);
        dimensions.push_back(numberOfMatrices());
        dimensions.push_back(physicalDimension(as_unsigned_integer));
        dimensions.push_back(physicalDimension(as_unsigned_integer));
        return boost::move(dimensions);
    }

    //! Returns a vector with the dimensions of the matrix data stored in this tensor in row-major order
    vector<unsigned int> indexDataDimensions() const {
        vector<unsigned int> dimensions;
        dimensions.reserve(2);
        dimensions.push_back(numberOfMatrices());
        dimensions.push_back(2);
        return boost::move(dimensions);
    }

    //! @}
    //@+node:gcross.20110428160636.2511: *4* Fields
    private:

    //! The number of transition matrices in this tensor.
    unsigned int number_of_matrices;
    //! The pointer to the transition data for this operator.
    uint32_t* index_data;
    //@+node:gcross.20110428160636.2515: *4* Informational
    //! \name Informational
    //! @{

    public:

    //! Returns the number of transition matrices.
    unsigned int numberOfMatrices() const { return number_of_matrices; }

    //! @}
    //@+node:gcross.20110428160636.2517: *4* Miscellaneous
    public:

    //! If this tensor is valid, the data is destroyed;  otherwise nothing is done.
    ~OperatorSite() { if(index_data) delete[] index_data; }

    //! The trivial operator site tensor with all dimensions and the number of matrices to 1, and the only transition matrix equal to the identity.
    static OperatorSite const trivial;
    //@-others
};
//@-others

//@+<< Connectors >>
//@+node:gcross.20110430163445.2577: *3* << Connectors >>
/*!
\defgroup TensorConnectors Tensor connectors

The tensor connectors provide a set of operator functions that allow one to determine the size of the connected dimension between two connectable tensors.  These functions include a sanity check that the two tensors agree on what this size should be;  an exception is thrown if they disagree.
*/

//! @{

//@+others
//@+node:gcross.20110430163445.2578: *4* ExpectationBoundary<Left> | OperatorSite
//! Connects the operator dimension of a left expectation boundary to the left dimension of an operator site.
inline unsigned int operator|(
      Nutcracker::ExpectationBoundary<Left> const& expectation_boundary
    , Nutcracker::OperatorSite const& operator_site
) {
    return connectDimensions(
         "left expectation boundary state"
        ,expectation_boundary.operatorDimension(as_unsigned_integer)
        ,"operator site left"
        ,operator_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110430163445.2579: *4* ExpectationBoundary<Left> | StateSiteAny
//! Connects the operator dimension of a left expectation boundary to the left dimension of an state site.
inline unsigned int operator|(
      Nutcracker::ExpectationBoundary<Left> const& expectation_boundary
    , Nutcracker::StateSiteAny const& state_site
) {
    return connectDimensions(
         "left expectation boundary state"
        ,expectation_boundary.stateDimension(as_unsigned_integer)
        ,"state site left"
        ,state_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110430163445.2580: *4* OperatorSite | ExpectationBoundary<Right>
//! Connects the right dimension of an operator site to the operator dimension of a right expectation boundary.
inline unsigned int operator|(
      Nutcracker::OperatorSite const& operator_site
    , Nutcracker::ExpectationBoundary<Right> const& expectation_boundary
) {
    return connectDimensions(
         "operator site right"
        ,operator_site.rightDimension(as_unsigned_integer)
        ,"right expectation boundary state"
        ,expectation_boundary.operatorDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110430163445.2581: *4* OperatorSite | StateSiteAny
//! Connects the physical dimension of an operator site to the physical dimension of a state site.
inline unsigned int operator|(
      Nutcracker::OperatorSite const& operator_site
    , Nutcracker::StateSiteAny const& state_site
) {
    return connectDimensions(
         "operator site physical"
        ,operator_site.physicalDimension(as_unsigned_integer)
        ,"middle state site physical"
        ,state_site.physicalDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110430163445.2582: *4* OverlapBoundary<Left> | OverlapSiteAny
//! Connects the operator dimension of a left overlap boundary to the left dimension of an overlap site.
inline unsigned int operator|(
      Nutcracker::OverlapBoundary<Left> const& overlap_boundary
    , Nutcracker::OverlapSiteAny const& overlap_site
) {
    return connectDimensions(
         "left overlap boundary overlap"
        ,overlap_boundary.overlapDimension(as_unsigned_integer)
        ,"overlap site left"
        ,overlap_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110430163445.2583: *4* OverlapBoundary<Left> | StateSiteAny
//! Connects the operator dimension of a left overlap boundary to the left dimension of a state site.
inline unsigned int operator|(
      Nutcracker::OverlapBoundary<Left> const& overlap_boundary
    , Nutcracker::StateSiteAny const& state_site
) {
    return connectDimensions(
         "left overlap boundary state"
        ,overlap_boundary.stateDimension(as_unsigned_integer)
        ,"state site left"
        ,state_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110430163445.2584: *4* OverlapSite<Middle> | OverlapBoundary<Right>
//! Connects the right dimension of a middle overlap site to the overlap dimension of a right overlap boundary.
inline unsigned int operator|(
      Nutcracker::OverlapSite<Middle> const& overlap_site
    , Nutcracker::OverlapBoundary<Right> const& overlap_boundary
) {
    return connectDimensions(
         "middle overlap site right"
        ,overlap_site.rightDimension(as_unsigned_integer)
        ,"right overlap boundary overlap"
        ,overlap_boundary.overlapDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110430163445.2585: *4* OverlapSite<Right> | OverlapBoundary<Right>
//! Connects the right dimension of a right overlap site to the overlap dimension of a right overlap boundary.
inline unsigned int operator|(
      Nutcracker::OverlapSite<Right> const& overlap_site
    , Nutcracker::OverlapBoundary<Right> const& overlap_boundary
) {
    return connectDimensions(
         "right overlap site right"
        ,overlap_site.rightDimension(as_unsigned_integer)
        ,"right overlap boundary overlap"
        ,overlap_boundary.overlapDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110430163445.2586: *4* OverlapSiteAny | StateSiteAny
//! Connects the physical dimension of an overlap site to the physical dimension of a state site.
inline unsigned int operator|(
      Nutcracker::OverlapSiteAny const& overlap_site
    , Nutcracker::StateSiteAny const& state_site
) {
    return connectDimensions(
         "overlap site physical"
        ,overlap_site.physicalDimension(as_unsigned_integer)
        ,"state site physical"
        ,state_site.physicalDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110430163445.2587: *4* StateSite<Left> | StateSite<Middle>
//! Connects the right dimension of a left state site to the left dimension of a middle state site.
inline unsigned int operator|(
      Nutcracker::StateSite<Left> const& state_site_1
    , Nutcracker::StateSite<Middle> const& state_site_2
) {
    return connectDimensions(
         "left state site right"
        ,state_site_1.rightDimension(as_unsigned_integer)
        ,"middle state site left"
        ,state_site_2.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110430163445.2588: *4* StateSite<Middle> | ExpectationBoundary<Right>
//! Connects the right dimension of a middle state site to the operator dimension of a right expectation boundary.
inline unsigned int operator|(
      Nutcracker::StateSite<Middle> const& state_site
    , Nutcracker::ExpectationBoundary<Right> const& expectation_boundary
) {
    return connectDimensions(
         "middle state site right"
        ,state_site.rightDimension(as_unsigned_integer)
        ,"right expectation boundary state"
        ,expectation_boundary.stateDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110430163445.2589: *4* StateSite<Middle> | OverlapBoundary<Right>
//! Connects the right dimension of a middle state site to the operator dimension of a right overlap boundary.
inline unsigned int operator|(
      Nutcracker::StateSite<Middle> const& state_site
    , Nutcracker::OverlapBoundary<Right> const& overlap_boundary
) {
    return connectDimensions(
         "middle state site right"
        ,state_site.rightDimension(as_unsigned_integer)
        ,"right overlap boundary state"
        ,overlap_boundary.stateDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110430163445.2590: *4* StateSite<Middle> | StateSite<Right>
//! Connects the right dimension of a middle state site to the left dimension of a right state site.
inline unsigned int operator|(
      Nutcracker::StateSite<Middle> const& state_site_1
    , Nutcracker::StateSite<Right> const& state_site_2
) {
    return connectDimensions(
         "middle state site right"
        ,state_site_1.rightDimension(as_unsigned_integer)
        ,"right state site left"
        ,state_site_2.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110430163445.2591: *4* StateSite<Right> | ExpectationBoundary<Right>
//! Connects the right dimension of a right state site to the operator dimension of a right expectation boundary.
inline unsigned int operator|(
      Nutcracker::StateSite<Right> const& state_site
    , Nutcracker::ExpectationBoundary<Right> const& expectation_boundary
) {
    return connectDimensions(
         "right state site right"
        ,state_site.rightDimension(as_unsigned_integer)
        ,"right overlap boundary state"
        ,expectation_boundary.stateDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110430163445.2592: *4* StateSite<Right> | OverlapBoundary<Right>
//! Connects the right dimension of a right state site to the operator dimension of a right overlap boundary.
inline unsigned int operator|(
      Nutcracker::StateSite<Right> const& state_site
    , Nutcracker::OverlapBoundary<Right> const& overlap_boundary
) {
    return connectDimensions(
         "right state site right"
        ,state_site.rightDimension(as_unsigned_integer)
        ,"right overlap boundary state"
        ,overlap_boundary.stateDimension(as_unsigned_integer)
    );
}
//@-others

//! @}
//@-<< Connectors >>

//! @}
//@-others

}

#endif
//@-leo
