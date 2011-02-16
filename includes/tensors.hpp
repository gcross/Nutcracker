//@+leo-ver=5-thin
//@+node:gcross.20110124161335.2009: * @thin tensors.hpp
//@@language cplusplus

#ifndef NUTCRACKER_TENSORS_HPP
#define NUTCRACKER_TENSORS_HPP

//@+<< Includes >>
//@+node:gcross.20110124161335.2010: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/concept_check.hpp>
#include <boost/container/vector.hpp>
#include <boost/foreach.hpp>
#include <boost/format.hpp>
#include <boost/move/move.hpp>
#include <boost/numeric/ublas/vector.hpp>
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
using boost::RandomAccessRangeConcept;
using boost::shared_ptr;

using std::copy;
using std::fill_n;
using std::ostream;

typedef boost::numeric::ublas::vector<complex<double> > StateVector;
//@-<< Usings >>

//@+others
//@+node:gcross.20110215231246.1878: ** Type aliases
class OperatorSite;
typedef vector<shared_ptr<OperatorSite const> > Operator;

template<typename side> class StateSite;
typedef vector<StateSite<None> > State;
//@+node:gcross.20110214164734.1918: ** Exceptions
//@+node:gcross.20110202172517.1694: *3* InvalidTensorException
struct InvalidTensorException : public Exception {
    InvalidTensorException() : Exception("Attempt to dereference an invalid tensor") {}
};
//@+node:gcross.20110215231246.1877: ** Dummy classes
class Left;
class Middle;
class Overlap;
class Physical;
class Right;
//@+node:gcross.20110127123226.2852: ** Parameters wrappers
template<typename T> class Parameter {
private:
    T& data;
protected:
    explicit Parameter(T& data) : data(data) {}
public:
    T& operator*() const { return data; }
    T* operator->() const { return &data; }
};

#define DEFINE_TEMPLATIZED_PARAMETER(ParameterName,parameter_name) \
    template<typename T> struct ParameterName : public Parameter<T> { \
        explicit ParameterName(T& data) : Parameter<T>(data) {} \
        template<typename U> ParameterName(ParameterName<U>& other) : Parameter<T>(*other) {} \
        template<typename U> operator ParameterName<U>() const { return ParameterName<U>(static_cast<U&>(**this)); } \
    }; \
    template<typename T> ParameterName<T> parameter_name(T& data) { return ParameterName<T>(data); } \
    template<typename T> ParameterName<T const> parameter_name(T const& data) { return ParameterName<T const>(data); }

DEFINE_TEMPLATIZED_PARAMETER(CopyFrom,copyFrom)
DEFINE_TEMPLATIZED_PARAMETER(DimensionsOf,dimensionsOf)
DEFINE_TEMPLATIZED_PARAMETER(FillWithGenerator,fillWithGenerator)
DEFINE_TEMPLATIZED_PARAMETER(FillWithRange,fillWithRange)
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
#define DEFINE_DUMMY_PARAMETER(Parameter,parameter) \
    static struct Parameter {} const parameter = Parameter();

DEFINE_DUMMY_PARAMETER(MakeTrivial,make_trivial);
DEFINE_DUMMY_PARAMETER(AsUnsignedInteger,as_unsigned_integer);
//@+node:gcross.20110124175241.1520: ** type function Other
template<typename other_side> struct Other { };
template<> struct Other<Left> { typedef Right value; };
template<> struct Other<Right> { typedef Left value; };
//@+node:gcross.20110124161335.2012: ** Classes
//@+node:gcross.20110126150230.1601: *3* BaseTensor
class BaseTensor {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(BaseTensor)
    unsigned int data_size;
    complex<double>* data;
protected:
    BaseTensor() : data_size(0), data(NULL) {}

    BaseTensor(BOOST_RV_REF(BaseTensor) other)
      : data_size(copyAndReset(other.data_size))
      , data(copyAndReset(other.data))
    { }

    BaseTensor(unsigned int const size)
      : data_size(size)
      , data(new complex<double>[data_size])
    { }

    template<typename Tensor> BaseTensor(CopyFrom<Tensor> const other)
      : data_size(other->data_size)
      , data(new complex<double>[data_size])
    {
        copy(*other,begin());
    }

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

    template<typename Range> BaseTensor(FillWithRange<Range> const init)
      : data_size(init->size())
      , data(new complex<double>[data_size])
    {
        BOOST_CONCEPT_ASSERT(( RandomAccessRangeConcept<Range> ));
        copy(*init,begin());
    }

    BaseTensor(MakeTrivial const make_trivial)
      : data_size(1)
      , data(new complex<double>[1])
    {
        data[0] = c(1,0);
    }

    void operator=(BOOST_RV_REF(BaseTensor) other) {
        data_size = copyAndReset(other.data_size);
        moveArrayToFrom(data,other.data);
    }

    void swap(BaseTensor& other) {
        std::swap(data_size,other.data_size);
        std::swap(data,other.data);
    }

    double norm() const { return dznrm2(data_size,data); }

public:
    ~BaseTensor() { if(valid()) delete[] data; }

    typedef complex<double> value_type;
    typedef value_type* iterator;
    typedef value_type const* const_iterator;
    typedef value_type& reference;
    typedef value_type const& const_reference;

    unsigned int size() const { return data_size; }

    operator complex<double>*() { return begin(); }
    operator complex<double> const*() const { return begin(); }

    complex<double>* begin() { if(invalid()) throw InvalidTensorException(); return data; }
    complex<double> const* begin() const { if(invalid()) throw InvalidTensorException(); return data; }

    complex<double>* end() { return begin()+size(); }
    complex<double> const* end() const { return begin()+size(); }

    complex<double>& operator[](unsigned int const index) { return *(begin()+index); }
    complex<double> operator[](unsigned int const index) const { return *(begin()+index); }

    bool valid() const { return data; }
    bool invalid() const { return !valid(); }
};
//@+node:gcross.20110214164734.1934: *3* SiteBaseTensor
class SiteBaseTensor : public BaseTensor {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(SiteBaseTensor)

    PhysicalDimension physical_dimension;
    LeftDimension left_dimension;
    RightDimension right_dimension;

protected:
    SiteBaseTensor() {}

    SiteBaseTensor(BOOST_RV_REF(SiteBaseTensor) other)
      : BaseTensor(boost::move(static_cast<BaseTensor&>(other)))
      , physical_dimension(boost::move(other.physical_dimension))
      , left_dimension(boost::move(other.left_dimension))
      , right_dimension(boost::move(other.right_dimension))
    { }

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

    template<typename Tensor> SiteBaseTensor(CopyFrom<Tensor const> const other)
      : BaseTensor(other)
      , physical_dimension(other->physicalDimension())
      , left_dimension(other->leftDimension())
      , right_dimension(other->rightDimension())
    { }

    template<typename Tensor> SiteBaseTensor(DimensionsOf<Tensor const> const other)
      : BaseTensor(other->size())
      , physical_dimension(other->physicalDimension())
      , left_dimension(other->leftDimension())
      , right_dimension(other->rightDimension())
    { }

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

    SiteBaseTensor(MakeTrivial const make_trivial)
      : BaseTensor(make_trivial)
      , physical_dimension(1)
      , left_dimension(1)
      , right_dimension(1)
    { }

    void operator=(BOOST_RV_REF(SiteBaseTensor) other) {
        BaseTensor::operator=(boost::move(static_cast<BaseTensor&>(other)));
        physical_dimension = boost::move(other.physical_dimension);
        left_dimension = boost::move(other.left_dimension);
        right_dimension = boost::move(other.right_dimension);
    }

    void swap(SiteBaseTensor& other) {
        BaseTensor::swap(other);
        std::swap(physical_dimension,other.physical_dimension);
        std::swap(left_dimension,other.left_dimension);
        std::swap(right_dimension,other.right_dimension);
    }

public:
    PhysicalDimension physicalDimension() const { return physical_dimension; }
    unsigned int physicalDimension(AsUnsignedInteger _) const { return *physical_dimension; }

    LeftDimension leftDimension() const { return left_dimension; }
    unsigned int leftDimension(AsUnsignedInteger _) const { return *left_dimension; }

    RightDimension rightDimension() const { return right_dimension; }
    unsigned int rightDimension(AsUnsignedInteger _) const { return *right_dimension; }
};
//@-others

}

#endif
//@-leo
