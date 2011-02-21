//@+leo-ver=5-thin
//@+node:gcross.20110124161335.2009: * @thin tensors.hpp
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
#include <boost/assign/list_of.hpp>
#include <boost/concept_check.hpp>
#include <boost/container/vector.hpp>
#include <boost/foreach.hpp>
#include <boost/format.hpp>
#include <boost/iterator/iterator_facade.hpp>
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
using boost::iterator_facade;
using boost::RandomAccessRangeConcept;
using boost::random_access_traversal_tag;
using boost::shared_ptr;

using std::copy;
using std::fill_n;
using std::ostream;

typedef boost::numeric::ublas::vector<complex<double> > StateVector;
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
//@+node:gcross.20110124161335.2012: ** Tensors
//@+node:gcross.20110215235924.2007: *3* Base
//@+node:gcross.20110126150230.1601: *4* BaseTensor
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
//@+node:gcross.20110214164734.1934: *4* SiteBaseTensor
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
//@+node:gcross.20110215235924.2004: *3* Boundary
//@+node:gcross.20110215235924.2005: *4* ExpectationBoundary
template<typename side> class ExpectationBoundary : public BaseTensor {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(ExpectationBoundary)
    OperatorDimension operator_dimension;
    StateDimension state_dimension;
public:
    ExpectationBoundary() {}

    ExpectationBoundary(BOOST_RV_REF(ExpectationBoundary) other)
      : BaseTensor(boost::move(static_cast<BaseTensor&>(other)))
      , operator_dimension(boost::move(other.operator_dimension))
      , state_dimension(boost::move(other.state_dimension))
    { }

    ExpectationBoundary(
          OperatorDimension const operator_dimension
        , StateDimension const state_dimension
    ) : BaseTensor((*operator_dimension)*(*state_dimension)*(*state_dimension))
      , operator_dimension(operator_dimension)
      , state_dimension(state_dimension)
    { }

    template<typename other_side> ExpectationBoundary(
          CopyFrom<ExpectationBoundary<other_side> const> const other
    ) : BaseTensor(other)
      , operator_dimension(other->operator_dimension)
      , state_dimension(other->state_dimension)
    { }

    template<typename G> ExpectationBoundary(
          OperatorDimension const operator_dimension
        , StateDimension const state_dimension
        , FillWithGenerator<G> const generator
    ) : BaseTensor((*operator_dimension)*(*state_dimension)*(*state_dimension),generator)
      , operator_dimension(operator_dimension)
      , state_dimension(state_dimension)
    { }

    template<typename Range> ExpectationBoundary(
          OperatorDimension const operator_dimension
        , FillWithRange<Range> const init
    ) : BaseTensor(init)
      , operator_dimension(operator_dimension)
      , state_dimension((unsigned int)sqrt(size()/(*operator_dimension)))
    { }

    ExpectationBoundary(
          MakeTrivial const make_trivial
    ) : BaseTensor(make_trivial)
      , operator_dimension(1)
      , state_dimension(1)
    { }

    ExpectationBoundary& operator=(BOOST_RV_REF(ExpectationBoundary) other) {
        if(this == &other) return *this;
        BaseTensor::operator=(boost::move(static_cast<BaseTensor&>(other)));
        operator_dimension = boost::move(other.operator_dimension);
        state_dimension = boost::move(other.state_dimension);
        return *this;
    }

    void swap(ExpectationBoundary& other) {
        if(this == &other) return;
        BaseTensor::swap(other);
        std::swap(operator_dimension,other.operator_dimension);
        std::swap(state_dimension,other.state_dimension);
    }

    OperatorDimension operatorDimension() const { return operator_dimension; }
    unsigned int operatorDimension(AsUnsignedInteger _) const { return *operator_dimension; }

    StateDimension stateDimension() const { return state_dimension; }
    unsigned int stateDimension(AsUnsignedInteger _) const { return *state_dimension; }

    static ExpectationBoundary const trivial;
};

template<typename side> ExpectationBoundary<side> const ExpectationBoundary<side>::trivial(make_trivial);
//@+node:gcross.20110215235924.2006: *4* OverlapBoundary
template<typename side> class OverlapBoundary : public BaseTensor {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(OverlapBoundary)
    OverlapDimension overlap_dimension;
    StateDimension state_dimension;
public:
    OverlapBoundary() {}

    OverlapBoundary(BOOST_RV_REF(OverlapBoundary) other)
      : BaseTensor(boost::move(static_cast<BaseTensor&>(other)))
      , overlap_dimension(boost::move(other.overlap_dimension))
      , state_dimension(boost::move(other.state_dimension))
    { }

    OverlapBoundary(
          OverlapDimension const overlap_dimension
        , StateDimension const state_dimension
    ) : BaseTensor((*overlap_dimension)*(*state_dimension))
      , overlap_dimension(overlap_dimension)
      , state_dimension(state_dimension)
    { }

    template<typename other_side> OverlapBoundary(
          CopyFrom<OverlapBoundary<other_side> const> const other
    ) : BaseTensor(other)
      , overlap_dimension(other->overlap_dimension)
      , state_dimension(other->state_dimension)
    { }

    template<typename G> OverlapBoundary(
          OverlapDimension const overlap_dimension
        , StateDimension const state_dimension
        , FillWithGenerator<G> const generator
    ) : BaseTensor((*overlap_dimension)*(*state_dimension),generator)
      , overlap_dimension(overlap_dimension)
      , state_dimension(state_dimension)
    { }

    template<typename Range> OverlapBoundary(
          OverlapDimension const overlap_dimension
        , FillWithRange<Range> const init
    ) : BaseTensor(init)
      , overlap_dimension(overlap_dimension)
      , state_dimension(size()/(*overlap_dimension))
    { }

    OverlapBoundary(
          MakeTrivial const make_trivial
    ) : BaseTensor(make_trivial)
      , overlap_dimension(1)
      , state_dimension(1)
    { }

    OverlapBoundary& operator=(BOOST_RV_REF(OverlapBoundary) other) {
        if(this == &other) return *this;
        BaseTensor::operator=(boost::move(static_cast<BaseTensor&>(other)));
        overlap_dimension = boost::move(other.overlap_dimension);
        state_dimension = boost::move(other.state_dimension);
        return *this;
    }

    void swap(OverlapBoundary& other) {
        if(this == &other) return;
        BaseTensor::swap(other);
        std::swap(overlap_dimension,other.overlap_dimension);
        std::swap(state_dimension,other.state_dimension);
    }

    OverlapDimension overlapDimension() const { return overlap_dimension; }
    unsigned int overlapDimension(AsUnsignedInteger _) const { return *overlap_dimension; }

    StateDimension stateDimension() const { return state_dimension; }
    unsigned int stateDimension(AsUnsignedInteger _) const { return *state_dimension; }

    static OverlapBoundary const trivial;
};

template<typename side> OverlapBoundary<side> const OverlapBoundary<side>::trivial(make_trivial);
//@+node:gcross.20110215235924.1926: *3* State
//@+node:gcross.20110215235924.1927: *4* StateSiteAny
class StateSiteAny : public SiteBaseTensor {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(StateSiteAny)
protected:
    StateSiteAny() {}

    StateSiteAny(BOOST_RV_REF(StateSiteAny) other)
      : SiteBaseTensor(boost::move(static_cast<SiteBaseTensor&>(other)))
    { }

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

    StateSiteAny(
          CopyFrom<StateSiteAny const> const other_site
    ) : SiteBaseTensor(other_site)
    { }

    StateSiteAny(
          DimensionsOf<StateSiteAny const> const other_site
    ) : SiteBaseTensor(other_site)
    { }

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

    StateSiteAny(MakeTrivial const make_trivial) : SiteBaseTensor(make_trivial) {}

    void operator=(BOOST_RV_REF(StateSiteAny) other) {
        SiteBaseTensor::operator=(boost::move(static_cast<SiteBaseTensor&>(other)));
    }

    void swap(StateSiteAny& other) {
        SiteBaseTensor::swap(other);
    }
public:
    unsigned int observationValueOffset(unsigned int const observation_value) const {
        assert(observation_value < physicalDimension(as_unsigned_integer));
        return observation_value*leftDimension(as_unsigned_integer)*rightDimension(as_unsigned_integer);
    }

    complex<double>* transitionMatrixForObservation(unsigned int observation_value) {
        return begin() + observationValueOffset(observation_value);
    }

    complex<double> const* transitionMatrixForObservation(unsigned int observation_value) const {
        return begin() + observationValueOffset(observation_value);
    }
};
//@+node:gcross.20110215235924.1928: *4* StateSite
template<typename side> class StateSite : public StateSiteAny {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(StateSite)
public:
    StateSite() {}

    StateSite(BOOST_RV_REF(StateSite) other)
      : StateSiteAny(boost::move(static_cast<StateSiteAny&>(other)))
    { }

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

    StateSite(
          CopyFrom<StateSiteAny const> const other_site
    ) : StateSiteAny(other_site)
    { }

    StateSite(
          DimensionsOf<StateSiteAny const> const other_site
    ) : StateSiteAny(other_site)
    { }

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

    StateSite(MakeTrivial const make_trivial) : StateSiteAny(make_trivial) {}

    StateSite& operator=(BOOST_RV_REF(StateSite) other) {
        if(this == &other) return *this;
        StateSiteAny::operator=(boost::move(static_cast<StateSiteAny&>(other)));
        return *this;
    }

    void swap(StateSite& other) {
        if(this == &other) return;
        StateSiteAny::swap(other);
    }

    double norm() const { return BaseTensor::norm(); }

    static StateSite const trivial;
};

template<typename side> StateSite<side> const StateSite<side>::trivial(make_trivial);
//@+node:gcross.20110216193817.1919: *3* Overlap
//@+node:gcross.20110216193817.1921: *4* OverlapSiteAny
class OverlapSiteAny : public SiteBaseTensor {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(OverlapSiteAny)
protected:
    OverlapSiteAny() {}

    OverlapSiteAny(BOOST_RV_REF(OverlapSiteAny) other)
      : SiteBaseTensor(boost::move(static_cast<SiteBaseTensor&>(other)))
    { }

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

    OverlapSiteAny(
          CopyFrom<OverlapSiteAny const> const other_site
    ) : SiteBaseTensor(other_site)
    { }

    OverlapSiteAny(
          DimensionsOf<StateSiteAny const> const other_site
    ) : SiteBaseTensor(other_site)
    { }

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

    OverlapSiteAny(MakeTrivial const make_trivial) : SiteBaseTensor(make_trivial) {}

    void operator=(BOOST_RV_REF(OverlapSiteAny) other) {
        SiteBaseTensor::operator=(boost::move(static_cast<SiteBaseTensor&>(other)));
    }

    void swap(OverlapSiteAny& other) {
        SiteBaseTensor::swap(other);
    }
};
//@+node:gcross.20110215235924.1929: *4* OverlapSite
template<typename side> class OverlapSite : public OverlapSiteAny {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(OverlapSite)
public:
    OverlapSite() {}

    OverlapSite(BOOST_RV_REF(OverlapSite) other)
      : OverlapSiteAny(boost::move(static_cast<OverlapSiteAny&>(other)))
    { }

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

    OverlapSite(
          CopyFrom<OverlapSiteAny const> const other_site
    ) : OverlapSiteAny(other_site)
    { }

    OverlapSite(
          DimensionsOf<StateSiteAny const> const other_site
    ) : OverlapSiteAny(other_site)
    { }

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

    OverlapSite(MakeTrivial const make_trivial) : OverlapSiteAny(make_trivial) {}

    OverlapSite& operator=(BOOST_RV_REF(OverlapSite) other) {
        if(this == &other) return *this;
        OverlapSiteAny::operator=(boost::move(static_cast<OverlapSiteAny&>(other)));
        return *this;
    }

    void swap(OverlapSite& other) {
        if(this == &other) return;
        OverlapSiteAny::swap(other);
    }

    static OverlapSite const trivial;
};

template<typename side> OverlapSite<side> const OverlapSite<side>::trivial(make_trivial);
//@+node:gcross.20110215235924.1933: *3* OperatorSite
class OperatorSite : public SiteBaseTensor {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(OperatorSite)

    unsigned int number_of_matrices;
    uint32_t* index_data;
public:
    OperatorSite()
      : number_of_matrices(0)
      , index_data(NULL)
    { }

    ~OperatorSite() { if(index_data) delete[] index_data; }

    OperatorSite(BOOST_RV_REF(OperatorSite) other)
      : SiteBaseTensor(boost::move(static_cast<SiteBaseTensor&>(other)))
      , number_of_matrices(copyAndReset(other.number_of_matrices))
      , index_data(copyAndReset(other.index_data))
    { }

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

    OperatorSite(
          CopyFrom<OperatorSite const> const other
    ) : SiteBaseTensor(other)
      , number_of_matrices(other->number_of_matrices)
      , index_data(new uint32_t[number_of_matrices*2])
    {
        copy(other->index_data,other->index_data+2*number_of_matrices,index_data);
    }

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
        BOOST_CONCEPT_ASSERT(( RandomAccessRangeConcept<Range1> ));
        copy(*index_init,index_data);
    }

    OperatorSite(MakeTrivial const make_trivial)
      : SiteBaseTensor(make_trivial)
      , number_of_matrices(1)
      , index_data(new uint32_t[2])
    {
        fill_n(index_data,2,1);
    }

    OperatorSite& operator=(BOOST_RV_REF(OperatorSite) other) {
        if(this == &other) return *this;
        SiteBaseTensor::operator=(boost::move(static_cast<SiteBaseTensor&>(other)));
        number_of_matrices = copyAndReset(other.number_of_matrices);
        moveArrayToFrom(index_data,other.index_data); 
        return *this;
    }

    void swap(OperatorSite& other) {
        if(this == &other) return;
        SiteBaseTensor::swap(other);
        std::swap(number_of_matrices,other.number_of_matrices);
        std::swap(index_data,other.index_data);
    }

    unsigned int numberOfMatrices() const { return number_of_matrices; }

    operator uint32_t*() { return index_data; }
    operator uint32_t const*() const { return index_data; }

    static OperatorSite const trivial;
};
//@+node:gcross.20110220182654.2072: *3* ProjectorSite
class ProjectorSite {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(ProjectorSite)

    OverlapSite<Left> left;
    OverlapSite<Middle> middle;
    OverlapSite<Right> right;
public:
    ProjectorSite() {}

    ProjectorSite(BOOST_RV_REF(ProjectorSite) other)
      : left(boost::move(other.left))
      , middle(boost::move(other.middle))
      , right(boost::move(other.right))
    { }

    ProjectorSite(
          BOOST_RV_REF(OverlapSite<Left>) left
        , BOOST_RV_REF(OverlapSite<Middle>) middle
        , BOOST_RV_REF(OverlapSite<Right>) right
    ) : left(left)
      , middle(middle)
      , right(right)
    { }

    void operator=(BOOST_RV_REF(ProjectorSite) other) {
        left = boost::move(other.left);
        middle = boost::move(other.middle);
        right = boost::move(other.right);
    }

    void swap(ProjectorSite& other) {
        left.swap(other.left);
        middle.swap(other.middle);
        right.swap(other.right);
    }

    template<typename side> OverlapSite<side> const& get() const {
        throw BadLabelException("OverlapSite::get",typeid(side));
    }
};
template<> inline OverlapSite<Left> const& ProjectorSite::get<Left>() const { return left; }
template<> inline OverlapSite<Middle> const& ProjectorSite::get<Middle>() const { return middle; }
template<> inline OverlapSite<Right> const& ProjectorSite::get<Right>() const { return right; }
//@+node:gcross.20110217014932.1927: ** Classes
//@+node:gcross.20110220182654.2074: *3* Projector
struct Projector : vector<ProjectorSite> {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(Projector)
    typedef vector<ProjectorSite> Base;
public:
    Projector() {}

    Projector(BOOST_RV_REF(Projector) other)
      : Base(boost::move(static_cast<Base&>(other)))
    {}

    Projector& operator=(BOOST_RV_REF(Projector) other) {
        if(this == &other) return *this;
        Base::operator=(boost::move(static_cast<Base&>(other)));
        return *this;
    }

    void swap(Projector& other) {
        Base::swap(other);
    }
};
//@+node:gcross.20110217014932.1928: *3* State
struct State {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(State)
protected:    
    StateSite<Middle> first_site;
    vector<StateSite<Right> > rest_sites;
public:
    State() {}

    State(BOOST_RV_REF(State) other)
      : first_site(boost::move(other.first_site))
      , rest_sites(boost::move(other.rest_sites))
    {}

    State(
          BOOST_RV_REF(StateSite<Middle>) first_site
        , BOOST_RV_REF(vector<StateSite<Right> >) rest_sites
    ) : first_site(first_site)
      , rest_sites(rest_sites)
    { }

    void operator=(BOOST_RV_REF(State) other) {
        first_site = boost::move(other.first_site);
        rest_sites = boost::move(other.rest_sites);
    }

    void swap(State& other) {
        first_site.swap(other.first_site);
        rest_sites.swap(other.rest_sites);
    }

    unsigned int numberOfSites() const { return 1+rest_sites.size(); }

    StateSite<Middle> const& getFirstSite() const { return first_site; }
    vector<StateSite<Right> > const& getRestSites() const { return rest_sites; }
    StateSite<Right> const& getRestSite(unsigned int i) const { return rest_sites[i]; }
    StateSiteAny const& operator[](unsigned int i) const {
        if(i == 0) return first_site;
        else return rest_sites[i-1];
    }


    class const_iterator :
        public iterator_facade<
             const_iterator
            ,StateSiteAny const
            ,random_access_traversal_tag
        >
    {
        State const* state;
        unsigned int index;
    public:
        const_iterator(
              State const* state
            , unsigned int const index
        ) : state(state)
          , index(index)
        {}

        StateSiteAny const& dereference() const {
            return (*state)[index];
        }

        bool equal(const_iterator const& other) const {
            return (state == other.state) && (index == other.index);
        }

        void increment() { ++index; }
        void decrement() { --index; }

        void advance(size_t n) { index += n; }

        size_t distance_to(const_iterator const& other) const { return other.index - index; }
    };

    const_iterator begin() const { return const_iterator(this,0); }
    const_iterator end() const { return const_iterator(this,numberOfSites()); }
};
//@+node:gcross.20110215235924.1956: ** Connectors
//@+node:gcross.20110215235924.1957: *3* ExpectationBoundary<Left> | OperatorSite
inline unsigned int operator|(
      ExpectationBoundary<Left> const& expectation_boundary
    , OperatorSite const& operator_site
) {
    return connectDimensions(
         "left expectation boundary state"
        ,expectation_boundary.operatorDimension(as_unsigned_integer)
        ,"operator site left"
        ,operator_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110215235924.1958: *3* ExpectationBoundary<Left> | StateSiteAny
inline unsigned int operator|(
      ExpectationBoundary<Left> const& expectation_boundary
    , StateSiteAny const& state_site
) {
    return connectDimensions(
         "left expectation boundary state"
        ,expectation_boundary.stateDimension(as_unsigned_integer)
        ,"state site left"
        ,state_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110215235924.1960: *3* OperatorSite | ExpectationBoundary<Right>
inline unsigned int operator|(
      OperatorSite const& operator_site
    , ExpectationBoundary<Right> const& expectation_boundary
) {
    return connectDimensions(
         "operator site right"
        ,operator_site.rightDimension(as_unsigned_integer)
        ,"right expectation boundary state"
        ,expectation_boundary.operatorDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110215235924.1961: *3* OperatorSite | StateSite<Middle>
inline unsigned int operator|(
      OperatorSite const& operator_site
    , StateSiteAny const& state_site
) {
    return connectDimensions(
         "operator site physical"
        ,operator_site.physicalDimension(as_unsigned_integer)
        ,"middle state site physical"
        ,state_site.physicalDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110215235924.1962: *3* OverlapBoundary<Left> | OverlapSiteAny
inline unsigned int operator|(
      OverlapBoundary<Left> const& overlap_boundary
    , OverlapSiteAny const& overlap_site
) {
    return connectDimensions(
         "left overlap boundary overlap"
        ,overlap_boundary.overlapDimension(as_unsigned_integer)
        ,"overlap site left"
        ,overlap_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110215235924.1964: *3* OverlapBoundary<Left> | StateSiteAny
inline unsigned int operator|(
      OverlapBoundary<Left> const& overlap_boundary
    , StateSiteAny const& state_site
) {
    return connectDimensions(
         "left overlap boundary state"
        ,overlap_boundary.stateDimension(as_unsigned_integer)
        ,"state site left"
        ,state_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110215235924.1967: *3* OverlapSite<Middle> | OverlapBoundary<Right>
inline unsigned int operator|(
      OverlapSite<Middle> const& overlap_site
    , OverlapBoundary<Right> const& overlap_boundary
) {
    return connectDimensions(
         "middle overlap site right"
        ,overlap_site.rightDimension(as_unsigned_integer)
        ,"right overlap boundary overlap"
        ,overlap_boundary.overlapDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110215235924.1968: *3* OverlapSite<Right> | OverlapBoundary<Right>
inline unsigned int operator|(
      OverlapSite<Right> const& overlap_site
    , OverlapBoundary<Right> const& overlap_boundary
) {
    return connectDimensions(
         "right overlap site right"
        ,overlap_site.rightDimension(as_unsigned_integer)
        ,"right overlap boundary overlap"
        ,overlap_boundary.overlapDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110215235924.1966: *3* OverlapSiteAny | StateSiteAny
inline unsigned int operator|(
      OverlapSiteAny const& overlap_site
    , StateSiteAny const& state_site
) {
    return connectDimensions(
         "overlap site physical"
        ,overlap_site.physicalDimension(as_unsigned_integer)
        ,"state site physical"
        ,state_site.physicalDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110215235924.1970: *3* StateSite<Left> | StateSite<Middle>
inline unsigned int operator|(
      StateSite<Left> const& state_site_1
    , StateSite<Middle> const& state_site_2
) {
    return connectDimensions(
         "left state site right"
        ,state_site_1.rightDimension(as_unsigned_integer)
        ,"middle state site left"
        ,state_site_2.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110215235924.1971: *3* StateSite<Middle> | ExpectationBoundary<Left>
inline unsigned int operator|(
      StateSite<Middle> const& state_site
    , ExpectationBoundary<Right> const& expectation_boundary
) {
    return connectDimensions(
         "middle state site right"
        ,state_site.rightDimension(as_unsigned_integer)
        ,"right expectation boundary state"
        ,expectation_boundary.stateDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110215235924.1972: *3* StateSite<Middle> | OverlapBoundary<Right>
inline unsigned int operator|(
      StateSite<Middle> const& state_site
    , OverlapBoundary<Right> const& overlap_boundary
) {
    return connectDimensions(
         "middle state site right"
        ,state_site.rightDimension(as_unsigned_integer)
        ,"right overlap boundary state"
        ,overlap_boundary.stateDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110215235924.1973: *3* StateSite<Middle> | StateSite<Right>
inline unsigned int operator|(
      StateSite<Middle> const& state_site_1
    , StateSite<Right> const& state_site_2
) {
    return connectDimensions(
         "middle state site right"
        ,state_site_1.rightDimension(as_unsigned_integer)
        ,"right state site left"
        ,state_site_2.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110215235924.1974: *3* StateSite<Right> | ExpectationBoundary<Right>
inline unsigned int operator|(
      StateSite<Right> const& state_site
    , ExpectationBoundary<Right> const& expectation_boundary
) {
    return connectDimensions(
         "right state site right"
        ,state_site.rightDimension(as_unsigned_integer)
        ,"right overlap boundary state"
        ,expectation_boundary.stateDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110215235924.1975: *3* StateSite<Right> | OverlapBoundary<Right>
inline unsigned int operator|(
      StateSite<Right> const& state_site
    , OverlapBoundary<Right> const& overlap_boundary
) {
    return connectDimensions(
         "right state site right"
        ,state_site.rightDimension(as_unsigned_integer)
        ,"right overlap boundary state"
        ,overlap_boundary.stateDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110220093853.1976: ** I/O
//@+node:gcross.20110220093853.1977: *3* Operator
void operator >> (const YAML::Node& node, Operator& operator_site);
YAML::Emitter& operator << (YAML::Emitter& emitter, Operator const& operator_site);
//@+node:gcross.20110220093853.1998: *3* OperatorSite
void operator >> (const YAML::Node& node, OperatorSite& operator_site);
YAML::Emitter& operator << (YAML::Emitter& emitter, OperatorSite const& operator_site);
//@-others

}

#endif
//@-leo
