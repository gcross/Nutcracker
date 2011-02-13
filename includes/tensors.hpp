//@+leo-ver=5-thin
//@+node:gcross.20110124161335.2009: * @thin tensors.hpp
//@@language cplusplus

#ifndef NUTCRACKER_TENSORS_HPP
#define NUTCRACKER_TENSORS_HPP

//@+<< Includes >>
//@+node:gcross.20110124161335.2010: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/concept_check.hpp>
#include <boost/foreach.hpp>
#include <boost/format.hpp>
#include <boost/move/move.hpp>
#include <boost/range/algorithm/copy.hpp>
#include <boost/range/concepts.hpp>
#include <boost/range/irange.hpp>
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
using boost::copy;
using boost::Generator;
using boost::RandomAccessRangeConcept;

using std::copy;
using std::fill_n;
using std::ostream;
//@-<< Usings >>

//@+others
//@+node:gcross.20110124175241.1520: ** type function Other
struct Left;
struct Middle;
struct Right;

template<typename other_side> struct Other { };
template<> struct Other<Left> { typedef Right value; };
template<> struct Other<Right> { typedef Left value; };
//@+node:gcross.20110202172517.1694: ** exception InvalidTensorException
struct InvalidTensorException : public Exception {
    InvalidTensorException() : Exception("Attempt to dereference an invalid tensor") {}
};
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
        template<typename U> operator ParameterName<U>() { return ParameterName<U>(static_cast<U&>(**this)); } \
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
    struct Name; typedef Dimension<Name> Name##Dimension;

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
//@+node:gcross.20110124161335.2012: ** Classes
//@+node:gcross.20110126150230.1601: *3* BaseTensor
class BaseTensor {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(BaseTensor)
    unsigned int data_size;
    complex<double>* data;
protected:
    ~BaseTensor() { if(valid()) delete[] data; }

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

    operator bool() const { return valid(); }
};
//@+node:gcross.20110124175241.1530: *3* Boundary
//@+node:gcross.20110124161335.2013: *4* ExpectationBoundary
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
//@+node:gcross.20110124175241.1526: *4* OverlapBoundary
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
//@+node:gcross.20110124175241.1538: *3* ProjectorMatrix
class ProjectorMatrix {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(ProjectorMatrix)
    unsigned int
          number_of_projectors
        , projector_length
        , number_of_reflectors
        , orthogonal_subspace_dimension
        ;
    complex<double> *reflector_data, *coefficient_data;
    uint32_t* swap_data;

public:
    ProjectorMatrix()
      : number_of_projectors(0)
      , projector_length(0)
      , number_of_reflectors(0)
      , orthogonal_subspace_dimension(0)
      , reflector_data(NULL)
      , coefficient_data(NULL)
      , swap_data(NULL)
    { }

    ~ProjectorMatrix() {
        if(valid()) {
            delete[] reflector_data;
            delete[] coefficient_data;
            delete[] swap_data;
        }
    }

    ProjectorMatrix(BOOST_RV_REF(ProjectorMatrix) other)
      : number_of_projectors(copyAndReset(other.number_of_projectors))
      , projector_length(copyAndReset(other.projector_length))
      , number_of_reflectors(copyAndReset(other.number_of_reflectors))
      , orthogonal_subspace_dimension(copyAndReset(other.orthogonal_subspace_dimension))
      , reflector_data(copyAndReset(other.reflector_data))
      , coefficient_data(copyAndReset(other.coefficient_data))
      , swap_data(copyAndReset(other.swap_data))
    { }

    ProjectorMatrix(
          unsigned int const number_of_projectors
        , unsigned int const projector_length
        , unsigned int const number_of_reflectors
        , unsigned int const orthogonal_subspace_dimension
        , complex<double>* reflector_data
        , complex<double>* coefficient_data
        , uint32_t* swap_data
    ) : number_of_projectors(number_of_projectors)
      , projector_length(projector_length)
      , number_of_reflectors(number_of_reflectors)
      , orthogonal_subspace_dimension(orthogonal_subspace_dimension)
      , reflector_data(reflector_data)
      , coefficient_data(coefficient_data)
      , swap_data(swap_data)
    { }

    ProjectorMatrix& operator=(BOOST_RV_REF(ProjectorMatrix) other) {
        if(this == &other) return *this;
        number_of_projectors = copyAndReset(other.number_of_projectors);
        projector_length = copyAndReset(other.projector_length);
        number_of_reflectors = copyAndReset(other.number_of_reflectors);
        orthogonal_subspace_dimension = copyAndReset(other.orthogonal_subspace_dimension);
        moveArrayToFrom(reflector_data,other.reflector_data);
        moveArrayToFrom(coefficient_data,other.coefficient_data);
        moveArrayToFrom(swap_data,other.swap_data);
        return *this;
    }

    void swap(ProjectorMatrix& other) {
        if(this == &other) return;
        std::swap(number_of_projectors,other.number_of_projectors);
        std::swap(projector_length,other.projector_length);
        std::swap(number_of_reflectors,other.number_of_reflectors);
        std::swap(orthogonal_subspace_dimension,other.orthogonal_subspace_dimension);
        std::swap(reflector_data,other.reflector_data);
        std::swap(coefficient_data,other.coefficient_data);
        std::swap(swap_data,other.swap_data);
    }

    unsigned int numberOfProjectors() const { return number_of_projectors; }
    unsigned int projectorLength() const { return projector_length; }
    unsigned int numberOfReflectors() const { return number_of_reflectors; }
    unsigned int orthogonalSubspaceDimension() const { return orthogonal_subspace_dimension; }

    complex<double>* reflectorData() { if(invalid()) throw InvalidTensorException(); return reflector_data; }
    complex<double> const* reflectorData() const { if(invalid()) throw InvalidTensorException(); return reflector_data; }

    complex<double>* coefficientData() { if(!*this) throw InvalidTensorException(); return coefficient_data; }
    complex<double> const* coefficientData() const { if(invalid()) throw InvalidTensorException(); return coefficient_data; }

    uint32_t* swapData() { if(invalid()) throw InvalidTensorException(); return swap_data; }
    uint32_t const* swapData() const { if(invalid()) throw InvalidTensorException(); return swap_data; }

    bool valid() const { return reflector_data && coefficient_data && swap_data; }
    bool invalid() const { return !valid(); }

    operator bool() const { return valid(); }
};
//@+node:gcross.20110124175241.1531: *3* Site
//@+node:gcross.20110202142407.1690: *4* SiteBaseTensor
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
//@+node:gcross.20110124175241.1533: *4* OperatorSite
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
//@+node:gcross.20110213125549.1800: *4* StateSiteAny
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
};
//@+node:gcross.20110124175241.1535: *4* StateSite
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
//@+node:gcross.20110124175241.1537: *4* OverlapSite
template<typename side> class OverlapSite : public SiteBaseTensor {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(OverlapSite)
public:
    OverlapSite() {}

    OverlapSite(BOOST_RV_REF(OverlapSite) other)
      : SiteBaseTensor(boost::move(static_cast<SiteBaseTensor&>(other)))
    { }

    OverlapSite(
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

    template<typename other_side> OverlapSite(
          CopyFrom<OverlapSite<other_side> const> const other_site
    ) : SiteBaseTensor(other_site)
    { }

    template<typename other_side> OverlapSite(
          DimensionsOf<StateSite<other_side> const> const other_site
    ) : SiteBaseTensor(other_site)
    { }

    template<typename G> OverlapSite(
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

    template<typename Range> OverlapSite(
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

    OverlapSite(MakeTrivial const make_trivial) : SiteBaseTensor(make_trivial) {}

    OverlapSite& operator=(BOOST_RV_REF(OverlapSite) other) {
        if(this == &other) return *this;
        SiteBaseTensor::operator=(boost::move(static_cast<SiteBaseTensor&>(other)));
        return *this;
    }

    void swap(OverlapSite& other) {
        if(this == &other) return;
        SiteBaseTensor::swap(other);
    }

    static OverlapSite const trivial;
};

template<typename side> OverlapSite<side> const OverlapSite<side>::trivial(make_trivial);
//@+node:gcross.20110125120748.2431: ** Connectors
//@+node:gcross.20110125120748.2434: *3* exception DimensionMismatch
struct DimensionMismatch : public Exception {
    DimensionMismatch(
          const char* n1
        , unsigned int const d1
        , const char* n2
        , unsigned int const d2
    ) : Exception((format("%1% dimension (%2%) does not match %3% dimension (%4%)") % n1 % d1 % n2 % d2).str())
    { }
};
//@+node:gcross.20110125120748.2435: *3* function connectDimensions
inline unsigned int connectDimensions(
      const char* n1
    , unsigned int const d1
    , const char* n2
    , unsigned int const d2
) {
    if(d1 != d2) throw DimensionMismatch(n1,d1,n2,d2);
    return d1;
}
//@+node:gcross.20110125120748.2439: *3* operator||
//@+node:gcross.20110125120748.2449: *4* ExpectationBoundary<Left> || OperatorSite
inline unsigned int operator||(
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
//@+node:gcross.20110125120748.2441: *4* ExpectationBoundary<Left> || StateSite<Middle>
inline unsigned int operator||(
      ExpectationBoundary<Left> const& expectation_boundary
    , StateSite<Middle> const& state_site
) {
    return connectDimensions(
         "left expectation boundary state"
        ,expectation_boundary.stateDimension(as_unsigned_integer)
        ,"middle state site left"
        ,state_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110127123226.2871: *4* ExpectationBoundary<Left> || StateSite<Left>
inline unsigned int operator||(
      ExpectationBoundary<Left> const& expectation_boundary
    , StateSite<Left> const& state_site
) {
    return connectDimensions(
         "left expectation boundary state"
        ,expectation_boundary.stateDimension(as_unsigned_integer)
        ,"middle state site left"
        ,state_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110125120748.2451: *4* OperatorSite || ExpectationBoundary<Right>
inline unsigned int operator||(
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
//@+node:gcross.20110125120748.2457: *4* OperatorSite || StateSite<Middle>
template<typename side> inline unsigned int operator||(
      OperatorSite const& operator_site
    , StateSite<side> const& state_site
) {
    return connectDimensions(
         "operator site physical"
        ,operator_site.physicalDimension(as_unsigned_integer)
        ,"middle state site physical"
        ,state_site.physicalDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110127123226.2843: *4* OverlapBoundary<Left> || OverlapSite<Left>
inline unsigned int operator||(
      OverlapBoundary<Left> const& overlap_boundary
    , OverlapSite<Left> const& overlap_site
) {
    return connectDimensions(
         "left overlap boundary overlap"
        ,overlap_boundary.overlapDimension(as_unsigned_integer)
        ,"left overlap site left"
        ,overlap_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110127123226.2841: *4* OverlapBoundary<Left> || StateSite<Left>
inline unsigned int operator||(
      OverlapBoundary<Left> const& overlap_boundary
    , StateSite<Left> const& state_site
) {
    return connectDimensions(
         "left overlap boundary state"
        ,overlap_boundary.stateDimension(as_unsigned_integer)
        ,"left state site left"
        ,state_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110125120748.2445: *4* OverlapBoundary<Left> || StateSite<Middle>
inline unsigned int operator||(
      OverlapBoundary<Left> const& overlap_boundary
    , StateSite<Middle> const& state_site
) {
    return connectDimensions(
         "left overlap boundary state"
        ,overlap_boundary.stateDimension(as_unsigned_integer)
        ,"middle state site left"
        ,state_site.leftDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110125120748.2453: *4* OverlapSite<*> || StateSite<*>
template<typename side> inline unsigned int operator||(
      OverlapSite<side> const& overlap_site
    , StateSite<side> const& state_site
) {
    return connectDimensions(
         "overlap site physical"
        ,overlap_site.physicalDimension(as_unsigned_integer)
        ,"state site physical"
        ,state_site.physicalDimension(as_unsigned_integer)
    );
}
//@+node:gcross.20110127123226.2847: *4* OverlapSite<Right> || OverlapBoundary<Right>
inline unsigned int operator||(
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
//@+node:gcross.20110126102637.2200: *4* ProjectorMatrix || StateSite<Middle>
inline unsigned int operator||(
      ProjectorMatrix const& projector_matrix
    , StateSite<Middle> const& state_site
) {
    return connectDimensions(
         "state site size"
        ,state_site.size()
        ,"projector length"
        ,projector_matrix.projectorLength()
    );
}
//@+node:gcross.20110125120748.2437: *4* StateSite<Left> || StateSite<Middle>
inline unsigned int operator||(
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
//@+node:gcross.20110125120748.2443: *4* StateSite<Middle> || ExpectationBoundary<Left>
inline unsigned int operator||(
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
//@+node:gcross.20110125120748.2447: *4* StateSite<Middle> || OverlapBoundary<Right>
inline unsigned int operator||(
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
//@+node:gcross.20110125120748.2436: *4* StateSite<Middle> || StateSite<Right>
inline unsigned int operator||(
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
//@+node:gcross.20110127123226.2869: *4* StateSite<Right> || ExpectationBoundary<Right>
inline unsigned int operator||(
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
//@+node:gcross.20110127123226.2867: *4* StateSite<Right> || OverlapBoundary<Right>
inline unsigned int operator||(
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
//@+node:gcross.20110127123226.2855: *4* StateSite<Right> || StateSite<Right>
inline unsigned int operator||(
      StateSite<Right> const& state_site_1
    , StateSite<Right> const& state_site_2
) {
    return connectDimensions(
         "right state site right"
        ,state_site_1.rightDimension(as_unsigned_integer)
        ,"right state site left"
        ,state_site_2.leftDimension(as_unsigned_integer)
    );
}
//@-others

}

#endif
//@-leo
