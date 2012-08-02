//@+leo-ver=5-thin
//@+node:gcross.20110904213222.2777: * @file common.hpp
//@@language cplusplus
//@+<< Includes >>
//@+node:gcross.20110904213222.2780: ** << Includes >>
#include <boost/scoped_ptr.hpp>
#include <google/protobuf/message_lite.h>

#include "nutcracker.h"

#include "nutcracker/compiler.hpp"
#include "nutcracker/operators.hpp"
#include "nutcracker/protobuf.hpp"
//@-<< Includes >>

//@+<< Usings >>
//@+node:gcross.20110904213222.2781: ** << Usings >>
//@-<< Usings >>

//@+others
//@+node:gcross.20110904213222.2782: ** Classes
//@+<< Base classes >>
//@+node:gcross.20110906155043.4866: *3* << Base classes >>
//@+others
//@+node:gcross.20110906155043.4867: *4* NutcrackerTerm
template<typename Builder, typename Facade> struct NutcrackerTerm {
    typedef Builder BuilderType;

    virtual ~NutcrackerTerm() {}
    virtual void operator()(Builder& builder) const = 0;
    virtual void multiplyBy(std::complex<double> const coefficient) = 0;
    virtual Facade* copy() const = 0;

    Facade* copyAndMultiplyBy(std::complex<double> const coefficient) const {
        Facade* x = this->copy();
        x->multiplyBy(coefficient);
        return x;
    }
};
//@-others
//@-<< Base classes >>

//@+others
//@+node:gcross.20110904235122.2782: *3* NutcrackerMatrix
struct NutcrackerMatrix : public Nutcracker::MatrixConstPtr {
    NutcrackerMatrix(Nutcracker::MatrixConstPtr const& matrix)
      : Nutcracker::MatrixConstPtr(matrix)
    {}
};
//@+node:gcross.20110904213222.2784: *3* NutcrackerOperator
struct NutcrackerOperator : public Nutcracker::Operator {
    NutcrackerOperator(BOOST_RV_REF(Nutcracker::Operator) op)
      : Nutcracker::Operator(op)
    {}
};
//@+node:gcross.20110904213222.2783: *3* NutcrackerOperatorBuilder
struct NutcrackerOperatorBuilder: public Nutcracker::OperatorBuilder {
    NutcrackerOperatorBuilder(uint32_t number_of_sites, uint32_t physical_dimension)
      : Nutcracker::OperatorBuilder(number_of_sites,Nutcracker::PhysicalDimension(physical_dimension))
    {}
    
    template<typename Dimensions> NutcrackerOperatorBuilder(Dimensions const& dimensions)
      : Nutcracker::OperatorBuilder(dimensions)
    {}
};
//@+node:gcross.20110904213222.2786: *3* NutcrackerOperatorTerm
struct NutcrackerOperatorTerm : public NutcrackerTerm<Nutcracker::OperatorBuilder,NutcrackerOperatorTerm> {};
//@+node:gcross.20110908221100.3013: *3* NutcrackerSerialization
struct NutcrackerSerialization: boost::scoped_ptr<google::protobuf::MessageLite const> {
    typedef boost::scoped_ptr<google::protobuf::MessageLite const> Base;

    NutcrackerSerialization(google::protobuf::MessageLite const* message)
      : Base(message)
    {}

    template<typename Message, typename Object> static NutcrackerSerialization* create(Object const& object) {
        Message* message = new Message();
        *message << object;
        return new NutcrackerSerialization(message);
    }
};
//@+node:gcross.20110904235122.2812: *3* NutcrackerState
struct NutcrackerState : public Nutcracker::State {
    NutcrackerState(BOOST_RV_REF(Nutcracker::State) state)
      : Nutcracker::State(state)
    {}
};
//@+node:gcross.20110904235122.2842: *3* NutcrackerStateBuilder
struct NutcrackerStateBuilder: public Nutcracker::StateBuilder {
    NutcrackerStateBuilder(uint32_t number_of_sites, uint32_t physical_dimension)
      : Nutcracker::StateBuilder(number_of_sites,Nutcracker::PhysicalDimension(physical_dimension))
    {}
    
    template<typename Dimensions> NutcrackerStateBuilder(Dimensions const& dimensions)
      : Nutcracker::StateBuilder(dimensions)
    {}
};
//@+node:gcross.20110904235122.2840: *3* NutcrackerStateTerm
struct NutcrackerStateTerm : public NutcrackerTerm<Nutcracker::StateBuilder,NutcrackerStateTerm> {};
//@+node:gcross.20110904235122.2834: *3* NutcrackerVector
struct NutcrackerVector : public Nutcracker::VectorConstPtr {
    NutcrackerVector(Nutcracker::VectorConstPtr const& vector)
      : Nutcracker::VectorConstPtr(vector)
    {}
};
//@-others
//@+node:gcross.20110904235122.2797: ** Macros
//@+node:gcross.20110904235122.2796: *3* (X)_ERROR_REGION
#define BEGIN_ERROR_REGION try {
#define END_ERROR_REGION(result) \
    } catch(std::exception const& e) { \
        Nutcracker_setError(e.what()); \
        return result; \
    }
//@-others
//@-leo
