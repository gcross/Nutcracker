#include <boost/scoped_ptr.hpp>
#include <google/protobuf/message_lite.h>

#include "nutcracker.h"

#include "nutcracker/compiler.hpp"
#include "nutcracker/operators.hpp"
#include "nutcracker/protobuf.hpp"


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

struct NutcrackerMatrix : public Nutcracker::MatrixConstPtr {
    NutcrackerMatrix(Nutcracker::MatrixConstPtr const& matrix)
      : Nutcracker::MatrixConstPtr(matrix)
    {}
};
struct NutcrackerOperator : public Nutcracker::Operator {
    NutcrackerOperator(BOOST_RV_REF(Nutcracker::Operator) op)
      : Nutcracker::Operator(op)
    {}
};
struct NutcrackerOperatorBuilder: public Nutcracker::OperatorBuilder {
    NutcrackerOperatorBuilder(uint32_t number_of_sites, uint32_t physical_dimension)
      : Nutcracker::OperatorBuilder(number_of_sites,Nutcracker::PhysicalDimension(physical_dimension))
    {}
    
    template<typename Dimensions> NutcrackerOperatorBuilder(Dimensions const& dimensions)
      : Nutcracker::OperatorBuilder(dimensions)
    {}
};
struct NutcrackerOperatorTerm : public NutcrackerTerm<Nutcracker::OperatorBuilder,NutcrackerOperatorTerm> {};
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
struct NutcrackerState : public Nutcracker::State {
    NutcrackerState(BOOST_RV_REF(Nutcracker::State) state)
      : Nutcracker::State(state)
    {}
};
struct NutcrackerStateBuilder: public Nutcracker::StateBuilder {
    NutcrackerStateBuilder(uint32_t number_of_sites, uint32_t physical_dimension)
      : Nutcracker::StateBuilder(number_of_sites,Nutcracker::PhysicalDimension(physical_dimension))
    {}
    
    template<typename Dimensions> NutcrackerStateBuilder(Dimensions const& dimensions)
      : Nutcracker::StateBuilder(dimensions)
    {}
};
struct NutcrackerStateTerm : public NutcrackerTerm<Nutcracker::StateBuilder,NutcrackerStateTerm> {};
struct NutcrackerVector : public Nutcracker::VectorConstPtr {
    NutcrackerVector(Nutcracker::VectorConstPtr const& vector)
      : Nutcracker::VectorConstPtr(vector)
    {}
};
#define BEGIN_ERROR_REGION try {
#define END_ERROR_REGION(result) \
    } catch(std::exception const& e) { \
        Nutcracker_setError(e.what()); \
        return result; \
    }
