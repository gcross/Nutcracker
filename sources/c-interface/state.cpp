#include "common.hpp"

#include "nutcracker/operators.hpp"



extern "C" {

void Nutcracker_State_computeExpectation(NutcrackerState const* state, NutcrackerOperator const* op, std::complex<double>* result) { BEGIN_ERROR_REGION {
    *result = Nutcracker::computeExpectationValue(*state,*op);
} END_ERROR_REGION() }
void Nutcracker_State_computeOverlap(NutcrackerState const* state1, NutcrackerState const* state2, std::complex<double>* result) { BEGIN_ERROR_REGION {
    *result = Nutcracker::computeStateOverlap(*state1,*state2);
} END_ERROR_REGION() }
NutcrackerState* Nutcracker_State_deserialize(uint32_t size, void const* data) { BEGIN_ERROR_REGION {
    using namespace Nutcracker;
    using namespace Nutcracker::Protobuf;
    StateBuffer buffer;
    buffer.ParseFromArray(data,size);
    State state;
    buffer >> state;
    return new NutcrackerState(boost::move(state));
} END_ERROR_REGION(NULL) }
void Nutcracker_State_free(NutcrackerState* state) { delete state; }
NutcrackerSerialization* Nutcracker_State_serialize(NutcrackerState const* state) { BEGIN_ERROR_REGION {
    return NutcrackerSerialization::create<Nutcracker::Protobuf::StateBuffer>(*state);
} END_ERROR_REGION(NULL) }

}
