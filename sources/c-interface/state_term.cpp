#include "terms.hpp"

#include "nutcracker/compiler.hpp"


typedef NutcrackerSumTerm<NutcrackerStateTerm> NutcrackerStateSumTerm;
template<typename Term> struct NutcrackerStateTermWrapper : public NutcrackerTermWrapper<NutcrackerStateTerm,Term> {
    typedef NutcrackerTermWrapper<NutcrackerStateTerm,Term>  Base;
    template<typename A> NutcrackerStateTermWrapper(A const& a) : Base(a) {}
    template<typename A, typename B> NutcrackerStateTermWrapper(A const& a, B const& b) : Base(a,b) {}
    template<typename A, typename B, typename C> NutcrackerStateTermWrapper(A const& a, B const& b, C const& c) : Base(a,b,c) {}
    template<typename A, typename B, typename C, typename D> NutcrackerStateTermWrapper(A const& a, B const& b, C const& c, D const& d) : Base(a,b,c,d) {}
};

extern "C" {

NutcrackerStateTerm* Nutcracker_StateTerm_add(NutcrackerStateTerm const* x, NutcrackerStateTerm const* y) {
    return new NutcrackerStateSumTerm(x,y);
}
NutcrackerStateTerm* Nutcracker_StateTerm_create_ProductWithOneSiteDifferent(uint32_t site_number, NutcrackerVector const* common_observation, NutcrackerVector const* special_observation) {
    return new NutcrackerStateTermWrapper<Nutcracker::ProductWithOneSiteDifferentTerm>(site_number,*common_observation,*special_observation);
}
NutcrackerStateTerm* Nutcracker_StateTerm_create_W(NutcrackerVector const* common_observation, NutcrackerVector const* special_observation, bool normalized) {
    return new NutcrackerStateTermWrapper<Nutcracker::WTerm>(*common_observation,*special_observation,normalized);
}
void Nutcracker_StateTerm_free(NutcrackerStateTerm* op) {
    delete op;
}
NutcrackerStateTerm* Nutcracker_StateTerm_multiply(std::complex<double> const* c, NutcrackerStateTerm const* x) {
    return x->copyAndMultiplyBy(*c);
}

}
