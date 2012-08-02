//@+leo-ver=5-thin
//@+node:gcross.20110906155043.4953: * @file terms.hpp
//@@language cplusplus
//@+<< Includes >>
//@+node:gcross.20110906155043.4955: ** << Includes >>
#include "common.hpp"
//@-<< Includes >>

//@+<< Usings >>
//@+node:gcross.20110906155043.4956: ** << Usings >>
//@-<< Usings >>

//@+others
//@+node:gcross.20110906155043.4970: ** Classes
//@+node:gcross.20110906155043.4973: *3* NutcrackerSumTerm
template<typename Superclass> struct NutcrackerSumTerm : public Superclass {
    typedef typename Superclass::BuilderType Builder;

    boost::shared_ptr<Superclass> term1, term2;

    NutcrackerSumTerm(Superclass const* term1, Superclass const* term2)
      : term1(term1->copy())
      , term2(term2->copy())
    {}

    virtual ~NutcrackerSumTerm() {}

    virtual void operator()(Builder& builder) const {
        (*term1)(builder);
        (*term2)(builder);
    }

    virtual void multiplyBy(std::complex<double> const coefficient) {
        term1->multiplyBy(coefficient);
    }

    virtual Superclass* copy() const { return new NutcrackerSumTerm(term1->copy(),term2->copy()); }
};
//@+node:gcross.20110906155043.4971: *3* NutcrackerTermWrapper
template<typename Superclass, typename Term> struct NutcrackerTermWrapper : public Superclass, private Term {
    typedef typename Superclass::BuilderType Builder;

    template<typename A> NutcrackerTermWrapper(A const& a) : Term(a) {}
    template<typename A, typename B> NutcrackerTermWrapper(A const& a, B const& b) : Term(a,b) {}
    template<typename A, typename B, typename C> NutcrackerTermWrapper(A const& a, B const& b, C const& c) : Term(a,b,c) {}
    template<typename A, typename B, typename C, typename D> NutcrackerTermWrapper(A const& a, B const& b, C const& c, D const& d) : Term(a,b,c,d) {}

    virtual ~NutcrackerTermWrapper() {}

    virtual void operator()(Builder& builder) const { return Term::operator()(builder); }
    virtual void multiplyBy(std::complex<double> const coefficient) { Term::multiplyBy(coefficient); }

    virtual Superclass* copy() const {
        return new NutcrackerTermWrapper(*this);
    }
};
//@-others
//@-leo
