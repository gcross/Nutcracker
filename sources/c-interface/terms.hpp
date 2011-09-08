//@+leo-ver=5-thin
//@+node:gcross.20110906155043.4953: * @file terms.hpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110906155043.4954: ** << License >>
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
