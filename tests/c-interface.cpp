//@+leo-ver=5-thin
//@+node:gcross.20110905151655.2845: * @file c-interface.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110905151655.2846: ** << License >>
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
//@+node:gcross.20110905151655.2847: ** << Includes >>
#include <boost/foreach.hpp>
#include <boost/range/algorithm/for_each.hpp>
#include <boost/range/irange.hpp>
#include <boost/scope_exit.hpp>
#include <complex>
#include <illuminate.hpp>
#include <vector>

#include "nutcracker.h"

using boost::irange;

using std::abs;
using std::complex;
using std::vector;
//@-<< Includes >>

//@+others
//@+node:gcross.20110905174854.2837: ** Macros
//@+node:gcross.20110905174854.2838: *3* REPEAT
#define REPEAT(n) for(unsigned int _counter##__LINE__ = 0; _counter##__LINE__ < n; ++_counter##__LINE__)
//@+node:gcross.20110905174854.2839: ** Functions
//@+node:gcross.20110905174854.2841: *3* c
//! Convenience function that constructs the complex number x + iy.
inline complex<double> c(double x, double y) { return complex<double>(x,y); }
//@+node:gcross.20110905151655.2848: ** Tests
TEST_SUITE(C_Interface) {
//@+others
//@+node:gcross.20110905174854.2846: *3* OperatorBuilder
TEST_SUITE(OperatorBuilder) {
//@+others
//@+node:gcross.20110905174854.2847: *4* product
TEST_CASE(product) {
    Nutcracker_clearError();
    BOOST_FOREACH(unsigned int const number_of_sites, irange(2u,6u)) {
        NutcrackerOperator* op = NULL;
        {
            NutcrackerOperatorBuilder* builder = Nutcracker_OperatorBuilder_newSimple(number_of_sites,2u);
            BOOST_SCOPE_EXIT((builder)) { Nutcracker_OperatorBuilder_free(builder); } BOOST_SCOPE_EXIT_END
            BOOST_FOREACH(unsigned int const site_number,irange(0u,number_of_sites)) {
                complex<double> const data[2] = {c(0,0),c(site_number,0)};
                NutcrackerMatrix* non_trivial_matrix = Nutcracker_Matrix_newDiagonal(2,data);
                BOOST_SCOPE_EXIT((non_trivial_matrix)) { Nutcracker_Matrix_free(non_trivial_matrix); } BOOST_SCOPE_EXIT_END
                vector<NutcrackerMatrix const*> components;
                REPEAT(site_number) { components.push_back(Nutcracker_Matrix_Pauli_I); }
                components.push_back(non_trivial_matrix);
                REPEAT(number_of_sites-site_number-1) { components.push_back(Nutcracker_Matrix_Pauli_I); }
                Nutcracker_OperatorBuilder_addProductTerm(builder,&components.front());
            }
            op = Nutcracker_OperatorBuilder_compile(builder);
            if(op == NULL) {
                ASSERT_TRUE(Nutcracker_getError() != NULL);
                FATALLY_FAIL(Nutcracker_getError());
            }
        }
        BOOST_SCOPE_EXIT((op)) { Nutcracker_Operator_free(op); } BOOST_SCOPE_EXIT_END
        BOOST_FOREACH(unsigned int const site_number,irange(0u,number_of_sites)) {
            NutcrackerStateBuilder* builder = Nutcracker_StateBuilder_newSimple(number_of_sites,2u);
            BOOST_SCOPE_EXIT((builder)) { Nutcracker_StateBuilder_free(builder); } BOOST_SCOPE_EXIT_END
            vector<NutcrackerVector const*> components;
            REPEAT(site_number) { components.push_back(Nutcracker_Vector_Qubit_Up); }
            components.push_back(Nutcracker_Vector_Qubit_Down);
            REPEAT(number_of_sites-site_number-1) { components.push_back(Nutcracker_Vector_Qubit_Up); }
            Nutcracker_StateBuilder_addProductTerm(builder,&components.front());
            NutcrackerState* state = Nutcracker_StateBuilder_compile(builder);
            ASSERT_TRUE(state != NULL);
            BOOST_SCOPE_EXIT((state)) { Nutcracker_State_free(state); } BOOST_SCOPE_EXIT_END
            std::complex<double> result;
            Nutcracker_State_computeExpectation(state,op,&result);
            ASSERT_NEAR_ABS(result,c(site_number,0),1e-12);
        }
    }
}
//@-others
}
//@+node:gcross.20110905174854.2830: *3* StateBuilder
TEST_SUITE(StateBuilder) {
//@+others
//@+node:gcross.20110905174854.2831: *4* orthogonal basis
TEST_CASE(orthogonal_basis) {
    BOOST_FOREACH(unsigned int const number_of_sites, irange(2u,6u)) {
        vector<NutcrackerState*> states;
        BOOST_SCOPE_EXIT((&states)) { boost::for_each(states,&Nutcracker_State_free); } BOOST_SCOPE_EXIT_END
        BOOST_FOREACH(unsigned int const site_number,irange(0u,number_of_sites)) {
            NutcrackerStateBuilder* builder = Nutcracker_StateBuilder_newSimple(number_of_sites,2u);
            BOOST_SCOPE_EXIT((builder)) { Nutcracker_StateBuilder_free(builder); } BOOST_SCOPE_EXIT_END
            vector<NutcrackerVector const*> components;
            REPEAT(site_number) { components.push_back(Nutcracker_Vector_Qubit_Up); }
            components.push_back(Nutcracker_Vector_Qubit_Down);
            REPEAT(number_of_sites-site_number-1) { components.push_back(Nutcracker_Vector_Qubit_Up); }
            Nutcracker_StateBuilder_addProductTerm(builder,&components.front());
            NutcrackerState* state = Nutcracker_StateBuilder_compile(builder);
            ASSERT_TRUE(state != NULL);
            states.push_back(state);
        }
        BOOST_FOREACH(unsigned int const i,irange(0u,number_of_sites)) {
            BOOST_FOREACH(unsigned int const j,irange(0u,number_of_sites)) {
                complex<double> result;
                Nutcracker_State_computeOverlap(states[i],states[j],&result);
                if(i == j) {
                    ASSERT_NEAR_ABS_VAL(result,c(1,0),1e-12);
                } else {
                    ASSERT_NEAR_ABS_VAL(result,c(0,0),1e-12);
                }
            }
        }
    }
}
//@-others
}
//@-others
}
//@-others
//@-leo
