//@+leo-ver=5-thin
//@+node:gcross.20110905151655.2845: * @file c-interface.cpp
//@@language cplusplus
//@+<< Includes >>
//@+node:gcross.20110905151655.2847: ** << Includes >>
#include <boost/container/vector.hpp>
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
//@+node:gcross.20110906155043.4873: *3* simpleComponents
template<typename T> boost::container::vector<T const*> simpleComponents(
      unsigned int number_of_sites
    , unsigned int site_number
    , T const* common
    , T const* special
) {
    boost::container::vector<T const*> components(number_of_sites,common);
    components[site_number] = special;
    return boost::move(components);
}
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
                Nutcracker_OperatorBuilder_addProductTerm(builder,&
                    simpleComponents(number_of_sites,site_number,Nutcracker_Matrix_Pauli_I,non_trivial_matrix)
                .front());
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
            Nutcracker_StateBuilder_addProductTerm(builder,&
                simpleComponents(number_of_sites,site_number,Nutcracker_Vector_Qubit_Up,Nutcracker_Vector_Qubit_Down)
            .front());
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
            Nutcracker_StateBuilder_addProductTerm(builder,&
                simpleComponents(number_of_sites,site_number,Nutcracker_Vector_Qubit_Up,Nutcracker_Vector_Qubit_Down)
            .front());
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
//@+node:gcross.20110910181738.4782: *3* Version
TEST_CASE(Version) {
    unsigned int version_size = Nutcracker_Version_getSize();
    vector<uint32_t> version(version_size);
    Nutcracker_Version_write(&version.front());
    BOOST_FOREACH(unsigned int const i, irange(0u,version_size)) {
        EXPECT_EQ(version[i],Nutcracker_Version_getComponent(i));
    }
}
//@-others
}
//@-others
//@-leo
