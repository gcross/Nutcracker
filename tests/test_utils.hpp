//@+leo-ver=5-thin
//@+node:gcross.20110202223558.1713: * @file test_utils.hpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2062: ** << License >>
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
//@+node:gcross.20110202223558.1715: ** << Includes >>
#include <boost/function.hpp>
#include <boost/optional.hpp>
#include <boost/random.hpp>
#include <boost/random/normal_distribution.hpp>
#include <boost/random/uniform_smallint.hpp>
#include <complex>
#include <cstdlib>
#include <iomanip>
#include <iostream>

#include "operators.hpp"
#include "states.hpp"

using namespace Nutcracker;

using boost::function;
using boost::taus88;
using boost::none;
using boost::normal_distribution;
using boost::optional;
using boost::uniform_smallint;
//@-<< Includes >>

//@+others
//@+node:gcross.20110307093706.2458: ** Functions
void assertFalse(char const* name, bool const value);
void assertNear(char const* name, bool const value);
void assertOperatorSitesEqual(OperatorSite const& operator_site_1,OperatorSite const& operator_site_2);
void assertOperatorsEqual(Operator const& operator_1,Operator const& operator_2);
void assertTrue(char const* name, bool const value);

void fail(string const& message);
//@+node:gcross.20110307093706.2460: *3* assertEqual
template<typename T1, typename T2> void assertEqual(char const* name_1, T1 const& value_1, char const* name_2, T2 const& value_2) {
    using namespace std;
    if(value_1 != value_2) {
        cerr << "Not equal:" << endl;
        cerr << name_1 << " = " << setprecision(15) << value_1 << endl;
        cerr << name_2 << " = " << setprecision(15) << value_2 << endl;
        exit(-1);
    }
}

template<typename T1, typename T2> void assertEqual(T1 const& expected_value, char const* actual_value_name, T2 const& actual_value) {
    using namespace std;
    if(expected_value != actual_value) {
        cerr << actual_value_name << " = " << actual_value << " /= " << expected_value << endl;
        exit(-1);
    }
}
//@+node:gcross.20110307093706.3158: *3* assertLessOrEqual
template<typename T1, typename T2> void assertLessOrEqual(char const* name_1, T1 const& value_1, char const* name_2, T2 const& value_2) {
    using namespace std;
    if(value_1 > value_2) {
        cerr << name_1 << " > " << name_2 << endl;
        cerr << name_1 << " = " << setprecision(15) << value_1 << endl;
        cerr << name_2 << " = " << setprecision(15) << value_2 << endl;
        exit(-1);
    }
}
//@+node:gcross.20110307093706.2567: *3* assertNearRelative
template<typename T1, typename T2, typename T3> void assertNearRelative(char const* name_1, T1 const& value_1, char const* name_2, T2 const& value_2, T3 const& relative_error) {
    using namespace std;
    if(((abs(value_1)+abs(value_2))/2 > relative_error) && (abs(value_1-value_2)/((abs(value_1)+abs(value_2))/2) > relative_error)) {
        cerr << "Not equal within relative tolerance " << relative_error << ":" << endl;
        cerr << name_1 << " = " << setprecision(15) << value_1 << endl;
        cerr << name_2 << " = " << setprecision(15) << value_2 << endl;
        cerr << "(abs(value_1)+abs(value_2))/2 = " << ((abs(value_1)+abs(value_2))/2) << endl;
        cerr << "abs(value_1-value_2)/((abs(value_1)+abs(value_2))/2) = " << (abs(value_1-value_2)/((abs(value_1)+abs(value_2))/2)) << endl;
        exit(-1);
    }
}

template<typename T1, typename T2, typename T3> void assertNearRelative(T1 const& value_1, char const* name_2, T2 const& value_2, T3 const& relative_error) {
    using namespace std;
    if(((abs(value_1)+abs(value_2))/2 > relative_error) && (abs(value_1-value_2)/((abs(value_1)+abs(value_2))/2) > relative_error)) {
        cerr << "Not equal within relative tolerance " << relative_error << ":" << endl;
        cerr << name_2 << " = " << setprecision(15) << value_2 <<  " /= " << value_1 << endl;
        cerr << "(abs(value_1)+abs(value_2))/2 = " << ((abs(value_1)+abs(value_2))/2) << endl;
        cerr << "abs(value_1-value_2)/((abs(value_1)+abs(value_2))/2) = " << (abs(value_1-value_2)/((abs(value_1)+abs(value_2))/2)) << endl;
        exit(-1);
    }
}
//@+node:gcross.20110202223558.1714: ** struct RNG
class RNG {
    friend class ComplexDoubleGenerator;
    friend class IndexGenerator;
    friend class IntegerGenerator;
protected:
    taus88 generator;
    normal_distribution<double> normal;
    uniform_smallint<unsigned int> smallint;
public:
    function<double()> const randomDouble;
    function<unsigned int()> const randomInteger;
    function<complex<double>()> const randomComplexDouble;

    RNG();

    unsigned int operator()(unsigned int lo, unsigned int hi);
    OperatorSite randomOperatorSite(
          PhysicalDimension const physical_dimension
        , LeftDimension const left_dimension
        , RightDimension const right_dimension
    );
    Operator randomOperator(
          optional<unsigned int> maybe_number_of_sites=none
        , unsigned int const maximum_physical_dimension=10
        , unsigned int const maximum_bandwidth_dimension=10
    );

    vector<unsigned int> randomUnsignedIntegerVector(unsigned int n, unsigned int lo=1,unsigned int hi=10);

    State randomState();
    State randomState(unsigned int number_of_sites);
    State randomState(vector<unsigned int> const& physical_dimensions);

    function<complex<double>()> generateRandomHermitianMatrices(unsigned int const size);
    function<unsigned int()> generateRandomIntegers(unsigned int lo, unsigned int hi);
    function<uint32_t()> generateRandomIndices(
          LeftDimension const left_index_bound
        , RightDimension const right_index_bound
    );

    operator unsigned int() { return randomInteger(); }
    operator complex<double>() { return randomComplexDouble(); }
};
//@+node:gcross.20110307093706.2365: ** Macros
#define ASSERT_EQUAL(A,B) assertEqual(#A,A,#B,B)
#define ASSERT_LESS_OR_EQUAL(A,B) assertLessOrEqual(#A,A,#B,B)
#define ASSERT_EQUAL_TO(A,B) assertEqual(A,#B,B)
#define ASSERT_FALSE(A) assertFalse(#A,A)
#define ASSERT_NEAR_RELATIVE(A,B,C) assertNearRelative(#A,A,#B,B,C)
#define ASSERT_NEAR_RELATIVE_TO(A,B,C) assertNearRelative(A,#B,B,C)
#define ASSERT_TRUE(A) assertTrue(#A,A)
//@-others
//@-leo
