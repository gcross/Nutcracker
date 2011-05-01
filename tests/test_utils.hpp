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

#include "operators.hpp"
#include "states.hpp"

using namespace Nutcracker;

using boost::function;
using boost::taus88;
using boost::none;
using boost::normal_distribution;
using boost::optional;
using boost::uniform_smallint;

using std::abs;
//@-<< Includes >>

//@+others
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
//@-others
//@-leo
