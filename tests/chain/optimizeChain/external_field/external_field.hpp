//@+leo-ver=5-thin
//@+node:gcross.20110307093706.3245: * @thin external_field.hpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.3246: ** << License >>
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
//@+node:gcross.20110307093706.3247: ** << Includes >>
#include <boost/assign.hpp>
#include <boost/container/vector.hpp>
#include <boost/foreach.hpp>
#include <boost/format.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/local/function.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <boost/range/algorithm/max_element.hpp>
#include <boost/range/irange.hpp>
#include <boost/range/numeric.hpp>
#include <boost/smart_ptr/shared_ptr.hpp>
#include <sstream>
#include <functional>

#include "chain.hpp"
#include "operators.hpp"
#include "utilities.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::accumulate;
using boost::container::vector;
using boost::equal;
using boost::format;
using boost::max_element;
using boost::shared_ptr;

using std::abs;
using std::conj;
using std::ostringstream;
using std::pair;
//@-<< Includes >>
static vector<pair<unsigned int,vector<unsigned int> > > system_parameters =
    list_of<pair<unsigned int,vector<unsigned int> > >
        (1,list_of(1))
        (2,list_of(1)(2))
        (3,list_of(1)(2))
        (4,list_of(1)(2)(4))
        (5,list_of(1)(2)(4))
        (10,list_of(1)(2)(4)(8)(17))
;

inline void runTests(
      unsigned int const physical_dimension
) {
    Matrix matrix;
    {
        vector<complex<double> > diagonal(physical_dimension,1); diagonal[0] = -1;
        matrix = diagonalMatrix(diagonal);
    }
    typedef pair<unsigned int,vector<unsigned int> > Parameters;
    BOOST_FOREACH(
         Parameters const& parameters
        ,system_parameters
    ) {
        unsigned int const number_of_sites = parameters.first;
        vector<unsigned int> const& initial_bandwidth_dimensions = parameters.second;
        BOOST_FOREACH(
             unsigned int const initial_bandwidth_dimension
            ,initial_bandwidth_dimensions
        ) {
            Chain chain(
                 constructExternalFieldOperator(
                      number_of_sites
                    , matrix
                 )
                ,initial_bandwidth_dimension
            );
            chain.signalOptimizeSiteFailure.connect(rethrow<OptimizerFailure>);
            unsigned int number_of_sweeps = 0;
            chain.signalSweepPerformed.connect(++lambda::var(number_of_sweeps));
            chain.optimizeChain();
            ASSERT_TRUE(number_of_sweeps < 5);
            ASSERT_NEAR_RELATIVE((double)number_of_sites,-chain.getEnergy(),1e-7);
        }
    }
}
//@-leo
