//@+leo-ver=5-thin
//@+node:gcross.20110307093706.3497: * @thin consistent.cpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.3498: ** << License >>
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
//@+node:gcross.20110307093706.3499: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/range/algorithm/equal.hpp>

#include "boundaries.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::assign::list_of;
using boost::equal;
//@-<< Includes >>
int main() {
    RNG random;

    REPEAT(10) {

        unsigned int const
             left_operator_dimension = random
            ,left_state_dimension = random
            ,physical_dimension = random
            ,right_operator_dimension = random
            ,right_state_dimension = random
            ,number_of_matrices = random
            ;
        ExpectationBoundary<Left> const left_boundary
            (OperatorDimension(left_operator_dimension)
            ,StateDimension(left_state_dimension)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        StateSite<Middle> const state_site
            (PhysicalDimension(physical_dimension)
            ,LeftDimension(left_state_dimension)
            ,RightDimension(right_state_dimension)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        StateSite<Left> const left_state_site(copyFrom(state_site));
        StateSite<Right> const right_state_site(copyFrom(state_site));
        OperatorSite const operator_site
            (number_of_matrices
            ,PhysicalDimension(physical_dimension)
            ,LeftDimension(left_operator_dimension)
            ,RightDimension(right_operator_dimension)
            ,fillWithGenerator(random.generateRandomIndices(
                 LeftDimension(left_operator_dimension)
                ,RightDimension(right_operator_dimension)
             ))
            ,fillWithGenerator(random.randomComplexDouble)
            );
        ExpectationBoundary<Right> const right_boundary
            (OperatorDimension(right_operator_dimension)
            ,StateDimension(right_state_dimension)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        ExpectationBoundary<Left> const new_left_boundary(
            contract<Left>::SOS(
                 left_boundary
                ,left_state_site
                ,operator_site
            )
        );
        ExpectationBoundary<Right> const new_right_boundary(
            contract<Right>::SOS(
                 right_boundary
                ,right_state_site
                ,operator_site
            )
        );

        complex<double> const
             result_from_computeExpectationValue =
                computeExpectationValueAtSite(
                     left_boundary
                    ,state_site
                    ,operator_site
                    ,right_boundary
                )
            ,result_from_contractSOSLeft =
                contractExpectationBoundaries(
                     new_left_boundary
                    ,right_boundary
                )
            ,result_from_contractSOSRight =
                contractExpectationBoundaries(
                     left_boundary
                    ,new_right_boundary
                )
            ;
        ASSERT_NEAR_RELATIVE(result_from_computeExpectationValue,result_from_contractSOSLeft,1e-10);
        ASSERT_NEAR_RELATIVE(result_from_computeExpectationValue,result_from_contractSOSRight,1e-10);
    }

    return 0;
}
//@-leo
