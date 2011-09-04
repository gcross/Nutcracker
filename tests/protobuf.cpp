//@+leo-ver=5-thin
//@+node:gcross.20110901221152.2684: * @file protobuf.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110901221152.2685: ** << License >>
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
//@+node:gcross.20110901221152.2686: ** << Includes >>
#include <boost/local/function.hpp>
#include <illuminate.hpp>
#include <fstream>

#include "io.hpp"
#include "protobuf.hpp"

#include "test_utils.hpp"

using Nutcracker::OutputFormat;

using Nutcracker::Protobuf::OperatorSiteBuffer;
using Nutcracker::Protobuf::OperatorBuffer;
using Nutcracker::Protobuf::SimulationResultsBuffer;
using Nutcracker::Protobuf::SolutionBuffer;
using Nutcracker::Protobuf::StateSiteBuffer;
using Nutcracker::Protobuf::StateBuffer;

using std::auto_ptr;
using std::ifstream;
//@-<< Includes >>

//@+others
//@+node:gcross.20110901221152.2687: ** Tests
TEST_SUITE(Protobuf) {

//@+others
//@+node:gcross.20110903120540.2693: *3* Format
TEST_SUITE(Format) {

//@+others
//@+node:gcross.20110903120540.2694: *4* Output
TEST_CASE(Output) {
    RNG random;

    OutputFormat const& output_format = OutputFormat::lookupName("protobuf");

    REPEAT(20) {
        TemporaryFilepath temporary_filepath(random.randomTemporaryFilepath("-Protobuf-Format-Output.prb"));

        bool output_states = random.randomBoolean();

        vector<double> levels;
        vector<State> states;

        unsigned int const number_of_levels = random(1,3);

        {

            Chain chain(constructTransverseIsingModelOperator(random(2,5),random));

            void BOOST_LOCAL_FUNCTION_PARAMS(
                const bind& chain,
                const bind& output_states,
                bind& levels,
                bind& states
            ) {
                levels.push_back(chain.getEnergy());
                if(output_states) states.push_back(chain.makeCopyOfState());
            } BOOST_LOCAL_FUNCTION_NAME(postResult)

            chain.signalChainOptimized.connect(postResult);

            auto_ptr<Destructable const> outputter =
                output_format(
                    temporary_filepath->native(),
                    none,
                    output_states,
                    false,
                    chain
                )
            ;

            chain.solveForMultipleLevels(number_of_levels);

        }

        SimulationResultsBuffer buffer;
        {
            ifstream in(temporary_filepath->c_str(),ifstream::binary);
            buffer.ParseFromIstream(&in);
        }

        ASSERT_EQ(buffer.solutions_size(),number_of_levels);

        BOOST_FOREACH(unsigned int i, irange(0u,number_of_levels)) {
            SolutionBuffer const& solution = buffer.solutions(i);
            ASSERT_EQ(solution.eigenvalue(),levels[i]);
            if(output_states) {
                ASSERT_TRUE(solution.has_eigenvector());
                State state;
                solution.eigenvector() >> state;
                checkStatesEqual(state,states[i]);
            }
        }
    }
}
//@-others

}
//@+node:gcross.20110902105950.2706: *3* Operator
TEST_SUITE(Operator) {

//@+others
//@+node:gcross.20110902105950.2709: *4* external_field
TEST_CASE(external_field) {
    BOOST_FOREACH(unsigned int const number_of_sites, irange(4u,21u)) {
        Operator operator_1 = constructExternalFieldOperator(number_of_sites,Pauli::Z), operator_2;
        OperatorBuffer buffer;
        buffer << operator_1;
        buffer >> operator_2;
        checkOperatorsEqual(operator_1,operator_2);
        BOOST_FOREACH(unsigned int const index, irange(2u,number_of_sites-1)) {
            ASSERT_EQ(operator_2[index],operator_2[1]);
        }
    }
}
//@+node:gcross.20110902105950.2708: *4* random
TEST_CASE(random) {
    RNG random;

    REPEAT(100) {
        Operator
            operator_1 = random.randomOperator(),
            operator_2 = random.randomOperator();
        OperatorBuffer buffer;
        buffer << operator_1;
        buffer >> operator_2;
        checkOperatorsEqual(operator_1,operator_2);
    }
}
//@-others

}
//@+node:gcross.20110902105950.2698: *3* OperatorSite
TEST_CASE(OperatorSite) {
    RNG random;

    REPEAT(100) {
        OperatorSite
            tensor_1 = random.randomOperatorSite(),
            tensor_2 = random.randomOperatorSite();
        OperatorSiteBuffer buffer;
        buffer << tensor_1;
        buffer >> tensor_2;
        checkOperatorSitesEqual(tensor_1,tensor_2);
    }
}
//@+node:gcross.20110902105950.2690: *3* State
TEST_CASE(State) {
    RNG random;

    REPEAT(100) {
        State
            state_1 = random.randomState(),
            state_2 = random.randomState();
        StateBuffer buffer;
        buffer << state_1;
        buffer >> state_2;
        checkStatesEqual(state_1,state_2);
    }
}
//@+node:gcross.20110901221152.2691: *3* StateSite
TEST_SUITE(StateSite) {

//@+others
//@+node:gcross.20110901221152.2692: *4* data
TEST_CASE(data) {
    RNG random;

    REPEAT(10) {
        PhysicalDimension const physical_dimension(random);
        LeftDimension const left_dimension(random);
        RightDimension const right_dimension(random);

        StateSite<None> state_site_tensor_1
            (physical_dimension
            ,left_dimension
            ,right_dimension
            ,fillWithGenerator(random.randomComplexDouble)
            );

        StateSiteBuffer buffer;
        buffer << state_site_tensor_1;

        StateSite<None> state_site_tensor_2 = StateSite<None>
            (PhysicalDimension(random)
            ,LeftDimension(random)
            ,RightDimension(random)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        buffer >> state_site_tensor_2;

        checkSiteTensorsEqual(state_site_tensor_1,state_site_tensor_2);
    }
}
//@+node:gcross.20110901221152.2693: *4* normalization
TEST_CASE(normalization) {
    StateSiteBuffer
        left_buffer,
        middle_buffer,
        right_buffer,
        none_buffer;

    left_buffer << StateSite<Left>::trivial;
    middle_buffer << StateSite<Middle>::trivial;
    right_buffer << StateSite<Right>::trivial;
    none_buffer << StateSite<None>::trivial;

    StateSite<Left> left_site;
    StateSite<Middle> middle_site;
    StateSite<Right> right_site;
    StateSite<None> none_site;

    left_buffer >> none_site;
    middle_buffer >> none_site;
    right_buffer >> none_site;

    left_buffer >> left_site;
    middle_buffer >> middle_site;
    right_buffer >> right_site;

    try {
        left_buffer >> middle_site;
        FAIL("A middle-normalized state site accepted a left-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        left_buffer >> right_site;
        FAIL("A right-normalized state site accepted a left-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        middle_buffer >> left_site;
        FAIL("A left-normalized state site accepted an middle-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        middle_buffer >> right_site;
        FAIL("A right-normalized state site accepted a middle-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        right_buffer >> left_site;
        FAIL("A left-normalized state site accepted a right-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        right_buffer >> middle_site;
        FAIL("A middle-normalized state site accepted a right-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        none_buffer >> left_site;
        FAIL("A left-normalized state site accepted an unnormalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        none_buffer >> middle_site;
        FAIL("A middle-normalized state site accepted an unnormalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        none_buffer >> right_site;
        FAIL("A right-normalized state site accepted an unnormalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}
}
//@-others

}
//@-others

}
//@-others
//@-leo
