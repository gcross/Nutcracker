#include <illuminate.hpp>
#include <fstream>

#include "nutcracker/io.hpp"
#include "nutcracker/protobuf.hpp"

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

TEST_SUITE(Protobuf) {

TEST_SUITE(Format) {


struct Output_postResult {
    Chain const& chain; bool const output_states; vector<double>& levels; vector<State>& states;
    Output_postResult(Chain const& chain, bool const output_states, vector<double>& levels, vector<State>& states)
        : chain(chain), output_states(output_states), levels(levels), states(states) {}
    void operator()() const {
        levels.push_back(chain.getEnergy());
        if(output_states) states.push_back(chain.makeCopyOfState());
    }
};

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

            Output_postResult postResult(chain,output_states,levels,states);

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

}
TEST_SUITE(Operator) {

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

}
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
TEST_SUITE(StateSite) {

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

}

}
