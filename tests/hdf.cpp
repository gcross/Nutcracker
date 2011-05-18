//@+leo-ver=5-thin
//@+node:gcross.20110511190907.2319: * @file hdf.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110511190907.2321: ** << License >>
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
//@+node:gcross.20110511190907.2322: ** << Includes >>
#include <algorithm>
#include <cstdlib>
#include <boost/foreach.hpp>
#include <boost/local/function.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <boost/range/algorithm/generate.hpp>
#include <boost/range/irange.hpp>
#include <hdf5.h>
#include <hdf5_hl.h>
#include <illuminate.hpp>

#include "chain.hpp"
#include "hdf.hpp"
#include "io.hpp"
#include "tensors.hpp"

#include "test_utils.hpp"

using namespace Nutcracker::HDF;

using boost::equal;
using boost::generate;
using boost::irange;

using std::equal;
//@-<< Includes >>

//@+others
//@+node:gcross.20110511190907.2333: ** Classes
//@+node:gcross.20110511190907.2334: *3* MemoryFile
struct MemoryFile : public File {
    MemoryFile() {
        Properties file_access_properties(H5P_FILE_ACCESS);
        assertSuccess(
            "setting file to use CORE (memory) driver",
            H5Pset_fapl_core(file_access_properties.getId(),4096,0)
        );
        id = assertSuccess(
            "opening memory file",
            H5Fcreate("dummy",H5F_ACC_TRUNC,H5P_DEFAULT,file_access_properties.getId())
        );
    }
};
//@+node:gcross.20110511190907.2325: ** Tests
TEST_SUITE(HDF) {

//@+others
//@+node:gcross.20110511190907.3821: *3* Format
TEST_SUITE(Format) {

//@+others
//@+node:gcross.20110511190907.3822: *4* Output
TEST_CASE(Output) {
    RNG random;

    OutputFormat const& output_format = OutputFormat::lookupName("hdf");

    REPEAT(20) {
        TemporaryFilepath temporary_filepath(random.randomTemporaryFilepath("-HDF-Format-Output.h5"));

        optional<string> maybe_location;

        unsigned int number_of_components = random(0,3);
        if(number_of_components > 0) {
            string location;
            switch(random(0,3)) {
                case 3:
                    if(random.randomBoolean()) {
                        location = "A";
                    }
                    location = location + "/";
                case 2:
                    if(random.randomBoolean()) {
                        location = "B";
                    }
                    location = location + "/";
                case 1:
                    if(random.randomBoolean()) {
                        location = "C";
                    }
                    location = location + "/";
                case 0: ;
            }
            maybe_location = location;
        }

        bool output_states = random.randomBoolean();

        Chain chain(constructTransverseIsingModelOperator(random(2,5),random));

        vector<double> levels;
        vector<State> states;

        BOOST_LOCAL_FUNCTION(
            (void)(postResult)(
                (const bind)((&chain)(output_states))
                (bind)((&levels)(&states))
            )
        ) {
            levels.push_back(chain.getEnergy());
            if(output_states) states.push_back(chain.makeCopyOfState());
        } BOOST_LOCAL_FUNCTION_END(postResult)

        chain.signalChainOptimized.connect(postResult);

        unsigned int const number_of_levels = random(1,3);

        {
            auto_ptr<Destructable const> outputter(
                output_format(
                    temporary_filepath->native(),
                    maybe_location,
                    output_states,
                    false,
                    chain
                )
            );

            chain.solveForMultipleLevels(number_of_levels);

        }

        File file(temporary_filepath->native().c_str());

        {
            Group configuration(file / "configuration");
            ASSERT_EQ(chain.options.site_convergence_threshold,static_cast<double>(configuration["site convergence tolerance"]));
            ASSERT_EQ(chain.options.sweep_convergence_threshold,static_cast<double>(configuration["sweep convergence tolerance"]));
            ASSERT_EQ(chain.options.chain_convergence_threshold,static_cast<double>(configuration["chain convergence tolerance"]));
            ASSERT_EQ(chain.options.sanity_check_threshold,static_cast<double>(configuration["sanity check tolerance"]));
        }

        ASSERT_TRUE(equal(levels,Dataset(file / "levels").readTensorData<double>()));

        if(output_states) {
            Group output_states_group(file / "states");
            ASSERT_EQ(number_of_levels,static_cast<unsigned int>(output_states_group["size"]));
            LocationIterator output_states = output_states_group.begin();
            BOOST_FOREACH(unsigned int i, irange(0u,number_of_levels)) {
                State output_state;
                output_states[i] >> output_state;
                checkStatesEqual(states[i],output_state);
            }
        }

    }
}
//@-others

}
//@+node:gcross.20110511190907.3522: *3* State
TEST_SUITE(State) {

//@+others
//@+node:gcross.20110511190907.3524: *4* encode
TEST_CASE(encode) {
    RNG random;

    REPEAT(10) {
        State state(random.randomState());

        MemoryFile file;
        Location location(file / "location");

        location << state;

        EXPECT_EQ(state.numberOfSites(),static_cast<unsigned int>(Group(location)["size"]));

        LocationIterator iter(location);

        BOOST_FOREACH(StateSiteAny const& state_site_tensor_1, state) {
            StateSite<Middle> state_site_tensor_2;
            iter >> state_site_tensor_2;
            checkSiteTensorsEqual(state_site_tensor_1,state_site_tensor_2);
        }
    }
}
//@+node:gcross.20110511190907.3525: *4* encode then decode
TEST_CASE(encode_then_decode) {
    RNG random;

    REPEAT(10) {
        State state_1(random.randomState());

        MemoryFile file;
        Location location(file / "location");

        location << state_1;

        State state_2;

        location >> state_2;

        checkStatesEqual(state_1,state_2);
    }
}
//@-others

}
//@+node:gcross.20110511190907.2326: *3* StateSite
TEST_SUITE(StateSite) {

//@+others
//@+node:gcross.20110512151124.2485: *4* data
TEST_SUITE(data) {

//@+others
//@+node:gcross.20110512151124.2484: *5* decode
TEST_CASE(decode) {
    RNG random;

    REPEAT(10) {
        PhysicalDimension const physical_dimension(random);
        LeftDimension const left_dimension(random);
        RightDimension const right_dimension(random);

        MemoryFile file;
        Location location(file / "location");

        vector<complex<double> > data((*physical_dimension)*(*left_dimension)*(*right_dimension));
        generate(data,random.randomComplexDouble);

        writeTensorData(
            location,
            list_of(*physical_dimension)(*left_dimension)(*right_dimension),
            &data.front()
        );

        StateSite<None> state_site_tensor;
        location >> state_site_tensor;

        EXPECT_EQ(physical_dimension,state_site_tensor.physicalDimension());
        EXPECT_EQ(left_dimension,state_site_tensor.leftDimension());
        EXPECT_EQ(right_dimension,state_site_tensor.rightDimension());

        EXPECT_TRUE(equal(state_site_tensor,data));
    }
}
//@+node:gcross.20110511190907.2328: *5* encode
TEST_CASE(encode) {
    RNG random;

    REPEAT(10) {
        PhysicalDimension const physical_dimension(random);
        LeftDimension const left_dimension(random);
        RightDimension const right_dimension(random);

        StateSite<None> state_site_tensor
            (physical_dimension
            ,left_dimension
            ,right_dimension
            ,fillWithGenerator(random.randomComplexDouble)
            );

        MemoryFile file;
        Location location(file / "location");

        location << state_site_tensor;

        Dataset dataset(location);

        vector<hsize_t> dimensions(dataset.dimensions());

        EXPECT_EQ(3,dimensions.size());
        EXPECT_EQ(*physical_dimension,dimensions[0]);
        EXPECT_EQ(*left_dimension,dimensions[1]);
        EXPECT_EQ(*right_dimension,dimensions[2]);

        vector<complex<double> > data = dataset.readTensorData<complex<double> >();

        EXPECT_TRUE(equal(state_site_tensor,data));
    }
}
//@+node:gcross.20110511190907.2332: *5* encode then decode
TEST_CASE(encode_then_decode) {
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

        MemoryFile file;
        Location location(file / "location");

        location << state_site_tensor_1;

        StateSite<None> state_site_tensor_2;

        location >> state_site_tensor_2;

        checkSiteTensorsEqual(state_site_tensor_1,state_site_tensor_2);
    }
}
//@-others

}
//@+node:gcross.20110512151124.2486: *4* normalization
TEST_SUITE(normalization) {

//@+others
//@+node:gcross.20110512151124.2487: *5* decode
TEST_CASE(decode) {
    MemoryFile file;

    complex<double> const one(1,1);

    Location left_location(file / "left");
    Location middle_location(file / "middle");
    Location right_location(file / "right");
    Location none_location(file / "none");

    writeTensorData(left_location,list_of(1)(1)(1),&one)["normalization"] = "left";
    writeTensorData(middle_location,list_of(1)(1)(1),&one)["normalization"] = "middle";
    writeTensorData(right_location,list_of(1)(1)(1),&one)["normalization"] = "right";
    writeTensorData(none_location,list_of(1)(1)(1),&one);

    StateSite<Left> left_site;
    StateSite<Middle> middle_site;
    StateSite<Right> right_site;
    StateSite<None> none_site;

    left_location >> none_site;
    middle_location >> none_site;
    right_location >> none_site;

    left_location >> left_site;
    middle_location >> middle_site;
    right_location >> right_site;

    try {
        left_location >> middle_site;
        FAIL("A middle-normalized state site accepted a left-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        left_location >> right_site;
        FAIL("A right-normalized state site accepted a left-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        middle_location >> left_site;
        FAIL("A left-normalized state site accepted an middle-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        middle_location >> right_site;
        FAIL("A right-normalized state site accepted a middle-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        right_location >> left_site;
        FAIL("A left-normalized state site accepted a right-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        right_location >> middle_site;
        FAIL("A middle-normalized state site accepted a right-normalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        none_location >> left_site;
        FAIL("A left-normalized state site accepted an unnormalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        none_location >> middle_site;
        FAIL("A middle-normalized state site accepted an unnormalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}

    try {
        none_location >> right_site;
        FAIL("A right-normalized state site accepted an unnormalized tensor!");
    } catch (WrongTensorNormalizationException const& _) {}
}
//@+node:gcross.20110512151124.2491: *5* encode
TEST_CASE(encode) {
    MemoryFile file;

    Location left_location(file / "left");
    Location middle_location(file / "middle");
    Location right_location(file / "right");
    Location none_location(file / "none");

    StateSite<Left> left_site(make_trivial);
    StateSite<Middle> middle_site(make_trivial);
    StateSite<Right> right_site(make_trivial);
    StateSite<None> none_site(make_trivial);

    left_location << left_site;
    middle_location << middle_site;
    right_location << right_site;
    none_location << none_site;

    ASSERT_EQ(string("left"),static_cast<string>(Dataset(left_location)["normalization"]));
    ASSERT_EQ(string("middle"),static_cast<string>(Dataset(middle_location)["normalization"]));
    ASSERT_EQ(string("right"),static_cast<string>(Dataset(right_location)["normalization"]));

    try {
        string const normalization = Dataset(none_location)["normalization"];
        FAIL((format("Unnormalized site has a non-empty normalization: %1%") % normalization).str());
    } catch (NoSuchAttributeException const& _) {}
}
//@-others

}
//@-others

}
//@+node:gcross.20110512151124.2496: *3* OperatorSite
TEST_SUITE(OperatorSite) {

//@+others
//@+node:gcross.20110512151124.2497: *4* decode
TEST_CASE(decode) {
    RNG random;

    REPEAT(10) {
        unsigned int const number_of_matrices = random;
        PhysicalDimension const physical_dimension(random);
        LeftDimension const left_dimension(random);
        RightDimension const right_dimension(random);

        MemoryFile file;
        Location location(file / "location");

        Group object(createAt(location));
        object["left dimension"] = *left_dimension;
        object["right dimension"] = *right_dimension;

        vector<complex<double> > matrices_data(number_of_matrices*(*physical_dimension)*(*physical_dimension));
        generate(matrices_data,random.randomComplexDouble);

        vector<uint32_t> indices_data(2*number_of_matrices);
        generate(indices_data,random.randomInteger);

        writeTensorData(
            location / "matrices",
            list_of(number_of_matrices)(*physical_dimension)(*physical_dimension),
            &matrices_data.front()
        );

        writeTensorData(
            location / "indices",
            list_of(number_of_matrices)(2),
            &indices_data.front()
        );

        OperatorSite operator_site_tensor;
        location >> operator_site_tensor;

        EXPECT_EQ(number_of_matrices,operator_site_tensor.numberOfMatrices());
        EXPECT_EQ(physical_dimension,operator_site_tensor.physicalDimension());
        EXPECT_EQ(left_dimension,operator_site_tensor.leftDimension());
        EXPECT_EQ(right_dimension,operator_site_tensor.rightDimension());

        EXPECT_TRUE(equal(operator_site_tensor,matrices_data));

        EXPECT_TRUE(equal((uint32_t*)operator_site_tensor,((uint32_t*)operator_site_tensor)+indices_data.size(),indices_data.begin()));
    }
}
//@+node:gcross.20110512151124.2501: *4* encode
TEST_CASE(encode) {
    RNG random;

    REPEAT(10) {
        OperatorSite operator_site_tensor(random.randomOperatorSite());

        MemoryFile file;
        Location location(file / "location");

        location << operator_site_tensor;

        Group object(location);

        EXPECT_EQ(operator_site_tensor.leftDimension(as_unsigned_integer),static_cast<unsigned int>(object["left dimension"]));
        EXPECT_EQ(operator_site_tensor.rightDimension(as_unsigned_integer),static_cast<unsigned int>(object["right dimension"]));

        {
            Dataset dataset(location / "matrices");

            vector<hsize_t> dimensions(dataset.dimensions());

            EXPECT_EQ(3,dimensions.size());
            EXPECT_EQ(operator_site_tensor.numberOfMatrices(),dimensions[0]);
            EXPECT_EQ(operator_site_tensor.physicalDimension(as_unsigned_integer),dimensions[1]);
            EXPECT_EQ(operator_site_tensor.physicalDimension(as_unsigned_integer),dimensions[2]);

            vector<complex<double> > data = dataset.readTensorData<complex<double> >();

            EXPECT_TRUE(equal(operator_site_tensor,data));
        }

        {
            Dataset dataset(location / "indices");

            vector<hsize_t> dimensions(dataset.dimensions());

            EXPECT_EQ(2,dimensions.size());
            EXPECT_EQ(operator_site_tensor.numberOfMatrices(),dimensions[0]);
            EXPECT_EQ(2,dimensions[1]);

            vector<uint32_t> data = dataset.readTensorData<uint32_t>();


            EXPECT_TRUE(equal((uint32_t*)operator_site_tensor,((uint32_t*)operator_site_tensor)+operator_site_tensor.size(),data.begin()));
        }

    }
}
//@+node:gcross.20110512151124.2512: *4* encode then decode
TEST_CASE(encode_then_decode) {
    RNG random;

    REPEAT(10) {
        OperatorSite operator_site_tensor_1(random.randomOperatorSite());

        MemoryFile file;
        Location location(file / "location");

        location << operator_site_tensor_1;

        OperatorSite operator_site_tensor_2(random.randomOperatorSite());

        location >> operator_site_tensor_2;

        checkOperatorSitesEqual(operator_site_tensor_1,operator_site_tensor_2);
    }
}
//@-others

}
//@+node:gcross.20110511190907.3572: *3* Operator
TEST_SUITE(Operator) {

//@+others
//@+node:gcross.20110511190907.3571: *4* encode then decode
TEST_CASE(encode_then_decode) {

    RNG random;

    REPEAT(10) {
        Operator operator_1 = random.randomOperator();

        MemoryFile file;
        Location location(file / "location");

        location << operator_1;

        Operator operator_2;

        location >> operator_2;

        checkOperatorsEqual(operator_1,operator_2);
    }

}
//@+node:gcross.20110511190907.3581: *4* examples
TEST_SUITE(examples) {

//@+others
//@+node:gcross.20110511190907.3582: *5* external field
TEST_CASE(external_field) {

    RNG random;

    REPEAT(10) {
        unsigned int const number_of_middle_sites = random+1;

        Operator operator_1 =
            constructExternalFieldOperator(
                 number_of_middle_sites+2
                ,Pauli::Z
            );

        MemoryFile file;
        Location location(file / "location");

        Group(createAt(location));

        writeRangeData(location / "sequence",list_of(0u).repeat(number_of_middle_sites,1u)(2u));

        Group(createAt(location / "sites")) << rangeOf(list_of(operator_1.front())(operator_1[1])(operator_1.back()) | indirected);

        Operator operator_2;

        location >> operator_2;

        checkOperatorsEqual(operator_1,operator_2);
    }

}
//@-others

}
//@-others

}
//@-others

}
//@-others
//@-leo
