//@+leo-ver=5-thin
//@+node:gcross.20110511190907.3785: * @file configuration.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110511190907.3787: ** << License >>
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
//@+node:gcross.20110511190907.3788: ** << Includes >>
#include <boost/lexical_cast.hpp>
#include <boost/optional.hpp>
#include <boost/program_options.hpp>
#include <illuminate.hpp>

#include "nutcracker/chain.hpp"
#include "nutcracker/configuration.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::lexical_cast;
using boost::optional;
using boost::program_options::basic_command_line_parser;
using boost::program_options::notify;
using boost::program_options::store;
using boost::program_options::variables_map;

using std::string;
//@-<< Includes >>

//@+others
//@+node:gcross.20110511190907.3789: ** Tests
TEST_SUITE(Configuration) {

//@+others
//@+node:gcross.20110511190907.3790: *3* InputOptions
TEST_CASE(InputOptions) {
    RNG random;

    REPEAT(20) {
        std::vector<string> arguments;

        optional<string> maybe_input_filepath;
        optional<string> maybe_input_format;
        optional<string> maybe_input_location;

        unsigned int n = random(1,4);
        REPEAT(n) {
            switch(random(1,3)) {
                case 1:
                    if(!maybe_input_filepath) {
                        maybe_input_filepath = "A";
                        arguments.push_back("--input-file");
                        arguments.push_back("A");
                    }
                    break;
                case 2:
                    if(!maybe_input_format) {
                        maybe_input_format = "B";
                        arguments.push_back("--input-format");
                        arguments.push_back("B");
                    }
                    break;
                case 3:
                    if(!maybe_input_location) {
                        maybe_input_location = "C";
                        arguments.push_back("--input-location");
                        arguments.push_back("C");
                    }
                    break;
            }
        }

        InputOptions options;

        variables_map vm;
        store(basic_command_line_parser<char>(arguments).options(options).run(), vm);
        notify(vm);

        ASSERT_EQ(maybe_input_filepath,options.getInputMaybeFilepath());
        ASSERT_EQ(maybe_input_format,options.getInputMaybeFormat());
        ASSERT_EQ(maybe_input_location,options.getInputMaybeLocation());
    }

}
//@+node:gcross.20110511190907.3796: *3* OutputOptions
TEST_CASE(OutputOptions) {
    RNG random;

    REPEAT(100) {
        std::vector<string> arguments;

        optional<string> maybe_output_filepath;
        optional<string> maybe_output_format;
        optional<string> maybe_output_location;
        bool maybe_output_states = false;
        bool maybe_output_overwrite = false;

        unsigned int n = random(1,8);
        REPEAT(n) {
            switch(random(1,5)) {
                case 1:
                    if(!maybe_output_filepath) {
                        maybe_output_filepath = "A";
                        arguments.push_back("--output-file");
                        arguments.push_back("A");
                    }
                    break;
                case 2:
                    if(!maybe_output_format) {
                        maybe_output_format = "B";
                        arguments.push_back("--output-format");
                        arguments.push_back("B");
                    }
                    break;
                case 3:
                    if(!maybe_output_location) {
                        maybe_output_location = "C";
                        arguments.push_back("--output-location");
                        arguments.push_back("C");
                    }
                    break;
                case 4:
                    if(!maybe_output_states) {
                        maybe_output_states = true;
                        arguments.push_back("--output-states");
                    }
                    break;
                case 5:
                    if(!maybe_output_overwrite) {
                        maybe_output_overwrite = true;
                        arguments.push_back("--output-overwrite");
                    }
                    break;
            }
        }

        OutputOptions options;

        variables_map vm;
        store(basic_command_line_parser<char>(arguments).options(options).run(), vm);
        notify(vm);

        ASSERT_EQ(maybe_output_filepath,options.getOutputMaybeFilepath());
        ASSERT_EQ(maybe_output_format,options.getOutputMaybeFormat());
        ASSERT_EQ(maybe_output_location,options.getOutputMaybeLocation());
        ASSERT_EQ(maybe_output_states,options.getOutputStates());
        ASSERT_EQ(maybe_output_overwrite,options.getOutputOverwrite());
    }

}
//@+node:gcross.20110511190907.3798: *3* ToleranceOptions
TEST_CASE(ToleranceOptions) {
    RNG random;

    REPEAT(20) {
        std::vector<string> arguments;

        double site_convergence_threshold = Chain::defaults.site_convergence_threshold;
        double sweep_convergence_threshold = Chain::defaults.sweep_convergence_threshold;
        double chain_convergence_threshold = Chain::defaults.chain_convergence_threshold;
        double sanity_check_threshold = Chain::defaults.sanity_check_threshold;
        unsigned int maximum_number_of_iterations = Chain::defaults.maximum_number_of_iterations;

        unsigned int n = random(1,7);
        REPEAT(n) {
            switch(random(1,4)) {
                case 1:
                    if(site_convergence_threshold == Chain::defaults.site_convergence_threshold) {
                        site_convergence_threshold = 0.1;
                        arguments.push_back("--site-tolerance");
                        arguments.push_back(lexical_cast<string>(0.1));
                    }
                    break;
                case 2:
                    if(sweep_convergence_threshold == Chain::defaults.sweep_convergence_threshold) {
                        sweep_convergence_threshold = 0.2;
                        arguments.push_back("--sweep-tolerance");
                        arguments.push_back(lexical_cast<string>(0.1));
                    }
                    break;
                case 3:
                    if(chain_convergence_threshold == Chain::defaults.chain_convergence_threshold) {
                        chain_convergence_threshold = 0.3;
                        arguments.push_back("--chain-tolerance");
                        arguments.push_back(lexical_cast<string>(0.1));
                    }
                    break;
                case 4:
                    if(sanity_check_threshold == Chain::defaults.sanity_check_threshold) {
                        sanity_check_threshold = 0.4;
                        arguments.push_back("--sanity-tolerance");
                        arguments.push_back(lexical_cast<string>(0.1));
                    }
                    break;
            }
        }

        ToleranceOptions options;

        variables_map vm;
        store(basic_command_line_parser<char>(arguments).options(options).run(), vm);
        notify(vm);

        ASSERT_EQ(site_convergence_threshold,options.getToleranceSite());
        ASSERT_EQ(sweep_convergence_threshold,options.getToleranceSweep());
        ASSERT_EQ(chain_convergence_threshold,options.getToleranceChain());
        ASSERT_EQ(sanity_check_threshold,options.getToleranceSanity());
        ASSERT_EQ(maximum_number_of_iterations,options.getToleranceIterations());
    }

}
//@-others

}
//@-others
//@-leo
