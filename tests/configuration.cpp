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

TEST_SUITE(Configuration) {

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

}
