//@+leo-ver=5-thin
//@+node:gcross.20110220141808.2717: * @file nutcracker.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2044: ** << License >>
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
//@+node:gcross.20110220141808.2718: ** << Includes >>
#include <boost/bind.hpp>
#include <boost/local/function.hpp>
#include <boost/format.hpp>
#include <boost/none.hpp>
#include <boost/none_t.hpp>
#include <boost/optional.hpp>
#include <boost/program_options.hpp>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <numeric>

#include "chain.hpp"
#include "configuration.hpp"
#include "io.hpp"

namespace opts = boost::program_options;

using boost::bind;
using boost::format;
using boost::lexical_cast;
using boost::none;
using boost::none_t;
using boost::optional;

using std::auto_ptr;
using std::cerr;
using std::endl;
using std::ifstream;
using std::string;

using Nutcracker::Chain;
using Nutcracker::Destructable;
using Nutcracker::FormatDoesNotSupportLocationsError;
using Nutcracker::FormatDoesNotSupportPipeError;
using Nutcracker::InputFormat;
using Nutcracker::InputOptions;
using Nutcracker::NoFormatTypeSpecifiedError;
using Nutcracker::NoSuchFormatError;
using Nutcracker::Operator;
using Nutcracker::Options;
using Nutcracker::OptimizerMode;
using Nutcracker::OutputFormat;
using Nutcracker::OutputFormatDoesNotSupportStatesError;
using Nutcracker::OutputOptions;
using Nutcracker::ToleranceOptions;
using Nutcracker::vector;
//@-<< Includes >>

//@+others
//@+node:gcross.20110511190907.3754: ** Classes
//@+node:gcross.20110511190907.3766: *3* HelpOptions
class HelpOptions : public Options {
    //@+others
    //@+node:gcross.20110511190907.3767: *4* Constructors
    public:

    HelpOptions(opts::options_description const& all_options)
      : Options("Help options")
      , all_options(all_options)
    {
        options.add_options()
            ("help", opts::bool_switch()->notifier(bind(&HelpOptions::runPrintHelpMessageAndExit,this,_1))->zero_tokens(),
                "print help message and exit\n")
            ("help-formats", opts::bool_switch()->notifier(bind(&HelpOptions::runPrintHelpFormatsMessageAndExit,this,_1))->zero_tokens(),
                "print the supported input and output formats and exit\n")
            ("help-modes", opts::bool_switch()->notifier(bind(&HelpOptions::runPrintHelpModesMessageAndExit,this,_1))->zero_tokens(),
                "print the supported optimizer modes\n")
        ;
    }
    //@+node:gcross.20110511190907.3768: *4* Messages
    protected:

    opts::options_description const& all_options;

    void runPrintHelpMessageAndExit(bool const& run) { if(run) printHelpMessageAndExit(); }
    void runPrintHelpFormatsMessageAndExit(bool const& run) { if(run) printHelpFormatsMessageAndExit(); }
    void runPrintHelpModesMessageAndExit(bool const& run) { if(run) printHelpModesMessageAndExit(); }

    public:

    void printHelpMessageAndExit() {
        cerr
            << all_options
            << endl
            << "Options may also be specified in 'nutcracker.cfg' (or in an alternative configuration file specified via --config-file) in the format option=value.  For example, the line" << endl
            << endl
            << "\tsite-tolerance = 1e-10" << endl
            << endl
            << "is equivalence to specifying --site-tolerance 1e-10 on the command line." << endl
        ;
        exit(1);
    }

    void printHelpFormatsMessageAndExit() {
        cerr << "The recognized input format types are:" << endl;
        vector<string> names(InputFormat::listNames());
        BOOST_FOREACH(string const& name, names) {
            cerr << "  - " << name << ": " << InputFormat::lookupName(name).description << endl;
        }
        cerr << endl;
        cerr << "The recognized output format types are:" << endl;
        names = OutputFormat::listNames();
        BOOST_FOREACH(string const& name, names) {
            cerr << "  - " << name << ": " << OutputFormat::lookupName(name).description << endl;
        }
        cerr << endl;
        exit(1);
    }

    void printHelpModesMessageAndExit() {
        cerr << "The recognized optimizer modes are:" << endl;
        vector<string> names(OptimizerMode::listNames());
        BOOST_FOREACH(string const& name, names) {
            cerr << "  '" << name << "' - " << OptimizerMode::lookupName(name).getDescription() << endl;
        }
        cerr << endl;
        exit(1);
    }
    //@-others
};
//@+node:gcross.20110511190907.3760: *3* SimulationOptions
class SimulationOptions : public Options {
    //@+others
    //@+node:gcross.20110511190907.3762: *4* Constructors
    public:

    SimulationOptions()
      : Options("Simulation options")
    {
        options.add_options()
            ("config-file,C",
                "configuration file\n"
                "------------------\n"
                "If this option is specified then Nutcracker will read option values from it instead of from 'nutcracker.cfg'.\n"
            )
            ("number-of-levels,n", opts::value<unsigned int>(&number_of_levels)->default_value(1),
                "number of levels\n"
                "----------------\n"
                "This value specifies the number of energy levels that Nutcracker should find (including the ground state).\n"
                "\n"
                "If this options is not specified then it defaults to 1 (i.e., ground state only).\n"
            )
            ("optimizer-mode", opts::value<OptimizerMode>(&optimizer_mode)->default_value(OptimizerMode::least_value),
                "optimizer-mode"
                "--------------\n"
                "This value specifies the mode of the optimizer;  this is the option you want to specify if you don't just want to find the solutions with the least eigenvalues.  Run Nutcracker with --help-modes to list the available modes.\n"
                "\n"
                "If this options is not specified then it defaults to '<v' (least value).\n"
            )
        ;
    }
    //@+node:gcross.20110511190907.3761: *4* Fields
    protected:

    unsigned int number_of_levels;
    OptimizerMode optimizer_mode;

    public:

    unsigned int getSimulationNumberOfLevels() const { return number_of_levels; }
    OptimizerMode const& getOptimizerMode() const { return optimizer_mode; }
    //@-others
};
//@+node:gcross.20110511190907.3776: *3* ProgramOptions
class ProgramOptions
  : public HelpOptions
  , public InputOptions
  , public OutputOptions
  , public SimulationOptions
  , public ToleranceOptions
{

    opts::options_description options;

    public:

    ProgramOptions()
        : HelpOptions(options)
        , options("All options")
    {
        options.add(HelpOptions::options);
        options.add(SimulationOptions::options);
        options.add(InputOptions::options);
        options.add(OutputOptions::options);
        options.add(ToleranceOptions::options);
    }

    operator opts::options_description&() { return options; }
};
//@+node:gcross.20110220141808.2719: ** main
int main(int argc, char** argv) {
    ProgramOptions options;

    opts::variables_map vm;
    try {
        opts::store(opts::parse_command_line(argc, argv, options), vm);
        string config_filename;
        if(vm.count("config-file")) {
            config_filename = vm["config-file"].as<string>();
        } else {
            config_filename = "nutcracker.cfg";
        }
        ifstream config_file(config_filename.c_str());
        opts::store(opts::parse_config_file(config_file, options), vm);
        opts::notify(vm);
    } catch(std::exception const& e) {
        cerr << "An error was encounted while parsing the options:" << endl
             << endl
             << e.what() << endl
             << endl
             << "Run Nutcracker with the --help option for a description of the available options." << endl
        ;
        return -1;
    }

    try {

        InputFormat const& input_format = options.resolveInputFormat();
        OutputFormat const& output_format = options.resolveOutputFormat();

        Operator hamiltonian(options.readOperatorUsingInputFormat(input_format));

        Chain chain
            (hamiltonian
            ,1
            ,options.getToleranceChainOptions()
            ,options.getOptimizerMode()
            );

        auto_ptr<Destructable const> outputter = options.connectToChainUsingOutputFormat(output_format,chain);

        chain.solveForMultipleLevels(options.getSimulationNumberOfLevels());

    } catch (NoSuchFormatError const& e) {
        cerr << e.format_name << " is not a recognized/supported " << e.format_type_name << " format type." << endl;
        cerr << endl;
        cerr << "The recognized " << e.format_type_name << " format types are:" << endl;
        vector<string> names(e.getAcceptedFormatNames());
        BOOST_FOREACH(string const& name, names) {
            cerr << "  - " << name << endl;
        }
        cerr << endl;
        cerr << "Run Nutcracker with the --help-formats option for more informaton about supported formats." << endl;
        return -1;
    } catch (NoFormatTypeSpecifiedError const& e) {
        cerr << "No " << e.format_type_name << " format type has been specified." << endl;
        cerr << endl;
        cerr << "The recognized " << e.format_type_name << " format types are:" << endl;
        vector<string> names(e.getAcceptedFormatNames());
        BOOST_FOREACH(string const& name, names) {
            cerr << "  - " << name << endl;
        }
        cerr << endl;
        cerr << "Run Nutcracker with the --help-formats option for more informaton about supported formats." << endl;
        return -1;
    } catch (FormatDoesNotSupportPipeError const& e) {
        cerr << "The " << e.format_type_name << " e.format type " << e.format_name << " does not support piped " << e.format_type_name << "." << endl;
        cerr << "You need to specify the " << e.format_type_name << " in a file instead." << endl;
        return -1;
    } catch (FormatDoesNotSupportLocationsError const& e) {
        cerr << "The " << e.format_type_name << " format type " << e.format_name << " does not support locations." << endl;
        return -1;
    } catch (OutputFormatDoesNotSupportStatesError const& e) {
        cerr << "Output format " << e.format_name << " does not support outputing states." << endl;
        return -1;
    }

    return 0;
}
//@-others
//@-leo
