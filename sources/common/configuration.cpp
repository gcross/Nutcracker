//@+leo-ver=5-thin
//@+node:gcross.20110511190907.3708: * @file configuration.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110511190907.3710: ** << License >>
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
//@+node:gcross.20110511190907.3711: ** << Includes >>
#include <boost/bind.hpp>

#include "nutcracker/configuration.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110511190907.3712: ** << Usings >>
using boost::bind;
using boost::program_options::bool_switch;
using boost::program_options::value;
//@-<< Usings >>

//@+others
//@+node:gcross.20110511190907.3713: ** Classes
//@+node:gcross.20110511190907.3714: *3* InputOptions
//@+node:gcross.20110511190907.3715: *4* Constructors
InputOptions::InputOptions()
  : Options("Input options")
{
    options.add_options()
        ("input-file,f", value<string>()->notifier(bind(&InputOptions::setInputFilepath,this,_1)),
            "location of input file\n"
            "----------------------\n"
            "This value specifies the location of the input file containing the description of the hamiltonian.\n"
            "\n"
            "If this option is not specified then the hamiltonian will be read from standard input.\n"
        )

        ("input-format", value<string>()->notifier(bind(&InputOptions::setInputFormat,this,_1)),
            "format of input file\n"
            "--------------------\n"
            "This value specifies the format of the file containing the description of the hamiltonian.\n"
            "\n"
            "If this option is not specified, then the format will be inferred from the extension of the input file, or assumed to be YAML if the hamiltonian is being read from standard input.\n"
        )

        ("input-location", value<string>()->notifier(bind(&InputOptions::setInputLocation,this,_1)),
            "location within the input file\n"
            "------------------------------\n"
            "This value specifies the location of the hamiltonian description within the input file;  note that this option is only sensible for semi-structured hierarchal file formats which can contain multiple kinds of data at multiple locations.\n"
            "\n"
            "If this option is not specified, then the hamiltonian will be assumed to be located at the root of the file.\n"
)
    ;
}
//@+node:gcross.20110511190907.3723: *4* Fields
void InputOptions::setInputFilepath(string const& input_filepath) { maybe_input_filepath = input_filepath; }
void InputOptions::setInputFormat(string const& input_format) { maybe_input_format = input_format; }
void InputOptions::setInputLocation(string const& input_location) { maybe_input_location = input_location; }

optional<string> const& InputOptions::getInputMaybeFilepath() const { return maybe_input_filepath; }
optional<string> const& InputOptions::getInputMaybeFormat() const { return maybe_input_format; }
optional<string> const& InputOptions::getInputMaybeLocation() const { return maybe_input_location; }
//@+node:gcross.20110511190907.3817: *4* Format
InputFormat const& InputOptions::resolveInputFormat() const {
    return
        resolveAndCheckFormat<InputFormat>(
            getInputMaybeFormat(),
            getInputMaybeFilepath(),
            getInputMaybeLocation()
        );
}

Operator InputOptions::readOperatorUsingInputFormat(InputFormat const& input_format) const {
    return input_format(getInputMaybeFilepath(),getInputMaybeLocation());
}
//@+node:gcross.20110511190907.3733: *3* OutputOptions
//@+node:gcross.20110511190907.3734: *4* Constructors
OutputOptions::OutputOptions()
  : Options("Output options")
{
    options.add_options()
        ("output-file,o", value<string>()->notifier(bind(&OutputOptions::setOutputFilepath,this,_1)),
            "location of output file\n"
            "-----------------------\n"
            "This value specifies the location of the output file to which the results of the simulation should be written.\n"
            "\n"
            "If this option is not specified then the results will be written to standard output.\n"
        )
        ("output-format", value<string>()->notifier(bind(&OutputOptions::setOutputFormat,this,_1)),
            "format of output file\n"
            "---------------------\n"
            "This value specifies the output format for the results.\n"
            "\n"
            "If this option is not specified, then the format will be inferred from the extension of the output file, or assumed to be TEXT if the results are being written to standard output.\n"
        )
        ("output-location", value<string>()->notifier(bind(&OutputOptions::setOutputLocation,this,_1)),
            "location within the output file\n"
            "-------------------------------\n"
            "This value specifies the location to which the results should be written within the output file;  note that this option is only sensible for semi-structured hierarchal file formats which can contain multiple kinds of data at multiple locations.\n"
            "\n"
            "If this option is not specified, then the hamiltonian will be assumed to be located at the root of the file.\n"
        )
        ("output-states", bool_switch(&output_states)->default_value(false)->implicit_value(true),
            "state output flag\n"
            "-----------------\n"
            "This flag indicates that states should be output in addition to the energy levels.\n"
            "\n"
            "If this option is not specified, then only the energy levels will be output.\n"
        )
        ("output-overwrite", bool_switch(&output_overwrite)->default_value(false)->implicit_value(true),
            "overwrite output flag\n"
            "---------------------\n"
            "This flag indicates that any existing data at the output file and location should be overwritten.  If this flag is false (or omitted), then Nutcracker will exit if existing data is detected at the output file and location.  (Note that for some formats, such as hdf, is possible to output data to the same file at different locations, and in such cases this flag will be ignored.)\n"
            "\n"
            "If this option is not specified, then it defaults to false --- that is, existing data will be protected against being overwritten.\n"
        )
    ;
}
//@+node:gcross.20110511190907.3735: *4* Fields
void OutputOptions::setOutputFilepath(string const& output_filepath) { maybe_output_filepath = output_filepath; }
void OutputOptions::setOutputFormat(string const& output_format) { maybe_output_format = output_format; }
void OutputOptions::setOutputLocation(string const& output_location) { maybe_output_location = output_location; }

optional<string> const& OutputOptions::getOutputMaybeFilepath() const { return maybe_output_filepath; }
optional<string> const& OutputOptions::getOutputMaybeFormat() const { return maybe_output_format; }
optional<string> const& OutputOptions::getOutputMaybeLocation() const { return maybe_output_location; }
bool OutputOptions::getOutputStates() const { return output_states; }
bool OutputOptions::getOutputOverwrite() const { return output_overwrite; }
//@+node:gcross.20110511190907.3813: *4* Format
OutputFormat const& OutputOptions::resolveOutputFormat() const {
    OutputFormat const& output_format =
        resolveAndCheckFormat<OutputFormat>(
            getOutputMaybeFormat(),
            getOutputMaybeFilepath(),
            getOutputMaybeLocation()
        );

    if(getOutputStates() && !output_format.supports_states)
        throw OutputFormatDoesNotSupportStatesError(output_format.name);

    return output_format;
}

auto_ptr<Destructable const> OutputOptions::connectToChainUsingOutputFormat(OutputFormat const& output_format, Chain& chain) {
    return
        output_format(
            getOutputMaybeFilepath(),
            getOutputMaybeLocation(),
            getOutputStates(),
            getOutputOverwrite(),
            chain
        );
}
//@+node:gcross.20110511190907.3751: *3* ToleranceOptions
//@+node:gcross.20110511190907.3752: *4* Constructors
ToleranceOptions::ToleranceOptions()
  : Options("Input options")
  , chain_options(Chain::defaults)
{
    options.add_options()
        ("site-tolerance", value<double>(&chain_options.site_convergence_threshold),(format(
            "site convergence tolerance\n"
            "--------------------------\n"
            "This value specifies the tolerance used when optimizing individual sites.  Specifically, the ARPACK eigenvalue solver (which is invoked to optimize each site in the chain) uses this value to decide how close it needs to get to the true minimum eigenvalue before declaring victory.\n"
            "\n"
            "If this options is not specified then it defaults to %1%.\n"
         ) % Chain::defaults.site_convergence_threshold).str().c_str()
        )
        ("sweep-tolerance", value<double>(&chain_options.sweep_convergence_threshold),(format(
            "sweep convergence tolerance\n"
            "---------------------------\n"
            "This value specifies the tolerance used when performing optimization sweeps on the chain.  Specifically, Nutcracker will sweep back and forth through the state optimizing sites until the difference in energy between two successive sweeps is less than this threshold.\n"
            "\n"
            "If this options is not specified then it defaults to %1%.\n"
         ) % Chain::defaults.sweep_convergence_threshold).str().c_str()
        )
        ("chain-tolerance", value<double>(&chain_options.chain_convergence_threshold),(format(
            "chain convergence tolerance\n"
            "---------------------------\n"
            "This value specifies the tolerance used when optimizing the chain.  Specifically, Nutcracker will keep increasing the bandwidth of the chain and performing optimization sweeps until the difference in energy between the current and previous bandwidth has fallen within this tolerance.\n"
            "\n"
            "If this options is not specified then it defaults to %1%.\n"
         ) % Chain::defaults.chain_convergence_threshold).str().c_str()
        )
        ("sanity-tolerance", value<double>(&chain_options.sanity_check_threshold),(format(
            "sanity check tolerance\n"
            "----------------------\n"
            "This value specifies the tolerance used when performing sanity checks throughout the optimization run;  sanity checks include making sure that the state is always normalized, making sure that excited states are perpendicular to the ground state, making sure that the eigenvalue solver returns an eigenvalue that is less than the old eigenvalue and consistent with the contraction of the environment, etc.  If one of the sanity checks sees a divergence that exceeds this threshold then the optimization of the site fails.\n"
            "\n"
            "If this options is not specified then it defaults to %1%.\n"
         ) % Chain::defaults.sanity_check_threshold).str().c_str()
        )
    ;
}
//@+node:gcross.20110511190907.3753: *4* Fields
double ToleranceOptions::getToleranceSite() const { return chain_options.site_convergence_threshold; }
double ToleranceOptions::getToleranceSweep() const { return chain_options.sweep_convergence_threshold; }
double ToleranceOptions::getToleranceChain() const { return chain_options.chain_convergence_threshold;}
double ToleranceOptions::getToleranceSanity() const { return chain_options.sanity_check_threshold; }
unsigned int ToleranceOptions::getToleranceIterations() const { return chain_options.maximum_number_of_iterations; }

ChainOptions ToleranceOptions::getToleranceChainOptions() const { return chain_options; }
//@-others

}
//@-leo
