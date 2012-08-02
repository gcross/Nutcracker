//@+leo-ver=5-thin
//@+node:gcross.20110511190907.3699: * @file configuration.hpp
//@@language cplusplus
#ifndef NUTCRACKER_CONFIGURATION_HPP
#define NUTCRACKER_CONFIGURATION_HPP

//@+<< Includes >>
//@+node:gcross.20110511190907.3702: ** << Includes >>
#include <boost/optional.hpp>
#include <boost/program_options.hpp>
#include <string>

#include "nutcracker/chain.hpp"
#include "nutcracker/io.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110511190907.3703: ** << Usings >>
using boost::optional;
using boost::program_options::options_description;

using std::string;
//@-<< Usings >>

//@+others
//@+node:gcross.20110511190907.3704: ** Classes
//@+node:gcross.20110511190907.3717: *3* Options
class Options {
    //@+others
    //@+node:gcross.20110511190907.3722: *4* Constructors
    public:

    Options(string const& name) : options(name) {}
    //@+node:gcross.20110511190907.3719: *4* Casts
    public:

    operator options_description&() { return options; }
    //@+node:gcross.20110511190907.3720: *4* Fields
    protected:

    options_description options;

    public:

    options_description& getOptions() { return options; }
    //@-others
};
//@+node:gcross.20110511190907.3705: *3* InputOptions
class InputOptions : public Options {
    //@+others
    //@+node:gcross.20110511190907.3707: *4* Constructors
    public:

    InputOptions();
    //@+node:gcross.20110511190907.3706: *4* Fields
    protected:

    optional<string> maybe_input_filepath, maybe_input_format, maybe_input_location;

    void setInputFilepath(string const& input_filepath);
    void setInputFormat(string const& input_format);
    void setInputLocation(string const& input_location);

    public:

    optional<string> const& getInputMaybeFilepath() const;
    optional<string> const& getInputMaybeFormat() const;
    optional<string> const& getInputMaybeLocation() const;
    //@+node:gcross.20110511190907.3810: *4* Format
    public:

    InputFormat const& resolveInputFormat() const;

    Operator readOperatorUsingInputFormat(InputFormat const& input_format) const;
    //@-others
};
//@+node:gcross.20110511190907.3727: *3* OutputOptions
class OutputOptions : public Options {
    //@+others
    //@+node:gcross.20110511190907.3728: *4* Constructors
    public:

    OutputOptions();
    //@+node:gcross.20110511190907.3729: *4* Fields
    protected:

    optional<string> maybe_output_filepath, maybe_output_format, maybe_output_location;
    bool output_states, output_overwrite;

    void setOutputFilepath(string const& output_filepath);
    void setOutputFormat(string const& output_format);
    void setOutputLocation(string const& output_location);

    public:

    optional<string> const& getOutputMaybeFilepath() const;
    optional<string> const& getOutputMaybeFormat() const;
    optional<string> const& getOutputMaybeLocation() const;
    bool getOutputStates() const;
    bool getOutputOverwrite() const;
    //@+node:gcross.20110511190907.3812: *4* Format
    public:

    OutputFormat const& resolveOutputFormat() const;

    auto_ptr<Destructable const> connectToChainUsingOutputFormat(OutputFormat const& output_format, Chain& chain);
    //@-others
};
//@+node:gcross.20110511190907.3739: *3* ToleranceOptions
class ToleranceOptions : public Options {
    //@+others
    //@+node:gcross.20110511190907.3740: *4* Constructors
    public:

    ToleranceOptions();
    //@+node:gcross.20110511190907.3741: *4* Fields
    protected:

    ChainOptions chain_options;

    public:

    double getToleranceSite() const;
    double getToleranceSweep() const;
    double getToleranceChain() const;
    double getToleranceSanity() const;
    unsigned int getToleranceIterations() const;

    ChainOptions getToleranceChainOptions() const;
    //@-others
};
//@-others

}

#endif
//@-leo
