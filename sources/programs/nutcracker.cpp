//@+leo-ver=5-thin
//@+node:gcross.20110220141808.2717: * @thin nutcracker.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110220141808.2718: ** << Includes >>
#include <boost/local/function.hpp>
#include <boost/format.hpp>
#include <boost/program_options.hpp>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <numeric>

#include "chain.hpp"

namespace po = boost::program_options;

using boost::format;

using std::ceil;
using std::cerr;
using std::cin;
using std::cout;
using std::endl;
using std::ifstream;
using std::setprecision;

using namespace Nutcracker;
//@-<< Includes >>

//@+others
//@+node:gcross.20110220141808.2719: ** main
int main(int argc, char** argv) {
    po::options_description all("Allowed options");
    all.add_options()
        //@+<< List of options >>
        //@+node:gcross.20110220182654.2003: *3* << List of options >>
        ("help", "print help message and exit\n")
        ("config-file,C",
            "configuration file\n"
            "------------------\n"
            "If this option is specified then Nutracker will read option values from it instead of from 'nutcracker.cfg'.\n"
        )
        ("hamiltonian-file,f", po::value<string>(),
            "location of hamiltonian description file\n"
            "----------------------------------------\n"
            "This value specifies the location of the file containing the description of the hamiltonian.\n"
            "\n"
            "If this options is not specified then the hamiltonian will be read from standard input.\n"
        )
        ("number-of-levels,n", po::value<unsigned int>()->default_value(1),
            "number of levels\n"
            "----------------\n"
            "This value specifies the number of energy levels that Nutcracker should find (including the ground state).\n"
            "\n"
            "If this options is not specified then it defaults to 1 (i.e., ground state only).\n"
        )
        ("site-tolerance", po::value<double>(),(format(
            "site convergence tolerance\n"
            "--------------------------\n"
            "This value specifies the tolerance used when optimizing individual sites.  Specifically, the ARPACK eigenvalue solver (which is invoked to optimize each site in the chain) uses this value to decide how close it needs to get to the true minimum eigenvalue before declaring victory.\n"
            "\n"
            "If this options is not specified then it defaults to %1%.\n"
         ) % Chain::defaults.site_convergence_threshold).str().c_str()
        )
        ("sweep-tolerance", po::value<double>(),(format(
            "sweep convergence tolerance\n"
            "---------------------------\n"
            "This value specifies the tolerance used when performing optimization sweeps on the chain.  Specifically, Nutcracker will sweep back and forth through the state optimizing sites until the difference in energy between two successive sweeps is less than this threshold.\n"
            "\n"
            "If this options is not specified then it defaults to %1%.\n"
         ) % Chain::defaults.sweep_convergence_threshold).str().c_str()
        )
        ("chain-tolerance", po::value<double>(),(format(
            "chain convergence tolerance\n"
            "---------------------------\n"
            "This value specifies the tolerance used when optimizing the chain.  Specifically, Nutcracker will keep increasing the bandwidth of the chain and performing optimization sweeps until the difference in energy between the current and previous bandwidth has fallen within this tolerance.\n"
            "\n"
            "If this options is not specified then it defaults to %1%.\n"
         ) % Chain::defaults.chain_convergence_threshold).str().c_str()
        )
        ("sanity-tolerance", po::value<double>(),(format(
            "sanity check tolerance\n"
            "----------------------\n"
            "This value specifies the tolerance used when performing sanity checks throughout the optimization run;  sanity checks include making sure that the state is always normalized, making sure that excited states are perpendicular to the ground state, making sure that the eigenvalue solver returns an eigenvalue that is less than the old eigenvalue and consistent with the contraction of the environment, etc.  If one of the sanity checks sees a divergence that exceeds this threshold then the optimization of the site fails.\n"
            "\n"
            "If this options is not specified then it defaults to %1%.\n"
         ) % Chain::defaults.sanity_check_threshold).str().c_str()
        )
        //@-<< List of options >>
    ;

    BOOST_LOCAL_FUNCTION((void)(printHelpAndExit)((const bind)((&all)))) {
        cout
            << all
            << "Options may also be specified in 'nutcracker.cfg' (or in an alternative configuration file specified via --config-file) in the format option=value.  For example, the line" << endl
            << endl
            << "\tsite-tolerance = 1e-10" << endl
            << endl
            << "is equivalence to specifying --site-tolerance 1e-10 on the command line." << endl
        ;
        exit(1);
    } BOOST_LOCAL_FUNCTION_END(printHelpAndExit);

    po::variables_map vm;
    try {
        po::store(po::parse_command_line(argc, argv, all), vm);
        string config_filename;
        if(vm.count("config-file")) {
            config_filename = vm["config-file"].as<string>();
        } else {
            config_filename = "nutcracker.cfg";
        }
        cout << "Reading options from '" << config_filename << "'" << endl;
        ifstream config_file(config_filename.c_str());
        po::store(po::parse_config_file(config_file, all), vm);
    } catch(...) {
        printHelpAndExit();
    }

    if (vm.count("help")) {
        printHelpAndExit();
    }

    Chain::Options options = Chain::defaults;
    if (vm.count("site-tolerance")) {
        options.site_convergence_threshold = vm["site-tolerance"].as<double>();
    }
    if (vm.count("sweep-tolerance")) {
        options.sweep_convergence_threshold = vm["sweep-tolerance"].as<double>();
    }
    if (vm.count("chain-tolerance")) {
        options.chain_convergence_threshold = vm["chain-tolerance"].as<double>();
    }

    unsigned int const number_of_levels = vm["number-of-levels"].as<unsigned int>();

    string hamiltonian_filename;
    if(vm.count("hamiltonian-file")) {
        hamiltonian_filename = vm["hamiltonian-file"].as<unsigned int>();
    }

    cout
        << "Configuration:" << endl
        << "  hamiltonian file: " << (hamiltonian_filename.empty() ? "<standard input>" : hamiltonian_filename.c_str()) << endl
        << "  number of levels: " << number_of_levels << endl
        << "  site convergence tolerance: " << options.site_convergence_threshold << endl
        << "  sweep convergence tolerance: " << options.sweep_convergence_threshold << endl
        << "  chain convergence tolerance: " << options.chain_convergence_threshold << endl
        << "  sanity check threshold: " << options.sanity_check_threshold << endl
    ;

    double const estimated_precision = ceil(-log10(options.chain_convergence_threshold));
    unsigned int const precision = estimated_precision > 0 ? (unsigned int)estimated_precision : 1;

    YAML::Node doc;
    if(hamiltonian_filename.empty()) {
        YAML::Parser parser(cin);
        parser.GetNextDocument(doc);
    } else {
        ifstream in(hamiltonian_filename.c_str());
        YAML::Parser parser(in);
        parser.GetNextDocument(doc);
    }

    Operator hamiltonian;
    doc >> hamiltonian;

    Chain chain(hamiltonian,1,options);

    BOOST_LOCAL_FUNCTION((void)(printChainEnergy)((const bind)((precision)(&chain)))) {
        cout << "  - " << setprecision(precision) << chain.getEnergy() << endl;
        cout.flush();
    } BOOST_LOCAL_FUNCTION_END(printChainEnergy);
    chain.signalChainOptimized.connect(printChainEnergy);

    cout << "Energy levels:" << endl;
    chain.solveForMultipleLevels(number_of_levels);

    return 0;
}
//@-others
//@-leo
