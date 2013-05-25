// Includes {{{
#include <boost/assign.hpp>
#include <boost/container/vector.hpp>
#include <boost/foreach.hpp>
#include <boost/format.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <boost/range/algorithm/max_element.hpp>
#include <boost/range/irange.hpp>
#include <boost/range/numeric.hpp>
#include <boost/smart_ptr/shared_ptr.hpp>
#include <complex>
#include <illuminate.hpp>
#include <sstream>
#include <functional>

#include "nutcracker/chain.hpp"
#include "nutcracker/compiler.hpp"
#include "nutcracker/operators.hpp"
#include "nutcracker/utilities.hpp"

#include "test_utils.hpp"
// }}}

// Usings {{{
using namespace Nutcracker;

using boost::accumulate;
using boost::container::vector;
using boost::equal;
using boost::format;
using boost::max_element;
using boost::shared_ptr;

using std::abs;
using std::conj;
using std::ostringstream;
using std::pair;
// }}}

TEST_SUITE(Chain) { // {{{

TEST_SUITE(computeBandwidthDimensionSequence) { // {{{

TEST_CASE(correct_properties) { // {{{
    RNG random;

    REPEAT(10) {
        unsigned int const
             requested_bandwidth_dimension = random(1,100000)
            ,number_of_sites = random(20,1000)
            ;
        vector<unsigned int> physical_dimensions;
        physical_dimensions.reserve(number_of_sites);
        generate_n(
             back_inserter(physical_dimensions)
            ,number_of_sites
            ,random.generateRandomIntegers(2,10)
        );

        vector<unsigned int> const bandwidth_dimensions =
            computeBandwidthDimensionSequence(
                 requested_bandwidth_dimension
                ,physical_dimensions
            );

        ASSERT_EQ(number_of_sites+1,bandwidth_dimensions.size());
        ASSERT_EQ(requested_bandwidth_dimension,*max_element(bandwidth_dimensions));

        BOOST_FOREACH(unsigned int const i, irange(0u,number_of_sites-1)) {
            ASSERT_LE(bandwidth_dimensions[i+1], bandwidth_dimensions[i]*physical_dimensions[i]);
            ASSERT_LE(bandwidth_dimensions[i], bandwidth_dimensions[i+1]*physical_dimensions[i]);
        }
    }
} // }}}

TEST_CASE(complains_if_too_large) { // {{{
    RNG random;

    REPEAT(10) {
        unsigned int const number_of_sites = random(10,20);
        vector<unsigned int> physical_dimensions;
        physical_dimensions.reserve(number_of_sites);
        generate_n(
             back_inserter(physical_dimensions)
            ,number_of_sites
            ,random.generateRandomIntegers(2,10)
        );
        unsigned int const requested_bandwidth_dimension = 2*accumulate(physical_dimensions,1u,multiplies<unsigned int>());
        try {
            computeBandwidthDimensionSequence(
                 requested_bandwidth_dimension
                ,physical_dimensions
            );
        } catch(RequestedBandwidthDimensionTooLargeError const& e) {
            ASSERT_EQ(requested_bandwidth_dimension,e.requested_bandwidth_dimension);
            continue;
        }
        FATALLY_FAIL("Exception was not thrown!");
    }
} // }}}

} // }}}

TEST_CASE(maximumBandwidthDimension) { // {{{
    RNG random;

    REPEAT(10) {
        unsigned int const number_of_sites = random+1;
        vector<unsigned int> physical_dimensions;
        physical_dimensions.reserve(number_of_sites);
        generate_n(
             back_inserter(physical_dimensions)
            ,number_of_sites
            ,random.generateRandomIntegers(2,10)
        );
        unsigned int maximum_bandwidth_dimension = maximumBandwidthDimension(physical_dimensions);
        try {
            computeBandwidthDimensionSequence(maximum_bandwidth_dimension,physical_dimensions);
        } catch(RequestedBandwidthDimensionTooLargeError const& e) {
            ostringstream message;
            message << "The maximum bandwidth (" << maximum_bandwidth_dimension << ") was too large for the sequence:";
            BOOST_FOREACH(unsigned int const physical_dimension, physical_dimensions) {
                message << ' ' << physical_dimension;
            }
            FATALLY_FAIL(message.str())
        }
        try {
            computeBandwidthDimensionSequence(maximum_bandwidth_dimension+1,physical_dimensions);
        } catch(RequestedBandwidthDimensionTooLargeError const& e) {
            continue;
        }
        ostringstream message;
        message << "The maximum bandwidth (" << maximum_bandwidth_dimension << ") was too small for the sequence:";
        BOOST_FOREACH(unsigned int const physical_dimension, physical_dimensions) {
            message << ' ' << physical_dimension;
        }
        FATALLY_FAIL(message.str())
    }
} // }}}

TEST_CASE(moveLeftAndRight) { // {{{
    RNG random;

    REPEAT(10) {
        unsigned int const number_of_operators = random+2;
        Chain chain(random.randomOperator(number_of_operators));

        #define VALIDATE_CHAIN_PROPERTIES \
            { \
                ASSERT_NEAR_REL(1,chain.computeStateNorm(),1e-14); \
                complex<double> const expectation_value = chain.computeExpectationValue(); \
                ASSERT_NEAR_REL(0,expectation_value.imag(),1e-10); \
                ASSERT_NEAR_REL(chain.getEnergy(),expectation_value.real(),1e-10); \
            }

        REPEAT(number_of_operators-1) {
            chain.move<Right>();
            VALIDATE_CHAIN_PROPERTIES
        }
        REPEAT(number_of_operators-1) {
            chain.move<Left>();
            VALIDATE_CHAIN_PROPERTIES
        }

        #undef VALIDATE_CHAIN_PROPERTIES
    }
} // }}}

TEST_CASE(optimizeSite) { // {{{
    RNG random;

    REPEAT(10) {
        unsigned int const number_of_operators = random+2;
        Chain chain(random.randomOperator(number_of_operators));

        double energy = chain.getEnergy();

        #define TEST_OPTIMIZER \
            chain.optimizeSite(); \
            ASSERT_TRUE((chain.getEnergy() - energy) / (abs(chain.getEnergy())+abs(energy)) < 1e-7); \
            energy = chain.getEnergy();

        TEST_OPTIMIZER

        REPEAT(number_of_operators-1) {
            chain.move<Right>();
            TEST_OPTIMIZER
        }
        REPEAT(number_of_operators-1) {
            chain.move<Left>();
            TEST_OPTIMIZER
        }

        #undef TEST_OPTIMIZER
    }
} // }}}

TEST_SUITE(performOptimizationSweep) { // {{{

    vector<pair<unsigned int,vector<unsigned int> > > system_parameters =
        list_of<pair<unsigned int,vector<unsigned int> > >
            (2,list_of(2))
            (3,list_of(2))
            (4,list_of(2)(4))
            (5,list_of(2)(4))
            (10,list_of(2)(4)(8)(17))
    ;

    void runTests(
          unsigned int const physical_dimension
    ) {
        MatrixConstPtr matrix;
        {
            vector<complex<double> > diagonal(physical_dimension,1); diagonal[0] = -1;
            matrix = diagonalMatrix(diagonal);
        }
        typedef pair<unsigned int,vector<unsigned int> > Parameters;
        BOOST_FOREACH(
             Parameters const& parameters
            ,system_parameters
        ) {
            unsigned int const number_of_sites = parameters.first;
            vector<unsigned int> const& initial_bandwidth_dimensions = parameters.second;
            BOOST_FOREACH(
                 unsigned int const initial_bandwidth_dimension
                ,initial_bandwidth_dimensions
            ) {
                Chain chain(
                     constructExternalFieldOperator(
                          number_of_sites
                        , matrix
                     ),
                     ChainOptions()
                        .setInitialBandwidthDimension(initial_bandwidth_dimension)
                );
                chain.signalOptimizeSiteFailure.connect(rethrow<OptimizerFailure>);
                chain.performOptimizationSweep();
                ASSERT_NEAR_REL(c(-(int)number_of_sites,0),chain.getEnergy(),1e-7);
            }
        }
    }

    TEST_CASE(physical_dimension_2) { runTests(2); }
    TEST_CASE(physical_dimension_3) { runTests(3); }
    TEST_CASE(physical_dimension_4) { runTests(4); }
} // }}}

TEST_CASE(increaseBandwidthDimension) { // {{{
    RNG random;

    REPEAT(10) {
        unsigned int const number_of_operators = random+2;
        Chain chain(random.randomOperator(number_of_operators));
        unsigned int const maximum_bandwidth_dimension = min(10u,chain.maximum_bandwidth_dimension);

        State old_state = chain.makeCopyOfState();

        #define VALIDATE_CHAIN_PROPERTIES \
            { \
                ASSERT_NEAR_REL(1,chain.computeStateNorm(),1e-9); \
                complex<double> const expectation_value = chain.computeExpectationValue(); \
                ASSERT_NEAR_REL(0,expectation_value.imag(),1e-9); \
                ASSERT_NEAR_REL(chain.getEnergy(),expectation_value.real(),1e-7); \
            }

        BOOST_FOREACH(unsigned int const bandwidth_dimension, irange(1u,maximum_bandwidth_dimension)) {
            chain.increaseBandwidthDimension(bandwidth_dimension);
            State new_state = chain.makeCopyOfState();
            ASSERT_NEAR_REL(c(1,0),computeStateOverlap(old_state,new_state),1e-13);
            REPEAT(number_of_operators-1) {
                chain.move<Right>();
                VALIDATE_CHAIN_PROPERTIES
            }
            REPEAT(number_of_operators-1) {
                chain.move<Left>();
                VALIDATE_CHAIN_PROPERTIES
            }
        }

        #undef VALIDATE_CHAIN_PROPERTIES
    }
} // }}}

TEST_SUITE(sweepUntilConverged) { // {{{

    void runTest(
          unsigned int const number_of_sites
        , double const coupling_strength
        , unsigned int const bandwidth_dimension
        , double const correct_energy
    ) {
        Chain chain(constructTransverseIsingModelOperator(number_of_sites,coupling_strength),ChainOptions().setInitialBandwidthDimension(bandwidth_dimension));
        chain.signalOptimizeSiteFailure.connect(rethrow<OptimizerFailure>);
        chain.sweepUntilConverged();
        ASSERT_NEAR_REL(correct_energy,chain.getEnergy(),1e-7);
    }

    TEST_CASE(2_sites_0p1)  { runTest( 2,0.1,2,- 2.00249843); }
    TEST_CASE(2_sites_1p0)  { runTest( 2,1.0,2,- 2.23606797); }
    TEST_CASE(4_sites_0p1)  { runTest( 4,0.1,2,- 4.00750155); }
    TEST_CASE(4_sites_1p0)  { runTest( 4,1.0,4,- 4.75877048); }
    TEST_CASE(10_sites_0p1) { runTest(10,0.1,2,-10.02251095); }
    TEST_CASE(10_sites_1p0) { runTest(10,1.0,6,-12.38148999); }

} // }}}

TEST_SUITE(optimizeChain) { // {{{

TEST_SUITE(external_field) { // {{{

    vector<pair<unsigned int,vector<unsigned int> > > system_parameters = // {{{
        list_of<pair<unsigned int,vector<unsigned int> > >
            (1,list_of(1))
            (2,list_of(1)(2))
            (3,list_of(1)(2))
            (4,list_of(1)(2)(4))
            (5,list_of(1)(2)(4))
            (10,list_of(1)(2)(4)(8)(17))
    ; // }}}

    void runTests( // {{{
          unsigned int const physical_dimension
        , OptimizerMode const& optimizer_mode
    ) {
        MatrixConstPtr matrix;
        {
            vector<complex<double> > diagonal(physical_dimension,1); diagonal[0] = -1;
            matrix = diagonalMatrix(diagonal);
        }
        typedef pair<unsigned int,vector<unsigned int> > Parameters;
        BOOST_FOREACH(
             Parameters const& parameters
            ,system_parameters
        ) {
            unsigned int const number_of_sites = parameters.first;
            vector<unsigned int> const& initial_bandwidth_dimensions = parameters.second;
            BOOST_FOREACH(
                 unsigned int const initial_bandwidth_dimension
                ,initial_bandwidth_dimensions
            ) {
                Chain chain(
                     constructExternalFieldOperator(
                          number_of_sites
                        , matrix
                     )
                    ,ChainOptions()
                        .setInitialBandwidthDimension(initial_bandwidth_dimension)
                        .setOptimizerMode(optimizer_mode)
                );
                chain.signalOptimizeSiteFailure.connect(rethrow<OptimizerFailure>);
                unsigned int number_of_sweeps = 0;
                chain.signalSweepPerformed.connect(++lambda::var(number_of_sweeps));
                chain.optimizeChain();
                ASSERT_TRUE(number_of_sweeps < 5);
                if(optimizer_mode == OptimizerMode::least_value) {
                    ASSERT_NEAR_REL(static_cast<double>(number_of_sites),-chain.getEnergy(),1e-7);
                } else if(optimizer_mode == OptimizerMode::greatest_value) {
                    ASSERT_NEAR_REL(static_cast<double>(number_of_sites),chain.getEnergy(),1e-7);
                } else if(optimizer_mode == OptimizerMode::largest_magnitude) {
                    ASSERT_NEAR_REL(static_cast<double>(number_of_sites),abs(chain.getEnergy()),1e-7);
                } else {
                    FATALLY_FAIL("Bad optimizer mode.");
                }
            }
        }
    } // }}}

    TEST_SUITE(least_value) { // {{{
        OptimizerMode const& mode = OptimizerMode::least_value;
        TEST_CASE(physical_dimension_2) { runTests(2,mode); }
        TEST_CASE(physical_dimension_3) { runTests(3,mode); }
        TEST_CASE(physical_dimension_4) { runTests(4,mode); }
    } // }}}
    TEST_SUITE(greatest_value) { // {{{
        OptimizerMode const& mode = OptimizerMode::greatest_value;
        TEST_CASE(physical_dimension_2) { runTests(2,mode); }
        TEST_CASE(physical_dimension_3) { runTests(3,mode); }
        TEST_CASE(physical_dimension_4) { runTests(4,mode); }
    } // }}}
    TEST_SUITE(largest_magnitude) { // {{{
        OptimizerMode const& mode = OptimizerMode::largest_magnitude;
        TEST_CASE(physical_dimension_2) { runTests(2,mode); }
        TEST_CASE(physical_dimension_3) { runTests(3,mode); }
        TEST_CASE(physical_dimension_4) { runTests(4,mode); }
    } // }}}
} // }}}

TEST_SUITE(transverse_Ising_model) { // {{{

    void runTest( // {{{
          unsigned int const number_of_sites
        , double const coupling_strength
        , double const correct_energy
        , OptimizerMode const& optimizer_mode
    ) {
        Chain chain(
            constructTransverseIsingModelOperator(number_of_sites,coupling_strength)
          , ChainOptions()
                .setOptimizerMode(optimizer_mode)
        );
        chain.signalOptimizeSiteFailure.connect(rethrow<OptimizerFailure>);
        chain.optimizeChain();
        if(optimizer_mode == OptimizerMode::least_value) {
            ASSERT_NEAR_REL(correct_energy,-chain.getEnergy(),1e-10);
        } else if(optimizer_mode == OptimizerMode::greatest_value) {
            ASSERT_NEAR_REL(correct_energy,chain.getEnergy(),1e-10);
        } else if(optimizer_mode == OptimizerMode::largest_magnitude) {
            ASSERT_NEAR_REL(correct_energy,abs(chain.getEnergy()),1e-10);
        } else {
            FATALLY_FAIL("Bad optimizer mode.");
        }
    } // }}}

    TEST_SUITE(least_value) { // {{{
        OptimizerMode const& mode = OptimizerMode::least_value;
        TEST_CASE(10_sites_0p1) { runTest(10,0.1,10.0225109571,mode); }
        TEST_CASE(10_sites_1p0) { runTest(10,1.0,12.3814899997,mode); }
    } // }}}
    TEST_SUITE(greatest_value) { // {{{
        OptimizerMode const& mode = OptimizerMode::greatest_value;
        TEST_CASE(10_sites_0p1) { runTest(10,0.1,10.0225109571,mode); }
        TEST_CASE(10_sites_1p0) { runTest(10,1.0,12.3814899997,mode); }
    } // }}}
    TEST_SUITE(largest_magnitude) { // {{{
        OptimizerMode const& mode = OptimizerMode::largest_magnitude;
        TEST_CASE(10_sites_0p1) { runTest(10,0.1,10.0225109571,mode); }
        TEST_CASE(10_sites_1p0) { runTest(10,1.0,12.3814899997,mode); }
    } // }}}

} // }}}

} // }}}

TEST_CASE(expectation_matches_computeExpectationValue) { // {{{
    RNG random;

    REPEAT(10) {
        unsigned int const number_of_sites = random;
        Operator O = random.randomOperator(number_of_sites);
        Chain chain(O);
        State state = chain.makeCopyOfState();
        ASSERT_NEAR_REL(chain.getEnergy(),computeExpectationValue(state,O),1e-10);
    }
} // }}}

TEST_SUITE(solveForMultipleLevels) { // {{{

    struct checkEnergies_checkOverlap { // {{{
        Chain const& chain;
        checkEnergies_checkOverlap(Chain const& chain) : chain(chain) {}
        void operator()(unsigned int const number_of_iterations=0) {
            ASSERT_NEAR_REL(0,chain.computeProjectorOverlapAtCurrentSite(),1e-12);
        }
    }; // }}}

    struct checkEnergies_postEnergy { // {{{
        Chain const& chain; bool absolute; vector<double>& actual_energies;
        checkEnergies_postEnergy(Chain const& chain, bool absolute, vector<double>& actual_energies)
            : chain(chain), absolute(absolute), actual_energies(actual_energies) {}
        void operator()() {
            if(absolute) {
                actual_energies.push_back(abs(chain.getEnergy()));
            } else {
                actual_energies.push_back(chain.getEnergy());
            }
        }
    }; // }}}

    void checkEnergies( // {{{
         Chain& chain
        ,vector<double> const& correct_energies
        ,double tolerance
        ,bool absolute = false
    ) {
        unsigned int const number_of_levels = correct_energies.size();
        chain.signalOptimizeSiteFailure.connect(rethrow<OptimizerFailure>);
        checkEnergies_checkOverlap checkOverlap(chain);
        chain.signalOptimizeSiteSuccess.connect(checkOverlap);
        chain.signalChainReset.connect(checkOverlap);
        vector<double> actual_energies; actual_energies.reserve(number_of_levels);
        checkEnergies_postEnergy postEnergy(chain,absolute,actual_energies);
        chain.signalChainOptimized.connect(postEnergy);
        chain.solveForMultipleLevels(number_of_levels);
        BOOST_FOREACH(unsigned int const i, irange(0u,number_of_levels)) {
            if(abs(correct_energies[i]-actual_energies[i])>tolerance) {
                ostringstream message;
                message << format("Wrong energies [#%||: %|.15| != %|.15|]: ") % (i+1) % correct_energies[i] % actual_energies[i];
                BOOST_FOREACH(unsigned int const i, irange(0u,number_of_levels)) {
                    message << format("%1% (%2%); ") % actual_energies[i] % correct_energies[i];
                }
                FATALLY_FAIL(message.str());
            }
        }
    } // }}}

    TEST_SUITE(external_field) { // {{{

        void runTest( // {{{
              unsigned int const physical_dimension
            , unsigned int const number_of_sites
            , vector<double> const& correct_energies
        ) {
            Chain chain(constructExternalFieldOperator(number_of_sites,diagonalMatrix(irange(0u,physical_dimension))));
        double tolerance = 1e-12;
        if(physical_dimension == 3 && number_of_sites == 3) {
          chain.sanity_check_threshold *= 10;
        }
            checkEnergies(chain,correct_energies,tolerance);
        } // }}}

        TEST_SUITE(physical_dimension_2) {
            TEST_CASE(1_site) { runTest(2,1,list_of(0)(1)); }
            TEST_CASE(2_sites) { runTest(2,2,list_of(0)(1)(1)(2)); }
            TEST_CASE(4_sites) { runTest(2,4,list_of(0)(1)(1)(1)); }
        }
        TEST_SUITE(physical_dimension_3) {
            TEST_CASE(1_site) { runTest(3,1,list_of(0)(1)(2)); }
            TEST_CASE(2_sites) { runTest(3,2,list_of(0)(1)(1)(2)(2)); }
            TEST_CASE(3_sites) { runTest(3,3,list_of(0)(1)(1)(1)(2)); }
        }

    } // }}}

} // }}}

TEST_CASE(solveForEigenvalues) { // {{{
    RNG random;
    BOOST_FOREACH(unsigned int const number_of_sites, irange(3u,6u)) {
        OperatorBuilder builder(number_of_sites,PhysicalDimension(2));
        BOOST_FOREACH(unsigned int site_number, irange(0u,number_of_sites)) {
            builder += LocalExternalField(site_number,squareMatrix(list_of(0)(0)(0)(1)));
        }
        Chain chain(builder.compile(),ChainOptions().setInitialBandwidthDimension(3));
        vector<double> eigenvalues = chain.solveForEigenvalues(4);
        ASSERT_EQ_VAL(eigenvalues.size(),4u);
        ASSERT_NEAR_ABS(eigenvalues[0],(double)0,1e-13);
        BOOST_FOREACH(unsigned int const i, irange(1u,4u)) {
            ASSERT_NEAR_ABS(eigenvalues[i],(double)1,1e-13);
        }
    }
} // }}}

TEST_CASE(solveForMultipleLevelsAndThenClearChain) { // {{{
    RNG random;
    BOOST_FOREACH(unsigned int const number_of_sites, irange(3u,6u)) {
        OperatorBuilder builder(number_of_sites,PhysicalDimension(2));
        BOOST_FOREACH(unsigned int site_number, irange(0u,number_of_sites)) {
            builder += LocalExternalField(site_number,squareMatrix(list_of(0)(0)(0)(1)));
        }
        Operator op = builder.compile();
        Chain chain(op,ChainOptions().setInitialBandwidthDimension(3));
        REPEAT(10) {
            vector<Solution> solutions(static_cast<BOOST_RV_REF(vector<Solution>)>(chain.solveForMultipleLevelsAndThenClearChain(4)));
            ASSERT_EQ_VAL(solutions.size(),4u);
            BOOST_FOREACH(unsigned int const i, irange(0u,4u)) {
                if(i == 0) {
                    ASSERT_NEAR_ABS(solutions[i].eigenvalue,(double)0,1e-13);
                } else {
                    ASSERT_NEAR_ABS(solutions[i].eigenvalue,(double)1,1e-13);
                }
                ASSERT_NEAR_ABS(computeExpectationValue(solutions[i].eigenvector,op),solutions[i].eigenvalue,1e-12);
                ASSERT_NEAR_ABS(computeStateOverlap(solutions[i].eigenvector,solutions[i].eigenvector),c(1,0),1e-12);
                BOOST_FOREACH(unsigned int const j, irange(i+1,4u)) {
                    ASSERT_NEAR_ABS(computeStateOverlap(solutions[i].eigenvector,solutions[j].eigenvector),c(0,0),1e-12);
                }
            }
        }
    }
} // }}}

}
