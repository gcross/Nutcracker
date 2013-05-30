// Includes {{{
#include <illuminate.hpp>

#include "nutcracker/infinite_chain.hpp"
#include "nutcracker/infinite_operators.hpp"
#include "nutcracker/utilities.hpp"

#include "test_utils.hpp"
// }}}

// Usings {{{
using namespace Nutcracker;
// }}}

TEST_SUITE(InfiniteChain) { // {{{

    TEST_CASE(optimizeSite) { // {{{
        RNG random;

        REPEAT(10) {
            InfiniteChain chain(random.randomInfiniteChain());
            chain.setOptimizerSiteFailureToThrow();

            double previous_energy;

            #define TEST_OPTIMIZER \
                previous_energy = chain.getEnergy(); \
                chain.optimizeSite(); \
                std::cerr << chain.getEnergy() << ' ' << previous_energy << std::endl; \
                ASSERT_TRUE((chain.getEnergy() - previous_energy) / (abs(chain.getEnergy())+abs(previous_energy)) < 1e-7); \

            TEST_OPTIMIZER

            REPEAT(10) {
                chain.move<Left>();
                TEST_OPTIMIZER
                chain.move<Right>();
                TEST_OPTIMIZER
            }

            #undef TEST_OPTIMIZER
        }
    } // }}}

    TEST_CASE(linear_energy_growth) { // {{{
        RNG random;

        InfiniteChain chain(constructExternalFieldInfiniteOperator(random.randomHermitianMatrix(random(1,10))));
        double energy_multiple = chain.getEnergy();
        double expected_energy = energy_multiple;

        REPEAT(10) {
            ASSERT_NEAR_REL(chain.getEnergy(),expected_energy,1e-6);
            chain.move<Left>(); expected_energy += energy_multiple;
            ASSERT_NEAR_REL(chain.getEnergy(),expected_energy,1e-6);
            chain.move<Right>(); expected_energy += energy_multiple;
        }
    } // }}}

    TEST_CASE(correct_external_Z_field_solution) { // {{{
        RNG random;

        REPEAT(10) {
            InfiniteChain chain(
                constructExternalFieldInfiniteOperator(Pauli::Z),
                random.randomInfiniteState(PhysicalDimension(2))
            );
            chain.optimizeSite();
            StateSite<Middle> const& state_site = chain.getStateSite();
            unsigned int const non_physical_dimension = chain.bandwidthDimension()*chain.bandwidthDimension();
            for(size_t i = 0; i < non_physical_dimension; ++i) {
                ASSERT_NEAR_ABS_VAL(state_site[i],complex<double>(0),1e-6);
            }
        }
    } // }}}

    TEST_CASE(increaseBandwidthDimension) { // {{{
        RNG random;

        REPEAT(10) {
            InfiniteChain chain(random.randomInfiniteChain());
            complex<double> exp = chain.computeExpectationValue();
            unsigned int const new_bandwidth = random(0,10) + chain.bandwidthDimension();
            chain.increaseBandwidthDimension(new_bandwidth);
            ASSERT_EQ(chain.bandwidthDimension(),new_bandwidth)
            ASSERT_NEAR_REL(chain.computeExpectationValue(),exp,1e-6);
        }
    } // }}}

    TEST_SUITE(sweepUntilConverged) { // {{{
        struct S { InfiniteChain& chain; S(InfiniteChain&chain):chain(chain){} void operator()() const { std::cerr << chain.getEnergy() << std::endl; } };

        void runTest(
              double const coupling_strength
            , unsigned int const bandwidth_dimension
            , double const correct_energy
        ) {
            InfiniteChain chain(constructTransverseIsingModelInfiniteOperator(coupling_strength),ChainOptions().setInitialBandwidthDimension(bandwidth_dimension));
            S s(chain);
            chain.signalSweepPerformed.connect(s);
            chain.signalOptimizeSiteFailure.connect(rethrow<OptimizerFailure>);
            REPEAT(10) { chain.performOptimizationSweep(); }
            chain.increaseBandwidthDimension(chain.bandwidthDimension()+1);
            REPEAT(10) { chain.performOptimizationSweep(); }
            chain.increaseBandwidthDimension(chain.bandwidthDimension()+1);
            REPEAT(10) { chain.performOptimizationSweep(); }
            chain.increaseBandwidthDimension(chain.bandwidthDimension()+1);
            REPEAT(10) { chain.performOptimizationSweep(); }
            double e1 = chain.getEnergy();
            chain.move<Left>();
            chain.optimizeSite();
            double e2 = chain.getEnergy();
            std::cerr << e1 << ' ' << e2 << std::endl;
            std::cerr << *chain.getConvergenceEnergy() << ::std::endl;
            chain.dump();
            ASSERT_NEAR_REL(correct_energy,*chain.getConvergenceEnergy(),1e-7);
        }

        TEST_CASE(0p1_b10)  { runTest(100,1,-1.0317908325); }

    } // }}}

} // }}}
