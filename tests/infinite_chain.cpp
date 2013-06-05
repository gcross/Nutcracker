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
            InfiniteChain chain_copy(copyFrom(chain));
            unsigned int const new_bandwidth = random(0,10) + chain.bandwidthDimension();
            chain.increaseBandwidthDimension(new_bandwidth);
            chain_copy.move<Left>();
            chain_copy.move<Right>();
            ASSERT_EQ(chain.bandwidthDimension(),new_bandwidth)
            ASSERT_NEAR_REL(chain.computeExpectationValue(),chain_copy.computeExpectationValue(),1e-6);
        }
    } // }}}

    TEST_SUITE(sweepUntilConverged) { // {{{

        void runTest(
              double const coupling_strength
            , unsigned int const bandwidth_dimension
            , double const correct_energy
            , double const precision
        ) {
            InfiniteChain chain(constructTransverseIsingModelInfiniteOperator(coupling_strength),ChainOptions().setInitialBandwidthDimension(bandwidth_dimension));
            chain.signalOptimizeSiteFailure.connect(rethrow<OptimizerFailure>);
            chain.sweepUntilConverged();
            ASSERT_NEAR_REL(correct_energy,*chain.getConvergenceEnergy(),precision);
        }

        TEST_CASE(0p01_b2)  { runTest(0.01,2,-1.0000250001562545,1e-12); }
        TEST_CASE(0p1_b2)  { runTest(0.1,2,-1.00250156,1e-8); }
        TEST_CASE(0p1_b4)  { runTest(0.1,4,-1.00250156,1e-8); }

    } // }}}

    TEST_SUITE(optimizeChain) { // {{{

        void runTest(
              double const coupling_strength
            , double const correct_energy
            , double const precision
        ) {
            InfiniteChain chain(constructTransverseIsingModelInfiniteOperator(coupling_strength));
            chain.signalOptimizeSiteFailure.connect(rethrow<OptimizerFailure>);
            chain.optimizeChain();
            ASSERT_NEAR_REL(correct_energy,*chain.getConvergenceEnergy(),precision);
        }

        TEST_CASE(0p01)  { runTest(0.01,-1.0000250001562545,1e-12); }
        TEST_CASE(0p1)  { runTest(0.1,-1.00250156,1e-8); }

    } // }}}

    TEST_SUITE(XY_model) { // {{{

        void runTest(
              double const coupling_strength
            , double const correct_energy
            , double const precision
        ) {
            InfiniteChain chain(constructXYModelInfiniteOperator(coupling_strength),ChainOptions().setChainConvergenceThreshold(precision/10));
            chain.signalOptimizeSiteFailure.connect(rethrow<OptimizerFailure>);
            chain.optimizeChain();
            ASSERT_NEAR_REL(correct_energy,*chain.getConvergenceEnergy(),precision);
        }

        TEST_CASE(0p01)  { runTest(0.01,-1.000025,1e-6); }
        TEST_CASE(0p1)  { runTest(0.1,-1.0025,1e-4); }
        TEST_CASE(1p0)  { runTest(1,-1.27,1e-2); }

    } // }}}

    TEST_CASE(Haldane_Shastry_model) { // {{{
        InfiniteChain chain(constructHaldaneShastryModelInfiniteOperator(),ChainOptions().setSweepConvergenceThreshold(1e-7).setChainConvergenceThreshold(1e-6));
        chain.signalOptimizeSiteFailure.connect(rethrow<OptimizerFailure>);
        chain.optimizeChain();
        ASSERT_NEAR_ABS_VAL(*chain.getConvergenceEnergy(),-1.6449340668482264,1e-3);
    } // }}}
} // }}}
