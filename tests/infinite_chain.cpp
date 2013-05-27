// Includes {{{
#include <illuminate.hpp>

#include "nutcracker/infinite_chain.hpp"
#include "nutcracker/utilities.hpp"

#include "test_utils.hpp"
// }}}

// Usings {{{
using namespace Nutcracker;
// }}}

TEST_SUITE(InfiniteChain) { // {{{

    TEST_CASE(optimizeSite) {
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
    }

    TEST_CASE(linear_energy_growth) {
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
    }

} // }}}
