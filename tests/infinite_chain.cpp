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

} // }}}
