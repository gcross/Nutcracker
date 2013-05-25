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

            double energy = chain.getEnergy();

            #define TEST_OPTIMIZER \
                chain.optimizeSite(); \
                ASSERT_TRUE((chain.getEnergy() - energy) / (abs(chain.getEnergy())+abs(energy)) < 1e-7); \
                energy = chain.getEnergy();

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
