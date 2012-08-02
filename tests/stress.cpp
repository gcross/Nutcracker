//@+leo-ver=5-thin
//@+node:gcross.20110902221314.2682: * @file stress.cpp
//@@language cplusplus
//@+<< Includes >>
//@+node:gcross.20110902221314.2684: ** << Includes >>
#include <boost/local/function.hpp>
#include <illuminate.hpp>

#include "nutcracker/chain.hpp"

#include "test_utils.hpp"

using std::ostringstream;
//@-<< Includes >>

//@+others
//@+node:gcross.20110902221314.2691: ** function checkEnergies
void checkEnergies(
     Chain& chain
    ,vector<double> const& correct_energies
    ,double tolerance
    ,bool absolute = false
) {
    unsigned int const number_of_levels = correct_energies.size();
    chain.signalOptimizeSiteFailure.connect(rethrow<OptimizerFailure>);
    void BOOST_LOCAL_FUNCTION_PARAMS(
        unsigned int const number_of_iterations, default 0,
        const bind& chain
    ) {
        ASSERT_NEAR_REL(0,chain.computeProjectorOverlapAtCurrentSite(),1e-12);
    } BOOST_LOCAL_FUNCTION_NAME(checkOverlap)
    chain.signalOptimizeSiteSuccess.connect(checkOverlap);
    chain.signalChainReset.connect(checkOverlap);
    vector<double> actual_energies; actual_energies.reserve(number_of_levels);
    void BOOST_LOCAL_FUNCTION_PARAMS(
        const bind& chain,
        const bind absolute,
        bind& actual_energies
    ) {
        if(absolute) {
            actual_energies.push_back(abs(chain.getEnergy()));
        } else {
            actual_energies.push_back(chain.getEnergy());
        }
    } BOOST_LOCAL_FUNCTION_NAME(postEnergy)
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
}
//@+node:gcross.20110902221314.2690: ** Stress
TEST_SUITE(Stress) {

//@+others
//@+node:gcross.20110902221314.2692: *3* external field
TEST_SUITE(external_field) {

    void runTest(
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
    }

    TEST_SUITE(physical_dimension_2) {
        TEST_CASE(1_site) { runTest(2,1,list_of(0)(1)); }
        TEST_CASE(2_sites) { runTest(2,2,list_of(0)(1)(1)(2)); }
        TEST_CASE(4_sites) { runTest(2,4,list_of(0)(1)(1)(1)(1)(2)(2)(2)(2)(2)(2)(3)(3)(3)(3)(4)); }
    }
    TEST_SUITE(physical_dimension_3) {
        TEST_CASE(1_site) { runTest(3,1,list_of(0)(1)(2)); }
        TEST_CASE(2_sites) { runTest(3,2,list_of(0)(1)(1)(2)(2)(2)(3)(3)(4)); }
        TEST_CASE(3_sites) { runTest(3,3,list_of(0)(1)(1)(1)(2)(2)(2)(2)(2)(2)(3)(3)(3)); }
    }

}
//@+node:gcross.20110902221314.2693: *3* transverse Ising model
TEST_SUITE(transverse_Ising_model) {

    void runTest(
          unsigned int const number_of_sites
        , double coupling_strength
        , OptimizerMode const& optimizer_mode
        , vector<double> const& correct_energies
        , double sanity_check_threshold = Chain::defaults.sanity_check_threshold
    ) {
        Chain chain(
            constructTransverseIsingModelOperator(number_of_sites,coupling_strength),
            ChainOptions()
                .setOptimizerMode(optimizer_mode)
                .setInitialBandwidthDimension(2u)
                .setSiteConvergenceThreshold(1e-10)
                .setSweepConvergenceThreshold(1e-9)
                .setChainConvergenceThreshold(1e-9)
                .setSanityCheckThreshold(sanity_check_threshold)
        );
        checkEnergies(
            chain,
            correct_energies,
            1e-7,
            optimizer_mode == OptimizerMode::largest_magnitude
        );
    }

    TEST_SUITE(least_value) {
        OptimizerMode const& mode = OptimizerMode::least_value;
        TEST_CASE( 6_sites_0p1) { runTest( 6,0.1,mode,list_of
            (-6.012504691)
            (-4.1912659256)
            (-4.13264494449)
            (-4.0501210912)
        ); }
        TEST_CASE(10_sites_0p1) { runTest(10,0.1,mode,list_of
            (-10.022510957)
            (-8.2137057257)
            (-8.18819723717)
            (-8.1485537719)
        ); }
        TEST_CASE(10_sites_0p5) { runTest(10,0.5,mode,list_of
            (-10.569659557)
            (-9.5030059614)
            (-9.32268792732)
            (-9.0714705801)
        ); }
        TEST_CASE(10_sites_2p0) { runTest(10,2.0,mode,list_of
            (-19.531007915)
            (-19.5280782081)
            (-17.3076728844)
            (-17.3047431766)
        ); }
    }
    TEST_SUITE(greatest_value) {
        OptimizerMode const& mode = OptimizerMode::greatest_value;
        TEST_CASE( 6_sites_0p1) { runTest( 6,0.1,mode,list_of
            (6.012504691)
            (4.1912659256)
            (4.13264494449)
            (4.0501210912)
        ); }
        TEST_CASE(10_sites_0p1) { runTest(10,0.1,mode,list_of
            (10.022510957)
            (8.2137057257)
            (8.18819723717)
            (8.1485537719)
        ); }
        TEST_CASE(10_sites_0p5) { runTest(10,0.5,mode,list_of
            (10.569659557)
            (9.5030059614)
            (9.32268792732)
            (9.0714705801)
        ); }
        TEST_CASE(10_sites_2p0) { runTest(10,2.0,mode,list_of
            (19.531007915)
            (19.5280782081)
            (17.3076728844)
            (17.3047431766)
        ); }
    }
    TEST_SUITE(largest_magnitude) {
        OptimizerMode const& mode = OptimizerMode::largest_magnitude;
        TEST_CASE( 6_sites_0p1) { runTest( 6,0.1,mode,list_of
            (6.012504691)
            (6.012504691)
            (4.1912659256)
        ); }
        TEST_CASE(10_sites_0p1) { runTest(10,0.1,mode,list_of
            (10.022510957)
            (10.022510957)
            (8.2137057257)
        ,   1e-10
        ); }
        TEST_CASE(10_sites_0p5) { runTest(10,0.5,mode,list_of
            (10.569659557)
            (10.569659557)
            (9.5030059614)
        ,   1e-10
        ); }
        TEST_CASE(10_sites_2p0) { runTest(10,2.0,mode,list_of
            (19.531007915)
            (19.531007915)
            (19.5280782081)
        ,   1e-10
        ); }
    }
}
//@-others

}
//@-others
//@-leo
