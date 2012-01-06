# Imports {{{
from .._2d.enumerations import *
from .._2d.infinite.grids import *
from .._2d.infinite.environment import *
from .._2d.tensors import *
from ..qubit import *
from ..utils import *

from . import *
from .test_2d_environment import randomNormalizationEnvironment, randomExpectationEnvironment

from random import choice
# }}}

# Helper functions {{{

def randomInfiniteNormalizationEnvironment():
    return randomNormalizationEnvironment(InfiniteNormalizationEnvironment)

def randomInfiniteExpectationEnvironment():
    return randomExpectationEnvironment(InfiniteExpectationEnvironment)

# }}}

class TestExpectationBehaviour(TestCase): # {{{
    @with_checker
    def test_trivial(self,directions=[Direction]): # {{{
        environment = InfiniteExpectationEnvironment(
            operator_grid = OperatorGrid.trivial(),
            state_grid = StateGrid.trivial(),
        )
        for direction in directions:
            environment.contract(direction)
        self.assertEqual(environment.computeNormalization(),1)
        self.assertEqual(environment.computeExpectation(),1)
    # }}}
# }}}

class TestInfiniteNormalizationEnvironment(TestCase): # {{{
    def test_computeNormalization_observable(self): # {{{
        for physical_dimension in range(1,5):
            self.assertAlmostEqual(InfiniteNormalizationEnvironment(physical_dimension).computeNormalization(),1)
    # }}}

    def test_computeNormalizationConditionNumber_observable(self): # {{{
        for physical_dimension in range(1,5):
            self.assertAlmostEqual(InfiniteNormalizationEnvironment(physical_dimension).computeNormalizationConditionNumber(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_computeNormalizationConditionNumber_post_contract(self, # {{{
        physical_dimension = irange(1,5),
        number_of_contractions = irange(0,5),
    ):
        environment = InfiniteNormalizationEnvironment(physical_dimension)
        for _ in range(number_of_contractions):
            environment.contract(randint(0,3))
        self.assertAlmostEqual(environment.computeNormalizationConditionNumber(),1)
    # }}}

    def test_computeNormalizationMatrix_observable(self): # {{{
        for physical_dimension in range(1,5):
            self.assertAllClose(InfiniteNormalizationEnvironment(physical_dimension).computeNormalizationMatrix(),identity(physical_dimension))
    # }}}

    @with_checker(number_of_calls=100)
    def test_contract(self,direction=Direction): # {{{
        environment = randomInfiniteNormalizationEnvironment()
        sides = copy(environment.sides)
        corners = copy(environment.corners)
        center = environment.center
        corners[direction] = corners[direction].absorbSideSiteAtCounterClockwise(sides[CCW(direction)])
        corners[CW(direction)] = corners[CW(direction)].absorbSideSiteAtClockwise(sides[CW(direction)])
        sides[direction] = sides[direction].absorbCenterSite(center,direction)
        environment.contract(direction)
        for correct_side, actual_side in zip(sides,environment.sides):
            self.assertAllClose(correct_side.data,actual_side.data)
        for correct_corner, actual_corner in zip(corners,environment.corners):
            self.assertAllClose(correct_corner.data,actual_corner.data)
    # }}}

    @with_checker
    def test_increaseAxialBandwidthDimensionsBy(self, # {{{
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        environment = randomInfiniteNormalizationEnvironment()

        bandwidth_dimensions = list(environment.bandwidthDimensions())
        bandwidth_dimensions[direction] += increment
        bandwidth_dimensions[OPP(direction)] += increment

        environment.sides[direction] = \
            ensurePhysicalDimensionSufficientlyLarge(
                environment.sides[direction],
                StateSideSite.inward_index,
                bandwidth_dimensions[direction]
            )
        environment.sides[OPP(direction)] = \
            ensurePhysicalDimensionSufficientlyLarge(
                environment.sides[OPP(direction)],
                StateSideSite.inward_index,
                bandwidth_dimensions[OPP(direction)]
            )

        old_normalization = environment.computeNormalization()

        environment.increaseAxialBandwidthDimensionsBy(increment,direction)

        self.assertEqual(environment.bandwidthDimensions(),environment.bandwidthDimensions())
        self.assertAlmostEqual(old_normalization,environment.computeNormalization())
    # }}}

    @with_checker
    def test_increaseAxialBandwidthDimensionsTo(self, # {{{
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        environment = randomInfiniteNormalizationEnvironment()

        bandwidth_dimensions = list(environment.bandwidthDimensions())
        bandwidth_dimensions[direction] = max(bandwidth_dimensions[direction],bandwidth_dimensions[OPP(direction)])
        bandwidth_dimensions[direction] += increment
        bandwidth_dimensions[OPP(direction)] = bandwidth_dimensions[direction]

        environment.sides[direction] = \
            ensurePhysicalDimensionSufficientlyLarge(
                environment.sides[direction],
                StateSideSite.inward_index,
                bandwidth_dimensions[direction]
            )
        environment.sides[OPP(direction)] = \
            ensurePhysicalDimensionSufficientlyLarge(
                environment.sides[OPP(direction)],
                StateSideSite.inward_index,
                bandwidth_dimensions[OPP(direction)]
            )

        old_normalization = environment.computeNormalization()

        environment.increaseAxialBandwidthDimensionsTo(bandwidth_dimensions[direction],direction)

        self.assertEqual(environment.bandwidthDimensions(),environment.bandwidthDimensions())
        self.assertAlmostEqual(old_normalization,environment.computeNormalization())
    # }}}

    @with_checker
    def test_increaseSingleDirectionBandwidthDimensionBy(self, # {{{
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        environment = randomInfiniteNormalizationEnvironment()

        bandwidth_dimensions = list(environment.bandwidthDimensions())
        bandwidth_dimensions[direction] += increment

        environment.sides[direction] = \
            ensurePhysicalDimensionSufficientlyLarge(
                environment.sides[direction],
                StateSideSite.inward_index,
                bandwidth_dimensions[direction]
            )

        old_normalization = environment.computeNormalization()

        environment.increaseSingleDirectionBandwidthDimensionBy(increment,direction)

        self.assertEqual(environment.bandwidthDimensions(),environment.bandwidthDimensions())
        self.assertAlmostEqual(old_normalization,environment.computeNormalization())
    # }}}

    @with_checker
    def test_increaseSingleDirectionBandwidthDimensionTo(self, # {{{
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        environment = randomInfiniteNormalizationEnvironment()

        bandwidth_dimensions = list(environment.bandwidthDimensions())
        bandwidth_dimensions[direction] += increment

        environment.sides[direction] = \
            ensurePhysicalDimensionSufficientlyLarge(
                environment.sides[direction],
                StateSideSite.inward_index,
                bandwidth_dimensions[direction]
            )

        old_normalization = environment.computeNormalization()

        environment.increaseSingleDirectionBandwidthDimensionTo(bandwidth_dimensions[direction],direction)

        self.assertEqual(environment.bandwidthDimensions(),environment.bandwidthDimensions())
        self.assertAlmostEqual(old_normalization,environment.computeNormalization())
    # }}}
# }}}

class TestInfiniteExpectationEnvironment(TestCase): # {{{
    @with_checker(number_of_calls=100)
    def test_contract(self,direction=Direction): # {{{
        environment = randomInfiniteExpectationEnvironment()
        sides = copy(environment.sides)
        corners = copy(environment.corners)
        center = environment.center
        O_sides = copy(environment.O_sides)
        O_corners = copy(environment.O_corners)
        O_center = copy(environment.O_center)
        corners[direction] = corners[direction].absorbSideSiteAtCounterClockwise(sides[CCW(direction)])
        corners[CW(direction)] = corners[CW(direction)].absorbSideSiteAtClockwise(sides[CW(direction)])
        sides[direction] = sides[direction].absorbCenterSite(center,direction)
        O_corners[direction] = O_corners[direction].absorbSideSiteAtCounterClockwise(O_sides[CCW(direction)])
        O_corners[CW(direction)] = O_corners[CW(direction)].absorbSideSiteAtClockwise(O_sides[CW(direction)])
        O_sides[direction] = O_sides[direction].absorbCenterSite(O_center,direction)
        environment.contract(direction)
        for correct_side, actual_side in zip(sides,environment.sides):
            self.assertAllClose(correct_side.data,actual_side.data)
        for correct_corner, actual_corner in zip(corners,environment.corners):
            self.assertAllClose(correct_corner.data,actual_corner.data)
        for correct_side, actual_side in zip(O_sides,environment.O_sides):
            self.assertAllClose(correct_side.data,actual_side.data)
        for correct_corner, actual_corner in zip(O_corners,environment.O_corners):
            self.assertAllClose(correct_corner.data,actual_corner.data)
    # }}}
# }}}
