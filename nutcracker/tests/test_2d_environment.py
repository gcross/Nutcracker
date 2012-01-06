# Imports {{{
from .._2d.environment import *
from .._2d.tensors import *

from . import *
# }}}

# Helper functions {{{

def randomNormalizationEnvironment(cls=NormalizationEnvironment): # {{{
    environment = cls.trivial()
    horizontal_dimension = randint(1,3)
    vertical_dimension = randint(1,3)
    environment.center = \
        StateCenterSite.random(
            physical_dimension=randint(1,3),
            rightward_dimension=horizontal_dimension,
            upward_dimension=vertical_dimension,
            leftward_dimension=horizontal_dimension,
            downward_dimension=vertical_dimension,
        )
    environment.sides = [
        StateSideSite.random(
            clockwise_dimension = randint(1,3),
            counterclockwise_dimension = randint(1,3),
            inward_dimension = environment.center.bandwidthDimension(i),
            physical_dimension = randint(1,3),
        )
        for i in range(4)
    ]
    environment.corners = [
        StateCornerSite.random(
            clockwise_dimension = environment.sides[i].counterclockwise_dimension,
            counterclockwise_dimension = environment.sides[CCW(i)].clockwise_dimension,
            physical_dimension = randint(1,3),
        )
        for i in range(4)
    ]
    return environment
# }}}

def randomExpectationEnvironment(cls=ExpectationEnvironment): # {{{
    environment = cls.trivial()
    O_horizontal_dimension = randint(1,2)
    O_vertical_dimension = randint(1,2)
    environment.O_center = \
        OperatorCenterSite.random(
            physical_dimension=randint(1,2),
            rightward_dimension=O_horizontal_dimension,
            upward_dimension=O_vertical_dimension,
            leftward_dimension=O_horizontal_dimension,
            downward_dimension=O_vertical_dimension,
        )
    environment.O_sides = [
        OperatorSideSite.random(
            clockwise_dimension = randint(1,2),
            counterclockwise_dimension = randint(1,2),
            inward_dimension = environment.O_center.bandwidthDimension(i),
            physical_dimension = randint(1,2),
        )
        for i in range(4)
    ]
    environment.O_corners = [
        OperatorCornerSite.random(
            clockwise_dimension = environment.O_sides[i].counterclockwise_dimension,
            counterclockwise_dimension = environment.O_sides[CCW(i)].clockwise_dimension,
            physical_dimension = randint(1,2),
        )
        for i in range(4)
    ]
    horizontal_dimension = randint(1,2)
    vertical_dimension = randint(1,2)
    environment.center = \
        StateCenterSite.random(
            physical_dimension=environment.O_center.physical_dimension,
            rightward_dimension=horizontal_dimension,
            upward_dimension=vertical_dimension,
            leftward_dimension=horizontal_dimension,
            downward_dimension=vertical_dimension,
        )
    environment.sides = [
        StateSideSite.random(
            clockwise_dimension = randint(1,2),
            counterclockwise_dimension = randint(1,2),
            inward_dimension = environment.center.bandwidthDimension(i),
            physical_dimension = environment.O_sides[i].physical_dimension,
        )
        for i in range(4)
    ]
    environment.corners = [
        StateCornerSite.random(
            clockwise_dimension = environment.sides[i].counterclockwise_dimension,
            counterclockwise_dimension = environment.sides[CCW(i)].clockwise_dimension,
            physical_dimension = environment.O_corners[i].physical_dimension,
        )
        for i in range(4)
    ]
    return environment
# }}}

# }}}

class TestNormalizationEnvironment(TestCase): # {{{
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCenter_threshold_zero(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        old_connection_dimension = environment.sides[direction].inward_dimension
        old_normalization = environment.computeNormalization()
        environment.compressConnectionBetweenSideAndCenter(direction,keep=len)
        self.assertLessEqual(environment.sides[direction].inward_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCenter_keep_some(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        number_to_keep = randint(1,min(
            product(withoutIndex(environment.sides[direction].dimensions(),StateSideSite.inward_index)),
            environment.sides[direction].inward_dimension
        ))
        environment.compressConnectionBetweenSideAndCenter(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,environment.sides[direction].inward_dimension)
        self.assertEqual(number_to_keep,environment.center.bandwidthDimension(direction))
        environment.computeNormalization()
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCenter_threshold_zero(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        old_connection_dimension = environment.sides[direction].inward_dimension
        old_normalization = environment.computeNormalization()
        environment.compressConnectionBetweenSideAndCenter(direction,threshold=0)
        self.assertLessEqual(environment.sides[direction].inward_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        old_connection_dimension = environment.sides[direction].clockwise_dimension
        old_normalization = environment.computeNormalization()
        environment.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=len)
        self.assertLessEqual(environment.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_keep_some(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        number_to_keep = \
            randint(1,
                min(
                    product(withoutIndex(environment.sides[direction].dimensions(),StateSideSite.clockwise_index)),
                    product(withoutIndex(environment.corners[CW(direction)].dimensions(),StateCornerSite.counterclockwise_index)),
                )
            )
        environment.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,environment.sides[direction].clockwise_dimension)
        self.assertEqual(number_to_keep,environment.corners[CW(direction)].counterclockwise_dimension)
        environment.computeNormalization()
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        old_connection_dimension = environment.sides[direction].clockwise_dimension
        old_normalization = environment.computeNormalization()
        environment.compressConnectionBetweenSideAndClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(environment.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        old_connection_dimension = environment.sides[direction].counterclockwise_dimension
        old_normalization = environment.computeNormalization()
        environment.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=len)
        self.assertLessEqual(environment.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_some(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        number_to_keep = \
            randint(1,
                min(
                    product(withoutIndex(environment.sides[direction].dimensions(),StateSideSite.counterclockwise_index)),
                    product(withoutIndex(environment.corners[direction].dimensions(),StateCornerSite.clockwise_index)),
                )
            )
        environment.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,environment.sides[direction].counterclockwise_dimension)
        self.assertEqual(number_to_keep,environment.corners[direction].clockwise_dimension)
        environment.computeNormalization()
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        old_connection_dimension = environment.sides[direction].counterclockwise_dimension
        old_normalization = environment.computeNormalization()
        environment.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(environment.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_all(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        old_normalization = environment.computeNormalization()
        number_to_keep = product(withoutIndex(environment.corners[direction].dimensions(),StateCornerSite.physical_index))
        environment.compressCorner(direction,keep=number_to_keep)
        self.assertEqual(environment.corners[direction].physical_dimension,number_to_keep)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_some(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        number_to_keep = randint(1,product(withoutIndex(environment.corners[direction].dimensions(),StateCornerSite.physical_index)))
        environment.compressCorner(direction,keep=number_to_keep)
        self.assertEqual(environment.corners[direction].physical_dimension,number_to_keep)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressCorner_threshold_zero(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        old_physical_dimension = environment.corners[direction].physical_dimension
        old_normalization = environment.computeNormalization()
        environment.compressCorner(direction,threshold=0)
        self.assertLessEqual(environment.corners[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressSide_keep_all(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        old_normalization = environment.computeNormalization()
        number_to_keep = product(withoutIndex(environment.sides[direction].dimensions(),StateSideSite.physical_index))
        environment.compressSide(direction,keep=number_to_keep)
        self.assertEqual(environment.sides[direction].physical_dimension,number_to_keep)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressSide_keep_some(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        number_to_keep = randint(1,product(withoutIndex(environment.sides[direction].dimensions(),StateSideSite.physical_index)))
        environment.compressSide(direction,keep=number_to_keep)
        self.assertEqual(environment.sides[direction].physical_dimension,number_to_keep)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressSide_threshold_zero(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        old_physical_dimension = environment.sides[direction].physical_dimension
        old_normalization = environment.computeNormalization()
        environment.compressSide(direction,threshold=0)
        self.assertLessEqual(environment.sides[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    # }}}

    # test computeNormalization with random data {{{
    @prependContractor(
        (['O','O*'] + ['S{}'.format(i) for i in range(4)] + ['C{}'.format(i) for i in range(4)]),
        ([(('S{}'.format(i),NormalizationSideBoundary.counterclockwise_index),('C{}'.format(i),CornerBoundary.clockwise_index)) for i in range(4)]
        +[(('C{}'.format(i),CornerBoundary.counterclockwise_index),('S{}'.format(CCW(i)),NormalizationSideBoundary.clockwise_index)) for i in range(4)]
        +[(('S{}'.format(i),NormalizationSideBoundary.inward_index),('O',StateCenterSite.bandwidthIndex(i))) for i in range(4)]
        +[(('S{}'.format(i),NormalizationSideBoundary.inward_conjugate_index),('O*',StateCenterSite.bandwidthIndex(i))) for i in range(4)]
        +[(('O',StateCenterSite.physical_index),('O*',StateCenterSite.physical_index))]
        ),
        []
    )
    @with_checker(number_of_calls=10)
    def test_computeNormalization_random(contract,self):
        environment = randomNormalizationEnvironment()
        self.assertAlmostEqual(
            contract(*([environment.center.data,environment.center.data.conj()] + [x.formNormalizationBoundary().data for x in environment.sides + environment.corners])),
            environment.computeNormalization(),
        )
    # }}}

    def test_computeNormalization_trivial(self): # {{{
        self.assertAlmostEqual(NormalizationEnvironment.trivial().computeNormalization(),1)
    # }}}

    def test_computeNormalizationConditionNumber_trivial(self): # {{{
        self.assertAlmostEqual(NormalizationEnvironment.trivial().computeNormalizationConditionNumber(),1)
    # }}}

    # test computeNormalizationMatrix using random data {{{
    @prependContractor(
        (['I'] + ['S{}'.format(i) for i in range(4)] + ['C{}'.format(i) for i in range(4)]),
        ([(('S{}'.format(i),NormalizationSideBoundary.counterclockwise_index),('C{}'.format(i),CornerBoundary.clockwise_index)) for i in range(4)]
        +[(('C{}'.format(i),CornerBoundary.counterclockwise_index),('S{}'.format(CCW(i)),NormalizationSideBoundary.clockwise_index)) for i in range(4)]
        ),
        [
            [('I',0)] + [('S{}'.format(i),NormalizationSideBoundary.inward_index) for i in range(4)],
            [('I',1)] + [('S{}'.format(i),NormalizationSideBoundary.inward_conjugate_index) for i in range(4)],
        ]
    )
    @with_checker(number_of_calls=10)
    def test_computeNormalizationMatrix_random(contract,self):
        environment = randomNormalizationEnvironment()
        self.assertAllClose(
            contract(*([identity(environment.physical_dimension)] + [x.formNormalizationBoundary().data for x in environment.sides + environment.corners])),
            environment.computeNormalizationMatrix(),
        )
    # }}}

    def test_computeNormalizationMatrix_trivial(self): # {{{
        self.assertAllClose(NormalizationEnvironment.trivial().computeNormalizationMatrix(),identity(1))
    # }}}

    @with_checker(number_of_calls=10)
    def test_normalizeCornerAndDenormalizeClockwiseSide(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        environment.corners[direction] = \
            ensurePhysicalDimensionSufficientlyLargeToNormalize(
                environment.corners[direction],
                StateCornerSite.clockwise_index
            )
        old_normalization = environment.computeNormalization()
        environment.normalizeCornerAndDenormalizeClockwiseSide(direction)
        self.assertNormalized(environment.corners[direction].data,StateCornerSite.clockwise_index)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_normalizeCornerAndDenormalizeCounterClockwiseSide(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        environment.corners[direction] = \
            ensurePhysicalDimensionSufficientlyLargeToNormalize(
                environment.corners[direction],
                StateCornerSite.counterclockwise_index
            )
        old_normalization = environment.computeNormalization()
        environment.normalizeCornerAndDenormalizeCounterClockwiseSide(direction)
        self.assertNormalized(environment.corners[direction].data,StateCornerSite.counterclockwise_index)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_normalizeSide(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomNormalizationEnvironment()
        environment.sides[direction] = \
            ensurePhysicalDimensionSufficientlyLargeToNormalize(
                environment.sides[direction],
                StateSideSite.inward_index
            )
        old_normalization = environment.computeNormalization()
        environment.normalizeSide(direction)
        self.assertNormalized(environment.sides[direction].data,StateSideSite.inward_index)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    # }}}
# }}}

class TestExpectationEnvironment(TestCase): # {{{
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomExpectationEnvironment()
        old_O_connection_dimension = environment.O_sides[direction].clockwise_dimension
        old_connection_dimension = environment.sides[direction].clockwise_dimension
        old_normalization = environment.computeNormalization()
        old_expectation = environment.computeExpectation()
        environment.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=len)
        self.assertLessEqual(environment.O_sides[direction].clockwise_dimension,old_O_connection_dimension)
        self.assertLessEqual(environment.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/environment.computeExpectation(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_keep_some(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomExpectationEnvironment()
        number_to_keep = \
            randint(1,
                min(
                    product(withoutIndex(environment.O_sides[direction].dimensions(),OperatorSideSite.clockwise_index)),
                    product(withoutIndex(environment.O_corners[CW(direction)].dimensions(),OperatorCornerSite.counterclockwise_index)),
                    product(withoutIndex(environment.sides[direction].dimensions(),StateSideSite.clockwise_index)),
                    product(withoutIndex(environment.corners[CW(direction)].dimensions(),StateCornerSite.counterclockwise_index)),
                ),
            )
        environment.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,environment.O_sides[direction].clockwise_dimension)
        self.assertEqual(number_to_keep,environment.O_corners[CW(direction)].counterclockwise_dimension)
        self.assertEqual(number_to_keep,environment.sides[direction].clockwise_dimension)
        self.assertEqual(number_to_keep,environment.corners[CW(direction)].counterclockwise_dimension)
        environment.computeNormalization()
        environment.computeExpectation()
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomExpectationEnvironment()
        old_O_connection_dimension = environment.O_sides[direction].clockwise_dimension
        old_connection_dimension = environment.sides[direction].clockwise_dimension
        old_normalization = environment.computeNormalization()
        old_expectation = environment.computeExpectation()
        environment.compressConnectionBetweenSideAndClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(environment.O_sides[direction].clockwise_dimension,old_O_connection_dimension)
        self.assertLessEqual(environment.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/environment.computeExpectation(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomExpectationEnvironment()
        old_O_connection_dimension = environment.O_sides[direction].counterclockwise_dimension
        old_connection_dimension = environment.sides[direction].counterclockwise_dimension
        old_normalization = environment.computeNormalization()
        old_expectation = environment.computeExpectation()
        environment.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=len)
        self.assertLessEqual(environment.O_sides[direction].counterclockwise_dimension,old_O_connection_dimension)
        self.assertLessEqual(environment.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/environment.computeExpectation(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_some(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomExpectationEnvironment()
        number_to_keep = \
            randint(1,
                min(
                    product(withoutIndex(environment.O_sides[direction].dimensions(),OperatorSideSite.counterclockwise_index)),
                    product(withoutIndex(environment.O_corners[direction].dimensions(),OperatorCornerSite.clockwise_index)),
                    product(withoutIndex(environment.sides[direction].dimensions(),StateSideSite.counterclockwise_index)),
                    product(withoutIndex(environment.corners[direction].dimensions(),StateCornerSite.clockwise_index)),
                )
            )
        environment.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,environment.O_sides[direction].counterclockwise_dimension)
        self.assertEqual(number_to_keep,environment.O_corners[direction].clockwise_dimension)
        self.assertEqual(number_to_keep,environment.sides[direction].counterclockwise_dimension)
        self.assertEqual(number_to_keep,environment.corners[direction].clockwise_dimension)
        environment.computeNormalization()
        environment.computeExpectation()
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomExpectationEnvironment()
        old_O_connection_dimension = environment.O_sides[direction].counterclockwise_dimension
        old_connection_dimension = environment.sides[direction].counterclockwise_dimension
        old_normalization = environment.computeNormalization()
        old_expectation = environment.computeExpectation()
        environment.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(environment.O_sides[direction].counterclockwise_dimension,old_O_connection_dimension)
        self.assertLessEqual(environment.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/environment.computeExpectation(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_all(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomExpectationEnvironment()
        old_physical_dimension = environment.corners[direction].physical_dimension
        old_normalization = environment.computeNormalization()
        old_expectation = environment.computeExpectation()
        environment.compressCorner(direction,keep=len)
        self.assertLessEqual(environment.corners[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/environment.computeExpectation(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_some(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomExpectationEnvironment()
        number_to_keep = randint(1,product(withoutIndex(environment.corners[direction].dimensions(),StateCornerSite.physical_index)))
        environment.compressCorner(direction,keep=lambda x: min(number_to_keep,len(x)))
        self.assertLessEqual(environment.corners[direction].physical_dimension,number_to_keep)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressCorner_threshold_zero(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomExpectationEnvironment()
        old_physical_dimension = environment.corners[direction].physical_dimension
        old_normalization = environment.computeNormalization()
        old_expectation = environment.computeExpectation()
        environment.compressCorner(direction,threshold=0)
        self.assertLessEqual(environment.corners[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/environment.computeExpectation(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressSide_keep_all(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomExpectationEnvironment()
        old_physical_dimension = environment.sides[direction].physical_dimension
        old_normalization = environment.computeNormalization()
        old_expectation = environment.computeExpectation()
        environment.compressSide(direction,keep=len)
        self.assertLessEqual(environment.sides[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/environment.computeExpectation(),1)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressSide_keep_some(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomExpectationEnvironment()
        number_to_keep = randint(1,product(withoutIndex(environment.sides[direction].dimensions(),StateSideSite.physical_index)))
        environment.compressSide(direction,keep=lambda x: min(number_to_keep,len(x)))
        self.assertLessEqual(environment.sides[direction].physical_dimension,number_to_keep)
    # }}}

    @with_checker(number_of_calls=10)
    def test_compressSide_threshold_zero(self, # {{{
        direction = irange(0,3),
    ):
        environment = randomExpectationEnvironment()
        old_physical_dimension = environment.sides[direction].physical_dimension
        old_normalization = environment.computeNormalization()
        old_expectation = environment.computeExpectation()
        environment.compressSide(direction,threshold=0)
        self.assertLessEqual(environment.sides[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/environment.computeExpectation(),1)
    # }}}

    # test computeExpectation using random data {{{
    @prependContractor(
        (['S','O','S*']
        +['S{}'.format(i) for i in range(4)]
        +['C{}'.format(i) for i in range(4)]
        ),
        ([(('S{}'.format(i),ExpectationSideBoundary.counterclockwise_index)
          ,('C{}'.format(i),CornerBoundary.clockwise_index)
          ) for i in range(4)
         ]
        +[(('C{}'.format(i),CornerBoundary.counterclockwise_index)
          ,('S{}'.format(CCW(i)),ExpectationSideBoundary.clockwise_index)
          ) for i in range(4)
         ]
        +[(('O',OperatorCenterSite.bandwidthIndex(i))
          ,('S{}'.format(i),ExpectationSideBoundary.inward_operator_index)
          ) for i in range(4)
         ]
        +[(('S',StateCenterSite.bandwidthIndex(i))
          ,('S{}'.format(i),ExpectationSideBoundary.inward_state_index)
          ) for i in range(4)
         ]
        +[(('S*',StateCenterSite.bandwidthIndex(i))
          ,('S{}'.format(i),ExpectationSideBoundary.inward_state_conjugate_index)
          ) for i in range(4)
         ]
        +[(('O',OperatorCenterSite.physical_index)
          ,('S',StateCenterSite.physical_index)
          )
         ,(('O',OperatorCenterSite.physical_conjugate_index)
          ,('S*',StateCenterSite.physical_index)
          )
         ]
        ),
        []
    )
    @with_checker
    def test_computeExpectation_random(contract,self):
        environment = randomExpectationEnvironment()
        self.assertAlmostEqual(
            contract(*(
                [environment.center.data,environment.O_center.data,environment.center.data.conj()]+
                [o.formExpectationBoundary(s).data for (s,o) in zip(environment.sides + environment.corners,environment.O_sides + environment.O_corners)]
            ))/environment.computeNormalization(),
            environment.computeExpectation(),
        )
    # }}}

    def test_computeNormalization_trivial(self): # {{{
        self.assertAlmostEqual(ExpectationEnvironment.trivial().computeExpectation(),1)
    # }}}

    # test computeExpectationMatrix using random data {{{
    @prependContractor(
        (['O']
        +['S{}'.format(i) for i in range(4)]
        +['C{}'.format(i) for i in range(4)]
        ),
        ([(('S{}'.format(i),ExpectationSideBoundary.counterclockwise_index)
          ,('C{}'.format(i),CornerBoundary.clockwise_index)
          ) for i in range(4)
         ]
        +[(('C{}'.format(i),CornerBoundary.counterclockwise_index)
          ,('S{}'.format(CCW(i)),ExpectationSideBoundary.clockwise_index)
          ) for i in range(4)
         ]
        +[(('O',OperatorCenterSite.bandwidthIndex(i))
          ,('S{}'.format(i),ExpectationSideBoundary.inward_operator_index)
          ) for i in range(4)
         ]
        ),
        [
            [('O',OperatorCenterSite.physical_index)] + [('S{}'.format(i),ExpectationSideBoundary.inward_state_index) for i in range(4)],
            [('O',OperatorCenterSite.physical_conjugate_index)] + [('S{}'.format(i),ExpectationSideBoundary.inward_state_conjugate_index) for i in range(4)],
        ]
    )
    @with_checker
    def test_computeExpectationMatrix_random(contract,self):
        environment = randomExpectationEnvironment()
        self.assertAllClose(
            contract(*(
                [environment.O_center.data]+
                [o.formExpectationBoundary(s).data for (s,o) in zip(environment.sides + environment.corners,environment.O_sides + environment.O_corners)]
            )),
            environment.computeExpectationMatrix(),
        )
    # }}}

    def test_computeExpectationMatrix_trivial(self): # {{{
        self.assertAllClose(ExpectationEnvironment.trivial().computeExpectationMatrix(),identity(1))
    # }}}
# }}}
