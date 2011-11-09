#@+leo-ver=5-thin
#@+node:gcross.20111107154810.1414: * @file test_2d_environment.py
#@+<< Imports >>
#@+node:gcross.20111107154810.1415: ** << Imports >>
from .._2d.environment import *
from .._2d.tensors import *

from . import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111107154810.1417: ** Tests
#@+node:gcross.20111009193003.5265: *3* NormalizationEnvironment
class TestNormalizationEnvironment(TestCase):
    #@+others
    #@+node:gcross.20111013080525.1249: *4* randomNormalizationEnvironment
    @staticmethod
    def randomNormalizationEnvironment():
        environment = NormalizationEnvironment(1)
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
    #@+node:gcross.20111024143336.1335: *4* test_compressConnectionBetweenSideAndCenter_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCenter_threshold_zero(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        old_connection_dimension = environment.sides[direction].inward_dimension
        old_normalization = environment.computeNormalization()
        environment.compressConnectionBetweenSideAndCenter(direction,keep=len)
        self.assertLessEqual(environment.sides[direction].inward_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    #@+node:gcross.20111024143336.1337: *4* test_compressConnectionBetweenSideAndCenter_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCenter_keep_some(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        number_to_keep = randint(1,min(
            product(withoutIndex(environment.sides[direction].data.shape,StateSideSite.inward_index)),
            environment.sides[direction].inward_dimension
        ))
        environment.compressConnectionBetweenSideAndCenter(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,environment.sides[direction].inward_dimension)
        self.assertEqual(number_to_keep,environment.center.bandwidthDimension(direction))
        environment.computeNormalization()
    #@+node:gcross.20111024143336.1339: *4* test_compressConnectionBetweenSideAndCenter_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCenter_threshold_zero(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        old_connection_dimension = environment.sides[direction].inward_dimension
        old_normalization = environment.computeNormalization()
        environment.compressConnectionBetweenSideAndCenter(direction,threshold=0)
        self.assertLessEqual(environment.sides[direction].inward_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    #@+node:gcross.20111024143336.1299: *4* test_compressConnectionBetweenSideAndClockwiseCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        old_connection_dimension = environment.sides[direction].clockwise_dimension
        old_normalization = environment.computeNormalization()
        environment.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=len)
        self.assertLessEqual(environment.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    #@+node:gcross.20111024143336.1305: *4* test_compressConnectionBetweenSideAndClockwiseCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_keep_some(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        number_to_keep = \
            randint(1,
                min(
                    product(withoutIndex(environment.sides[direction].data.shape,StateSideSite.clockwise_index)),
                    product(withoutIndex(environment.corners[CW(direction)].data.shape,StateCornerSite.counterclockwise_index)),
                )
            )
        environment.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,environment.sides[direction].clockwise_dimension)
        self.assertEqual(number_to_keep,environment.corners[CW(direction)].counterclockwise_dimension)
        environment.computeNormalization()
    #@+node:gcross.20111024143336.1303: *4* test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        old_connection_dimension = environment.sides[direction].clockwise_dimension
        old_normalization = environment.computeNormalization()
        environment.compressConnectionBetweenSideAndClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(environment.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    #@+node:gcross.20111022200315.1342: *4* test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        old_connection_dimension = environment.sides[direction].counterclockwise_dimension
        old_normalization = environment.computeNormalization()
        environment.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=len)
        self.assertLessEqual(environment.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    #@+node:gcross.20111024143336.1307: *4* test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_some(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        number_to_keep = \
            randint(1,
                min(
                    product(withoutIndex(environment.sides[direction].data.shape,StateSideSite.counterclockwise_index)),
                    product(withoutIndex(environment.corners[direction].data.shape,StateCornerSite.clockwise_index)),
                )
            )
        environment.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,environment.sides[direction].counterclockwise_dimension)
        self.assertEqual(number_to_keep,environment.corners[direction].clockwise_dimension)
        environment.computeNormalization()
    #@+node:gcross.20111022200315.1340: *4* test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        old_connection_dimension = environment.sides[direction].counterclockwise_dimension
        old_normalization = environment.computeNormalization()
        environment.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(environment.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    #@+node:gcross.20111022200315.1288: *4* test_compressCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_all(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        old_normalization = environment.computeNormalization()
        number_to_keep = product(withoutIndex(environment.corners[direction].data.shape,StateCornerSite.physical_index))
        environment.compressCorner(direction,keep=number_to_keep)
        self.assertEqual(environment.corners[direction].physical_dimension,number_to_keep)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    #@+node:gcross.20111022200315.1290: *4* test_compressCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_some(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        number_to_keep = randint(1,product(withoutIndex(environment.corners[direction].data.shape,StateCornerSite.physical_index)))
        environment.compressCorner(direction,keep=number_to_keep)
        self.assertEqual(environment.corners[direction].physical_dimension,number_to_keep)
    #@+node:gcross.20111022200315.1286: *4* test_compressCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        old_physical_dimension = environment.corners[direction].physical_dimension
        old_normalization = environment.computeNormalization()
        environment.compressCorner(direction,threshold=0)
        self.assertLessEqual(environment.corners[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    #@+node:gcross.20111022200315.1298: *4* test_compressSide_keep_all
    @with_checker(number_of_calls=10)
    def test_compressSide_keep_all(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        old_normalization = environment.computeNormalization()
        number_to_keep = product(withoutIndex(environment.sides[direction].data.shape,StateSideSite.physical_index))
        environment.compressSide(direction,keep=number_to_keep)
        self.assertEqual(environment.sides[direction].physical_dimension,number_to_keep)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    #@+node:gcross.20111022200315.1300: *4* test_compressSide_keep_some
    @with_checker(number_of_calls=10)
    def test_compressSide_keep_some(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        number_to_keep = randint(1,product(withoutIndex(environment.sides[direction].data.shape,StateSideSite.physical_index)))
        environment.compressSide(direction,keep=number_to_keep)
        self.assertEqual(environment.sides[direction].physical_dimension,number_to_keep)
    #@+node:gcross.20111022200315.1294: *4* test_compressSide_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressSide_threshold_zero(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        old_physical_dimension = environment.sides[direction].physical_dimension
        old_normalization = environment.computeNormalization()
        environment.compressSide(direction,threshold=0)
        self.assertLessEqual(environment.sides[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    #@+node:gcross.20111013165152.1231: *4* test_computeNormalization_random
    @with_checker(number_of_calls=10)
    def test_computeNormalization_random(self):
        environment = self.randomNormalizationEnvironment()
        self.assertAlmostEqual(
            self.test_computeNormalization_random.contract(*([environment.center.data,environment.center.data.conj()] + [x.formNormalizationBoundary().data for x in environment.sides + environment.corners])),
            environment.computeNormalization(),
        )

    test_computeNormalization_random.contract = \
        formContractor(
            (['O','O*'] + ['S{}'.format(i) for i in range(4)] + ['C{}'.format(i) for i in range(4)]),
            ([(('S{}'.format(i),NormalizationSideBoundary.counterclockwise_index),('C{}'.format(i),CornerBoundary.clockwise_index)) for i in range(4)]
            +[(('C{}'.format(i),CornerBoundary.counterclockwise_index),('S{}'.format(CCW(i)),NormalizationSideBoundary.clockwise_index)) for i in range(4)]
            +[(('S{}'.format(i),NormalizationSideBoundary.inward_index),('O',StateCenterSite.bandwidthIndex(i))) for i in range(4)]
            +[(('S{}'.format(i),NormalizationSideBoundary.inward_conjugate_index),('O*',StateCenterSite.bandwidthIndex(i))) for i in range(4)]
            +[(('O',StateCenterSite.physical_index),('O*',StateCenterSite.physical_index))]
            ),
            []
        )
    #@+node:gcross.20111013165152.1227: *4* test_computeNormalization_trivial
    def test_computeNormalization_trivial(self):
        for physical_dimension in range(1,5):
            self.assertAlmostEqual(NormalizationEnvironment(physical_dimension).computeNormalization(),1)
    #@+node:gcross.20111013080525.1263: *4* test_computeNormalizationConditionNumber_post_contract
    @with_checker(number_of_calls=10)
    def test_computeNormalizationConditionNumber_post_contract(self,
        physical_dimension = irange(1,5),
        number_of_contractions = irange(0,5),
    ):
        environment = NormalizationEnvironment(physical_dimension)
        for _ in range(number_of_contractions):
            environment.contract(randint(0,3))
        self.assertAlmostEqual(environment.computeNormalizationConditionNumber(),1)
    #@+node:gcross.20111013080525.1261: *4* test_computeNormalizationConditionNumber_trivial
    def test_computeNormalizationConditionNumber_trivial(self):
        for physical_dimension in range(1,5):
            self.assertAlmostEqual(NormalizationEnvironment(physical_dimension).computeNormalizationConditionNumber(),1)
    #@+node:gcross.20111010182600.1199: *4* test_computeNormalizationMatrix_random
    @with_checker(number_of_calls=10)
    def test_computeNormalizationMatrix_random(self):
        environment = self.randomNormalizationEnvironment()
        self.assertAllClose(
            self.test_computeNormalizationMatrix_random.contract(*([identity(environment.physical_dimension)] + [x.formNormalizationBoundary().data for x in environment.sides + environment.corners])),
            environment.computeNormalizationMatrix(),
        )

    test_computeNormalizationMatrix_random.contract = \
        formContractor(
            (['I'] + ['S{}'.format(i) for i in range(4)] + ['C{}'.format(i) for i in range(4)]),
            ([(('S{}'.format(i),NormalizationSideBoundary.counterclockwise_index),('C{}'.format(i),CornerBoundary.clockwise_index)) for i in range(4)]
            +[(('C{}'.format(i),CornerBoundary.counterclockwise_index),('S{}'.format(CCW(i)),NormalizationSideBoundary.clockwise_index)) for i in range(4)]
            ),
            [
                [('I',0)] + [('S{}'.format(i),NormalizationSideBoundary.inward_index) for i in range(4)],
                [('I',1)] + [('S{}'.format(i),NormalizationSideBoundary.inward_conjugate_index) for i in range(4)],
            ]
        )
    #@+node:gcross.20111010182517.1196: *4* test_computeNormalizationMatrix_trivial
    def test_computeNormalizationMatrix_trivial(self):
        for physical_dimension in range(1,5):
            self.assertAllClose(NormalizationEnvironment(physical_dimension).computeNormalizationMatrix(),identity(physical_dimension))
    #@+node:gcross.20111103170337.1388: *4* test_contract
    @with_checker(number_of_calls=100)
    def test_contract(self,direction=irange(0,3)):
        environment = self.randomNormalizationEnvironment()
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
    #@+node:gcross.20111014172511.1244: *4* test_increaseAxialBandwidthDimensionsBy
    @with_checker
    def test_increaseAxialBandwidthDimensionsBy(self,
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()

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
    #@+node:gcross.20111014172511.1246: *4* test_increaseAxialBandwidthDimensionsTo
    @with_checker
    def test_increaseAxialBandwidthDimensionsTo(self,
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()

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
    #@+node:gcross.20111013165152.1225: *4* test_increaseSingleDirectionBandwidthDimensionBy
    @with_checker
    def test_increaseSingleDirectionBandwidthDimensionBy(self,
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()

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
    #@+node:gcross.20111014113710.1241: *4* test_increaseSingleDirectionBandwidthDimensionTo
    @with_checker
    def test_increaseSingleDirectionBandwidthDimensionTo(self,
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()

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
    #@+node:gcross.20111017110141.1273: *4* test_normalizeCornerAndDenormalizeClockwiseSide
    @with_checker(number_of_calls=10)
    def test_normalizeCornerAndDenormalizeClockwiseSide(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        environment.corners[direction] = \
            ensurePhysicalDimensionSufficientlyLargeToNormalize(
                environment.corners[direction],
                StateCornerSite.clockwise_index
            )
        old_normalization = environment.computeNormalization()
        environment.normalizeCornerAndDenormalizeClockwiseSide(direction)
        self.assertNormalized(environment.corners[direction].data,StateCornerSite.clockwise_index)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    #@+node:gcross.20111017110141.1275: *4* test_normalizeCornerAndDenormalizeCounterClockwiseSide
    @with_checker(number_of_calls=10)
    def test_normalizeCornerAndDenormalizeCounterClockwiseSide(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        environment.corners[direction] = \
            ensurePhysicalDimensionSufficientlyLargeToNormalize(
                environment.corners[direction],
                StateCornerSite.counterclockwise_index
            )
        old_normalization = environment.computeNormalization()
        environment.normalizeCornerAndDenormalizeCounterClockwiseSide(direction)
        self.assertNormalized(environment.corners[direction].data,StateCornerSite.counterclockwise_index)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    #@+node:gcross.20111014113710.1231: *4* test_normalizeSide
    @with_checker(number_of_calls=10)
    def test_normalizeSide(self,
        direction = irange(0,3),
    ):
        environment = self.randomNormalizationEnvironment()
        environment.sides[direction] = \
            ensurePhysicalDimensionSufficientlyLargeToNormalize(
                environment.sides[direction],
                StateSideSite.inward_index
            )
        old_normalization = environment.computeNormalization()
        environment.normalizeSide(direction)
        self.assertNormalized(environment.sides[direction].data,StateSideSite.inward_index)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
    #@-others
#@+node:gcross.20111103170337.1389: *3* ExpectationEnvironment
class TestExpectationEnvironment(TestCase):
    #@+others
    #@+node:gcross.20111103170337.1392: *4* randomExpectationEnvironment
    @staticmethod
    def randomExpectationEnvironment():
        environment = TestExpectationEnvironment.trivialExpectationEnvironment()
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
    #@+node:gcross.20111107123047.1382: *4* test_compressConnectionBetweenSideAndClockwiseCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        environment = self.randomExpectationEnvironment()
        old_O_connection_dimension = environment.O_sides[direction].clockwise_dimension
        old_connection_dimension = environment.sides[direction].clockwise_dimension
        old_normalization = environment.computeNormalization()
        old_expectation = environment.computeExpectation()
        environment.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=len)
        self.assertLessEqual(environment.O_sides[direction].clockwise_dimension,old_O_connection_dimension)
        self.assertLessEqual(environment.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/environment.computeExpectation(),1)
    #@+node:gcross.20111107123047.1384: *4* test_compressConnectionBetweenSideAndClockwiseCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_keep_some(self,
        direction = irange(0,3),
    ):
        environment = self.randomExpectationEnvironment()
        number_to_keep = \
            randint(1,
                min(
                    product(withoutIndex(environment.O_sides[direction].data.shape,OperatorSideSite.clockwise_index)),
                    product(withoutIndex(environment.O_corners[CW(direction)].data.shape,OperatorCornerSite.counterclockwise_index)),
                    product(withoutIndex(environment.sides[direction].data.shape,StateSideSite.clockwise_index)),
                    product(withoutIndex(environment.corners[CW(direction)].data.shape,StateCornerSite.counterclockwise_index)),
                ),
            )
        environment.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,environment.O_sides[direction].clockwise_dimension)
        self.assertEqual(number_to_keep,environment.O_corners[CW(direction)].counterclockwise_dimension)
        self.assertEqual(number_to_keep,environment.sides[direction].clockwise_dimension)
        self.assertEqual(number_to_keep,environment.corners[CW(direction)].counterclockwise_dimension)
        environment.computeNormalization()
        environment.computeExpectation()
    #@+node:gcross.20111107123047.1388: *4* test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        environment = self.randomExpectationEnvironment()
        old_O_connection_dimension = environment.O_sides[direction].clockwise_dimension
        old_connection_dimension = environment.sides[direction].clockwise_dimension
        old_normalization = environment.computeNormalization()
        old_expectation = environment.computeExpectation()
        environment.compressConnectionBetweenSideAndClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(environment.O_sides[direction].clockwise_dimension,old_O_connection_dimension)
        self.assertLessEqual(environment.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/environment.computeExpectation(),1)
    #@+node:gcross.20111107123047.1390: *4* test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        environment = self.randomExpectationEnvironment()
        old_O_connection_dimension = environment.O_sides[direction].counterclockwise_dimension
        old_connection_dimension = environment.sides[direction].counterclockwise_dimension
        old_normalization = environment.computeNormalization()
        old_expectation = environment.computeExpectation()
        environment.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=len)
        self.assertLessEqual(environment.O_sides[direction].counterclockwise_dimension,old_O_connection_dimension)
        self.assertLessEqual(environment.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/environment.computeExpectation(),1)
    #@+node:gcross.20111107123047.1394: *4* test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_some(self,
        direction = irange(0,3),
    ):
        environment = self.randomExpectationEnvironment()
        number_to_keep = \
            randint(1,
                min(
                    product(withoutIndex(environment.O_sides[direction].data.shape,OperatorSideSite.counterclockwise_index)),
                    product(withoutIndex(environment.O_corners[direction].data.shape,OperatorCornerSite.clockwise_index)),
                    product(withoutIndex(environment.sides[direction].data.shape,StateSideSite.counterclockwise_index)),
                    product(withoutIndex(environment.corners[direction].data.shape,StateCornerSite.clockwise_index)),
                )
            )
        environment.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,environment.O_sides[direction].counterclockwise_dimension)
        self.assertEqual(number_to_keep,environment.O_corners[direction].clockwise_dimension)
        self.assertEqual(number_to_keep,environment.sides[direction].counterclockwise_dimension)
        self.assertEqual(number_to_keep,environment.corners[direction].clockwise_dimension)
        environment.computeNormalization()
        environment.computeExpectation()
    #@+node:gcross.20111107123047.1392: *4* test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        environment = self.randomExpectationEnvironment()
        old_O_connection_dimension = environment.O_sides[direction].counterclockwise_dimension
        old_connection_dimension = environment.sides[direction].counterclockwise_dimension
        old_normalization = environment.computeNormalization()
        old_expectation = environment.computeExpectation()
        environment.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(environment.O_sides[direction].counterclockwise_dimension,old_O_connection_dimension)
        self.assertLessEqual(environment.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/environment.computeExpectation(),1)
    #@+node:gcross.20111107154810.1388: *4* test_compressCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_all(self,
        direction = irange(0,3),
    ):
        environment = self.randomExpectationEnvironment()
        old_physical_dimension = environment.corners[direction].physical_dimension
        old_normalization = environment.computeNormalization()
        old_expectation = environment.computeExpectation()
        environment.compressCorner(direction,keep=len)
        self.assertLessEqual(environment.corners[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/environment.computeExpectation(),1)
    #@+node:gcross.20111107154810.1390: *4* test_compressCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_some(self,
        direction = irange(0,3),
    ):
        environment = self.randomExpectationEnvironment()
        number_to_keep = randint(1,product(withoutIndex(environment.corners[direction].data.shape,StateCornerSite.physical_index)))
        environment.compressCorner(direction,keep=lambda x: min(number_to_keep,len(x)))
        self.assertLessEqual(environment.corners[direction].physical_dimension,number_to_keep)
    #@+node:gcross.20111107154810.1392: *4* test_compressCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        environment = self.randomExpectationEnvironment()
        old_physical_dimension = environment.corners[direction].physical_dimension
        old_normalization = environment.computeNormalization()
        old_expectation = environment.computeExpectation()
        environment.compressCorner(direction,threshold=0)
        self.assertLessEqual(environment.corners[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/environment.computeExpectation(),1)
    #@+node:gcross.20111107154810.1398: *4* test_compressSide_keep_all
    @with_checker(number_of_calls=10)
    def test_compressSide_keep_all(self,
        direction = irange(0,3),
    ):
        environment = self.randomExpectationEnvironment()
        old_physical_dimension = environment.sides[direction].physical_dimension
        old_normalization = environment.computeNormalization()
        old_expectation = environment.computeExpectation()
        environment.compressSide(direction,keep=len)
        self.assertLessEqual(environment.sides[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/environment.computeExpectation(),1)
    #@+node:gcross.20111107154810.1400: *4* test_compressSide_keep_some
    @with_checker(number_of_calls=10)
    def test_compressSide_keep_some(self,
        direction = irange(0,3),
    ):
        environment = self.randomExpectationEnvironment()
        number_to_keep = randint(1,product(withoutIndex(environment.sides[direction].data.shape,StateSideSite.physical_index)))
        environment.compressSide(direction,keep=lambda x: min(number_to_keep,len(x)))
        self.assertLessEqual(environment.sides[direction].physical_dimension,number_to_keep)
    #@+node:gcross.20111107154810.1402: *4* test_compressSide_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressSide_threshold_zero(self,
        direction = irange(0,3),
    ):
        environment = self.randomExpectationEnvironment()
        old_physical_dimension = environment.sides[direction].physical_dimension
        old_normalization = environment.computeNormalization()
        old_expectation = environment.computeExpectation()
        environment.compressSide(direction,threshold=0)
        self.assertLessEqual(environment.sides[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/environment.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/environment.computeExpectation(),1)
    #@+node:gcross.20111103170337.1403: *4* test_computeExpectation_random
    @with_checker(number_of_calls=100)
    def test_computeExpectation_random(self):
        environment = self.randomExpectationEnvironment()
        self.assertAlmostEqual(
            self.test_computeExpectation_random.contract(*(
                [environment.center.data,environment.O_center.data,environment.center.data.conj()]+
                [o.formExpectationBoundary(s).data for (s,o) in zip(environment.sides + environment.corners,environment.O_sides + environment.O_corners)]
            ))/environment.computeNormalization(),
            environment.computeExpectation(),
        )

    test_computeExpectation_random.contract = \
        formContractor(
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
    #@+node:gcross.20111103170337.1401: *4* test_computeExpectation_trivial
    def test_computeNormalization_trivial(self):
        self.assertAlmostEqual(self.trivialExpectationEnvironment().computeExpectation(),1)
    #@+node:gcross.20111103170337.1399: *4* test_computeExpectationMatrix_random
    @with_checker(number_of_calls=100)
    def test_computeExpectationMatrix_random(self):
        environment = self.randomExpectationEnvironment()
        self.assertAllClose(
            self.test_computeExpectationMatrix_random.contract(*(
                [environment.O_center.data]+
                [o.formExpectationBoundary(s).data for (s,o) in zip(environment.sides + environment.corners,environment.O_sides + environment.O_corners)]
            )),
            environment.computeExpectationMatrix(),
        )

    test_computeExpectationMatrix_random.contract = \
        formContractor(
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
    #@+node:gcross.20111103170337.1396: *4* test_computeExpectationMatrix_trivial
    def test_computeExpectationMatrix_trivial(self):
        self.assertAllClose(self.trivialExpectationEnvironment().computeExpectationMatrix(),identity(1))
    #@+node:gcross.20111103170337.1394: *4* test_contract
    @with_checker(number_of_calls=100)
    def test_contract(self,direction=irange(0,3)):
        environment = self.randomExpectationEnvironment()
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
    #@+node:gcross.20111103170337.1397: *4* trivialExpectationEnvironment
    @staticmethod
    def trivialExpectationEnvironment():
        return ExpectationEnvironment(OperatorCenterSite.trivial(),[OperatorSideSite.trivial()]*4,[OperatorCornerSite.trivial()]*4)
    #@-others
#@-others
#@-leo
