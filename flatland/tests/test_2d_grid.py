#@+leo-ver=5-thin
#@+node:gcross.20111107154810.1414: * @file test_2d_grid.py
#@+<< Imports >>
#@+node:gcross.20111107154810.1415: ** << Imports >>
from .._2d.grid import *
from .._2d.tensors import *

from . import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111107154810.1417: ** Tests
#@+node:gcross.20111009193003.5265: *3* NormalizationGrid
class TestNormalizationGrid(TestCase):
    #@+others
    #@+node:gcross.20111013080525.1249: *4* randomNormalizationGrid
    @staticmethod
    def randomNormalizationGrid():
        grid = NormalizationGrid(1)
        horizontal_dimension = randint(1,3)
        vertical_dimension = randint(1,3)
        grid.center = \
            StateCenterSite(
                physical_dimension=randint(1,3),
                rightward_dimension=horizontal_dimension,
                upward_dimension=vertical_dimension,
                leftward_dimension=horizontal_dimension,
                downward_dimension=vertical_dimension,
                randomize=True,
            )
        grid.sides = [
            StateSideSite(
                clockwise_dimension = randint(1,3),
                counterclockwise_dimension = randint(1,3),
                inward_dimension = grid.center.bandwidthDimension(i),
                physical_dimension = randint(1,3),
                randomize = True
            )
            for i in range(4)
        ]
        grid.corners = [
            StateCornerSite(
                clockwise_dimension = grid.sides[i].counterclockwise_dimension,
                counterclockwise_dimension = grid.sides[CCW(i)].clockwise_dimension,
                physical_dimension = randint(1,3),
                randomize = True
            )
            for i in range(4)
        ]
        return grid
    #@+node:gcross.20111024143336.1335: *4* test_compressConnectionBetweenSideAndCenter_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCenter_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_connection_dimension = grid.sides[direction].inward_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndCenter(direction,keep=len)
        self.assertLessEqual(grid.sides[direction].inward_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111024143336.1337: *4* test_compressConnectionBetweenSideAndCenter_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCenter_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        number_to_keep = randint(1,min(
            product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.inward_index)),
            grid.sides[direction].inward_dimension
        ))
        grid.compressConnectionBetweenSideAndCenter(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,grid.sides[direction].inward_dimension)
        self.assertEqual(number_to_keep,grid.center.bandwidthDimension(direction))
        grid.computeNormalization()
    #@+node:gcross.20111024143336.1339: *4* test_compressConnectionBetweenSideAndCenter_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCenter_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_connection_dimension = grid.sides[direction].inward_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndCenter(direction,threshold=0)
        self.assertLessEqual(grid.sides[direction].inward_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111024143336.1299: *4* test_compressConnectionBetweenSideAndClockwiseCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_connection_dimension = grid.sides[direction].clockwise_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=len)
        self.assertLessEqual(grid.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111024143336.1305: *4* test_compressConnectionBetweenSideAndClockwiseCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        number_to_keep = \
            randint(1,
                min(
                    product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.clockwise_index)),
                    product(withoutIndex(grid.corners[CW(direction)].data.shape,StateCornerSite.counterclockwise_index)),
                )
            )
        grid.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,grid.sides[direction].clockwise_dimension)
        self.assertEqual(number_to_keep,grid.corners[CW(direction)].counterclockwise_dimension)
        grid.computeNormalization()
    #@+node:gcross.20111024143336.1303: *4* test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_connection_dimension = grid.sides[direction].clockwise_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(grid.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111022200315.1342: *4* test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_connection_dimension = grid.sides[direction].counterclockwise_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=len)
        self.assertLessEqual(grid.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111024143336.1307: *4* test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        number_to_keep = \
            randint(1,
                min(
                    product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.counterclockwise_index)),
                    product(withoutIndex(grid.corners[direction].data.shape,StateCornerSite.clockwise_index)),
                )
            )
        grid.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,grid.sides[direction].counterclockwise_dimension)
        self.assertEqual(number_to_keep,grid.corners[direction].clockwise_dimension)
        grid.computeNormalization()
    #@+node:gcross.20111022200315.1340: *4* test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_connection_dimension = grid.sides[direction].counterclockwise_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(grid.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111022200315.1288: *4* test_compressCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_all(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_normalization = grid.computeNormalization()
        number_to_keep = product(withoutIndex(grid.corners[direction].data.shape,StateCornerSite.physical_index))
        grid.compressCorner(direction,keep=number_to_keep)
        self.assertEqual(grid.corners[direction].physical_dimension,number_to_keep)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111022200315.1290: *4* test_compressCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        number_to_keep = randint(1,product(withoutIndex(grid.corners[direction].data.shape,StateCornerSite.physical_index)))
        grid.compressCorner(direction,keep=number_to_keep)
        self.assertEqual(grid.corners[direction].physical_dimension,number_to_keep)
    #@+node:gcross.20111022200315.1286: *4* test_compressCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_physical_dimension = grid.corners[direction].physical_dimension
        old_normalization = grid.computeNormalization()
        grid.compressCorner(direction,threshold=0)
        self.assertLessEqual(grid.corners[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111022200315.1298: *4* test_compressSide_keep_all
    @with_checker(number_of_calls=10)
    def test_compressSide_keep_all(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_normalization = grid.computeNormalization()
        number_to_keep = product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.physical_index))
        grid.compressSide(direction,keep=number_to_keep)
        self.assertEqual(grid.sides[direction].physical_dimension,number_to_keep)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111022200315.1300: *4* test_compressSide_keep_some
    @with_checker(number_of_calls=10)
    def test_compressSide_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        number_to_keep = randint(1,product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.physical_index)))
        grid.compressSide(direction,keep=number_to_keep)
        self.assertEqual(grid.sides[direction].physical_dimension,number_to_keep)
    #@+node:gcross.20111022200315.1294: *4* test_compressSide_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressSide_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_physical_dimension = grid.sides[direction].physical_dimension
        old_normalization = grid.computeNormalization()
        grid.compressSide(direction,threshold=0)
        self.assertLessEqual(grid.sides[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111013165152.1231: *4* test_computeNormalization_random
    @with_checker(number_of_calls=10)
    def test_computeNormalization_random(self):
        grid = self.randomNormalizationGrid()
        self.assertAlmostEqual(
            self.test_computeNormalization_random.contract(*([grid.center.data,grid.center.data.conj()] + [x.formNormalizationBoundary().data for x in grid.sides + grid.corners])),
            grid.computeNormalization(),
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
            self.assertAlmostEqual(NormalizationGrid(physical_dimension).computeNormalization(),1)
    #@+node:gcross.20111013080525.1263: *4* test_computeNormalizationConditionNumber_post_contract
    @with_checker(number_of_calls=10)
    def test_computeNormalizationConditionNumber_post_contract(self,
        physical_dimension = irange(1,5),
        number_of_contractions = irange(0,5),
    ):
        grid = NormalizationGrid(physical_dimension)
        for _ in range(number_of_contractions):
            grid.contract(randint(0,3))
        self.assertAlmostEqual(grid.computeNormalizationConditionNumber(),1)
    #@+node:gcross.20111013080525.1261: *4* test_computeNormalizationConditionNumber_trivial
    def test_computeNormalizationConditionNumber_trivial(self):
        for physical_dimension in range(1,5):
            self.assertAlmostEqual(NormalizationGrid(physical_dimension).computeNormalizationConditionNumber(),1)
    #@+node:gcross.20111010182600.1199: *4* test_computeNormalizationMatrix_random
    @with_checker(number_of_calls=10)
    def test_computeNormalizationMatrix_random(self):
        grid = self.randomNormalizationGrid()
        self.assertAllClose(
            self.test_computeNormalizationMatrix_random.contract(*([identity(grid.physical_dimension)] + [x.formNormalizationBoundary().data for x in grid.sides + grid.corners])),
            grid.computeNormalizationMatrix(),
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
            self.assertAllClose(NormalizationGrid(physical_dimension).computeNormalizationMatrix(),identity(physical_dimension))
    #@+node:gcross.20111103170337.1388: *4* test_contract
    @with_checker(number_of_calls=100)
    def test_contract(self,direction=irange(0,3)):
        grid = self.randomNormalizationGrid()
        sides = copy(grid.sides)
        corners = copy(grid.corners)
        center = grid.center
        corners[direction] = corners[direction].absorbSideSiteAtCounterClockwise(sides[CCW(direction)])
        corners[CW(direction)] = corners[CW(direction)].absorbSideSiteAtClockwise(sides[CW(direction)])
        sides[direction] = sides[direction].absorbCenterSite(center,direction)
        grid.contract(direction)
        for correct_side, actual_side in zip(sides,grid.sides):
            self.assertAllClose(correct_side.data,actual_side.data)
        for correct_corner, actual_corner in zip(corners,grid.corners):
            self.assertAllClose(correct_corner.data,actual_corner.data)
    #@+node:gcross.20111014172511.1244: *4* test_increaseAxialBandwidthDimensionsBy
    @with_checker
    def test_increaseAxialBandwidthDimensionsBy(self,
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()

        bandwidth_dimensions = list(grid.bandwidthDimensions())
        bandwidth_dimensions[direction] += increment
        bandwidth_dimensions[OPP(direction)] += increment

        grid.sides[direction] = \
            ensurePhysicalDimensionSufficientlyLarge(
                grid.sides[direction],
                StateSideSite.inward_index,
                bandwidth_dimensions[direction]
            )
        grid.sides[OPP(direction)] = \
            ensurePhysicalDimensionSufficientlyLarge(
                grid.sides[OPP(direction)],
                StateSideSite.inward_index,
                bandwidth_dimensions[OPP(direction)]
            )

        old_normalization = grid.computeNormalization()

        grid.increaseAxialBandwidthDimensionsBy(increment,direction)

        self.assertEqual(grid.bandwidthDimensions(),grid.bandwidthDimensions())
        self.assertAlmostEqual(old_normalization,grid.computeNormalization())
    #@+node:gcross.20111014172511.1246: *4* test_increaseAxialBandwidthDimensionsTo
    @with_checker
    def test_increaseAxialBandwidthDimensionsTo(self,
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()

        bandwidth_dimensions = list(grid.bandwidthDimensions())
        bandwidth_dimensions[direction] = max(bandwidth_dimensions[direction],bandwidth_dimensions[OPP(direction)])
        bandwidth_dimensions[direction] += increment
        bandwidth_dimensions[OPP(direction)] = bandwidth_dimensions[direction]

        grid.sides[direction] = \
            ensurePhysicalDimensionSufficientlyLarge(
                grid.sides[direction],
                StateSideSite.inward_index,
                bandwidth_dimensions[direction]
            )
        grid.sides[OPP(direction)] = \
            ensurePhysicalDimensionSufficientlyLarge(
                grid.sides[OPP(direction)],
                StateSideSite.inward_index,
                bandwidth_dimensions[OPP(direction)]
            )

        old_normalization = grid.computeNormalization()

        grid.increaseAxialBandwidthDimensionsTo(bandwidth_dimensions[direction],direction)

        self.assertEqual(grid.bandwidthDimensions(),grid.bandwidthDimensions())
        self.assertAlmostEqual(old_normalization,grid.computeNormalization())
    #@+node:gcross.20111013165152.1225: *4* test_increaseSingleDirectionBandwidthDimensionBy
    @with_checker
    def test_increaseSingleDirectionBandwidthDimensionBy(self,
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()

        bandwidth_dimensions = list(grid.bandwidthDimensions())
        bandwidth_dimensions[direction] += increment

        grid.sides[direction] = \
            ensurePhysicalDimensionSufficientlyLarge(
                grid.sides[direction],
                StateSideSite.inward_index,
                bandwidth_dimensions[direction]
            )

        old_normalization = grid.computeNormalization()

        grid.increaseSingleDirectionBandwidthDimensionBy(increment,direction)

        self.assertEqual(grid.bandwidthDimensions(),grid.bandwidthDimensions())
        self.assertAlmostEqual(old_normalization,grid.computeNormalization())
    #@+node:gcross.20111014113710.1241: *4* test_increaseSingleDirectionBandwidthDimensionTo
    @with_checker
    def test_increaseSingleDirectionBandwidthDimensionTo(self,
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()

        bandwidth_dimensions = list(grid.bandwidthDimensions())
        bandwidth_dimensions[direction] += increment

        grid.sides[direction] = \
            ensurePhysicalDimensionSufficientlyLarge(
                grid.sides[direction],
                StateSideSite.inward_index,
                bandwidth_dimensions[direction]
            )

        old_normalization = grid.computeNormalization()

        grid.increaseSingleDirectionBandwidthDimensionTo(bandwidth_dimensions[direction],direction)

        self.assertEqual(grid.bandwidthDimensions(),grid.bandwidthDimensions())
        self.assertAlmostEqual(old_normalization,grid.computeNormalization())
    #@+node:gcross.20111017110141.1273: *4* test_normalizeCornerAndDenormalizeClockwiseSide
    @with_checker(number_of_calls=10)
    def test_normalizeCornerAndDenormalizeClockwiseSide(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        grid.corners[direction] = \
            ensurePhysicalDimensionSufficientlyLargeToNormalize(
                grid.corners[direction],
                StateCornerSite.clockwise_index
            )
        old_normalization = grid.computeNormalization()
        grid.normalizeCornerAndDenormalizeClockwiseSide(direction)
        self.assertIsNormalized(grid.corners[direction].data,StateCornerSite.clockwise_index)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111017110141.1275: *4* test_normalizeCornerAndDenormalizeCounterClockwiseSide
    @with_checker(number_of_calls=10)
    def test_normalizeCornerAndDenormalizeCounterClockwiseSide(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        grid.corners[direction] = \
            ensurePhysicalDimensionSufficientlyLargeToNormalize(
                grid.corners[direction],
                StateCornerSite.counterclockwise_index
            )
        old_normalization = grid.computeNormalization()
        grid.normalizeCornerAndDenormalizeCounterClockwiseSide(direction)
        self.assertIsNormalized(grid.corners[direction].data,StateCornerSite.counterclockwise_index)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111014113710.1231: *4* test_normalizeSide
    @with_checker(number_of_calls=10)
    def test_normalizeSide(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        grid.sides[direction] = \
            ensurePhysicalDimensionSufficientlyLargeToNormalize(
                grid.sides[direction],
                StateSideSite.inward_index
            )
        old_normalization = grid.computeNormalization()
        grid.normalizeSide(direction)
        self.assertIsNormalized(grid.sides[direction].data,StateSideSite.inward_index)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@-others
#@+node:gcross.20111103170337.1389: *3* ExpectationGrid
class TestExpectationGrid(TestCase):
    #@+others
    #@+node:gcross.20111103170337.1392: *4* randomExpectationGrid
    @staticmethod
    def randomExpectationGrid():
        grid = TestExpectationGrid.trivialExpectationGrid()
        O_horizontal_dimension = randint(1,2)
        O_vertical_dimension = randint(1,2)
        grid.O_center = \
            OperatorCenterSite(
                physical_dimension=randint(1,2),
                rightward_dimension=O_horizontal_dimension,
                upward_dimension=O_vertical_dimension,
                leftward_dimension=O_horizontal_dimension,
                downward_dimension=O_vertical_dimension,
                randomize=True,
            )
        grid.O_sides = [
            OperatorSideSite(
                clockwise_dimension = randint(1,2),
                counterclockwise_dimension = randint(1,2),
                inward_dimension = grid.O_center.bandwidthDimension(i),
                physical_dimension = randint(1,2),
                randomize = True
            )
            for i in range(4)
        ]
        grid.O_corners = [
            OperatorCornerSite(
                clockwise_dimension = grid.O_sides[i].counterclockwise_dimension,
                counterclockwise_dimension = grid.O_sides[CCW(i)].clockwise_dimension,
                physical_dimension = randint(1,2),
                randomize = True
            )
            for i in range(4)
        ]
        horizontal_dimension = randint(1,2)
        vertical_dimension = randint(1,2)
        grid.center = \
            StateCenterSite(
                physical_dimension=grid.O_center.physical_dimension,
                rightward_dimension=horizontal_dimension,
                upward_dimension=vertical_dimension,
                leftward_dimension=horizontal_dimension,
                downward_dimension=vertical_dimension,
                randomize=True,
            )
        grid.sides = [
            StateSideSite(
                clockwise_dimension = randint(1,2),
                counterclockwise_dimension = randint(1,2),
                inward_dimension = grid.center.bandwidthDimension(i),
                physical_dimension = grid.O_sides[i].physical_dimension,
                randomize = True
            )
            for i in range(4)
        ]
        grid.corners = [
            StateCornerSite(
                clockwise_dimension = grid.sides[i].counterclockwise_dimension,
                counterclockwise_dimension = grid.sides[CCW(i)].clockwise_dimension,
                physical_dimension = grid.O_corners[i].physical_dimension,
                randomize = True
            )
            for i in range(4)
        ]
        return grid
    #@+node:gcross.20111107123047.1382: *4* test_compressConnectionBetweenSideAndClockwiseCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        old_O_connection_dimension = grid.O_sides[direction].clockwise_dimension
        old_connection_dimension = grid.sides[direction].clockwise_dimension
        old_normalization = grid.computeNormalization()
        old_expectation = grid.computeExpectation()
        grid.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=len)
        self.assertLessEqual(grid.O_sides[direction].clockwise_dimension,old_O_connection_dimension)
        self.assertLessEqual(grid.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/grid.computeExpectation(),1)
    #@+node:gcross.20111107123047.1384: *4* test_compressConnectionBetweenSideAndClockwiseCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        number_to_keep = \
            randint(1,
                min(
                    product(withoutIndex(grid.O_sides[direction].data.shape,OperatorSideSite.clockwise_index)),
                    product(withoutIndex(grid.O_corners[CW(direction)].data.shape,OperatorCornerSite.counterclockwise_index)),
                    product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.clockwise_index)),
                    product(withoutIndex(grid.corners[CW(direction)].data.shape,StateCornerSite.counterclockwise_index)),
                ),
            )
        grid.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,grid.O_sides[direction].clockwise_dimension)
        self.assertEqual(number_to_keep,grid.O_corners[CW(direction)].counterclockwise_dimension)
        self.assertEqual(number_to_keep,grid.sides[direction].clockwise_dimension)
        self.assertEqual(number_to_keep,grid.corners[CW(direction)].counterclockwise_dimension)
        grid.computeNormalization()
        grid.computeExpectation()
    #@+node:gcross.20111107123047.1388: *4* test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        old_O_connection_dimension = grid.O_sides[direction].clockwise_dimension
        old_connection_dimension = grid.sides[direction].clockwise_dimension
        old_normalization = grid.computeNormalization()
        old_expectation = grid.computeExpectation()
        grid.compressConnectionBetweenSideAndClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(grid.O_sides[direction].clockwise_dimension,old_O_connection_dimension)
        self.assertLessEqual(grid.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/grid.computeExpectation(),1)
    #@+node:gcross.20111107123047.1390: *4* test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        old_O_connection_dimension = grid.O_sides[direction].counterclockwise_dimension
        old_connection_dimension = grid.sides[direction].counterclockwise_dimension
        old_normalization = grid.computeNormalization()
        old_expectation = grid.computeExpectation()
        grid.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=len)
        self.assertLessEqual(grid.O_sides[direction].counterclockwise_dimension,old_O_connection_dimension)
        self.assertLessEqual(grid.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/grid.computeExpectation(),1)
    #@+node:gcross.20111107123047.1394: *4* test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        number_to_keep = \
            randint(1,
                min(
                    product(withoutIndex(grid.O_sides[direction].data.shape,OperatorSideSite.counterclockwise_index)),
                    product(withoutIndex(grid.O_corners[direction].data.shape,OperatorCornerSite.clockwise_index)),
                    product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.counterclockwise_index)),
                    product(withoutIndex(grid.corners[direction].data.shape,StateCornerSite.clockwise_index)),
                )
            )
        grid.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,grid.O_sides[direction].counterclockwise_dimension)
        self.assertEqual(number_to_keep,grid.O_corners[direction].clockwise_dimension)
        self.assertEqual(number_to_keep,grid.sides[direction].counterclockwise_dimension)
        self.assertEqual(number_to_keep,grid.corners[direction].clockwise_dimension)
        grid.computeNormalization()
        grid.computeExpectation()
    #@+node:gcross.20111107123047.1392: *4* test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        old_O_connection_dimension = grid.O_sides[direction].counterclockwise_dimension
        old_connection_dimension = grid.sides[direction].counterclockwise_dimension
        old_normalization = grid.computeNormalization()
        old_expectation = grid.computeExpectation()
        grid.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(grid.O_sides[direction].counterclockwise_dimension,old_O_connection_dimension)
        self.assertLessEqual(grid.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/grid.computeExpectation(),1)
    #@+node:gcross.20111107154810.1388: *4* test_compressCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_all(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        old_physical_dimension = grid.corners[direction].physical_dimension
        old_normalization = grid.computeNormalization()
        old_expectation = grid.computeExpectation()
        grid.compressCorner(direction,keep=len)
        self.assertLessEqual(grid.corners[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/grid.computeExpectation(),1)
    #@+node:gcross.20111107154810.1390: *4* test_compressCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        number_to_keep = randint(1,product(withoutIndex(grid.corners[direction].data.shape,StateCornerSite.physical_index)))
        grid.compressCorner(direction,keep=lambda x: min(number_to_keep,len(x)))
        self.assertLessEqual(grid.corners[direction].physical_dimension,number_to_keep)
    #@+node:gcross.20111107154810.1392: *4* test_compressCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        old_physical_dimension = grid.corners[direction].physical_dimension
        old_normalization = grid.computeNormalization()
        old_expectation = grid.computeExpectation()
        grid.compressCorner(direction,threshold=0)
        self.assertLessEqual(grid.corners[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/grid.computeExpectation(),1)
    #@+node:gcross.20111107154810.1398: *4* test_compressSide_keep_all
    @with_checker(number_of_calls=10)
    def test_compressSide_keep_all(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        old_physical_dimension = grid.sides[direction].physical_dimension
        old_normalization = grid.computeNormalization()
        old_expectation = grid.computeExpectation()
        grid.compressSide(direction,keep=len)
        self.assertLessEqual(grid.sides[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/grid.computeExpectation(),1)
    #@+node:gcross.20111107154810.1400: *4* test_compressSide_keep_some
    @with_checker(number_of_calls=10)
    def test_compressSide_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        number_to_keep = randint(1,product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.physical_index)))
        grid.compressSide(direction,keep=lambda x: min(number_to_keep,len(x)))
        self.assertLessEqual(grid.sides[direction].physical_dimension,number_to_keep)
    #@+node:gcross.20111107154810.1402: *4* test_compressSide_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressSide_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        old_physical_dimension = grid.sides[direction].physical_dimension
        old_normalization = grid.computeNormalization()
        old_expectation = grid.computeExpectation()
        grid.compressSide(direction,threshold=0)
        self.assertLessEqual(grid.sides[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/grid.computeExpectation(),1)
    #@+node:gcross.20111103170337.1403: *4* test_computeExpectation_random
    @with_checker(number_of_calls=100)
    def test_computeExpectation_random(self):
        grid = self.randomExpectationGrid()
        self.assertAlmostEqual(
            self.test_computeExpectation_random.contract(*(
                [grid.center.data,grid.O_center.data,grid.center.data.conj()]+
                [o.formExpectationBoundary(s).data for (s,o) in zip(grid.sides + grid.corners,grid.O_sides + grid.O_corners)]
            ))/grid.computeNormalization(),
            grid.computeExpectation(),
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
        self.assertAlmostEqual(self.trivialExpectationGrid().computeExpectation(),1)
    #@+node:gcross.20111103170337.1399: *4* test_computeExpectationMatrix_random
    @with_checker(number_of_calls=100)
    def test_computeExpectationMatrix_random(self):
        grid = self.randomExpectationGrid()
        self.assertAllClose(
            self.test_computeExpectationMatrix_random.contract(*(
                [grid.O_center.data]+
                [o.formExpectationBoundary(s).data for (s,o) in zip(grid.sides + grid.corners,grid.O_sides + grid.O_corners)]
            )),
            grid.computeExpectationMatrix(),
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
        self.assertAllClose(self.trivialExpectationGrid().computeExpectationMatrix(),identity(1))
    #@+node:gcross.20111103170337.1394: *4* test_contract
    @with_checker(number_of_calls=100)
    def test_contract(self,direction=irange(0,3)):
        grid = self.randomExpectationGrid()
        sides = copy(grid.sides)
        corners = copy(grid.corners)
        center = grid.center
        O_sides = copy(grid.O_sides)
        O_corners = copy(grid.O_corners)
        O_center = copy(grid.O_center)
        corners[direction] = corners[direction].absorbSideSiteAtCounterClockwise(sides[CCW(direction)])
        corners[CW(direction)] = corners[CW(direction)].absorbSideSiteAtClockwise(sides[CW(direction)])
        sides[direction] = sides[direction].absorbCenterSite(center,direction)
        O_corners[direction] = O_corners[direction].absorbSideSiteAtCounterClockwise(O_sides[CCW(direction)])
        O_corners[CW(direction)] = O_corners[CW(direction)].absorbSideSiteAtClockwise(O_sides[CW(direction)])
        O_sides[direction] = O_sides[direction].absorbCenterSite(O_center,direction)
        grid.contract(direction)
        for correct_side, actual_side in zip(sides,grid.sides):
            self.assertAllClose(correct_side.data,actual_side.data)
        for correct_corner, actual_corner in zip(corners,grid.corners):
            self.assertAllClose(correct_corner.data,actual_corner.data)
        for correct_side, actual_side in zip(O_sides,grid.O_sides):
            self.assertAllClose(correct_side.data,actual_side.data)
        for correct_corner, actual_corner in zip(O_corners,grid.O_corners):
            self.assertAllClose(correct_corner.data,actual_corner.data)
    #@+node:gcross.20111103170337.1397: *4* trivialExpectationGrid
    @staticmethod
    def trivialExpectationGrid():
        return ExpectationGrid(OperatorCenterSite.trivial(),[OperatorSideSite.trivial()]*4,[OperatorCornerSite.trivial()]*4)
    #@-others
#@-others
#@-leo
