#@+leo-ver=5-thin
#@+node:gcross.20111107154810.1418: * @file test_2d_tensors.py
#@+<< Imports >>
#@+node:gcross.20111107154810.1420: ** << Imports >>
from .._2d.tensors import *

from . import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111009193003.1168: ** Tests
#@+node:gcross.20111103170337.1371: *3* ExpectationSideBoundary
class TestExpectationSideBoundary(TestCase):
    #@+others
    #@+node:gcross.20111103170337.1372: *4* absorbCounterClockwiseCornerBoundary
    @with_checker(number_of_calls=10)
    def test_absorbCounterClockwiseCornerBoundary(self):
        side = \
            ExpectationSideBoundary(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_state_dimension = randint(1,5),
                inward_operator_dimension = randint(1,5),
                randomize = True,
            )
        corner = \
            CornerBoundary(
                clockwise_dimension = side.counterclockwise_dimension,
                counterclockwise_dimension = side.counterclockwise_dimension,
                randomize = True,
            )
        self.assertAllClose(
            self.test_absorbCounterClockwiseCornerBoundary.contract(side.data,corner.data),
            side.absorbCounterClockwiseCornerBoundary(corner).data
        )

    test_absorbCounterClockwiseCornerBoundary.contract = \
        formContractor(
            ['S','C'],
            [
                (('C',CornerBoundary.clockwise_index),('S',ExpectationSideBoundary.counterclockwise_index)),
            ],
            [
                {"clockwise":[('S',ExpectationSideBoundary.clockwise_index)]
                ,"counterclockwise":[('C',CornerBoundary.counterclockwise_index)]
                ,"inward_state":[('S',ExpectationSideBoundary.inward_state_index)]
                ,"inward_operator":[('S',ExpectationSideBoundary.inward_operator_index)]
                ,"inward_state_conjugate":[('S',ExpectationSideBoundary.inward_state_conjugate_index)]
                }[name] for name in ExpectationSideBoundary._dimensions
            ]
        )
    #@+node:gcross.20111103170337.1373: *4* absorbCounterClockwiseSideBoundary
    @with_checker(number_of_calls=10)
    def test_absorbCounterClockwiseSideBoundary(self):
        side1 = \
            ExpectationSideBoundary(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_state_dimension = randint(1,5),
                inward_operator_dimension = randint(1,5),
                randomize = True,
            )
        side2 = \
            ExpectationSideBoundary(
                clockwise_dimension = side1.counterclockwise_dimension,
                counterclockwise_dimension = randint(1,5),
                inward_state_dimension = randint(1,5),
                inward_operator_dimension = randint(1,5),
                randomize = True,
            )
        self.assertAllClose(
            self.test_absorbCounterClockwiseSideBoundary.contract(side1.data,side2.data),
            side1.absorbCounterClockwiseSideBoundary(side2).data
        )

    test_absorbCounterClockwiseSideBoundary.contract = \
        formContractor(
            ['A','B'],
            [
                (('B',ExpectationSideBoundary.clockwise_index),('A',ExpectationSideBoundary.counterclockwise_index)),
            ],
            [
                {"clockwise":[('A',ExpectationSideBoundary.clockwise_index)]
                ,"counterclockwise":[('B',ExpectationSideBoundary.counterclockwise_index)]
                ,"inward_state":[(x,ExpectationSideBoundary.inward_state_index) for x in ('A','B')]
                ,"inward_operator":[(x,ExpectationSideBoundary.inward_operator_index) for x in ('A','B')]
                ,"inward_state_conjugate":[(x,ExpectationSideBoundary.inward_state_conjugate_index) for x in ('A','B')]
                }[name] for name in ExpectationSideBoundary._dimensions
            ]
        )
    #@-others
#@+node:gcross.20111009193003.5248: *3* NormalizationSideBoundary
class TestNormalizationSideBoundary(TestCase):
    #@+others
    #@+node:gcross.20111009193003.5250: *4* absorbCounterClockwiseCornerBoundary
    @with_checker(number_of_calls=10)
    def test_absorbCounterClockwiseCornerBoundary(self):
        side = \
            NormalizationSideBoundary(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        corner = \
            CornerBoundary(
                clockwise_dimension = side.counterclockwise_dimension,
                counterclockwise_dimension = side.counterclockwise_dimension,
                randomize = True,
            )
        self.assertAllClose(
            self.test_absorbCounterClockwiseCornerBoundary.contract(side.data,corner.data),
            side.absorbCounterClockwiseCornerBoundary(corner).data
        )

    test_absorbCounterClockwiseCornerBoundary.contract = \
        formContractor(
            ['S','C'],
            [
                (('C',CornerBoundary.clockwise_index),('S',NormalizationSideBoundary.counterclockwise_index)),
            ],
            [
                {"clockwise":[('S',NormalizationSideBoundary.clockwise_index)]
                ,"counterclockwise":[('C',CornerBoundary.counterclockwise_index)]
                ,"inward":[('S',NormalizationSideBoundary.inward_index)]
                ,"inward_conjugate":[('S',NormalizationSideBoundary.inward_conjugate_index)]
                }[name] for name in NormalizationSideBoundary._dimensions
            ]
        )
    #@+node:gcross.20111009193003.5264: *4* absorbCounterClockwiseSideBoundary
    @with_checker(number_of_calls=10)
    def test_absorbCounterClockwiseSideBoundary(self):
        side1 = \
            NormalizationSideBoundary(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        side2 = \
            NormalizationSideBoundary(
                clockwise_dimension = side1.counterclockwise_dimension,
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        self.assertAllClose(
            self.test_absorbCounterClockwiseSideBoundary.contract(side1.data,side2.data),
            side1.absorbCounterClockwiseSideBoundary(side2).data
        )

    test_absorbCounterClockwiseSideBoundary.contract = \
        formContractor(
            ['A','B'],
            [
                (('B',NormalizationSideBoundary.clockwise_index),('A',NormalizationSideBoundary.counterclockwise_index)),
            ],
            [
                {"clockwise":[('A',NormalizationSideBoundary.clockwise_index)]
                ,"counterclockwise":[('B',NormalizationSideBoundary.counterclockwise_index)]
                ,"inward":[(x,NormalizationSideBoundary.inward_index) for x in ('A','B')]
                ,"inward_conjugate":[(x,NormalizationSideBoundary.inward_conjugate_index) for x in ('A','B')]
                }[name] for name in NormalizationSideBoundary._dimensions
            ]
        )
    #@-others
#@+node:gcross.20111103170337.1353: *3* OperatorCornerSite
class TestOperatorCornerSite(TestCase):
    #@+others
    #@+node:gcross.20111103170337.1357: *4* absorbSideSiteAtClockwise
    @with_checker(number_of_calls=10)
    def test_absorbSideSiteAtClockwise(self):
        corner = \
            OperatorCornerSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                randomize = True,
            )
        site = \
            OperatorSideSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = corner.clockwise_dimension,
                inward_dimension = randint(1,5),
                randomize = True,
            )
        self.assertAllClose(
            self.test_absorbSideSiteAtClockwise.contract(corner.data,site.data),
            corner.absorbSideSiteAtClockwise(site).data
        )

    test_absorbSideSiteAtClockwise.contract = \
        formContractor(
            ['C','S'],
            [
                (('C',OperatorCornerSite.clockwise_index),('S',OperatorSideSite.counterclockwise_index)),
            ],
            [
                {"physical":[('C',OperatorCornerSite.physical_index),('S',OperatorSideSite.physical_index)]
                ,"physical_conjugate":[('C',OperatorCornerSite.physical_conjugate_index),('S',OperatorSideSite.physical_conjugate_index)]
                ,"counterclockwise":[('C',OperatorCornerSite.counterclockwise_index),('S',OperatorSideSite.inward_index)]
                ,"clockwise":[('S',OperatorSideSite.clockwise_index)]
                }[name] for name in OperatorCornerSite._dimensions
            ]
        )
    #@+node:gcross.20111103170337.1361: *4* absorbSideSiteAtCounterClockwise
    @with_checker(number_of_calls=10)
    def test_absorbSideSiteAtCounterClockwise(self):
        corner = \
            OperatorCornerSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                randomize = True,
            )
        site = \
            OperatorSideSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = corner.counterclockwise_dimension,
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        self.assertAllClose(
            self.test_absorbSideSiteAtCounterClockwise.contract(corner.data,site.data),
            corner.absorbSideSiteAtCounterClockwise(site).data
        )

    test_absorbSideSiteAtCounterClockwise.contract = \
        formContractor(
            ['C','S'],
            [
                (('C',OperatorCornerSite.counterclockwise_index),('S',OperatorSideSite.clockwise_index)),
            ],
            [
                {"physical":[('C',OperatorCornerSite.physical_index),('S',OperatorSideSite.physical_index)]
                ,"physical_conjugate":[('C',OperatorCornerSite.physical_conjugate_index),('S',OperatorSideSite.physical_conjugate_index)]
                ,"counterclockwise":[('S',OperatorSideSite.counterclockwise_index)]
                ,"clockwise":[('C',OperatorCornerSite.clockwise_index),('S',OperatorSideSite.inward_index)]
                }[name] for name in OperatorCornerSite._dimensions
            ]
        )
    #@+node:gcross.20111103170337.1355: *4* formExpectationBoundary
    @with_checker(number_of_calls=10)
    def test_formExpectationBoundary(self):
        state = \
            StateCornerSite(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                physical_dimension = randint(1,5),
                randomize = True,
            )
        operator = \
            OperatorCornerSite(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                physical_dimension = state.physical_dimension,
                randomize = True,
            )
        self.assertAllClose(
            self.test_formExpectationBoundary.contract(state.data,operator.data,state.data.conj()),
            operator.formExpectationBoundary(state).data
        )

    test_formExpectationBoundary.contract = \
        formContractor(
            ['S','O','S*'],
            [
                [('S',StateCornerSite.physical_index),('O',OperatorCornerSite.physical_index)],
                [('S*',StateCornerSite.physical_index),('O',OperatorCornerSite.physical_conjugate_index)],
            ],
            [
                {"clockwise":[('S',StateCornerSite.clockwise_index),('O',OperatorCornerSite.clockwise_index),('S*',StateCornerSite.clockwise_index)]
                ,"counterclockwise":[('S',StateCornerSite.counterclockwise_index),('O',OperatorCornerSite.counterclockwise_index),('S*',StateCornerSite.counterclockwise_index)]
                }[name] for name in CornerBoundary._dimensions
            ]
        )
    #@-others
#@+node:gcross.20111103110300.1375: *3* OperatorSideSite
class TestOperatorSideSite(TestCase):
    #@+others
    #@+node:gcross.20111103110300.1385: *4* absorbCenterSite
    @with_checker(number_of_calls=100)
    def test_absorbCenterSite(self,direction=irange(0,3)):
        side = \
            OperatorSideSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        center = OperatorCenterSite(**{
            "physical_dimension": randint(1,5),
            "rightward_dimension": randint(1,5),
            "upward_dimension": randint(1,5),
            "leftward_dimension": randint(1,5),
            "downward_dimension": randint(1,5),
            "randomize": True,
            OperatorCenterSite._bandwidth_dimensions[direction]+"_dimension": side.inward_dimension
        })
        self.assertAllClose(
             tensordot(side.data,center.data,(side.inward_index,center.bandwidthIndex(direction)))
            .transpose(
                side.physical_index,
                center.physical_index+4,
                side.physical_conjugate_index,
                center.physical_conjugate_index+4,
                side.clockwise_index,
                center.bandwidthIndex(CW(direction))+4 - (1 if CW(direction) > direction else 0),
                side.counterclockwise_index,
                center.bandwidthIndex(CCW(direction))+4 - (1 if CCW(direction) > direction else 0),
                center.bandwidthIndex(OPP(direction))+4 - (1 if OPP(direction) > direction else 0),
             )
            .reshape(
                side.physical_dimension*center.physical_dimension,
                side.physical_dimension*center.physical_dimension,
                side.clockwise_dimension*center.bandwidthDimension(CW(direction)),
                side.counterclockwise_dimension*center.bandwidthDimension(CCW(direction)),
                center.bandwidthDimension(OPP(direction)),
             )
            ,side.absorbCenterSite(center,direction).data
        )
    #@+node:gcross.20111103110300.1380: *4* formExpectationBoundary
    @with_checker(number_of_calls=10)
    def test_formExpectationBoundary(self):
        state = \
            StateSideSite(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                physical_dimension = randint(1,5),
                randomize = True,
            )
        operator = \
            OperatorSideSite(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                physical_dimension = state.physical_dimension,
                randomize = True,
            )
        self.assertAllClose(
            self.test_formExpectationBoundary.contract(state.data,operator.data,state.data.conj()),
            operator.formExpectationBoundary(state).data
        )

    test_formExpectationBoundary.contract = \
        formContractor(
            ['S','O','S*'],
            [
                [('S',StateSideSite.physical_index),('O',OperatorSideSite.physical_index)],
                [('S*',StateSideSite.physical_index),('O',OperatorSideSite.physical_conjugate_index)],
            ],
            [
                {"clockwise":[('S',StateSideSite.clockwise_index),('O',OperatorSideSite.clockwise_index),('S*',StateSideSite.clockwise_index)]
                ,"counterclockwise":[('S',StateSideSite.counterclockwise_index),('O',OperatorSideSite.counterclockwise_index),('S*',StateSideSite.counterclockwise_index)]
                ,"inward_state":[('S',StateSideSite.inward_index)]
                ,"inward_operator":[('O',OperatorSideSite.inward_index)]
                ,"inward_state_conjugate":[('S*',StateSideSite.inward_index)]
                }[name] for name in ExpectationSideBoundary._dimensions
            ]
        )
    #@-others
#@+node:gcross.20111009193003.5240: *3* StateCornerSite
class TestStateCornerSite(TestCase):
    #@+others
    #@+node:gcross.20111013080525.1205: *4* absorbSideSiteAtCounterClockwise
    @with_checker(number_of_calls=10)
    def test_absorbSideSiteAtCounterClockwise(self):
        corner = \
            StateCornerSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                randomize = True,
            )
        site = \
            StateSideSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = corner.counterclockwise_dimension,
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        self.assertAllClose(
            self.test_absorbSideSiteAtCounterClockwise.contract(corner.data,site.data),
            corner.absorbSideSiteAtCounterClockwise(site).data
        )

    test_absorbSideSiteAtCounterClockwise.contract = \
        formContractor(
            ['C','S'],
            [
                (('C',StateCornerSite.counterclockwise_index),('S',StateSideSite.clockwise_index)),
            ],
            [
                {"physical":[('C',StateCornerSite.physical_index),('S',StateSideSite.physical_index)]
                ,"clockwise":[('C',StateCornerSite.clockwise_index),('S',StateSideSite.inward_index)]
                ,"counterclockwise":[('S',StateSideSite.counterclockwise_index)]
                }[name] for name in StateCornerSite._dimensions
            ]
        )
    #@+node:gcross.20111013080525.1209: *4* absorbSideSiteAtClockwise
    @with_checker(number_of_calls=10)
    def test_absorbSideSiteAtClockwise(self):
        corner = \
            StateCornerSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                randomize = True,
            )
        site = \
            StateSideSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = corner.clockwise_dimension,
                inward_dimension = randint(1,5),
                randomize = True,
            )
        self.assertAllClose(
            self.test_absorbSideSiteAtClockwise.contract(corner.data,site.data),
            corner.absorbSideSiteAtClockwise(site).data
        )

    test_absorbSideSiteAtClockwise.contract = \
        formContractor(
            ['C','S'],
            [
                (('C',StateCornerSite.clockwise_index),('S',StateSideSite.counterclockwise_index)),
            ],
            [
                {"physical":[('C',StateCornerSite.physical_index),('S',StateSideSite.physical_index)]
                ,"counterclockwise":[('C',StateCornerSite.counterclockwise_index),('S',StateSideSite.inward_index)]
                ,"clockwise":[('S',StateSideSite.clockwise_index)]
                }[name] for name in StateCornerSite._dimensions
            ]
        )
    #@+node:gcross.20111009193003.5241: *4* formNormalizationBoundary
    @with_checker(number_of_calls=10)
    def test_formNormalizationBoundary(self):
        site = \
            StateCornerSite(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                physical_dimension = randint(1,5),
                randomize = True,
            )
        self.assertAllClose(
            self.test_formNormalizationBoundary.contract(site.data,site.data.conj()),
            site.formNormalizationBoundary().data
        )

    test_formNormalizationBoundary.contract = \
        formContractor(
            ['S','S*'],
            [
                [('S'+c,StateCornerSite.physical_index) for c in ('','*')]
            ],
            [
                [('S'+c,getattr(StateCornerSite,name+'_index')) for c in ('','*')]
                for name in CornerBoundary._dimensions
            ]
        )
    #@-others
#@+node:gcross.20111009193003.1169: *3* StateSideSite
class TestStateSideSite(TestCase):
    #@+others
    #@+node:gcross.20111103110300.1383: *4* absorbCenterSite
    @with_checker(number_of_calls=100)
    def test_absorbCenterSite(self,direction=irange(0,3)):
        side = \
            StateSideSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        center = StateCenterSite(**{
            "physical_dimension": randint(1,5),
            "rightward_dimension": randint(1,5),
            "upward_dimension": randint(1,5),
            "leftward_dimension": randint(1,5),
            "downward_dimension": randint(1,5),
            "randomize": True,
            StateCenterSite._bandwidth_dimensions[direction]+"_dimension": side.inward_dimension
        })
        self.assertAllClose(
             tensordot(side.data,center.data,(side.inward_index,center.bandwidthIndex(direction)))
            .transpose(
                side.physical_index,
                center.physical_index+3,
                side.clockwise_index,
                center.bandwidthIndex(CW(direction))+3 - (1 if CW(direction) > direction else 0),
                side.counterclockwise_index,
                center.bandwidthIndex(CCW(direction))+3 - (1 if CCW(direction) > direction else 0),
                center.bandwidthIndex(OPP(direction))+3 - (1 if OPP(direction) > direction else 0),
             )
            .reshape(
                side.physical_dimension*center.physical_dimension,
                side.clockwise_dimension*center.bandwidthDimension(CW(direction)),
                side.counterclockwise_dimension*center.bandwidthDimension(CCW(direction)),
                center.bandwidthDimension(OPP(direction)),
             )
            ,side.absorbCenterSite(center,direction).data
        )
    #@+node:gcross.20111009193003.1170: *4* formNormalizationBoundary
    @with_checker(number_of_calls=10)
    def test_formNormalizationBoundary(self):
        site = \
            StateSideSite(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                physical_dimension = randint(1,5),
                randomize = True,
            )
        self.assertAllClose(
            self.test_formNormalizationBoundary.contract(site.data,site.data.conj()),
            site.formNormalizationBoundary().data
        )

    test_formNormalizationBoundary.contract = \
        formContractor(
            ['S','S*'],
            [
                [('S'+c,StateSideSite.physical_index) for c in ('','*')]
            ],
            [
                {"clockwise":[('S'+c,StateSideSite.clockwise_index) for c in ('','*')]
                ,"counterclockwise":[('S'+c,StateSideSite.counterclockwise_index) for c in ('','*')]
                ,"inward":[('S',StateSideSite.inward_index)]
                ,"inward_conjugate":[('S*',StateSideSite.inward_index)]
                }[name] for name in NormalizationSideBoundary._dimensions
            ]
        )
    #@-others
#@-others
#@-leo
