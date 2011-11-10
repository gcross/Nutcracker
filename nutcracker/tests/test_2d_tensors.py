#@+leo-ver=5-thin
#@+node:gcross.20111107154810.1418: * @file test_2d_tensors.py
#@+<< Imports >>
#@+node:gcross.20111107154810.1420: ** << Imports >>
from numpy import zeros

from .._2d.enumerations import *
from .._2d.tensors import *

from . import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111009193003.1168: ** Tests
#@+node:gcross.20111103170337.1371: *3* ExpectationSideBoundary
class TestExpectationSideBoundary(TestCase):
    #@+others
    #@+node:gcross.20111103170337.1372: *4* absorbCounterClockwiseCornerBoundary
    @prependContractor(
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
            }[name] for name in ExpectationSideBoundary.dimension_names
        ]
    )
    @with_checker(number_of_calls=10)
    def test_absorbCounterClockwiseCornerBoundary(contract,self):
        side = \
            ExpectationSideBoundary.random(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_state_dimension = randint(1,5),
                inward_operator_dimension = randint(1,5),
            )
        corner = \
            CornerBoundary.random(
                clockwise_dimension = side.counterclockwise_dimension,
                counterclockwise_dimension = side.counterclockwise_dimension,
            )
        self.assertAllClose(
            contract(side.data,corner.data),
            side.absorbCounterClockwiseCornerBoundary(corner).data
        )
    #@+node:gcross.20111103170337.1373: *4* absorbCounterClockwiseSideBoundary
    @prependContractor(
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
                }[name] for name in ExpectationSideBoundary.dimension_names
            ]
        )
    @with_checker(number_of_calls=10)
    def test_absorbCounterClockwiseSideBoundary(contract,self):
        side1 = \
            ExpectationSideBoundary.random(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_state_dimension = randint(1,5),
                inward_operator_dimension = randint(1,5),
            )
        side2 = \
            ExpectationSideBoundary.random(
                clockwise_dimension = side1.counterclockwise_dimension,
                counterclockwise_dimension = randint(1,5),
                inward_state_dimension = randint(1,5),
                inward_operator_dimension = randint(1,5),
            )
        self.assertAllClose(
            contract(side1.data,side2.data),
            side1.absorbCounterClockwiseSideBoundary(side2).data
        )
    #@-others
#@+node:gcross.20111009193003.5248: *3* NormalizationSideBoundary
class TestNormalizationSideBoundary(TestCase):
    #@+others
    #@+node:gcross.20111009193003.5250: *4* absorbCounterClockwiseCornerBoundary
    @prependContractor(
        ['S','C'],
        [
            (('C',CornerBoundary.clockwise_index),('S',NormalizationSideBoundary.counterclockwise_index)),
        ],
        [
            {"clockwise":[('S',NormalizationSideBoundary.clockwise_index)]
            ,"counterclockwise":[('C',CornerBoundary.counterclockwise_index)]
            ,"inward":[('S',NormalizationSideBoundary.inward_index)]
            ,"inward_conjugate":[('S',NormalizationSideBoundary.inward_conjugate_index)]
            }[name] for name in NormalizationSideBoundary.dimension_names
        ]
    )
    @with_checker(number_of_calls=10)
    def test_absorbCounterClockwiseCornerBoundary(contract,self):
        side = \
            NormalizationSideBoundary.random(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
            )
        corner = \
            CornerBoundary.random(
                clockwise_dimension = side.counterclockwise_dimension,
                counterclockwise_dimension = side.counterclockwise_dimension,
            )
        self.assertAllClose(
            contract(side.data,corner.data),
            side.absorbCounterClockwiseCornerBoundary(corner).data
        )
    #@+node:gcross.20111009193003.5264: *4* absorbCounterClockwiseSideBoundary
    @prependContractor(
        ['A','B'],
        [
            (('B',NormalizationSideBoundary.clockwise_index),('A',NormalizationSideBoundary.counterclockwise_index)),
        ],
        [
            {"clockwise":[('A',NormalizationSideBoundary.clockwise_index)]
            ,"counterclockwise":[('B',NormalizationSideBoundary.counterclockwise_index)]
            ,"inward":[(x,NormalizationSideBoundary.inward_index) for x in ('A','B')]
            ,"inward_conjugate":[(x,NormalizationSideBoundary.inward_conjugate_index) for x in ('A','B')]
            }[name] for name in NormalizationSideBoundary.dimension_names
        ]
    )
    @with_checker(number_of_calls=10)
    def test_absorbCounterClockwiseSideBoundary(contract,self):
        side1 = \
            NormalizationSideBoundary.random(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
            )
        side2 = \
            NormalizationSideBoundary.random(
                clockwise_dimension = side1.counterclockwise_dimension,
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
            )
        self.assertAllClose(
            contract(side1.data,side2.data),
            side1.absorbCounterClockwiseSideBoundary(side2).data
        )
    #@-others
#@+node:gcross.20111103170337.1353: *3* OperatorCornerSite
class TestOperatorCornerSite(TestCase):
    #@+others
    #@+node:gcross.20111103170337.1357: *4* absorbSideSiteAtClockwise
    @prependContractor(
        ['C','S'],
        [
            (('C',OperatorCornerSite.clockwise_index),('S',OperatorSideSite.counterclockwise_index)),
        ],
        [
            {"physical":[('C',OperatorCornerSite.physical_index),('S',OperatorSideSite.physical_index)]
            ,"physical_conjugate":[('C',OperatorCornerSite.physical_conjugate_index),('S',OperatorSideSite.physical_conjugate_index)]
            ,"counterclockwise":[('C',OperatorCornerSite.counterclockwise_index),('S',OperatorSideSite.inward_index)]
            ,"clockwise":[('S',OperatorSideSite.clockwise_index)]
            }[name] for name in OperatorCornerSite.dimension_names
        ]
    )
    @with_checker(number_of_calls=10)
    def test_absorbSideSiteAtClockwise(contract,self):
        corner = \
            OperatorCornerSite.random(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
            )
        site = \
            OperatorSideSite.random(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = corner.clockwise_dimension,
                inward_dimension = randint(1,5),
            )
        self.assertAllClose(
            contract(corner.data,site.data),
            corner.absorbSideSiteAtClockwise(site).data
        )
    #@+node:gcross.20111103170337.1361: *4* absorbSideSiteAtCounterClockwise
    @prependContractor(
        ['C','S'],
        [
            (('C',OperatorCornerSite.counterclockwise_index),('S',OperatorSideSite.clockwise_index)),
        ],
        [
            {"physical":[('C',OperatorCornerSite.physical_index),('S',OperatorSideSite.physical_index)]
            ,"physical_conjugate":[('C',OperatorCornerSite.physical_conjugate_index),('S',OperatorSideSite.physical_conjugate_index)]
            ,"counterclockwise":[('S',OperatorSideSite.counterclockwise_index)]
            ,"clockwise":[('C',OperatorCornerSite.clockwise_index),('S',OperatorSideSite.inward_index)]
            }[name] for name in OperatorCornerSite.dimension_names
        ]
    )
    @with_checker(number_of_calls=10)
    def test_absorbSideSiteAtCounterClockwise(contract,self):
        corner = \
            OperatorCornerSite.random(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
            )
        site = \
            OperatorSideSite.random(
                physical_dimension = randint(1,5),
                clockwise_dimension = corner.counterclockwise_dimension,
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
            )
        self.assertAllClose(
            contract(corner.data,site.data),
            corner.absorbSideSiteAtCounterClockwise(site).data
        )
    #@+node:gcross.20111103170337.1355: *4* formExpectationBoundary
    @prependContractor(
        ['S','O','S*'],
        [
            [('S',StateCornerSite.physical_index),('O',OperatorCornerSite.physical_index)],
            [('S*',StateCornerSite.physical_index),('O',OperatorCornerSite.physical_conjugate_index)],
        ],
        [
            {"clockwise":[('S',StateCornerSite.clockwise_index),('O',OperatorCornerSite.clockwise_index),('S*',StateCornerSite.clockwise_index)]
            ,"counterclockwise":[('S',StateCornerSite.counterclockwise_index),('O',OperatorCornerSite.counterclockwise_index),('S*',StateCornerSite.counterclockwise_index)]
            }[name] for name in CornerBoundary.dimension_names
        ]
    )
    @with_checker(number_of_calls=10)
    def test_formExpectationBoundary(contract,self):
        state = \
            StateCornerSite.random(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                physical_dimension = randint(1,5),
            )
        operator = \
            OperatorCornerSite.random(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                physical_dimension = state.physical_dimension,
            )
        self.assertAllClose(
            contract(state.data,operator.data,state.data.conj()),
            operator.formExpectationBoundary(state).data
        )
    #@-others
#@+node:gcross.20111103110300.1375: *3* OperatorSideSite
class TestOperatorSideSite(TestCase):
    #@+others
    #@+node:gcross.20111103110300.1385: *4* absorbCenterSite
    @with_checker(number_of_calls=100)
    def test_absorbCenterSite(self,direction=Direction):
        side = \
            OperatorSideSite.random(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
            )
        center = OperatorCenterSite.random(**{
            "physical_dimension": randint(1,5),
            "rightward_dimension": randint(1,5),
            "upward_dimension": randint(1,5),
            "leftward_dimension": randint(1,5),
            "downward_dimension": randint(1,5),
            OperatorCenterSite.bandwidth_dimension_names[direction]+"_dimension": side.inward_dimension
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
    @prependContractor(
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
            }[name] for name in ExpectationSideBoundary.dimension_names
        ]
    )
    @with_checker(number_of_calls=10)
    def test_formExpectationBoundary(contract,self):
        state = \
            StateSideSite.random(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                physical_dimension = randint(1,5),
            )
        operator = \
            OperatorSideSite.random(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                physical_dimension = state.physical_dimension,
            )
        self.assertAllClose(
            contract(state.data,operator.data,state.data.conj()),
            operator.formExpectationBoundary(state).data
        )
    #@-others
#@+node:gcross.20111109104457.1796: *3* StateCenterSite
class TestStateCenterSite(TestCase):
    #@+others
    #@+node:gcross.20111109104457.1798: *4* simpleObservable
    @with_checker
    def test_simpleObservation(self,physical_dimension=irange(1,10)):
        observation = randint(0,physical_dimension-1)
        state_site = StateCenterSite.simpleObservation(physical_dimension,observation)
        shape = [1]*StateCenterSite.number_of_dimensions
        shape[StateCenterSite.indexForName("physical")] = physical_dimension
        correct_data = zeros(physical_dimension)
        correct_data[observation] = 1
        correct_data = correct_data.reshape(shape)
        self.assertAllClose(state_site.data,correct_data)
    #@-others
#@+node:gcross.20111009193003.5240: *3* StateCornerSite
class TestStateCornerSite(TestCase):
    #@+others
    #@+node:gcross.20111013080525.1205: *4* absorbSideSiteAtCounterClockwise
    @prependContractor(
        ['C','S'],
        [
            (('C',StateCornerSite.counterclockwise_index),('S',StateSideSite.clockwise_index)),
        ],
        [
            {"physical":[('C',StateCornerSite.physical_index),('S',StateSideSite.physical_index)]
            ,"clockwise":[('C',StateCornerSite.clockwise_index),('S',StateSideSite.inward_index)]
            ,"counterclockwise":[('S',StateSideSite.counterclockwise_index)]
            }[name] for name in StateCornerSite.dimension_names
        ]
    )
    @with_checker(number_of_calls=10)
    def test_absorbSideSiteAtCounterClockwise(contract,self):
        corner = \
            StateCornerSite.random(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
            )
        site = \
            StateSideSite.random(
                physical_dimension = randint(1,5),
                clockwise_dimension = corner.counterclockwise_dimension,
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
            )
        self.assertAllClose(
            contract(corner.data,site.data),
            corner.absorbSideSiteAtCounterClockwise(site).data
        )
    #@+node:gcross.20111013080525.1209: *4* absorbSideSiteAtClockwise
    @prependContractor(
        ['C','S'],
        [
            (('C',StateCornerSite.clockwise_index),('S',StateSideSite.counterclockwise_index)),
        ],
        [
            {"physical":[('C',StateCornerSite.physical_index),('S',StateSideSite.physical_index)]
            ,"counterclockwise":[('C',StateCornerSite.counterclockwise_index),('S',StateSideSite.inward_index)]
            ,"clockwise":[('S',StateSideSite.clockwise_index)]
            }[name] for name in StateCornerSite.dimension_names
        ]
    )
    @with_checker(number_of_calls=10)
    def test_absorbSideSiteAtClockwise(contract,self):
        corner = \
            StateCornerSite.random(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
            )
        site = \
            StateSideSite.random(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = corner.clockwise_dimension,
                inward_dimension = randint(1,5),
            )
        self.assertAllClose(
            contract(corner.data,site.data),
            corner.absorbSideSiteAtClockwise(site).data
        )
    #@+node:gcross.20111009193003.5241: *4* formNormalizationBoundary
    @prependContractor(
        ['S','S*'],
        [
            [('S'+c,StateCornerSite.physical_index) for c in ('','*')]
        ],
        [
            [('S'+c,getattr(StateCornerSite,name+'_index')) for c in ('','*')]
            for name in CornerBoundary.dimension_names
        ]
    )
    @with_checker(number_of_calls=10)
    def test_formNormalizationBoundary(contract,self):
        site = \
            StateCornerSite.random(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                physical_dimension = randint(1,5),
            )
        self.assertAllClose(
            contract(site.data,site.data.conj()),
            site.formNormalizationBoundary().data
        )
    #@-others
#@+node:gcross.20111009193003.1169: *3* StateSideSite
class TestStateSideSite(TestCase):
    #@+others
    #@+node:gcross.20111103110300.1383: *4* absorbCenterSite
    @with_checker(number_of_calls=100)
    def test_absorbCenterSite(self,direction=Direction):
        side = \
            StateSideSite.random(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
            )
        center = StateCenterSite.random(**{
            "physical_dimension": randint(1,5),
            "rightward_dimension": randint(1,5),
            "upward_dimension": randint(1,5),
            "leftward_dimension": randint(1,5),
            "downward_dimension": randint(1,5),
            StateCenterSite.bandwidth_dimension_names[direction]+"_dimension": side.inward_dimension
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
    @prependContractor(
        ['S','S*'],
        [
            [('S'+c,StateSideSite.physical_index) for c in ('','*')]
        ],
        [
            {"clockwise":[('S'+c,StateSideSite.clockwise_index) for c in ('','*')]
            ,"counterclockwise":[('S'+c,StateSideSite.counterclockwise_index) for c in ('','*')]
            ,"inward":[('S',StateSideSite.inward_index)]
            ,"inward_conjugate":[('S*',StateSideSite.inward_index)]
            }[name] for name in NormalizationSideBoundary.dimension_names
        ]
    )
    @with_checker(number_of_calls=10)
    def test_formNormalizationBoundary(contract,self):
        site = \
            StateSideSite.random(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                physical_dimension = randint(1,5),
            )
        self.assertAllClose(
            contract(site.data,site.data.conj()),
            site.formNormalizationBoundary().data
        )
    #@-others
#@-others
#@-leo
