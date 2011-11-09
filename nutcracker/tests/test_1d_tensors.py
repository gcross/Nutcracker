#@+leo-ver=5-thin
#@+node:gcross.20111107131531.1333: * @file test_1d_tensors.py
#@+<< Imports >>
#@+node:gcross.20111107131531.1334: ** << Imports >>
from numpy import identity, tensordot, zeros
from paycheck import *
from random import randint

from .._1d.enumerations import *
from .._1d.tensors import *
from ..utils import *

from . import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111107131531.1347: ** Tests
#@+node:gcross.20111107131531.1348: *3* TestLeftExpectationBoundary
class TestLeftExpectationBoundary(TestCase):
    #@+others
    #@+node:gcross.20111107131531.1349: *4* absorb
    @with_checker
    def test_absorb(self,
        physical_dimension=irange(1,4),
        left_state_dimension=irange(1,4),
        right_state_dimension=irange(1,4),
        left_operator_dimension=irange(1,4),
        right_operator_dimension=irange(1,4),
    ):
        L = LeftExpectationBoundary.random(
            state_dimension = left_state_dimension,
            operator_dimension = left_operator_dimension,
        )
        O = OperatorSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_operator_dimension,
            right_dimension = right_operator_dimension,
        )
        S = StateSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_state_dimension,
            right_dimension = right_state_dimension,
        )
        self.assertAllClose(
            L.absorb(O,S).data,
            self.test_absorb.contract(L.data,O.formDenseTensor(),S.data,S.data.conj())
        )

    test_absorb.contract = formContractor(
        ['L','O','S','S*'],
        [(('L',LeftExpectationBoundary.operator_index),('O',OperatorSite.left_index))
        ,(('L',LeftExpectationBoundary.state_index),('S',StateSite.left_index))
        ,(('L',LeftExpectationBoundary.state_conjugate_index),('S*',StateSite.left_index))
        ,(('O',OperatorSite.physical_index),('S',StateSite.physical_index))
        ,(('O',OperatorSite.physical_conjugate_index),('S*',StateSite.physical_index))
        ],
        [{'state':[('S',StateSite.right_index)]
         ,'state_conjugate':[('S*',StateSite.right_index)]
         ,'operator':[('O',OperatorSite.right_index)]
         }[name] for name in LeftExpectationBoundary._dimensions
        ]
    )
    #@-others
#@+node:gcross.20111108100704.1438: *3* TestLeftOverlapBoundary
class TestLeftOverlapBoundary(TestCase):
    #@+others
    #@+node:gcross.20111108100704.1439: *4* absorb
    @with_checker
    def test_absorb(self,
        physical_dimension=irange(1,4),
        left_state_dimension=irange(1,4),
        right_state_dimension=irange(1,4),
        left_overlap_dimension=irange(1,4),
        right_overlap_dimension=irange(1,4),
    ):
        L = LeftOverlapBoundary.random(
            state_dimension = left_state_dimension,
            overlap_dimension = left_overlap_dimension,
        )
        V = OverlapSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_overlap_dimension,
            right_dimension = right_overlap_dimension,
        )
        S = StateSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_state_dimension,
            right_dimension = right_state_dimension,
        )
        self.assertAllClose(
            L.absorb(V,S).data,
            self.test_absorb.contract(L.data,V.data,S.data)
        )

    test_absorb.contract = formContractor(
        ['L','V','S'],
        [(('L',LeftOverlapBoundary.overlap_index),('V',OverlapSite.left_index))
        ,(('L',LeftOverlapBoundary.state_index),('S',StateSite.left_index))
        ,(('V',OverlapSite.physical_index),('S',StateSite.physical_index))
        ],
        [{'state':[('S',StateSite.right_index)]
         ,'overlap':[('V',OverlapSite.right_index)]
         }[name] for name in LeftOverlapBoundary._dimensions
        ]
    )
    #@-others
#@+node:gcross.20111107131531.3583: *3* TestRightExpectationBoundary
class TestRightExpectationBoundary(TestCase):
    #@+others
    #@+node:gcross.20111107131531.3584: *4* absorb
    @with_checker
    def test_absorb(self,
        physical_dimension=irange(1,4),
        left_state_dimension=irange(1,4),
        right_state_dimension=irange(1,4),
        left_operator_dimension=irange(1,4),
        right_operator_dimension=irange(1,4),
    ):
        R = RightExpectationBoundary.random(
            state_dimension = right_state_dimension,
            operator_dimension = right_operator_dimension,
        )
        O = OperatorSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_operator_dimension,
            right_dimension = right_operator_dimension,
        )
        S = StateSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_state_dimension,
            right_dimension = right_state_dimension,
        )
        self.assertAllClose(
            R.absorb(O,S).data,
            self.test_absorb.contract(R.data,O.formDenseTensor(),S.data,S.data.conj())
        )

    test_absorb.contract = formContractor(
        ['R','O','S','S*'],
        [(('R',RightExpectationBoundary.operator_index),('O',OperatorSite.right_index))
        ,(('R',RightExpectationBoundary.state_index),('S',StateSite.right_index))
        ,(('R',RightExpectationBoundary.state_conjugate_index),('S*',StateSite.right_index))
        ,(('O',OperatorSite.physical_index),('S',StateSite.physical_index))
        ,(('O',OperatorSite.physical_conjugate_index),('S*',StateSite.physical_index))
        ],
        [{'state':[('S',StateSite.left_index)]
         ,'state_conjugate':[('S*',StateSite.left_index)]
         ,'operator':[('O',OperatorSite.left_index)]
         }[name] for name in RightExpectationBoundary._dimensions
        ]
    )
    #@-others
#@+node:gcross.20111108100704.1442: *3* TestRightOverlapBoundary
class TestLeftOverlapBoundary(TestCase):
    #@+others
    #@+node:gcross.20111108100704.1443: *4* absorb
    @with_checker
    def test_absorb(self,
        physical_dimension=irange(1,4),
        left_state_dimension=irange(1,4),
        right_state_dimension=irange(1,4),
        left_overlap_dimension=irange(1,4),
        right_overlap_dimension=irange(1,4),
    ):
        R = RightOverlapBoundary.random(
            state_dimension = right_state_dimension,
            overlap_dimension = right_overlap_dimension,
        )
        V = OverlapSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_overlap_dimension,
            right_dimension = right_overlap_dimension,
        )
        S = StateSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_state_dimension,
            right_dimension = right_state_dimension,
        )
        self.assertAllClose(
            R.absorb(V,S).data,
            self.test_absorb.contract(R.data,V.data,S.data)
        )

    test_absorb.contract = formContractor(
        ['R','V','S'],
        [(('R',RightOverlapBoundary.overlap_index),('V',OverlapSite.right_index))
        ,(('R',RightOverlapBoundary.state_index),('S',StateSite.right_index))
        ,(('V',OverlapSite.physical_index),('S',StateSite.physical_index))
        ],
        [{'state':[('S',StateSite.left_index)]
         ,'overlap':[('V',OverlapSite.left_index)]
         }[name] for name in RightOverlapBoundary._dimensions
        ]
    )
    #@-others
#@+node:gcross.20111107131531.3591: *3* TestStateSite
class TestStateSite(TestCase):
    #@+others
    #@+node:gcross.20111108100704.1372: *4* formNormalizedOverlapSites
    @with_checker
    def test_formNormalizedOverlapSites(self,
        left_physical_dimension = irange(2,4),
        right_physical_dimension = irange(2,4),
        leftmost_bandwidth_dimension = irange(2,4),
        middle_bandwidth_dimension = irange(2,4),
        rightmost_bandwidth_dimension = irange(2,4),
    ):
        unnormalized_left_state_site = StateSite.random(
            physical_dimension = left_physical_dimension,
            left_dimension = leftmost_bandwidth_dimension,
            right_dimension = middle_bandwidth_dimension,
        )
        right_normalized_right_state_site = StateSite.random(
            physical_dimension = right_physical_dimension,
            left_dimension = middle_bandwidth_dimension,
            right_dimension = rightmost_bandwidth_dimension,
        )
        result = unnormalized_left_state_site.formNormalizedOverlapSites(right_normalized_right_state_site)
        self.assertAllClose(
            result.unnormalized_left_overlap_site.data,
            unnormalized_left_state_site.data.transpose(2,0,1).conj()
        )
        self.assertAllClose(
            result.right_normalized_right_overlap_site.data,
            right_normalized_right_state_site.data.transpose(2,0,1).conj()
        )
        self.assertAllClose(
            tensordot(
                unnormalized_left_state_site.data,
                right_normalized_right_state_site.data,
                (StateSite.right_index,StateSite.left_index)
            ),
            tensordot(
                result.left_normalized_left_overlap_site.data.conj().transpose(1,2,0),
                result.unnormalized_right_state_site.data,
                (StateSite.right_index,StateSite.left_index)
            )
        )
        self.assertNormalized(result.left_normalized_left_overlap_site.data,OverlapSite.right_index)
    #@+node:gcross.20111107131531.5850: *4* formOverlapSite
    @with_checker
    def test_formOverlapSite(self,
        physical_dimension = irange(1,5),
        left_dimension = irange(1,5),
        right_dimension = irange(1,5)
    ):
        state_site = StateSite.random(
            physical_dimension = physical_dimension,
            left_dimension = left_dimension,
            right_dimension = right_dimension,
        )
        self.assertAllClose(
            state_site.formOverlapSite().data,
            state_site.data.conj().transpose(2,0,1)
        )
    #@+node:gcross.20111107131531.3593: *4* normalizeAndDenormalize
    @with_checker
    def test_normalizeAndDenormalize(self,
        direction=irange(0,1),
        leftmost_dimension=irange(3,9),
        middle_dimension=irange(3,9),
        rightmost_dimension=irange(3,9),
    ):
        direction = Direction.values()[direction]
        left_site = StateSite.random(
            physical_dimension = 9,
            left_dimension = leftmost_dimension,
            right_dimension = middle_dimension,
        )
        right_site = StateSite.random(
            physical_dimension = 9,
            left_dimension = middle_dimension,
            right_dimension = rightmost_dimension,
        )
        if direction == Direction.right:
            new_right_site, new_left_site = right_site.normalizeAndDenormalize(direction,left_site)

        else:
            new_left_site, new_right_site = left_site.normalizeAndDenormalize(direction,right_site)
        self.assertEqual(new_left_site.data.shape,left_site.data.shape)
        self.assertEqual(new_right_site.data.shape,right_site.data.shape)
        self.assertAllClose(
            tensordot(new_left_site.data,new_right_site.data,(StateSite.right_index,StateSite.left_index)),
            tensordot(left_site.data,right_site.data,(StateSite.right_index,StateSite.left_index)),
        )
        if direction == Direction.right:
            self.assertNormalized(new_right_site.data,StateSite.left_index)
        else:
            self.assertNormalized(new_left_site.data,StateSite.right_index)
    #@+node:gcross.20111108100704.1419: *4* random (left normalized)
    @with_checker
    def test_random_left_normalized(self,
        physical_dimension = irange(3,9),
        left_dimension = irange(3,9),
        right_dimension = irange(3,9)
    ):
        state_site = StateSite.random(
            normalization = Normalization.left,
            physical_dimension = physical_dimension,
            left_dimension = left_dimension,
            right_dimension = right_dimension,
        )
        self.assertNormalized(state_site.data,StateSite.right_index)
    #@+node:gcross.20111108100704.1383: *4* random (middle normalized)
    @with_checker
    def test_random_middle_normalized(self,
        physical_dimension = irange(3,9),
        left_dimension = irange(3,9),
        right_dimension = irange(3,9)
    ):
        state_site = StateSite.random(
            normalization = Normalization.middle,
            physical_dimension = physical_dimension,
            left_dimension = left_dimension,
            right_dimension = right_dimension,
        )
        self.assertAlmostEqual(norm(state_site.data),1)
    #@+node:gcross.20111108100704.1423: *4* random (right normalized)
    @with_checker
    def test_random_right_normalized(self,
        physical_dimension = irange(3,9),
        left_dimension = irange(3,9),
        right_dimension = irange(3,9)
    ):
        state_site = StateSite.random(
            normalization = Normalization.right,
            physical_dimension = physical_dimension,
            left_dimension = left_dimension,
            right_dimension = right_dimension,
        )
        self.assertNormalized(state_site.data,StateSite.left_index)
    #@+node:gcross.20111109104457.1793: *4* simpleObservable
    @with_checker
    def test_simpleObservation(self,physical_dimension=irange(1,10)):
        observation = randint(0,physical_dimension-1)
        state_site = StateSite.simpleObservation(physical_dimension,observation)
        shape = [1]*StateSite.number_of_dimensions
        shape[StateSite.indexForName("physical")] = physical_dimension
        correct_data = zeros(physical_dimension)
        correct_data[observation] = 1
        correct_data = correct_data.reshape(shape)
        self.assertAllClose(state_site.data,correct_data)
    #@-others
#@-others
#@-leo
